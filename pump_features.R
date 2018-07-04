# The idea here is to build some kind of windowed analysis that allows us
# to identify heuristics or sequences indicative of an upcoming spike.
# We're probably going to have to use a random forest to tell though

suppressWarnings(library(zoo))
library(TTR)
reload_coins = FALSE

plot.candles <- function(candles, mas=list(), start=1, end=60, trades=c(), separate.plot=F) {
  if (separate.plot) {
    par(mfrow=c(2,1))
  } else {
    par(mfrow=c(1,1))
  }
  cc <- candles[start:end,]
  if (length(trades)){
    trades <- trades[start:end]
  }
  with(cc, symbols(time, 
                   (open+close)/2, 
                   boxplots=cbind(35,
                                  abs(close-open), 
                                  abs(open-low),
                                  abs(high-close),
                                  0),
                   inches=F,
                   ylim = c(min(low), max(high)),
                   xaxt="n",
                   ylab="", 
                   bg=(close-open>0)+2
  ))
  axis(1,seq_along(cc$time),labels=1:length(cc$time))
  lines(cc$time, cc$price)
  
  # a "1" in trades means buy (sma is now > ema, g),
  # a "-1" in trades means sell (sma is now < ema, r)
  if (length(trades)) {
    for (i in 1:length(trades)) {
      if (trades[i] == 1) {
        abline(v=cc$time[i], col=2, lwd=2, lty=2)
      } else if (trades[i] == -1) {
        abline(v=cc$time[i], col=3, lwd=2, lty=2)
      }
    }
  }
  
  color <- 4
  if (length(mas)) {
    for (i in 1:length(mas)) {
      if (separate.plot && i==1) {
        plot(cc$time, mas[[i]][start:end], col=color, lwd=3, type='l', ylim=c(-1,1))
      } else {
        lines(cc$time, mas[[i]][start:end], col=color, lwd=3)
      }
      color <- color+1
    }
    lines(cc$time, mas[[1]][start:end]-mas[[2]][start:end], type='h', col=(abs(mas[[1]][start:end]-mas[[2]][start:end])>0.4)+1)
  }
}


library(zoo)
#library(moments)
#library(Rwave)
#library(randomForest)
#library(party)

#load(coins, "coins.RData")

lag=1
win.size=30
buffer=100
process=TRUE


get_seq <- function(coin, from.start=0, from.end=0) {
  x <- coin$x
  start <- coin$seq$x[1]
  end <- coin$seq$x[2]
  idx <-x$time >= start & x$time <= end
  if (from.start == 0 && from.end == 0) {
    iidx <- 1:sum(idx)
  } else if (from.start != 0) {
      iidx <- 1:from.start
  } else {
      iidx <- (sum(idx)-from.end):sum(idx)
  }
  return(list(x=x[idx,][iidx,], rvrp=coin$rvrp[idx][iidx], low=coin$low[idx][iidx]))
}

# Compute the pct increase/decrease of a series
pct_diff <- function(x, lag=1) {
  dd <- c(rep(0, lag), diff(x, lag=lag))
  return(dd/x)
}

# Compute the ratio of volume change to price change
# Lag - the amount of look back to diff prices on
# win.size - the # of minutes to compute over
# buffer - the # of minutes to throw out at the front to ensure we don't get false signals
# 
# This indicator is much faster to compute than pc, but may not be as robust.
# Needs testing...
rvrp.fun <- function(x, lag=1, win.size=30, buffer=100) {
  
  # THIS IS THE PART I COMMENTED OUT TO MAKE THE NA RVRP WARNING GO AWAY.
  # CONSIDER PUTTING THIS BACK IF PERFORMANCE IS LOW
  # Update 7/4 - I got rid of 0 entries in the DB, so I'm hoping this helps and we can go back to this
  diff.price <- c(diff(x$price.n, lag=lag), rep(0,lag))
  pct.price <- diff.price/x$price.n
  
  #diff.price <- c(diff(x$price, lag=lag), rep(0,lag))
  #pct.price <- diff.price/x$price

  cum.vol <- cumsum(x$nmvol)
  pct.vol <- x$nmvol/cum.vol
  
  # Clear out the NaNs that occur because cumsum is 0 at first
  idx <- which(cum.vol != 0)
  if (!length(idx)) {
    #Uh oh, we never have any volume. Bail
    rvrp <- rep(0, nrow(x))
    return(rvrp)
  }
  pct.vol[1:idx[1]] <- 0
  
  # TODO: There's a rounding error when we read data in from the db
  # which is going to be a problem. Decimals are getting rounded, which
  # can create 0's in pct.vol in the case of a chance cumsum == 0. 
  # 
  # This needs to be fixed elsewhere too as I can only imagine what other
  # satoshi issues will crop up around this...
  # For now, just make sure to deal with those cases.
  
  pct.vol[is.infinite(pct.vol)] <- 0
  rp <- c(jitter(rep(0.0001, win.size-1)), rollsum(pct.price, win.size))
  rv <- c(jitter(rep(0.0001, win.size-1)), rollsum(pct.vol, win.size))
  if (any(is.na(rp)) || any(is.na(rv))) {
     # uh oh, something went wrong
     cat("Found NAs in rvrp for this coin. Exiting.\n")
     rvrp <- rep(0, nrow(x))
     return(rvrp)
  }  
  idx <- rp == 0
  rvrp <- rv/rp
  if (length(idx)) {
    #rvrp[idx] <- 1e20 # This might be what's throwing things off
    rvrp[idx] <- 0
  }
  
  negs <- rvrp <= 0
  if (length(negs)) {
    rvrp[negs] <- rvrp[negs]*-1
  }
  rvrp[rvrp!=0] <- log10(rvrp[rvrp!=0])
  
  if (length(negs)) {
    rvrp[negs] <- rvrp[negs]*-1
  }
  return(rvrp)
}

# See how many windows have low values, but >0 values
low_pump <- function(rvrp, win.size=30, max_x=8, min_x=2) {
  lil <- c(rep(0, win.size-1), rollapply(as.zoo(rvrp), win.size, function(x) { sum(x < max_x & x > min_x)}))
  return(lil)
}

find_closest <- function(x, val) {
  y <- abs(x-val)
  return (which(y == min(y))[1])
}

# Run a linear regression through a set of the points and return the slope
vol_check <- function(x) {
  idx <- 1:length(x)
  a <- lm(x~idx)$coefficients[2]
  return(a)
}

# Just cumulative sum the window
cumsum_check <- function(x) {
  return(cumsum(x))
}

# Run a linear regression through the cumulative sum points and return the slope
cvol_check <- function(x) {
  idx <- 1:(length(x))
  a <- lm(cumsum(x)~idx)$coefficients[2]
  return(a)
}


ttc.pct.gain <- function(x, pct_gain=0.11) {
  ttc <- rep(0, nrow(x))
  for (j in 1:nrow(x)) {
    target <- x$price[j]*(1+pct_gain)
    hits <- which(x$price[(j+1):nrow(x)] >= target)
    if (!length(hits)) {
      ttc[j] <- -100
    } else {
      ttc[j] <- hits[1]
    }
  }
  return(ttc)
}

# Do linear regression through price and vol values and compare the ratio
# to find spikes
pump_check <- function(x, from=1, to=-1, win.size=100) {
  nr = nrow(x)
  if (nr == 0) {
    return(c())
  }
  if (from != 1 || to !=-1) {
    x <- x[from:to,]
  } else {
    x <- x[1:nr,]
  }
  eps = 0.000001
  
  rp <- c(rep(1, win.size-1), rollapply(as.zoo(x$close), win.size, vol_check))
  rv <- c(rep(1, win.size-1), rollapply(as.zoo(x$nmvol), win.size, cvol_check))
  
  #rv[rv < 0] = 0   # Why did I used to have this? I can't recall...
  rv[rv == 0] = eps
  rp[rp == 0] = eps
  rvrp <- rv/rp
  
  #negs <- rvrp < 0
  #rvrp <- abs(rvrp)
  #rvrp[rvrp==0] <- eps
  #log.rvrp <- log(rvrp)
  #log.rvrp[negs] <- log.rvrp[negs]*-1
  #rvrp[negs] <- rvrp[negs]*-1
  
  # Uh....we do this twice....?
  negs <- rvrp <= 0
  if (length(negs)) {
    rvrp[negs] <- rvrp[negs]*-1
  }
  rvrp[rvrp!=0] <- log10(rvrp[rvrp!=0])
  
  if (length(negs)) {
    rvrp[negs] <- rvrp[negs]*-1
  }
  
  rvrp[1:(win.size-1)] <- 0
  return(rvrp)
}

# ACF indicator
acf.it <- function(x) {
  aa <- acf(x, plot=F)
  idx <- which(aa$acf < 0.06)
  f <- 0
  if(length(idx) == 0) {
    f <- length(aa$acf)
  } else {
    f <- idx[1]
  }
  return(f)
}


# divide all data by existing btc prices and turn volumes neg/pos
normalize_data <- function(x, btc) {
  x$nmvol <- x$volume_to
  negs <- x$close < x$open
  x$nmvol[negs] <- x$nmvol[negs]*-1 # make the ones where close < open negative
  btcp <- btc$price[match(x$time, btc$time)]
  if (is.na(btcp[1])) {
    btcp[1] <- btcp[which(!is.na(btcp))[1]]
  }
  if (is.na(btcp[length(btcp)])) {
    idx <- which(!is.na(btcp))
    btcp[length(btcp)] <- btcp[idx[length(idx)]]
  }
  btcp <- na.approx(btcp)
  x$price.n <- x$price * btcp
  return(x)
}


########################################################################
########################################################################
####################     LOAD THE COINS     ############################
########################################################################
########################################################################
# Load it up! Be careful not to load empty coins or weird, other csvs

load.coins <- function() {
  
  coins <- list()
  btc <- read.table("btc.csv", h=T, sep="\t", as.is=T)
  
  f = dir(pattern="*.csv")
  for (i in 1:length(f)) {
    coin <- substr(f[i], 1,(nchar(f[i])-4))
    if (toupper(coin) != coin) {
      next
    }
    if (coin == "BCC" || coin == "CRB" || coin == "MYR" || coin=="XBB" || coin == "new_spikes" || coin == "btc") { # These crashed because they had no data
      next
    }
    if (file.info(f[i])$size == 0) {
      print (paste(coin, "has no data in it. See if it's taken off the market?"))
      next
    }
    print(coin)
    
    if (reload_coins) {
      x <- read.table(f[i], sep="\t", h=T, as.is=T)
      if (nrow(x) < 180) {
        next
      } 
      x <- normalize_data(x, btc)
      coins[[coin]]$x <- x
      coins[[coin]]$coin <- coin
    }
  }
  return(coins)
}
  
  

########################################################################
########################################################################
####################     ADD THE FEATURES     ###########################
########################################################################
########################################################################

add.rsi <- function(coin, win.size=180) {
  rsi <- RSI(coin$x$price.n, n=win.size)
  rsi[is.na(rsi)] <- 0
  return(rsi)
}

add.rvrp <- function(coin, win.size=30, lag=1) {
  rvrp.fun(coin$x, win.size=win.size, lag=lag)
}

add.sd.rvrp <- function(coin, win.size=30) {
  coins[[coin]]$sd.rvrp <- c(jitter(rep(0,win.size)), rollapply(as.zoo(coins[[coin]]$rvrp), win.size+1, sd))
}

add.sd.rvrp.alarm <- function(coin) {
  
}

add.low <- function(coin, win.size=30, max_x=8, min_x=2) {
  coins[[coin]]$low <- low_pump(coins[[coin]]$rvrp, win.size=win.size, max_x=max_x, min_x=min_x)
}

add.low.alarm <- function(coin, thresh=30) {
  alarm.low <- rep(0, length(coins[[coin]]$low))
  for (j in 2:length(coins[[coin]]$low)) {
    if (coins[[coin]]$low[j]==thresh) {
      alarm.low[j] <- alarm.low[j-1]+1
    } else {
      alarm.low[j] <- max(0,alarm.low[j-1]-2)
    }
  }
  coins[[coin]]$alarm.low <- alarm.low
}

add.pc <- function(coin, win.size=30) {
  coins[[coin]]$pc <- pump_check(coins[[coin]]$x, win.size=win.size)
}

add.gabor <- function(coin, nvoice=10, scale=30) {
  m <- coins[[coin]]$rvrp
  m <- m - min(m)
  m <- m / max(m)
  coins[[coin]]$rvrp.g <- cgt(m, nvoice, scale, F)

  #m <- coins[[coin]]$low
  #m <- m - min(m)
  #m <- m / max(m)
  #coins[[coin]]$low.g <- cgt(m, 10, 0.001, 30, F)
  #coins[[coin]]$low.g <- cgt(m, nvoice, scale, F)}
}

add.acf <- function(coin, win.size=300, by=30, zero.buffer=3) {
  coins[[coin]]$acf <- c(rep(0,zero.buffer), rollapply(as.zoo(coins[[coin]]$rvrp), win.size, acf.it, by=by))
}

add.acf.alarm <- function(coin, thresh=32) {
  
  alarm <- rep(0, length(coins[[coin]]$acf))
  for (j in 2:length(coins[[coin]]$acf)) {
    if (coins[[coin]]$acf[j] >= thresh) {
      alarm[j] <- alarm[j-1]+1
    } else {
      alarm[j] <- max(0, alarm[j-1]-2)
    }
  }
  # Try expanding the acf alarm likeso:
  # NOTE: THIS FIGURE HAS TO COME FROM ACF. ACF SHOULD HAVE BEEN FIXED FIRST
  # SO YOU DON'T HAVE TO EXPAND HERE
   # idx <- 1:length(alarm)
   # idx3 <- rep(idx, eac=30)
   # new.alarm <- alarm[idx3]
   # new.alarm <- c(rep(0, nrow(coins[[coin]]$x)-length(new.alarm)), new.alarm)
   #coins[[coin]]$alarm.acf <- new.alarm
}

add.ttc.pct.gain <- function(coin, pct_gain=0.2) {
  ttc.pct.gain(coin$x, pct_gain=pct_gain)
}


add.ttc.daily.gain <- function(coin, time.span=24) {
  x <- coin$x
  add.ttc.daily.gain2(x, time.span)
  
}

add.ttc.daily.gain2 <- function(x, time.span=24) {
  
  ttc <- rep(0, nrow(x))
  ttp <- rep(0, nrow(x))
  
  ii <- 2
  ts <- time.span*60
  jj <- min(ts, nrow(x))
  nr <- nrow(x) - 1
  xp <- x$price
  mx <- max(xp[ii:jj])[1]
  wm <- which(xp[ii:jj]==mx)[1]
  while(ii < nr-2) {
    if( xp[ii-1] >= mx ) {
      mx <- max(xp[ii:jj])
      wm <- which(xp[ii:jj]==mx)[1]
    }
    if (xp[jj] >= mx) {
      mx <- xp[jj]
      wm <- ts
    }
#    if (wm == 1) {
#      # Everything after this point is lower, so we want to register a negative label
#      mx <- min(xp[ii:jj])
#    }
    ttc[ii] <- (mx-xp[ii])/xp[ii]
    ttp[ii] <- wm
    ii <- ii+1
    jj <- min(jj+1, nr)
    wm <- wm-1
  } 
  
  return(list(ttc=ttc, ttp=ttp))
}

add.pdiff <- function(coin, lag=60) {
  coins[[coin]]$pdiff <- pct_diff(coins[[coin]]$x$price, lag=lag)
}


add.tmf <- function(coin, ema1.n=21, ema2.n=21) {
  hh <- coin$x$high
  ll <- coin$x$low
  oo <- coin$x$open
  cc <- coin$x$close
  vv <- coin$x$volume_to

  ad <- rep(0, length(hh))
  for (j in 2:length(hh)) {
    trh <- max(hh[j], cc[j-1])
    trl <- min(ll[j], oo[j-1])
    denom <- (trh-trl)*vv[j]
    if (denom == 0) {
      denom = 1e-5
    }
    adx <- ((cc[j]-trl)-(trh-cc[j])) / denom
    ad[j] <- adx
  }

  ad <- EMA(ad, n=ema1.n, wilder=T)
  vd <- EMA(vv, n=ema2.n, wilder=T)
  advd <- ad/vd
  advd[is.na(advd)] <- 0
  return(advd)
  
  # new.advd.alarm <- rep(0, length(new.advd))
  # for (j in 2:length(new.advd)) {
  #   if (new.advd[j] < 0) {
  #     new.advd.alarm[j] <- new.advd.alarm[j-1]+2
  #   } else if (new.advd[j] > 0) {
  #     new.advd.alarm[j] <- 0
  #   }
  # }
  #coin$tmf.alarm <- new.advd.alarm
}

add.whisker1 <- function(coin, win.size=5, vwap.n=21) {
  eps <- 1e-10
  hh <- rollapply(coin$x$high, win.size, max, by=win.size)
  ll <- rollapply(coin$x$low, win.size, min, by=win.size)
  oo <- coin$x$open[seq(1,nrow(coin$x), by=win.size)]
  cc <- coin$x$close[seq(1,nrow(coin$x), by=win.size)]
  pp <- coin$x$price[seq(1,nrow(coin$x), by=win.size)]
  vv <- rollapply(coin$x$volume_to, win.size, sum, by=win.size)
  flat <- rep(0, length(hh))
  for (j in 2:length(hh)) {
    candle.size <- max(eps,abs(cc[j]-oo[j]))
    whisker.size <- max(eps, abs(hh[j]-ll[j]))
    step <- max(eps, abs(pp[j]-pp[j-1]))

    flat[j] <- 1/(candle.size+whisker.size+step)
  }

  idx2 <- 1:length(flat)
  idx3 <- rep(idx2, each=win.size)
  flat <- flat[idx3]
  flat <- c(rep(0, nrow(coin$x)-length(flat)), flat)

  idx <- 1:length(vv)
  idx3 <- rep(idx2, each=win.size)
  vv <- vv[idx3]
  vv <- c(rep(0, nrow(coin$x)-length(vv)),vv)
  ad <- VWAP(flat, vv, n=21, wilder=T)
  ad[is.na(ad)] <- 0
  return(ad)
}

add.whisker1.alarm <- function(coin, thresh=0) {
  ad <- coin$whisker1
  ad.alarm <- rep(0, length(ad))
  for (j in 2:length(ad.alarm)) {
    if (ad[j] < thresh) {
      ad.alarm[j] <- ad.alarm[j-1]+1
    } else {
      ad.alarm[j] <- max(0,ad.alarm[j-1]-2)
    }
  }
  return(ad.alarm)
}

add.x15 <- function(coin, win.size=15) {
  hh <- rollapply(coin$x$high, win.size, max, by=win.size)
  ll <- rollapply(coin$x$low, win.size, min, by=win.size)
  oo <- coin$x$open[seq(1,nrow(coin$x), by=win.size)]
  cc <- coin$x$close[seq(1,nrow(coin$x), by=win.size)]
  pp <- coin$x$price[seq(1,nrow(coin$x), by=win.size)]
  tt <- coin$x$time[seq(1,nrow(coin$x), by=win.size)]
  vv <- rollapply(coin$x$volume_to, win.size, sum, by=win.size)
  ttc <- rollapply(coin$ttc, win.size, max, by=win.size)
  
  
  ultra.min <- min(unlist(lapply(list(hh, ll, oo, cc, pp), length)))
  hh <- hh[1:ultra.min]
  ll <- ll[1:ultra.min]
  cc <- cc[1:ultra.min]
  oo <- oo[1:ultra.min]
  pp <- pp[1:ultra.min]
  tt <- tt[1:ultra.min]
  vv <- vv[1:ultra.min]
  ttc <- ttc[1:ultra.min]
  xx <- data.frame(high=hh, low=ll, open=oo, close=cc, price=pp, time=tt, volume=vv)
  return(xx)
}

add.whisker2 <- function(coin, ema.n=21, win.size=15) {
  
  xx <- coin$x15
  vv <- xx$volume
  vz <- abs(xx$close-xx$open) + abs(xx$high-xx$low)
  vz[vz > 0] <- -100
  vz[vz == 0] <- 1
  vz[vz < 0] <- 0
  vz2 <- EVWMA(vz, vv, n=ema.n, wilder=T)
  
  vz2[vz2 == 0] <- 1e-9
  idx2 <- 1:length(vz2)
  idx3 <- rep(idx2, each=win.size)
  w2 <- vz2[idx3]
  w2 <- c(rep(0, nrow(coin$x)-length(w2)), w2)
  return(w2)
}

add.whisker3 <- function(coin, ema.n=21, win.size=15) {

  xx <- coin$x15
  vv <- xx$volume
  vz <- abs(xx$close-xx$open) + abs(xx$high-xx$low)
  vz[vz > 0] <- -100
  vz[vz == 0] <- 1
  vz[vz < 0] <- 0
  
  vz2 <- VWAP(vz, vv, n=ema.n, wilder=T)
  
  vz2[vz2 == 0] <- 1e-9
  idx2 <- 1:length(vz2)
  idx3 <- rep(idx2, each=win.size)
  w2 <- vz2[idx3]
  w2 <- c(rep(0, nrow(coin$x)-length(w2)), w2)
  return(w2)
}

add.fft <- function(gg, fft.size=30) {
  fft.x <- rollapply(gg$rvrp, fft.size, fft)
  z <- fft.x
  nc <- length(fft.x)/fft.size
  z[is.na(z)] <- 0
  zz <- data.frame(z)
  for (j in 1:ncol(z)) {
    zz[,j] <- as.numeric(Re(z[,j]))
  }
  m <- matrix(0,fft.size-1, fft.size)
  colnames(m) <- c(paste("X", seq(1,fft.size), sep=""))
  colnames(zz) <- c(paste("X", seq(1,fft.size), sep=""))
  zz <- rbind(m, zz)
  return(zz)
  # X1 SIG
  #for (mmm in 1:fft.size) {
  #  eval(parse(text=paste("gg$sig",mmm, " <- c(rep(0,fft.size-1),zz$X", mmm, ")",sep="")))
  #}
}

add.features <- function(coins) {
    
  for (coin in 1:length(coins)) {
    gg <- coins[[coin]]
    print(gg$coin)
    coins[[coin]]$rvrp <- add.rvrp(coins[[coin]], win.size=120)
    #coins[[coin]]$fft <- add.fft(coins[[coin]], fft.size)
    #coins[[coin]]$rsi <- add.rsi(coins[[coin]], win.size=180)
    #add.sd.rvrp(coin, win.size=30)
    #add.low(coin, win.size=30, max_x=8, min_x=2)
    #add.gabor(coin, nvoice=10, scale=30)
    #add.acf(coin, win.size=300, by=30, zero.buffer=3)
    #add.acf.alarm(coin, thresh=32)
    #add.pc(coin, win.size=180)
    #add.ttc(coin, pct_gain=0.2)
    coins[[coin]]$ttc.24 <- add.ttc.daily.gain(coins[[coin]], time.span=24)
    #add.pdiff(coin, lag=60)
    
    #TMF
    #coins[[coin]]$tmf <- add.tmf(coins[[coin]], ema1.n=120, ema2.n=120)
    
    #whiskers
    #agg.size = 15 # minutes to aggregate over
    #coins[[coin]]$x15 <- add.x15(coins[[coin]], win.size=agg.size)
    #coins[[coin]]$whisker1 <- add.whisker1(coins[[coin]], win.size=5, vwap.n=21)
    #add.whisker1.alarm(coin, thresh=0)
    #coins[[coin]]$whisker2 <- add.whisker2(coins[[coin]], ema.n=21, win.size=agg.size)
    #coins[[coin]]$whisker3 <- add.whisker3(coins[[coin]], ema.n=21, win.size=agg.size)
    
  }
  return(coins)
}


if (reload_coins) {
  test3.coins <- load.coins()
}
#coins <- add.features(coins)
#print("Saving...")
#save(coins, file="coins.testdata.RData")
