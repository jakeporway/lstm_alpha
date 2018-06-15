library(TTR)
library(TSclust)
library(zoo)
library(DBI)
#source("../training/pump_features.R")
#source("../training/core_pump_library.R")


load.coins <- function(coin.names, start.time, end.time, features.n, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset, max.end.time=0) {
 
  print("Loading coins...")
  coins <- list()
  
  con <- dbConnect(RMySQL::MySQL(fetch.default.rec = 500000), user="jake", password="fandang0", dbname="coins")
  buffer <- 0
  res <- dbSendQuery(con, paste("SELECT time, high, low, open, close, volume_from, volume_to, price FROM btc_prices WHERE time >= ", start.time-buffer*60, " AND time <= ", end.time, ";", sep=""))
  btc <- dbFetch(res, n=-1)
  dbClearResult(res)
  dbDisconnect(con)

  # ADDING THIS TO MAKE UP FOR BAD SQL DATA
  bad.data <- btc$price==0
  btc <- btc[!bad.data,]
  
  for (coin in coin.names) {
    print(coin)
    cc <- load.coin(coin, start.time, end.time, features.n, btc, max.end.time)
    coins[[coin]] <- cc
  }
  
  coins <- add.features(coins, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset)
  return(coins)
}

load.coin <- function(coin, start.time=-1, end.time=-1, features.n, btc=NULL, max.end.time=0) {
  
  con <- dbConnect(RMySQL::MySQL(fetch.default.rec = 500000), user="jake", password="fandang0", dbname="coins")
  buffer <- 0
  
  prices <- paste(tolower(coin), "_prices", sep="")
  
  # Make sure that the coin has as much data as we're going to ever pull before pulling this
  query <- dbSendQuery(con, paste("SELECT time FROM ", prices, " order by time desc limit 1;", sep=""))
  res <- dbFetch(query, n=-1)
  if (nrow(res) < 1 || res$time[1] < max.end.time) {
     print(paste(coin, "didn't have data running up to max.end.time=", max.end.time, ". Skipping."))
     dbDisconnect(con)
     return(NULL)
  }

  res <- tryCatch( {
    query <- dbSendQuery(con, paste("SELECT time, high, low, open, close, volume_from, volume_to, price FROM ", prices, " WHERE time >= ", start.time-buffer*60, " AND time <= ", end.time, ";", sep=""))
  }, error = function(err) {
    print(paste("Hit an error:", err))
    return(NULL)
  })
    
  if (is.null(res)) { dbDisconnect(con); return(NULL) }
  
    gg <- dbFetch(res, n=-1)
    gg <- gg[!duplicated(gg),]
    if (nrow(gg) < features.n) {
      print(paste("Not enough data for", coin))
      dbDisconnect(con)
      return(NULL)
    }

    # ADDING THIS TO MAKE UP FOR BAD SQL DATA
    bad.data <- gg$price==0
    gg <- gg[!bad.data,]

    if (is.null(btc)) {
       res <- dbSendQuery(con, paste("SELECT time, high, low, open, close, volume_from, volume_to, price FROM btc_prices WHERE time >= ", start.time-buffer*60, " AND time <= ", end.time, ";", sep=""))
       btc <- dbFetch(res, n=-1)
       dbClearResult(res)
    }
    
    gg <- normalize_data(gg, btc)
    dbClearResult(res)
    dbDisconnect(con)
    
    real.coin <- list()
    real.coin$gg <- gg
    real.coin$coin <- coin
    return(real.coin)
}
 
add.features <- function(coins, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset) {
  print("Adding features...")
  for (coin in names(coins)) {
    print(coin)
    if (is.null(coin)) {
       next
    }
    coins[[coin]] <- add.features.coin(coins[[coin]], win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset)
  }
 return(coins)
}

add.features.coin <- function(coin, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset) {

    gg <- coin$gg
    
    if (nrow(gg) < features.n) {
      print(paste("Not enough data found for", coin))
      return(NULL)
    }
    
    #if (length(unique(gg$price)) < length(gg$price)/200) {
    rls <- rle(gg$price)
    if (any(rls$lengths) > length(gg$price)/20) {
      print(paste("not enough unique values for", coin))
      return(NULL)
    }
    
    ttc <- add.ttc.daily.gain2(gg, ttc.time)
    
    gg$ttc.24 <- ttc$ttc
    gg$ttp <- ttc$ttp
    
    
    gg <- add_labels(gg, method="ttc.24.quantiles", breaks=gain.breaks, subtract_out=subtract_offset)
  
    coin$gg <- gg
    
    rvrps <- list()
    # ffts <- list()
    # sax <- list()
    # ccis <- list()
    # macds <- list()
    # aroons <- list()
    # rsis <- list()
    
    fft.w = 1000
    by = ceiling(fft.w/10)
    for (kk in win.sizes) {
      print(kk)
      rvrps[[kk]] <- rvrp.fun(gg, win.size=kk)
      if (sum(rvrps[[kk]])==0) {
        return(NULL)
      }
      # fft.x <- rollapply(rvrps[[kk]], width=fft.w, by=by,FUN=fft)
      # fft.1 <- c(rep(0,fft.w-1), rep(fft.x[,1], each=by))
      # fft.1 <- fft.1[1:length(gg$time)]
      # ffts[[kk]] <- fft.1
      # 
      # z <- rvrps[[kk]]
      # splits = ceiling(length(z)/split.size)
      # w <- PAA(z, splits)
      # sax.720 <- convert.to.SAX.symbol(w, alpha=alpha)
      # sax.720 <- rep(sax.720, each=split.size)[1:length(rvrps[[kk]])]
      # sax[[kk]] <- sax.720
      # 
      # ccis[[kk]] <- CCI(z, n=kk)
      # aroons[[kk]] <- aroon(z, n=kk)
      # rsis[[kk]] <- RSI(z, n=kk)
      # macds[[kk]] <- MACD(z, nFast=kk/8, nSlow=kk/4, nSig=kk/2)
      
    }
    coin$rvrps <- rvrps
    # coins[[coin]]$ffts <- ffts
    # coins[[coin]]$sax <- sax
    # coins[[coin]]$ccis <- ccis
    # coins[[coin]]$rsis <- rsis
    # coins[[coin]]$aroons <- aroons
    # coins[[coin]]$macds <- macds
  return(coin)
}

create.features.fft <- function(gg, label, win.sizes, features.n, hours.to.ignore.at.end) {
  w=1000
  rvrps <- list()
  ffts <- list()
  #ranges <- list()
  #ultra.range <- c()
  by = ceiling(w/10)
  
  data.train <- data.frame(matrix(0, length(gg$time), length(win.sizes)*3))
  
  col.count <- 1
  for (l in win.sizes) {
    print(l)
    
    data.train[,col.count] <- rvrps[[l]]
    data.train[,col.count+1] <- Re(ffts[[l]])
    data.train[,col.count+2] <- Mod(ffts[[l]])
    #rvrps[[l]] <- rvrps[[l]][gg$time >= start.time]
    #ffts[[l]] <- ffts[[l]][gg$time >= start.time]
    col.count <- col.count + 3
  }
  data.train$label <- gg$label
  data.train$time <- gg$time
  names(data.train) <- c(paste(rep(c("rvrp", "fft.re", "fft.mod")), rep(1:length(win.sizes), each=3),sep=""), "label", "time")
  #ultra.range <- ultra.range[gg$time >= start.time]
  
  return(list(data.train=data.train, rvrps=rvrps,ffts=ffts))
}

plot.coin <- function(coin, win.sizes) {
  gg <- coin$gg
  rvrps <- coin$rvrps
  sax <- coin$sax
  rsis <- coin$rsis
  macds <- coin$macds
  ccis <- coin$ccis
  ffts <- coin$ffts
  aroons <- coin$aroons
  
  label.idx <- gg$time[gg$label >= 5]
  par(mfrow=c(2+length(win.sizes),1),mar=c(2,2,2,2))
  #rvrp
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    #plot(rvrps[[kk]]~gg$time, type='h', main="rvrp", col=viridis(ceiling(max(rvrps[[kk]]-min(rvrps[[kk]]))))[rvrps[[kk]]])
    plot(rvrps[[kk]]~gg$time, type='h', main="rvrp")
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
  #fft
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    plot(ffts[[kk]]~gg$time, type='l', main="ffts")
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
  #sax
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    plot(sax[[kk]]~gg$time, type='h', main="sax", col=viridis(alpha)[sax[[kk]]])
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
  #ccis
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    ccis[[kk]][1:2000] <- 0
    plot(ccis[[kk]]~gg$time, type='l', main="cci")
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
  #macds
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    plot(macds[[kk]][,2]~gg$time, type='l', main="macd")
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
  #rsis
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    rsis[[kk]][1:2000] <- 50
    plot(rsis[[kk]]~gg$time, type='l', main="rsi")
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
  #aroons
  plot(gg$price.n~gg$time, type='l', main=coin$coin)
  plot(gg$label~gg$time, type='l')
  for (kk in win.sizes) {
    plot(aroons[[kk]][,3]~gg$time, type='l', main="aroon")
    abline(v=label.idx, col=rgb(0,1,0,0.01))
  }
  
}

plot.feature.hists <- function(coin, win.sizes) {
  
  print(coin$coin)
  # Let's go look at these whack-a-doo histograms first
  
  gg <- coin$gg
  rvrps <- coin$rvrps
  sax <- coin$sax
  rsis <- coin$rsis
  macds <- coin$macds
  ccis <- coin$ccis
  ffts <- coin$ffts
  aroons <- coin$aroons
  
  
  feature.names <- c("RVRP", "SAX", "RSI", "MACD", "CCI", "AROON")
  features <- list(rvrps, sax, rsis, macds, ccis, aroons)
  label.limit <- 4
  
  for (i in 1:length(features)) {
    par(mfrow=c(3,1), mar=c(2,2,2,2))
    feature <- features[[i]]
    for (kk in win.sizes) {
      feat <- feature[[kk]]
      if (feature.names[i] == "MACD") {
        feat <- feat[2000:nrow(feat),2]
      } else if (feature.names[i] == "AROON") {
        feat <- feat[2000:nrow(feat),3]
      } else {
        feat <- feat[2000:length(feat)]
      }
      brks <- seq(min(feat), max(feat), length.out=100)
      hist(feat[gg$label <= label.limit], breaks=brks, main=paste(coin$coin, ":", feature.names[i], kk), col=rgb(1,0,0,0.5))
      hist(feat[gg$label > label.limit], breaks=brks, col=rgb(0,1,0,0.5), add=T)
      
    } 
    
    for (kk in win.sizes) {
      
      feat <- feature[[kk]]
      if (feature.names[i] == "MACD") {
        feat <- feat[2000:nrow(feat),2]
      } else if (feature.names[i] == "AROON") {
        feat <- feat[2000:nrow(feat),3]
      } else {
        feat <- feat[2000:length(feat)]
      }
      brks <- seq(min(feat), max(feat), length.out=100)
      h1 <- hist(feat[gg$label <= label.limit], breaks=brks, plot=F)
      h2 <- hist(feat[gg$label > label.limit], breaks=brks, plot=F)
      barplot(h1$count+0.001, log="y", col=rgb(1,0,0,0.5), main=paste(coin$coin, ": Log", feature.names[i], kk))
      barplot(h2$count+0.001, log="y", col=rgb(0,1,0,0.5), add=T)
    } 
    
    for (kk in win.sizes) {
      feat <- feature[[kk]]
      if (feature.names[i] == "MACD") {
        feat <- feat[2000:nrow(feat),2]
      } else if (feature.names[i] == "AROON") {
        feat <- feat[2000:nrow(feat),3]
      } else {
        feat <- feat[2000:length(feat)]
      }
      feat2 <- c(0, diff(feat))
      brks <- seq(min(feat2), max(feat2), length.out=100)
      hist(feat2[gg$label <= label.limit], breaks=brks, main=paste(coin$coin, ": Diff", feature.names[i], kk), col=rgb(1,0,0,0.5))
      hist(feat2[gg$label > label.limit], breaks=brks, col=rgb(0,1,0,0.5), add=T)
    } 
    
    for (kk in win.sizes) {
      feat <- feature[[kk]]
      if (feature.names[i] == "MACD") {
        feat <- feat[2000:nrow(feat),2]
      } else if (feature.names[i] == "AROON") {
        feat <- feat[2000:nrow(feat),3]
      } else {
        feat <- feat[2000:length(feat)]
      }
      feat2 <- c(0, diff(feat))
      brks <- seq(min(feat2), max(feat2), length.out=100)
      h1 <- hist(feat2[gg$label <= label.limit], breaks=brks, plot=F)
      h2 <- hist(feat2[gg$label > label.limit], breaks=brks, plot=F)
      barplot(h1$count+0.001, log="y", col=rgb(1,0,0,0.5), main=paste(coin$coin, ": Log Diff", feature.names[i], kk))
      barplot(h2$count+0.001, log="y", col=rgb(0,1,0,0.5), add=T)
    } 
  }
}

z.normalize <- function(x) { 
  return (x-min(x) / (max(x)-min(x)))
}

re.normalize <- function(x, mn, mx) {
  return (x-mn)/(mx-mn)
}

generate.features <- function(coin, features.n, label.thresh, min.max=data.frame()) {
  
  gg <- coin$gg
  nr <- nrow(gg)
  feature.names <- c("SAX", "SAX.diff", "CCI", "CCI.diff", "MACD", "MACD.diff", "RSI", "RSI.diff", "AROON", "AROON.diff")
  labs <- rep(win.sizes, times=length(feature.names))
  feature.names <- rep(feature.names, each=3)
  feature.names <- paste(feature.names, labs, sep="")
  features <- data.frame(matrix(0, nrow=nr-features.n, ncol=length(feature.names)))
  names(features) <- feature.names
  
  
  idx <- (features.n+1):nr
  col.count <- 1
  
  #SAX
  feat <- coin$sax
  for (kk in win.sizes) {
    features[,col.count] <- feat[[kk]][idx]
    col.count <- col.count+1
  }
  
  for (kk in win.sizes) {
    features[,col.count] <- c(0, diff(feat[[kk]][idx]))
    col.count <- col.count+1
  }
  
  
  #CCI
  feat <- coin$ccis
  for (kk in win.sizes) {
    features[,col.count] <- feat[[kk]][idx]
    col.count <- col.count+1
  }
  
  for (kk in win.sizes) {
    features[,col.count] <- c(0, diff(feat[[kk]][idx]))
    col.count <- col.count+1
  }
  
  #MACD
  feat <- coin$macds
  for (kk in win.sizes) {
    features[,col.count] <- feat[[kk]][idx,2]
    col.count <- col.count+1
  }
  
  for (kk in win.sizes) {
    features[,col.count] <- c(0,diff(feat[[kk]][idx,2]))
    col.count <- col.count+1
  }
  
  #RSI
  feat <- coin$rsis
  for (kk in win.sizes) {
    features[,col.count] <- feat[[kk]][idx]
    col.count <- col.count+1
  }
  
  for (kk in win.sizes) {
    features[,col.count] <- c(0,diff(feat[[kk]][idx]))
    col.count <- col.count+1
  }
  
  #AROON
  feat <- coin$aroon
  for (kk in win.sizes) {
    features[,col.count] <- feat[[kk]][idx,3]
    col.count <- col.count+1
  }
  
  for (kk in win.sizes) {
    features[,col.count] <- c(0,diff(feat[[kk]][idx,3]))
    col.count <- col.count+1
  }
  
  if (nrow(min.max) == 0) {
    min.max <- matrix(0, ncol=2, nrow=ncol(features))
    for (i in 1:ncol(features)) {
      min.max[i,1] <- min(features[,i])
      min.max[i,2] <- max(features[,i])
      features[,i] <- as.numeric(z.normalize(features[,i]))
    }
  } else {
    for (i in 1:ncol(features)) {
      #features[,i] <- as.numeric(re.normalize(features[,i], min.max[i,1], min.max[i,2]))
      features[,i] <- as.numeric(z.normalize(features[,i]))
    }
  }
  
  features$label <- gg$label[idx]
  features$label <- as.factor(features$label >= label.thresh)
  return(list(features=features, min.max=min.max))
}


update.labels <- function(coins, ttc.time, gain.breaks) {
  for (coin in names(coins)) {
    print(coin)
    gg <- coins[[coin]]$gg
    ttc <- add.ttc.daily.gain2(gg, ttc.time)
    gg$ttc.24 <- ttc$ttc
    gg$ttp <- ttc$ttp
    ##gg$ttc <- ttc.pct.gain(gg, 0.2)
    gg <- add_labels(gg, method="ttc.24.quantiles", breaks=gain.breaks)
    coins[[coin]]$gg <- gg
  }
  return(coins)
}
