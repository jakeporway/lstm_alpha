# pull data from our DB and add features for LSTM training
source("pump_features.R")
source("core_pump_library.R")
source("utility_file.R")

days_to_lookback=8
end.time <- as.integer(Sys.time())
end.time <- end.time-4*24*3600
start.time <- end.time-24*3600*days_to_lookback

root_path="prediction_data/"
filename="_predict.csv"

rsi.vals <- c(720, 1440, 2880)

win.sizes <- c(1440)
features.n <- 2000
gain.breaks=seq(0,10,0.05)
subtract_offset=0 # Make the gain.breaks work with negatives
ttc.time = 24
alpha = 26
split.size = 30
rvrp.length <- 1440

convert.for.lstm <- function(t.coin, rvrp.length) {
  
  real.start <- tail(rsi.vals,1)
  if (real.start < 1) {
    return(c())
  }
  end_idx <- nrow(t.coin$gg)
  
  ma.alarm.lag <- 5
  macdv.tot.lag <- 50
  macdv.window <- 120
  macdv.pos.thresh <- 2.5
  macdv.neg.thresh <- -2.5
  
  aroonvp.window <- 120
  aroonvp.left.pos <- 20
  
  rsi.lag <- 120
  
  ma.alarm.thresh <- -2.5e5
  macdv.thresh <- -80
  aroonvp.thresh <- -100
  avp.thresh <- 140
  feature.step <- 20
  rsi.thresh <- 60
  
  feature.window <- 360
  trim.1 <- 20
  trim.2 <- feature.window/trim.1
  
  pgrt.scan <- 3500
  
  gg <- t.coin$gg #[(real.start:end_idx),]
  
  #Aroon of price
  aroons <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  #Aroon of rvrp
  aroonvp <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # MACD of price
  macds <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # MACD of volume
  macdv <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # MACD of rvrp
  macdvp <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  rsis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  rvrp <- rvrp.fun(gg, win.size=rvrp.length)
  
  for (i in 1:length(rsi.vals)) {
    v <- rsi.vals[i]
    zz <- cbind(gg$high, gg$low)
    aroons[,i] <- aroon(cbind(gg$high, gg$low), n=v)[,3]
    aroonvp[,i] <- aroon(rvrp, n=floor(v))[,3]
    macds[,i] <- MACD(gg$price, nSlow=v, nFast=floor(v/2), nSig=floor(v/3))[,2]
    if (sum(gg$volume_from) == 0) {
      macdv[,i] <- rep(0, nrow(macdv)) 
    } else {
      macdv[,i] <- tryCatch({
        MACD(gg$volume_from, nSlow=v, nFast=floor(v/2), nSig=floor(v/3))[,2]},
        warning = function(war) {},
        error = function(err) {
          print(paste("Error computing MACDv:", err, ". Using all 0s"))
          rep(0, nrow(macdv)) 
        })
    }
    if (sum(rvrp) == 0) {
      macdvp[,i] <- rep(0, nrow(macdvp))
    } else {
      macdvp[,i] <- MACD(rvrp, nSlow=v, nFast=floor(v/2), nSig=floor(v/3))[,2]
    }
    rsis[,i] <- RSI(gg$price, n=floor(rsi.vals/2))
  }
  
  # OLD VERSION: Values to ignore (NAs in the beginning, 0 labels at the end)
  #idx <- (tail(rsi.vals,1)*2+10):(nrow(aroons))
  # NEW VERSION: This is production, go straight up to the end
  idx <- (tail(rsi.vals,1)*2+10):nrow(gg)

  # 
  # aroons <- rowSums(aroons)
  # aroonvp  <- rowSums(aroonvp)
  # macds <- rowSums(macds)
  # macdv <- rowSums(macdv)
  # macdvp <- rowSums(macdvp)
  
  rvrp2 <- rvrp.fun(gg, win.size=360)
  rvrp3 <- rvrp.fun(gg, win.size=720)
  rvrp4 <- rvrp.fun(gg, win.size=2880)

  names(aroons) <- paste("aroon.", rsi.vals, sep="")
  names(aroonvp) <- paste("aroonvp.", rsi.vals, sep="")
  names(macds) <- paste("macds.", rsi.vals, sep="")
  names(macdv) <- paste("macdv.", rsi.vals, sep="")
  names(macdvp) <- paste("macdvp.", rsi.vals, sep="")
  names(rsis) <- paste("rsis.", rsi.vals, sep="")
  
  #d = data.frame(t.coin$gg[idx,], rvrp[idx], rvrp2[idx], rvrp3[idx], rvrp4[idx], aroons[idx,], aroonvp[idx,], macds[idx,], macdv[idx,], macdvp[idx,], rsis[idx,])
  # NEW VERSION: Trying to take the end of all of this data
  d = data.frame(t.coin$gg[idx,], rvrp[idx], rvrp2[idx], rvrp3[idx], rvrp4[idx], aroons[idx,], aroonvp[idx,], macds[idx,], macdv[idx,], macdvp[idx,], rsis[idx,])
  return(d)
}

cat(" WRITING DATA ------------------\n")

coins <- list()

con <- dbConnect(RMySQL::MySQL(fetch.default.rec = 500000), user="jake", password="fandang0", dbname="coins")
buffer <- 0
res <- dbSendQuery(con, paste("SELECT price_table FROM config where Exchange='Bittrex';", sep=""))
coins_to_save <- dbFetch(res, n=-1)[,1]
coins_to_save <- unlist(lapply(strsplit(coins_to_save,"_"), function(x) { toupper(x[1])}))
dbClearResult(res)
dbDisconnect(con)

coins <- load.coins(coins_to_save, start.time, end.time, features.n, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset)
btc <- load.coins("BTC", start.time, end.time, features.n, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset)
btc2 <- btc[["BTC"]]$gg[,c("time", "price", "volume_to")]

lstm.res <- list()
times <- list()


for (t.coin in coins) {
  print(paste("Writing csv for", t.coin$coin))
  
  res <- convert.for.lstm(t.coin, rvrp.length)
  if (!("label" == names(res)[13])) {
    print("skipping") 
    next
  }
  times[[t.coin$coin]] <- res$time
  
  # put label last
  res <- res[,c(setdiff(1:ncol(res),13), 13)]
  
  t1 <- btc2$time
  t2 <- res$time
  mm <- match(t2, t1)
  if (any(is.na(mm))) {
    print(paste("res had", sum(is.na(mm)), "timestamps not in BTC"))
  }
  
  # Drop time, ttc.24 and ttp
  res <- res[,-c(1,11,12)]
  #res <- cbind(btc2[mm[!is.na(mm)], -c(1)], res[!is.na(mm),])
  res <- cbind(btc2[mm[!is.na(mm)], ], res[!is.na(mm),])
  
  write.csv(res, file=paste(root_path,t.coin$coin, filename, sep=""), row.names=F)
}

