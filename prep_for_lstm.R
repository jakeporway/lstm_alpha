# pull data from our DB and add features for LSTM training
source("pump_features.R")
source("core_pump_library.R")
source("utility_file.R")

# ADDING CHANGES TO TRY NEW FEATURES

oct.times <- c(1506830400, 1509508799)
nov.times <- c(1509508800, 1512104399)
dec.times <- c(1512104400, 1514782799)
jan.times <- c(1514782800, 1517461199)
spike.times <- c(1510531200, 1513382400)
bull.trap.times <- c(1513382400, 1515196800)
dip.times <- c(1515196800, 1517875200)
wander.times <- c(1517875201, 1519874760)
feb.times <- c(1517474760, 1519874760)
mar.times <- c(1519874760, 1522574760)               
apr.times <- c(1522574760, 1525174760)
may.times <- c(1525174760, 1527811200)
june.times <- c(1527811200, 1530403200)

batch_list = list(

  list(
    name="Oct to May",
    root_path="/home/ec2-user/stocks/lstm_alpha/training_data/",
    filename="_oct_may_diff.csv",
    start.time=1506830400,
    end.time=1525174760
  ),

  list(
    name="May to Jun",
    root_path="/home/ec2-user/stocks/lstm_alpha/training_data/",
    filename="_may_jun_diff.csv",
    start.time=1525174760,
    end.time=1529280000
  )
)

run_min <- 60
pct_gain_sell <- 0.11
stop_limit <- 0.30

rsi.vals <- c(50, 200, 720, 1440, 2880)

win.sizes <- c(1440)
features.n <- 2000
gain.breaks=seq(0,10,0.05)
subtract_offset=0 # Make the gain.breaks work with negatives
ttc.time = 24
alpha = 26
split.size = 30
rvrp.length <- 1440


convert.only.one.feature <- function(t.coin, rvrp.length) {
  
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
  
  gg <- t.coin$gg#[(real.start:end_idx),]
  
  #Aroon of price
  ncols=2
  plain.mat <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)*ncols))
  #Aroon of rvrp
  rvrp.mat <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)*ncols))
  # # MACD of price
  # macds <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # # MACD of volume
  # macdv <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # # MACD of rvrp
  # macdvp <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # rsis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  rvrp <- rvrp.fun(gg, win.size=rvrp.length)
  # 
  # # ALL THE EXTRA TTR STUFF
  # adxs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # atrs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # bbandss <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))	
  # ccis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # chaikinads <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # chaikinvs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # cmfs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # cmos <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # dvis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # ksts <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # sars <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # stochs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # tdis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # uos <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # vhfs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # wads <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  # wprs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  
  for (i in 1:length(rsi.vals)) {
    
    v <- rsi.vals[i]
    print(v)
    sidx <- (i-1)*ncols+1
    plain.mat[,sidx:(sidx+ncols-1)] <- ultimateOscillator(cbind(gg$high, gg$low, gg$close))
    rvrp.mat[,sidx:(sidx+ncols-1)] <- ultimateOscillator(cbind(rvrp, rvrp, rvrp))
    # macds[,i] <- MACD(gg$price, nSlow=v, nFast=floor(v/2), nSig=floor(v/3))[,2]
    # if (sum(gg$volume_from) == 0) {
    #   macdv[,i] <- rep(0, nrow(macdv)) 
    # } else {
    #   macdv[,i] <- tryCatch({
    #     MACD(gg$volume_from, nSlow=v, nFast=floor(v/2), nSig=floor(v/3))[,2]},
    #     warning = function(war) {},
    #     error = function(err) {
    #       print(paste("Error computing MACDv:", err, ". Using all 0s"))
    #       rep(0, nrow(macdv)) 
    #     })
    # }
    # if (sum(rvrp) == 0) {
    #   macdvp[,i] <- rep(0, nrow(macdvp))
    # } else {
    #   macdvp[,i] <- MACD(rvrp, nSlow=v, nFast=floor(v/2), nSig=floor(v/3))[,2]
    # }
    # rsis[,i] <- RSI(gg$price, n=floor(v/2))
    # 
    # # ADD EVERYTHING FROM TTR
    # adxs[,i] <- ADX(cbind(gg$high, gg$low, gg$close), n=floor(v/30))[,4]
    # atrs[,i] <- ATR(cbind(gg$high, gg$low, gg$close), n=floor(v/30))[,2]
    # ccis[,i] <- CCI(cbind(gg$high, gg$low, gg$close), n=floor(v/30))
    # chaikinads[,i] <- chaikinAD(cbind(gg$high, gg$low, gg$close), gg$volume_from)
    # chaikinvs[,i] <- chaikinVolatility(cbind(gg$high, gg$low), n=floor(v/30))
    # cmfs[,i] <- CMF(cbind(gg$high, gg$low, gg$close), gg$volume_from, n=floor(v/30))
    # cmos[,i] <- CMO(cbind(gg$price), n=floor(v/30))
    # cmos[,i][is.na(cmos[,i])] <- 0
    # dvis[,i] <- DVI(cbind(gg$price), n=floor(v/30))[,2]
    # ksts[,i] <- KST(cbind(gg$price))[,2]
    # sars[,i] <- SAR(cbind(gg$high, gg$low))
    # stochs[,i] <- SMI(cbind(gg$high, gg$low, gg$close), n=floor(v/4), nFast=floor(v/20), nSlow=floor(v/2))[,2]
    # tdis[,i] <- TDI(cbind(gg$price), n=floor(v/30))[,1]
    # uos[,i] <- ultimateOscillator(cbind(gg$high, gg$low, gg$close))
    # uos[,i][is.na(uos[,i])] <- 0
    # vhfs[,i] <- VHF(gg$price, n=floor(v/30))
    # wprs[,i] <- WPR(cbind(gg$high, gg$low, gg$close), n=floor(v/30))
    
  }
  
  # Values to ignore (NAs in the beginning, 0 labels at the end)
  idx <- (tail(rsi.vals,1)*2+10):(nrow(gg))
  
  # 
  # rvrp2 <- rvrp.fun(gg, win.size=360)
  # rvrp3 <- rvrp.fun(gg, win.size=720)
  # rvrp4 <- rvrp.fun(gg, win.size=2880)
  # 
  # names(aroons) <- paste("aroon.", rsi.vals, sep="")
  # names(aroonvp) <- paste("aroonvp.", rsi.vals, sep="")
  # names(macds) <- paste("macds.", rsi.vals, sep="")
  # names(macdv) <- paste("macdv.", rsi.vals, sep="")
  # names(macdvp) <- paste("macdvp.", rsi.vals, sep="")
  # names(rsis) <- paste("rsis.", rsi.vals, sep="")
  # 
  # d = data.frame(gg[idx,], rvrp[idx], rvrp2[idx], rvrp3[idx], rvrp4[idx], 
  #                aroons[idx,], aroonvp[idx,], macds[idx,], macdv[idx,], macdvp[idx,], 
  #                rsis[idx,], adxs[idx,], atrs[idx,], ccis[idx,], chaikinads[idx,], chaikinvs[idx,], cmfs[idx,], cmos[idx,], dvis[idx,], 
  #                ksts[idx,], sars[idx,], stochs[idx,], tdis[idx,], uos[idx,], vhfs[idx,], wprs[idx,]) 
  # 
  # HACK: Get rid of all NAs so we don't screw stuff up in python
  # Brute force
  
  
  d = data.frame(gg[idx,], rvrp[idx], plain.mat[idx,], rvrp.mat[idx,])
  d[is.na(d)] <- -1
  return(d)
}


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
  
  gg <- t.coin$gg#[(real.start:end_idx),]
  
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

  # ALL THE EXTRA TTR STUFF
    adxs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    atrs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    bbandss <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))	
    ccis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    chaikinads <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    chaikinvs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    cmfs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    cmos <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    dvis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    ksts <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    sars <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    stochs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    tdis <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    uos <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    vhfs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    wads <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
    wprs <- data.frame(matrix(0, nrow=nrow(gg), ncol=length(rsi.vals)))
  
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
    rsis[,i] <- RSI(gg$price, n=floor(v/2))
    
    # ADD EVERYTHING FROM TTR
    adxs[,i] <- ADX(cbind(gg$high, gg$low, gg$close), n=floor(v/30))[,4]
    atrs[,i] <- ATR(cbind(gg$high, gg$low, gg$close), n=floor(v/30))[,2]
    ccis[,i] <- CCI(cbind(gg$high, gg$low, gg$close), n=floor(v/30))
    chaikinads[,i] <- chaikinAD(cbind(gg$high, gg$low, gg$close), gg$volume_from)
    chaikinvs[,i] <- chaikinVolatility(cbind(gg$high, gg$low), n=floor(v/30))
    cmfs[,i] <- CMF(cbind(gg$high, gg$low, gg$close), gg$volume_from, n=floor(v/30))
    cmos[,i] <- CMO(cbind(gg$price), n=floor(v/30))
    cmos[,i][is.na(cmos[,i])] <- 0
    dvis[,i] <- DVI(cbind(gg$price), n=floor(v/30))[,2]
    ksts[,i] <- KST(cbind(gg$price))[,2]
    sars[,i] <- SAR(cbind(gg$high, gg$low))
    stochs[,i] <- SMI(cbind(gg$high, gg$low, gg$close), n=floor(v/4), nFast=floor(v/20), nSlow=floor(v/2))[,2]
    tdis[,i] <- TDI(cbind(gg$price), n=floor(v/30))[,1]
    uos[,i] <- ultimateOscillator(cbind(gg$high, gg$low, gg$close))
    uos[,i][is.na(uos[,i])] <- 0
    vhfs[,i] <- VHF(gg$price, n=floor(v/30))
    wprs[,i] <- WPR(cbind(gg$high, gg$low, gg$close), n=floor(v/30))

  }
  
  # Values to ignore (NAs in the beginning, 0 labels at the end)
  idx <- (tail(rsi.vals,1)*2+10):(nrow(gg))
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
  
  d = data.frame(gg[idx,], rvrp[idx], rvrp2[idx], rvrp3[idx], rvrp4[idx], 
      		 aroons[idx,], aroonvp[idx,], macds[idx,], macdv[idx,], macdvp[idx,], 
		 rsis[idx,], adxs[idx,], atrs[idx,], ccis[idx,], chaikinads[idx,], chaikinvs[idx,], cmfs[idx,], cmos[idx,], dvis[idx,], 
		 ksts[idx,], sars[idx,], stochs[idx,], tdis[idx,], uos[idx,], vhfs[idx,], wprs[idx,]) 

  # HACK: Get rid of all NAs so we don't screw stuff up in python
  # Brute force
  d[is.na(d)] <- -1
  return(d)
}

output_batch_of_data <- function(coins_to_save, start.time, end.time, root_path, filename, features.n, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset, max.end.time=0) {
  #coins <- load.coins(coins_to_save, start.time, end.time, features.n, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset, max.end.time)

  con <- dbConnect(RMySQL::MySQL(fetch.default.rec = 500000), user="jake", password="fandang0", dbname="coins")                              
  buffer <- 0                                                                                                                                  
  res <- dbSendQuery(con, paste("SELECT time, high, low, open, close, volume_from, volume_to, price FROM btc_prices WHERE time >= ", start.time-buffer*60, " AND time <= ", end.time, ";", sep=""))                                                                                           
  btc <- dbFetch(res, n=-1)                                                                                                                    
  dbClearResult(res)                                                                                                                          
  dbDisconnect(con)   

  btc2 <- btc[,c("time", "price", "volume_to")]
  
  # ADDING THIS TO MAKE UP FOR GARBAGE DATA
  bad.data <- btc2$price==0
  btc2 <- btc2[!bad.data,]

  lstm.res <- list()
  times <- list()
  
  lag <- 360
  
  for (t.coin in coins_to_save) {
    print(paste("Writing csv for", t.coin))
    fname <- paste(root_path,"/",t.coin, filename, sep="")

    if (file.exists(fname)) {
       print("CSV already exists. Skipping.")
       next
    }

    coin <- load.coin(t.coin, start.time, end.time, features.n, btc, max.end.time)
    if (is.null(coin)) {
       next
    }
    t.coin <- add.features.coin(coin, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset)
    if (is.null(t.coin)) {
       next
    }
    #res <- convert.for.lstm(t.coin, rvrp.length)
    res <- convert.only.one.feature(t.coin, rvrp.length)
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
    res <- cbind(btc2[mm[!is.na(mm)], -c(1)], res[!is.na(mm),])
    names(res)[c(1,2)] <- c("btc.price", "btc.vol")
    
    # 6/18 taking out the diffs for right now
    # Add diffs of all variables too
     res2 <- apply(res, 2, diff)
     res <- cbind(res[2:nrow(res),-ncol(res)], res2[,-ncol(res2)], res[2:nrow(res),ncol(res)])
     names(res)[ncol(res)] <- "label"
    
    print(paste("Writing", fname))
    write.csv(res, file=fname, row.names=F)
  }
}

get_coin_names_from_db <- function() {
  con <- dbConnect(RMySQL::MySQL(fetch.default.rec = 500000), user="jake", password="fandang0", dbname="coins")
  buffer <- 0
  res <- dbSendQuery(con, paste("SELECT price_table FROM config where Exchange='Bittrex';", sep=""))
  coins_to_save <- dbFetch(res, n=-1)[,1]
  coins_to_save <- unlist(lapply(strsplit(coins_to_save,"_"), function(x) { toupper(x[1])}))
  dbClearResult(res)
  dbDisconnect(con)
  return(coins_to_save)
}

coins_to_save = get_coin_names_from_db()

#coins_to_save =c("MANA")
coins_to_save=c("2GIVE", "EMC", "BRX", "BAT", "ANT", "CLOAK", "MANA")
#coins_to_save = c("2GIVE", "ABY", "ADA", "ADT", "ADX", "AEON", "AMP", "ANT", "ARDR", "ARK", "AUR", "BAT", "BAY", "BCY", "BITB", "BLITZ", "BLK", "BLOCK", "BNT", "BRK", "BRX", "BTG", "BURST", "BYC", "CANN", "CFI", "CLAM", "CLOAK", "COVAL", "CRB", "CRW", "CURE", "CVC", "DASH", "DCR", "DCT", "DGB", "DMD", "DNT", "DOGE", "DOPE", "DTB", "DYN", "EBST", "EDG", "EFL", "EGC", "EMC", "EMC2", "ENG", "ENRG", "ERC", "ETC", "EXCL", "EXP", "FCT", "FLDC", "FLO", "FTC", "GAM", "GAME", "GBG", "GBYTE", "GEO", "GLD", "GNO", "GNT", "GOLOS", "GRC", "GRS", "GUP", "HMQ", "INCNT", "IOC", "ION", "IOP", "KMD", "KORE", "LBC", "LGD", "LMC", "LSK", "LUN", "MANA", "MCO", "MEME", "MER", "MLN", "MONA", "MUE", "MUSIC", "NAV", "NBT", "NEO", "NEOS", "NLG", "NMR", "NXC", "NXS", "NXT", "OK", "OMG", "OMNI", "PART", "PAY", "PINK", "PIVX", "POT", "POWR", "PPC", "PTC", "PTOY", "QRL", "QTUM", "QWARK", "RADS", "RBY", "RCN", "RDD", "REP", "RLC", "SALT", "SC", "SEQ", "SHIFT", "SIB", "SLR", "SLS", "SNT", "SPHR", "SPR", "STEEM", "STORJ", "STRAT", "SWIFT", "SWT", "SYNX", "SYS", "THC", "TIX", "TKS", "TRST", "TRUST", "TX", "UBQ", "UKG", "UNB", "VIA", "VIB", "VRC", "VRM", "VTC", "VTR", "WAVES", "WINGS", "XCP", "XDN", "XEL", "XEM", "XLM", "XMG", "XMR", "XMY", "XRP", "XST", "XVG", "XWC", "XZC", "ZCL", "ZEC", "ZEN")


times = unlist(lapply(batch_list, function(x) { x$end.time }))
max.end.time = max(times)

for (batch in batch_list) {
  cat("------ Writing", batch$name, "\n")
  output_batch_of_data(coins_to_save, batch$start.time, batch$end.time, batch$root_path, batch$filename, features.n, win.sizes, gain.breaks, ttc.time, alpha, split.size, subtract_offset, max.end.time)
}



