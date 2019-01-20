# for all csv files in training directory

setwd("~/projects/stocks/lstm_alpha/training_data")
files <- list.files(path=".", pattern="*.csv", full.names=TRUE, recursive=FALSE)

for (f in files) {
  if (length(grep("whiskers",f))&&grep("whiskers",f)==1) {
    print(paste("File", f, "already exists. Skipping."))
    next
  }
  orig.data <- read.csv(f, h=T)
  print(paste("Processing", f))
  # Keep only the OHLC values
  d <- orig.data[,c("high", "low", "open", "close")]
  d$height <- d$high-d$low
  d$body <- abs(d$open-d$close)
  d$color = as.integer(d$close > d$open)
  
  # If color = 0, it's a red candle (0 or neg). If color = 1, it's a green (positive) candle
  
  # If color = 0 (red), top whisker is high - open
  # If color = 1 (green), top whisker - high - close
  d$top.whisker[d$color==0] <- d$high[d$color==0]-d$open[d$color==0]
  d$top.whisker[d$color==1] <- d$high[d$color==1]-d$close[d$color==1]
  
  # If color = 0 (red), bottom whisker is close - low
  # If color = 1 (green), top whisker is open - low
  d$bottom.whisker[d$color==0] <- d$close[d$color==0] - d$low[d$color==0]
  d$bottom.whisker[d$color==1] <- d$open[d$color==1] - d$low[d$color==1]
  
  dd <- cbind(apply(d, 2, diff))
  dd <- rbind(rep(0, ncol(dd)), dd)
  colnames(dd) <- paste("d.", colnames(dd), sep="")
  # Let's shove the new values after price so we don't have to mess with the price_col
  orig.data <- cbind(orig.data[,c(1:9)], d[,5:9], orig.data[,10:(ncol(orig.data)-1)], dd[,5:9], orig.data$label)
  colnames(orig.data)[ncol(orig.data)] <- "label"
  write.csv(orig.data, file=paste(strsplit(f, ".csv")[[1]],"_whiskers.csv", sep=""), row.names=F)
}
