# Trendline tester

dd <- read.csv("old_data/BAT.csv", h=T, sep="\t")
# Different file formats name these different things. Reset them accordingly
colnames(dd) <- c("Time", "High", "Low", "Open", "Close", "Volume_From", "Price", "Volume_To")
d <- dd[189000:nrow(dd),]
# Find peaks
win = 10
period = 500
idx <- 1:nrow(d)
num_periods = ceiling(nrow(d)/period)
lines_list <- list()

# Inefficient loop...

for (j in 1:num_periods) {
  start_val <- (j-1)*period+1
  end_val <- j*period
  peaks <- rep(0, period)
  
  for (i in (win+1):(period-win)) {
    ii <- i + start_val - 1
    if (ii >= nrow(d)-win) {
      break
    }
    current_price <- d$High[ii]
    left_idx <- (ii-win):ii
    tops <- pmax(d$Open[left_idx], d$Close[left_idx])
    left_less <- all(tops <= current_price)
    
    right_idx <- ii:(ii+win)
    tops <- pmax(d$Open[right_idx], d$Close[right_idx])
    right_less <- all(tops <= current_price)
    
    if (left_less && right_less) {
      peaks[i] <- 1
    }
  }
  
  valleys <- rep(0, period)
  for (i in (win+1):(period-win)) {
    ii <- i+start_val - 1
    if (ii >= nrow(d)-win) {
      break
    }
    current_price <- d$Low[ii]
    left_idx <- (ii-win):ii
    bottoms <- pmin(d$Open[left_idx], d$Close[left_idx])
    left_less <- all(bottoms >= current_price)
    
    right_idx <- ii:(ii+win)
    bottoms <- pmin(d$Open[right_idx], d$Close[right_idx])
    right_less <- all(bottoms >= current_price)
    
    if (left_less && right_less) {
      valleys[i] <- 1
    }
  }
 
  peak_list <- d$High[start_val:end_val][peaks==1]
  peak_idx <- idx[start_val:end_val][peaks==1]
  valley_list <- d$Low[start_val:end_val][valleys==1]
  valley_idx <- idx[start_val:end_val][valleys==1]
  
  lines_list[[j]] = list(peak_list=peak_list, peak_idx=peak_idx, valley_list=valley_list, valley_idx=valley_idx)
}

price <- (d$High + d$Low)/2

plot(price, type='l')

for (i in 1:length(lines_list)) {
  ll <- lines_list[[i]]
  if (length(ll$peak_list) < 2 || length(ll$valley_list) < 2) {
    next
  }
  points(ll$peak_list~ll$peak_idx, col=3, pch=19)
  lines(loess(ll$peak_list~ll$peak_idx), col=3)
  abline(lm(ll$peak_list~ll$peak_idx), col=3, lty=2)
  points(ll$valley_list~ll$valley_idx, col=2, pch=19)
  lines(loess(ll$valley_list~ll$valley_idx), col=2)
  abline(lm(ll$valley_list~ll$valley_idx), col=2, lty=2)
}

# Find potential support/resistance lines by looking at the density


# What would we do with this next?

# - Find stocks that have nearly parallel peak / valley lines to
#   indicate stocks that are trading within a channel
# - Look for triangles, wedges, etc. in the the past time period
# - Understand if Loess is giving us any signals
