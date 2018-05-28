add_labels <- function(coin, method=NULL, breaks=c(), subtract_out=0) {
  if (is.null(method)) {
    print("No method selected for add_labels. Returning")
    return(coin)
  }
  if (method == "ttc.24.quantiles") {
    coin$label <- as.integer(cut(coin$ttc.24, breaks, right=T))
    coin$label[is.na(coin$label)] <- 0
    coin$label <- coin$label-subtract_out
  }
  return(coin)
}

get_ngrams <- function(x, n, overlap=TRUE) {
  s1 <- seq(1, length(x), n)
  s2 <- s1 + n - 1
  
  s1 <- s1[s2 <= length(x)]
  s2 <- s2[s2 <= length(x)]
  seq <- cbind(s1, s2)
  if (overlap && n > 1) {
    for (i in 2:n) {
      s1 <- seq(i, length(x), n)
      s2 <- s1 + n - 1
      s1 <- s1[s2 <= length(x)]
      s2 <- s2[s2 <= length(x)]
      seq.s <- cbind(s1, s2)
      seq <- rbind(seq, seq.s)
    }
  }
  
  grams <- apply(seq, 1, function(z) { return(x[z[1]:z[2]]) })
  if (!is.null(dim(grams))) {
    grams <- apply(grams, 2, paste, collapse=".")
  } else {
    grams <- as.character(grams)
  }
  return(data.frame(grams=as.character(grams), start=as.numeric(seq[,1]), end=as.numeric(seq[,2]),stringsAsFactors=F))
}


tf.idf <- function(ngram.counts) {
  # We'll count term frequency over all sentences in the class label (a text)
  # We'll count each text as a document, so we'll see how many docs each appeared in
  
  tf <- tapply(ngram.counts$ngram, ngram.counts$text, function(x) { table(x)/length(x)})
  text.names <- names(tf)
  num.texts <- length(text.names)
  
  full.names <- unique(unlist(lapply(tf, names)))
  tfs <- data.frame(ngram=full.names)

  for(i in 1:length(unique(ngram.counts$text))) {
    tfs <- cbind(tfs, as.numeric(tf[[i]][match(full.names, names(tf[[i]]))]))
  }
  names(tfs) <- c("ngram", paste("tf",names(tf), sep=""))
  tfs[is.na(tfs)] <- 0
  
  # IDF
  aidx <- match(ngram.counts$ngram, tfs$ngram)
  doc.counts <- tapply(ngram.counts$text, aidx, function(x) { length(unique(x))})
  saidx <- unique(aidx)
  idfs <- doc.counts[saidx]
  idfs <- log10(length(unique(ngram.counts$text))/idfs)
  tfs$idf <- idfs

  
  for (i in 1:num.texts) {
    tfs[,paste("tfidf",i-1,sep="")] <- tfs[,(i+1)]*tfs$idf
  }
  tfs$ngram <- as.character(tfs$ngram)
 return(tfs)
}



#### ARCHIVED FROM MASTER_SPIKE_SCRATCH.R ####

#setwd("~/projects/stocks/forensics")
#load("coins.forensic.RData")
#load("true.positive.spikes")
#load("true.negative.spikes")
library(TSclust)
# Need to update whatever else should go here

green <- rgb(0,1,0,0.4)
red <- rgb(1,0,0,0.4)
main.cols <- c(red, green)


###### NEW TRAINING SCRATCH #######
# I have a lot of different pieces of working code in different files
# that I'm going to try to consolidate here.
# 
# 
# Here's what I'm thinking:
#   
# 1. ASSUMPTION: You can identify pump behavior within 24 hours of the pump itself. This might be untrue - it sometimes seems that pumps begin accumulating >24 hours before the pump, but it then becomes difficult for me to say confidently that the signal I detect is in fact indicative of a pump.
# 
# 1a. ASSUMPTION: rvrp is in some way indicative of a coming spike :)
# 
# 2. Identify massive spikes across a month's worth of data of the coins (this currently exists in more_spikes_training)
# 
# 3. Hand prune these points. I know, I know, this sucks, but just have it display each one and do a "yes/no" click. 
# 
# 4. See if it has missed any obvious spikes you like. Add them as needed.
# 
# 5. Focus on the ones that have a 24 hour quietish lead in. Don't worry about chaotic spikes just yet. For another project, we should think about how we classify spikes and if we care about different classes of spikes.
# 
# 6. Identify non-spike periods of the same length and similar price-pattern
# 
# 7. Explore! Look at the patterns to find similarities/dissimilarities. Play with 1000 different FFT and wavelet things
# 
# 8. Once you have something that seems promising, try to predict a green label based on a pattern. Or use a decision tree to see if you can do it even just for one coin over time. *That's* the part that's missing and that's hard to test
# 
# 9. If you find something promising, huzzah!
# 
# 10. If you don't, you may still need to do more feature engineering. Are the run ups to a spike a bag of words model? Is there just one signal in there that's important to scan for? Is it (very likely) a sequence of "phonemes"? If so, how do we create those and learn those?


####### BUILD A TRAINING SET ########


# Identify spikes that increase 20% in 60 minutes or fewer

find.spikes.in.training.data <- function() {
  win.size = 60
  increase.pct = 0.2
  
  spikes <- data.frame(coin=character(), idx=integer())
  for (i in 1:length(coins)) {
    gg <- coins[[i]]
    dd <- pct_diff(gg$x$price, lag=win.size)
    m <- which(dd > increase.pct)
    if (length(m)) {
      last_spike <- 0
      for (j in 1:length(m)) {
        if (m[j] < last_spike + win.size*2) {
          next
        }
        spikes <- rbind(spikes, data.frame(coin=as.character(gg$coin), idx=as.integer(m[j])))
        last_spike <- m[j]
      }
    }
  }
  spikes$coin <- as.character(spikes$coin)
  return(spikes)
}


adjust.training.set <- function() {
  new.spikes <- data.frame(coin=character(), start=integer(), end=integer())
  time.backwards = 24*60
  buffer = 6*60
  spike.step = 2*60
  for (i in 1:nrow(spikes)) {
    if (spikes[i,2] <= time.backwards) {
      next
    }
    gg <- coins[[spikes[i,1]]]
    start.time <- max(1,spikes[i,2]-time.backwards-buffer)
    end.time <- min(nrow(gg$x), spikes[i,2]+buffer)
    idx <- start.time:end.time
    min.val <- min(gg$x$price[idx])
    max.val <- max(gg$x$price[idx])
    plot(gg$x$price[idx]~gg$x$time[idx], type='l', main=gg$coin)
    abline(v=gg$x$time[spikes[i,2]-time.backwards], lty=2, col=2)
    abline(v=gg$x$time[spikes[i,2]-spike.step], lty=2, col=2)
    choose <- locator(1)
    if (choose$y > max.val) {
      print("Adding")
      new.spikes <- rbind(new.spikes, data.frame(coin=as.character(gg$coin), start=as.integer(max(1,spikes[i,2]-time.backwards)), end=as.integer(spikes[i,2]-spike.step)))
    }
  }
  new.spikes$coin <- as.character(new.spikes$coin)
  return(new.spikes)
}

add.true.negatives <- function() {
  true.negatives <- data.frame(coin=character(), start=integer(), end=integer())
  time.backwards = 24*60
  buffer = 6*60
  spike.step = 2*60
  while (nrow(true.negatives) < nrow(new.spikes)) {
    gg <- coins[[sample(1:length(coins),1)]]
    sp <- sample(time.backwards:(nrow(gg$x)-buffer), 1)
    start.time <- max(1,sp-time.backwards-buffer)
    end.time <- min(nrow(gg$x), sp+buffer*3)
    idx <- start.time:end.time
    min.val <- min(gg$x$price[idx])
    max.val <- max(gg$x$price[idx])
    plot(gg$x$price[idx]~gg$x$time[idx], type='l', main=gg$coin)
    abline(v=gg$x$time[sp-time.backwards], lty=2, col=2)
    abline(v=gg$x$time[sp-spike.step], lty=2, col=2)
    choose <- locator(1)
    if (choose$y > max.val) {
      print("Adding")
      true.negatives <- rbind(true.negatives, data.frame(coin=as.character(gg$coin), start=as.integer(max(1,sp-time.backwards)), end=as.integer(sp-spike.step)))
    }
  }
  true.negatives$coin <- as.character(true.negatives$coin)
  true.negatives <- true.negatives[order(true.negatives$coin),]
  return(true.negatives)
}


explore.pos.neg <- function() {
  # Put up a few swatches next to each other to visually inspect differences
  # in signals
  
  
  par(mfrow=c(5,2))
  for (i in 1:10) {
    print(i)
    for(j in 1:5) {
      sp <-sax.pos[(i-1)*5+j,]
      sn <-sax.neg[(i-1)*5+j,] 
      plot(sp[order(sp)], type='l', col=3)
      plot(sn[order(sn)], type='l', col=2)
    }
  }
}

# Normalizes across rows! PAA expects to operate on one time series per row
z.normalize <- function(x) { 
  if (class(x)=="data.frame" || class(x)== "matrix") {
    apply(x, 1, function(y) { (y-mean(y))/sd(y)})
  } else {
    (x-mean(x))/sd(x)
  }
}


# We store stuff in columns so we gotta transpose this for PAA
# so that it reads each column as a feature in time
features.to.cluster <- function(gg) {
  return (t(cbind(gg$tmf)))
}
# Convert the positive and negative sequences into SAXs and build
# transition matrices based on the symbols for positives and negatives
sax.cluster <- function(splits=48, alpha=50) {
  
  sax.pos <- matrix(0, nrow(true.positives), splits)
  for (i in 1:nrow(true.positives)) {
    sp <- true.positives[i,]
    start <- sp$start
    end <- sp$end
    idx <- start:end
    g2 <- features.to.cluster(coins[[sp$coin]])
    if (is.null(dim(g2))) { #1D
      w <- PAA(z.normalize(g2[idx]), splits)
    } else {                #2D
      w <- PAA(z.normalize(g2[,idx]), splits)
    }
    sax.pos[i,] <- convert.to.SAX.symbol(w, alpha=alpha)
  }
  
  
  sax.neg <- matrix(0, nrow(true.negatives), splits)
  for (i in 1:nrow(true.negatives)) {
    sp <- true.negatives[i,]
    start <- sp$start
    end <- sp$end
    idx <- start:end
    g2 <- features.to.cluster(coins[[sp$coin]])
    if (is.null(dim(g2))) { #1D
      w <- PAA(z.normalize(g2[idx]), splits)
    } else {                #2D
      w <- PAA(z.normalize(g2[,idx]), splits)
    }
    sax.neg[i,] <- convert.to.SAX.symbol(w, alpha=alpha)
  }
  
  # Look at transition probabilities
  trans.pos <- matrix(0, alpha, alpha)
  trans.neg <- matrix(0, alpha, alpha)
  
  idx.pos <- sample(1:nrow(sax.pos), nrow(sax.pos))
  idx.neg <- sample(1:nrow(sax.neg), nrow(sax.neg))
  
  for (i in idx.pos) {
    for (j in 2:length(sax.pos[i,])) {
      trans.pos[sax.pos[i,j-1], sax.pos[i,j]] <- trans.pos[sax.pos[i,j-1], sax.pos[i,j]] + 1
    }
  }
  
  for (i in idx.neg) {
    for (j in 2:length(sax.neg[i,])) {
      trans.neg[sax.neg[i,j-1], sax.neg[i,j]] <- trans.neg[sax.neg[i,j-1], sax.neg[i,j]] + 1
    }
  }
  
  return(list(sax.pos=sax.pos, sax.neg=sax.neg, trans.pos=trans.pos, trans.neg=trans.neg))
  
}

test.ngrams <- function(sax.pos, sax.neg, n=2) {
  pos.grams <- list()
  neg.grams <- list()
  size <- ceiling(log10(alpha))
  fmt <- as.character(paste("%0", size, "d", sep=""))
  for (i in 1:nrow(sax.pos)) {
    for (j in n:(ncol(sax.pos)-n)) {
      gram <- paste(sprintf(fmt,sax.pos[i,j:(j+n)]),collapse="")
      if (is.null(pos.grams[[gram]])) {
        pos.grams[[gram]] <- 0
      }
      pos.grams[[gram]] <- pos.grams[[gram]]+1
    }
  }
  for (i in 1:nrow(sax.neg)) {
    for (j in n:(ncol(sax.neg)-n)) {
      gram <- paste(sprintf(fmt,sax.neg[i,j:(j+n)]),collapse="")
      if (is.null(neg.grams[[gram]])) {
        neg.grams[[gram]] <- 0
      }
      neg.grams[[gram]] <- neg.grams[[gram]]+1
    }
  }
  return(list(p=pos.grams, n=neg.grams))
}


# Given the transition matrices, go through each sequence and have it vote
# on being a positive or negative sequence
test.training.sequences <- function(sax.pos, sax.neg, trans.pos, trans.neg) {
  
  zero.bonus <- 1
  final.votes.pos <- rep(0, nrow(sax.pos))
  for (i in 1:nrow(sax.pos)) {
    votes <- rep(0, length(sax.pos[i,]))
    for (j in 2:length(sax.pos[i,])) {
      vote.pos <- trans.pos[sax.pos[i,j-1], sax.pos[i,j]]
      vote.neg <- trans.neg[sax.pos[i,j-1], sax.pos[i,j]]
      if (vote.pos > vote.neg) {
        votes[j] <- 1
      } else {
        votes[j] <- -1
      }
      if (vote.pos > 0 || vote.neg > 0) { # Rule out the case that they're both 0
        mx <- max(vote.pos, vote.neg)
        mn <- min(vote.pos, vote.neg)
        if (vote.pos > 0 && vote.neg > 0) { # If they're both positive
          votes[j] <- votes[j]*((mx-mn)/mn) # Give a boost based on how different it is
        } else {
          # The losing vote must have been a 0 cell, so boost by a multiplier
          # times how many votes there are in the matrix
          votes[j] <- votes[j]*mx*zero.bonus
        }
      }
      final.votes.pos[i] <- ifelse(sum(votes)/length(sax.pos[i,]) >= 0, 1, 0)
    }
  }
  
  final.votes.neg <- rep(0, nrow(sax.neg))
  for (i in 1:nrow(sax.neg)) {
    votes <- rep(0, length(sax.neg[i,]))
    for (j in 2:length(sax.neg[i,])) {
      vote.pos <- trans.pos[sax.neg[i,j-1], sax.neg[i,j]]
      vote.neg <- trans.neg[sax.neg[i,j-1], sax.neg[i,j]]
      if (vote.pos > vote.neg) {
        votes[j] <- 1
      } else {
        votes[j] <- -1
      }
      if (vote.pos > 0 || vote.neg > 0) { # Rule out the case that they're both 0
        mx <- max(vote.pos, vote.neg)
        mn <- min(vote.pos, vote.neg)
        if (vote.pos > 0 && vote.neg > 0) { # If they're both positive
          votes[j] <- votes[j]*((mx-mn)/mn) # Give a boost based on how different it is
        } else {
          # The losing vote must have been a 0 cell, so boost by a multiplier
          # times how many votes there are in the matrix
          votes[j] <- votes[j]*mx*zero.bonus
        }
      }
      final.votes.neg[i] <- ifelse(sum(votes)/length(sax.neg[i,]) >= 0.5, 1, 0)
    }
  }
  return(list(votes.pos=final.votes.pos, votes.neg=final.votes.neg))
}



#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
#  FIND SPIKES VOTE ON BIGRAM TRANSITION
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

find.spikes <- function(trans.pos, trans.neg, splits, alpha, slice.size) {
  results <- list()
  ttcs <- c()
  total.buys <- c()
  for (i in c(1,2,3,5)) {
    gg <- coins[[i]]
    res <- find.spikes.for.coin(gg, trans.pos, trans.neg, splits, alpha, slice.size)
    results[[length(results)+1]] <- res
    buys <- res$idx[res$votes==1]
    if (length(buys) == 1 && buys[1] == 0) {
      next
    }
    
    ttcs <- c(ttcs, gg$ttc[buys])
    total.buys <- c(total.buys, buys)
  }
  return(list(ttcs=ttcs, buys=total.buys, res=res))
}


# Using our transition matrices, classify the whole sequence of data
find.spikes.for.coin <- function(gg, trans.pos, trans.neg, splits, alpha, slice.size) {
  if (sum(gg$rvrp)==0) {
    return(0)
  }
  g2 <- features.to.cluster(gg)
  
  #votes <- rollapply(g2, width=24*60, by=60, by.column=T, FUN=classify.sequence, trans.pos, trans.neg, splits, alpha)
  # Harumph, not sure that we can rollapply given the weird dimensions needed for PAA
  num.slices <- floor((ncol(g2)-slice.size*60)/60)
  votes <- rep(0, num.slices)
  for (j in 1:num.slices) {
    x <- g2[,(60*j):(60*j+(60*slice.size))]; 
    votes[j] <- classify.sequence(x, trans.pos, trans.neg, splits, alpha)
  }
  idx <- seq(slice.size*60, nrow(gg$x), 60)
  times <- gg$x$time[idx]
  plot(gg$x$price~gg$x$time, type='l', main=gg$coin)
  abline(v=times, col=main.cols[votes+1])
  print(paste(gg$coin,"- num spikes found: ", sum(votes)))
  
  return(list(votes=votes, times=times, idx=idx))
}


classify.sequence <- function(x, trans.pos, trans.neg, splits=48, alpha=50) {
  zero.bonus <- 1
  w <- PAA(z.normalize(x), splits)
  sx <- convert.to.SAX.symbol(w, alpha=alpha)
  if (any(is.na(sx))) {
    return(0)
  }
  votes <- rep(0, length(sx))
  for (j in 2:length(sx)) {
    vote.pos <- trans.pos[sx[j-1], sx[j]]
    vote.neg <- trans.neg[sx[j-1], sx[j]]
    if (vote.pos > vote.neg) {
      votes[j] <- 1
    } else {
      votes[j] <- -1
    }
    if (vote.pos > 0 || vote.neg > 0) { # Rule out the case that they're both 0
      mx <- max(vote.pos, vote.neg)
      mn <- min(vote.pos, vote.neg)
      if (vote.pos > 0 && vote.neg > 0) { # If they're both positive
        votes[j] <- votes[j]*((mx-mn)/mn) # Give a boost based on how different it is
      } else {
        # The losing vote must have been a 0 cell, so boost by a multiplier
        # times how many votes there are in the matrix
        votes[j] <- votes[j]*mx*zero.bonus
      }
    }
  }
  print(votes)
  return(ifelse(sum(votes)/length(sx) >= 0, 1, 0))
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
#  FIND SPIKES NGRAMS
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

find.spikes.ngrams <- function(watch.list, splits, alpha, slice.size) {
  results <- list()
  ttcs <- c()
  total.buys <- c()
  for (i in c(1,2,3,5)) {
    gg <- coins[[i]]
    res <- find.spikes.for.coin.ngrams(gg, watch.list, splits, alpha, slice.size)
    results[[length(results)+1]] <- res
  }
  return(results)
}


find.spikes.for.coin.ngrams <- function(gg, watch.list, splits, alpha, slice.size) {
  if (sum(gg$rvrp)==0) {
    return(0)
  }
  g2 <- features.to.cluster(gg)
  
  #votes <- rollapply(g2, width=24*60, by=60, by.column=T, FUN=classify.sequence, trans.pos, trans.neg, splits, alpha)
  # Harumph, not sure that we can rollapply given the weird dimensions needed for PAA
  num.slices <- floor((ncol(g2)-slice.size*60)/60)
  votes <- rep(0, num.slices)
  for (j in 1:num.slices) {
    x <- g2[,(60*j):(60*j+(60*slice.size))]; 
    votes[j] <- classify.sequence.ngrams(x, watch.list, splits, alpha)
  }
  idx <- seq(slice.size*60, nrow(gg$x), 60)
  times <- gg$x$time[idx]
  plot(gg$x$price~gg$x$time, type='l', main=gg$coin)
  abline(v=times, col=main.cols[as.numeric(votes>ngram.votes)+1])
  print(paste(gg$coin,"- num spikes found: ", sum(votes>ngram.votes)))
  
  return(list(votes=votes, times=times, idx=idx))
  
}


classify.sequence.ngrams <- function(x, watch.list, splits=48, alpha=50) {
  
  w <- PAA(z.normalize(x), splits)
  sx <- convert.to.SAX.symbol(w, alpha=alpha)
  if (any(is.na(sx))) {
    return(0)
  }
  size <- nchar(watch.list[1])/2 - 1
  fmt <- as.character(paste("%0", size, "d", sep=""))
  num.uniques <- 0
  for (j in size:(length(sx)-size)) {
    gram <- paste(sprintf(fmt,sx[j:(j+size)]),collapse="")
    if (gram %in% watch.list) {
      num.uniques <- num.uniques+1
    }
  }
  return(num.uniques)
}



#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
#  FIND SPIKES QDA
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

find.spikes.qda <- function(qd, splits, alpha, slice.size) {
  results <- list()
  ttcs <- c()
  total.buys <- c()
  for (i in c(1,2,3,5)) {
    gg <- coins[[i]]
    res <- find.spikes.for.coin.qda(gg, qd, splits, alpha, slice.size)
    results[[length(results)+1]] <- res
  }
  return(results)
}


find.spikes.for.coin.qda <- function(gg, qd, splits, alpha, slice.size) {
  if (sum(gg$rvrp)==0) {
    return(0)
  }
  g2 <- features.to.cluster(gg)
  
  #votes <- rollapply(g2, width=24*60, by=60, by.column=T, FUN=classify.sequence, trans.pos, trans.neg, splits, alpha)
  # Harumph, not sure that we can rollapply given the weird dimensions needed for PAA
  num.slices <- floor((ncol(g2)-slice.size*60)/60)
  votes <- rep(0, num.slices)
  for (j in 1:num.slices) {
    x <- g2[,(60*j):(60*j+(60*slice.size))]; 
    votes[j] <- classify.sequence.qda(x, qd, splits, alpha)
  }
  idx <- seq(slice.size*60, nrow(gg$x), 60)
  times <- gg$x$time[idx]
  plot(gg$x$price~gg$x$time, type='l', main=gg$coin)
  abline(v=times, col=main.cols[as.numeric(votes==1)+1])
  print(paste(gg$coin,"- num spikes found: ", sum(votes==1)))
  
  return(list(votes=votes, times=times, idx=idx))
  
}


classify.sequence.qda <- function(x, qd, splits=48, alpha=50) {
  w <- PAA(z.normalize(x), splits)
  sx <- convert.to.SAX.symbol(w, alpha=alpha)
  if (any(is.na(sx))) {
    return(0)
  }
  h <- data.frame(t(hist(sx, breaks=1:alpha, plot=F)$counts[2:(alpha-1)]))
  colnames(h) <- paste("V", 2:49, sep="")
  as.numeric(predict(qd, newdata=h)$class)-1
}


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
#  FIND SPIKES HISTS
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

find.spikes.hists <- function(hists) {
  results <- list()
  ttcs <- c()
  total.buys <- c()
  for (i in names(hists)[1:4]) {
    gg <- coins[[i]]
    res <- find.spikes.for.coin.hists(gg, hists[[gg$coin]])
    results[[length(results)+1]] <- res
  }
  return(results)
}


find.spikes.for.coin.hists <- function(gg, hists) {
  if (sum(gg$rvrp)==0) {
    return(0)
  }
  g2 <- features.to.cluster(gg)
  
  #votes <- rollapply(g2, width=24*60, by=60, by.column=T, FUN=classify.sequence, trans.pos, trans.neg, splits, alpha)
  # Harumph, not sure that we can rollapply given the weird dimensions needed for PAA
  num.slices <- floor((ncol(g2)-slice.size*60)/60)
  votes <- rep(0, num.slices)
  for (j in 1:num.slices) {
    x <- g2[,(60*j):(60*j+(60*slice.size))]; 
    votes[j] <- classify.sequence.hists(x, hists)
  }
  idx <- seq(slice.size*60, nrow(gg$x), 60)
  times <- gg$x$time[idx]
  plot(gg$x$price~gg$x$time, type='l', main=gg$coin)
  abline(v=times, col=main.cols[as.numeric(votes==1)+1])
  print(paste(gg$coin,"- num spikes found: ", sum(votes==1)))
  
  return(list(votes=votes, times=times, idx=idx))
  
}


classify.sequence.hists <- function(x, hh) {
  h <- hist(x, breaks=-200:200, plot=F)$counts
  
  d1 <- dist(rbind(h, hh$pos))
  d2 <- dist(rbind(h, hh$neg))
  #print(paste(d1, d2))
  if (d1 < hh$dist*1.05) {
    return(1)
  } else {
    return(0)
  }
}

plot.coin <- function(gg, names) {
  l <- length(names)
  par(mfrow=c((l+1),1),mar=c(2,2,2,2))
  plot(gg$x$price~gg$x$time, type='l', main=gg$coin)
  for (i in names) {
    plot(gg[[i]]~gg$x$time, type='l')
  }
}

