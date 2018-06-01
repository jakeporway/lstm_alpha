args = commandArgs(trailingOnly=T)
files = dir(pattern=args[1])

for (f in files) {

	d <- read.csv(f)
	s1 <- sum(d[,3])
	s2 <- sum(d[,5])

	m1 <- mean(d[,3])
	m2 <- mean(d[,5])

	b1 <- sum(d[,4])
	b2 <- sum(d[,6])


	st <- s1+s2
	mt <- m1+m2
	bt <- b1+b2

	negs <- length(which(d[,3]<0))+length(which(d[,5]<0))

	nr <- nrow(d)

	cat( "\n\nResults for",f)
	cat("\nTotal sum:", st,"\n")
	cat( "Total mean:", mt, "\n")
	cat("Total buys:", bt, "\n")
	cat("\n")

	cat( "Month 1:", s1, m1, b1, "\n")
	cat( "Month 2:", s2, m2, b2, "\n")
	cat( "Neg months: ", negs, "/", nr*2, " ", negs/(nr*2), "\n", sep="")


	num_coins <- nr
	btc_investment <- 0.05
	btc_price <- 5000
	
	cat("\n---\n")
	se <-  floor(st*(num_coins/nr)*btc_investment*btc_price/30)
	cat( "Sum estimate at BTC=$5000 (downsampled by 30):", se, "\n")
	cat( "Sum estimate per buy", se/bt, "\n")
	
	me <- floor(mt*(num_coins/nr)*btc_investment*btc_price/30)
	cat( "Mean estimate at BTC=$5000 (downsampled by 30):", me, "\n")
	cat( "Mean estimate per buy (downsampled by 30):", me/bt, "\n")      
	cat("========================\n")
}