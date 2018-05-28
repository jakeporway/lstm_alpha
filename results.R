files = dir(pattern="*_results.csv")

for (f in files) {

	d <- read.csv(f)
	s1 <- sum(d[,3])
	s2 <- sum(d[,5])
	s3 <- sum(d[,7])
	m1 <- mean(d[,3])
	m2 <- mean(d[,5])
	m3 <- mean(d[,7])
	b1 <- sum(d[,4])
	b2 <- sum(d[,6])
	b3 <- sum(d[,8])

	st <- s1+s2+s3
	mt <- m1+m2+m3
	bt <- b1+b2+b3

	negs <- length(which(d[,3]<0))+length(which(d[,5]<0))+length(which(d[,7]<0))

	nr <- nrow(d)

	cat( "\n\nResults for",f)
	cat("\nTotal sum:", st,"\n")
	cat( "Total mean:", mt, "\n")
	cat("Total buys:", bt, "\n")
	cat("\n")

	cat( "Month 1:", s1, m1, b1, "\n")
	cat( "Month 2:", s2, m2, b2, "\n")
	cat( "Month 3:", s3, m3, b3, "\n")
	cat( "Neg months: ", negs, "/", nr*3, " ", negs/(nr*3), "\n", sep="")

	cat("\n---\n")
	se <-  floor(st*(180/nr)*0.05*5000/30)
	cat( "Sum estimate at BTC=$5000 (downsampled by 30):", se, "\n")
	cat( "Sum estimate per buy", se/bt, "\n")
	
	me <- floor(mt*(180/nr)*0.05*5000/30)
	cat( "Mean estimate at BTC=$5000 (downsampled by 30):", me, "\n")
	cat( "Mean estimate per buy (downsampled by 30):", me/bt, "\n")      
	cat("========================\n")
}