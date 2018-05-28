files = dir(pattern="*_results.csv")

for (f in files) {

	d <- read.csv(f)
	s1 <- sum(d[,3])
	s2 <- sum(d[,5])
	s3 <- sum(d[,7])
	m1 <- mean(d[,3])
	m2 <- mean(d[,5])
	m3 <- mean(d[,7])
	st <- s1+s2+s3
	mt <- m1+m2+m3

	negs <- length(which(d[,3]<0))+length(which(d[,5]<0))+length(which(d[,7]<0))

	nr <- nrow(d)

	cat( "\n\nResults for",f)
	cat("\nTotal sum:", st,"\n")
	cat( "Total mean:", mt, "\n")
	cat("\n")

	cat( "Month 1:", s1, m1, "\n")
	cat( "Month 2:", s2, m2, "\n")
	cat( "Month 3:", s3, m3, "\n")
	cat( "Neg months: ", negs, "/", nr*3, " ", negs/(nr*3), "\n", sep="")

	cat("\n---\n")
	cat( "Sum estimate at BTC=$5000 (downsampled by 30):", floor(st*(180/nr)*0.05*5000/30), "\n")
	cat( "Mean estimate at BTC=$5000 (downsampled by 30):", floor(mt*(180/nr)*0.05*5000/30), "\n")
	cat("========================\n")
}