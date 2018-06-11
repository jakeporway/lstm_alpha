files = dir(pattern="*training_through_apr.csv")
output_filename="_oct_may_diff.csv"
for (f in files) {
  print(paste("Writing", f, "diff"))
  d <- read.csv(f, h=T)
  res2 <- apply(d, 2, diff)
  d <- cbind(d[2:nrow(d),-ncol(d)], res2[,-ncol(res2)], d[2:nrow(d),ncol(d)])
  coin=strsplit(f, "_")[[1]][1]
  write.csv(d, file=paste(coin, output_filename, sep=""), row.names=F)
}