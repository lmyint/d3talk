set.seed(1)
n <- 5000
samples <- 6
sampleLabels <- c(paste0("Case_", 1:3), paste0("Ctrl_", 1:3))
ids <- paste0("id", 1:n)
numer <- matrix(round(rexp(n*samples, 0.001)), nrow = n, ncol = samples)
denom <- matrix(round(rexp(n*samples, 0.01)), nrow = n, ncol = samples)
colnames(numer) <- sampleLabels
colnames(denom) <- sampleLabels

logRatio <- log2(numer+1) - log2(denom+1)
tstats <- sapply(1:nrow(logRatio), function(r) {
	t.test(logRatio[r,1:3], logRatio[,4:6])$statistic
})

df <- data.frame(id = ids, tstat = tstats, meanDenom = rowMeans(denom), logr = logRatio, numer = numer, denom = denom)
rownames(df) <- NULL
colnames(df) <- gsub("[.]", "_", colnames(df))

head(df)
write.csv(df, file = "data.csv", row.names = FALSE, quote = FALSE)