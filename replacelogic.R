replace.nas <- function(myvec) {
  chkint <- as.character(myvec[3])
  myvec[1] <- intsum.ordered$avg.steps[intsum.ordered$interval == chkint]
}

test1 <- activity.table
# test1$interval <- as.character(test1$interval)
testmiss <- is.na(test1$steps)


test1 <- sapply(test1[testmiss, ], replace.nas)