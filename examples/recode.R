set.seed(12345)
# variables with values 1:4
x <- sample( 1:4, 20, replace=TRUE )
x

# recode 1->2, 3->4
m <- matrix(c( 1,2, 3,4), ncol=2, byrow=TRUE)
m

# recode and show the results
r <- rbind(x=x, recoded=recode(x, m))
colnames(r) <- rep("", ncol(r))
colnames(r)[ r[1,] != r[2,] ] <- "v"
r
