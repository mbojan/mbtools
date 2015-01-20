set.seed(1234)
x <- matrix(rnorm(1000), 250, 4)
y <- rnorm(250)
hellwig(y, x)
