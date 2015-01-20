# create some matrix
m <- matrix( c(0, 1, 1, 0), ncol=2 )
m
is.symmetric(m) # TRUE

m1 <- m
m1[2,1] <- 0
m1
is.symmetric(m1) # FALSE

# supplying a non-square matrix
m2 <- matrix( 1:6, ncol=2 )
is.symmetric(m2) # FALSE with warning

# clean-up
rm(m, m1, m2)

