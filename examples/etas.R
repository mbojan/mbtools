x1 <- rnorm(50)
x2 <- rnorm(50)
y <- 5 + 2*x1 + rnorm(50,0,2) + 3*x2 + rnorm(50,0,.5)

# method for numeric
etas( y, rep(1:2, each=25) )

# method for 'lm' which calls 'anova'
m <- lm( y ~ x1 + x2 )
etas(m)
