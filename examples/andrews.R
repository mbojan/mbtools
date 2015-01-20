### Using artificial data
d <- data.frame( x = c( rnorm(50, 1), rnorm(50, 4), rnorm(50, 7)),
    y = c( rnorm(50,1), rnorm(50, 7), rnorm(50, 1)),
    k = rep(1:3, each=50) )
# plotting
# show data
plot(d$x, d$y, pch=d$k)
# Andrew's plots
layout( matrix(1:4, ncol=2))
andrews( d[1:2], main="Unsupervised x,y")
andrews( d[2:1], main="Unsupervised y,x")
andrews( d[1:2], col=d$k, main="Color-coded x,y")
andrews( d[2:1], col=d$k, main="Color-coded y,x")
# three curves for cluster means
andrews( cbind( tapply(d$x, d$k, mean), tapply(d$y, d$k, mean)),
    col=1:3 )

### Using 'iris' data
d <- iris[1:4]
# Andrew's plots
layout( matrix(1:4, ncol=2) )
# "unsupervised"
andrews(d, lty=1, col=1, main="Andrew's plot of Iris data")
# colored with species
andrews(d, lty=1, col=match( iris$Species, unique(iris$Species)),
    main="Andrew's plot of Iris data\n color-coded species")
# Andrew's plot on standardized data
andrews( scale(d), main="Andrew's plot of standardized Iris data")
# Andrew's plot on principal components
pcad <- princomp(d)
andrews( pcad$scores, main="Andrew's plot of PCA of Iris data")
