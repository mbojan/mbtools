set.seed(123)
x <- rnorm(20)
y <- rescale(x)
summary(x)
summary(y)
# coefficients of the transformation
k <- rescale(x, coef=TRUE)
k

# show how it works on the plot
plot(x, y, xlab="x", ylab="Result", main="Mechanics of 'rescale()'",
  axes=TRUE, asp=1, sub=paste("a=", k[1], ", b=", k[2], sep=""))
abline(v=c(min(x), max(x)), lty=2, col="gray")
abline(h=c(min(y), max(y)), lty=2, col="gray")
abline(a=k[1], b=k[2], col="gray")
text( (max(x) + min(x))/2, (max(y) + min(y))/2,
  label="a + bx", srt=tan(k[2]) / pi * 180, pos=3)
