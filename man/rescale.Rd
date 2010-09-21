\name{rescale}
\alias{rescale}
\title{Rescale linearly to new minimum and maximum}
\description{
Linerarly rescale values of a numeric vector so that the result
have new prescribed minimum and maximum.
}
\usage{
rescale(x, nmin = 0, nmax = 1, na.rm = FALSE, coef=FALSE)
}
\arguments{
  \item{x}{numeric vector to rescale}
  \item{nmin}{numeric scalar, new minimum, defaults to 0}
  \item{nmax}{numeric scalar, new maximum, defaults to 1}
  \item{na.rm}{logical, should the missing values be removed, defaults to
  \code{FALSE}}
  \item{coef}{logical, should a list with coefficients of the transformation be
  returned instead}
}
\details{
The function linearly rescales the values of \code{x} so that the minimum and
maximum of the result are equal to the values supplied with \code{nmin} and
\code{nmax}.

The formula for the transformation is \eqn{a + bx} where the coefficients of
the transformation are given by:
\deqn{a = nmin - \frac{\min(x) (nmax - nmin)}{\max(x) - \min(x)}}{a = nmin - (min(x) * (nmax - nmin)) / (max(x) - min(x))}
and
\deqn{b = \frac{nmax - nmin}{\max(x) - \min(x)}}{b = (nmax - nmin) / (omax - omin)}

If \code{x} contains missing data (\code{NA}s) and \code{na.rm}
is not \code{TRUE}, the function will return an error.

If \code{coef} is \code{TRUE} then instead of transformed values
of \code{x} the coefficients \eqn{a} and \eqn{b} are returned in a list.
}
\value{
If \code{coef} is \code{FALSE}, which is the default, a vector
of transformed values of \code{x} is returned. If it is \code{TRUE}
then the list with components \code{a} and \code{b} is returned
containing the coefficients of the transformation.
}
\seealso{\code{\link{scale}}}
\examples{
# simple functionality
x <- rnorm(20)
y <- rescale(x)
summary(x)
summary(y)
# coefficients of the transformation
k <- rescale(x, coef=TRUE)
k

# show how it works on the plot
plot(x, y, xlab="x", ylab="Result", main="Mechanics of 'rescale()'",
  axes=TRUE, asp=1, sub=paste("a=", k$a, ", b=", k$b, sep=""))
abline(v=c(min(x), max(x)), lty=2, col="gray")
abline(h=c(min(y), max(y)), lty=2, col="gray")
abline(a=k$a, b=k$b, col="gray")
text( (max(x) + min(x))/2, (max(y) + min(y))/2,
  label="a + bx", srt=tan(k$b) / pi * 180, pos=3)
}
\keyword{arith}
