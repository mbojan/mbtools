\name{setcompare}
\alias{setcompare}
\alias{setcompare.default}
\alias{setcompare.data.frame}
\title{Compare to sets}
\description{
This is an wrapper around \code{\link{setdiff}} and \code{\link{intersect}}
to quickly compare to sets.
}
\usage{
setcompare(x, y)
\method{setcompare}{data.frame}(x, y)
}
\arguments{
  \item{x, y}{sets to compare. Implemented methods support atomic vectors and
  data frames, see Details}
}
\details{
The default method expects atomic vectors.

The method for data frames compares the sets of variable names.
}
\value{
A list with three components
\describe{
  \item{x}{Elements of \code{x} not fount in \code{y}}
  \item{int}{Elements present in both \code{x} and \code{y}}
  \item{y}{Elements in \code{y} not found in \code{x}}
}
}
\seealso{
\code{\link{setdiff}}, \code{\link{intersect}}
}
\examples{
# default method for vectors
x <- 1:5
y <- 3:7
setcompare(x, y)
setcompare(letters[x], letters[y]) 

# method for data frames
d1 <- structure(as.data.frame(matrix(rnorm(25), 5, 5)), names=letters[x])
d2 <- structure(as.data.frame(matrix(rnorm(25), 5, 5)), names=letters[y])
setcompare(d1, d2)
}
