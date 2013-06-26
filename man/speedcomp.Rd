\name{speedcomp}
\alias{show,speedcomp-method}
\alias{speedcomp}
\alias{speedcomp-class}
\title{Comparing CPU times of several R expressions}
\usage{
  speedcomp(...)
}
\arguments{
  \item{\dots}{a collection of expressions in the form of
  tag=value}
}
\value{
  An object of S4 class \code{speedcomp} with two slots

  The included \code{\link{show}} method prints the results
  nicely and additionally calculates the ratios of times
  elapsed.
}
\description{
  This function enables comparing CPU times of several
  expressions. The times are calculated via
  \code{\link{system.time}}, additionally time ratios are
  calculated.
}
\details{
  The function uses \code{\link{system.time}} to estimate
  CPU times needed to evaluate each of the expressions. The
  output provides raw timings as well as ratios of time
  elapsed.
}
\examples{
# some testing of 'lm' fitting with different data sizes
e1 <- expression( lm( I(rnorm(10000)) ~ I(runif(10000)) ) )
e2 <- expression( lm( I(runif(20000)) ~ I(rnorm(20000)) ) )

# compare
speedcomp(e1, e2)
}
\seealso{
  \code{\link{system.time}}
}
\keyword{programming}

