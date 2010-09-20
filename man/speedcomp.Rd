\name{speedcomp}
\alias{speedcomp}
\alias{speedcomp-class}
\alias{show,speedcomp-method}
\title{Comparing CPU times of several \R expressions}
\description{
This function enables comparing CPU times of several \R expressions.
The times are calculated via \code{\link{system.time}},
additionally time ratios are calculated.}
\usage{
speedcomp(...)
}
\arguments{
  \item{\dots}{a collection of \R expressions in the form of tag=value}
}
\details{
The function uses \code{\link{system.time}} to estimate CPU times
needed to evaluate each of the expressions. The output
provides raw timings as well as ratios of time elapsed.
}
\value{
An object of S4 class \code{speedcomp} with two slots
  \item{e}{a list of \R expressions processed}

  \item{timings}{a matrix containing the CPU times of the expressions in
  \code{e} estimated with \code{\link{system.time}}. Rows correspond to
  expressions and columns to the fields returned by \code{\link{system.time}},
  i.e. "user cpu", "system cpu", "elapsed", "subproc1" and "subproc2".}

The included \code{\link{show}} method prints the results
nicely and additionally calculates the ratios of times elapsed.
}
\seealso{\code{\link{system.time}}}
\examples{
# some testing of 'lm' fitting with different data sizes
e1 <- expression( lm( I(rnorm(10000)) ~ I(runif(10000)) ) )
e2 <- expression( lm( I(runif(20000)) ~ I(rnorm(20000)) ) )

# compare
speedcomp(e1, e2)

}
\keyword{programming}
