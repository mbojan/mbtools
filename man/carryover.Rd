\name{carryover}
\alias{carryover}
\title{Carrying over values from previous elements}
\description{
This function recursively carries over values from previous
to subsequent elements of a vector
}
\usage{
carryover(x, empty = NA )
}
\arguments{
  \item{x}{numeric or character, vector to be processed}
  \item{empty}{numeric or character scalar, value to be replaced}
}
\details{
The function recursively imputes to all values of \code{x} that are equal
to \code{empty} the values of the preceding elements of \code{x}.

If the firs value of \code{x} is equal to \code{empty}, then an error is
returned. For now you should take care of it by manually imputing the
appropriate value.
}
\value{
A vector \code{x} with all elements equal to \code{empty} replaced
with last non-\code{empty} values.
}
\seealso{\code{\link{approx}} for other ways of filling-in the gaps in vectors}
\examples{
x <- c(1, 2, 2, NA, NA, 1, 4)

# replacing NA's with last non-NA observation
carryover(x)
}
\keyword{manip}
