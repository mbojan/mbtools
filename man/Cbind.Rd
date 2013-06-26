\name{Cbind}
\alias{Cbind}
\title{Combine objects by columns matching the rows on row names}
\usage{
  Cbind(...)
}
\arguments{
  \item{\dots}{matrices, vectors or data.frames that are to
  be combined. These are converted to data frames before
  merging.}
}
\value{
  Data frame consisting of merged objects.
}
\description{
  Take a sequence of matrix-like objects and combine the
  column-wise matching the rows on the common row names.
}
\details{
  All the supplied objects have to have row names defined,
  i.e. calling \code{\link{rownames}} on the should return
  character vector.

  All the objects are converted to data frames prior
  combining.

  The result will have the number of columns equal to the
  sum of the numbers of columns in the supplied objects.
  The number of rows will be equal to the number of unique
  row names in all the arguments.
}
\examples{
# two matrices
m1 <- m2 <- matrix(1:4, 2, 2)
colnames(m1) <- colnames(m2) <- 1:2
rownames(m1) <- c("a", "b")
rownames(m2) <- c("b", "c")
Cbind(m1, m2)

# matrices and a data frame
l <- list(  m1 = m1,
            m2 = m2,
            d1 = data.frame( x=1:4, y=2:5,
              row.names=letters[1:4])
  )
do.call("Cbind", l)
}
\seealso{
  \code{\link{cbind}}, \code{\link{rownames}}
}

