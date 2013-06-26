\name{recode}
\alias{recode}
\title{Recoding of variables}
\usage{
  recode(x, mat)
}
\arguments{
  \item{x}{atomic vector, variable to be recoded}

  \item{mat}{two-column matrix, the recoding scheme, see
  Details}
}
\value{
  Vector of the same mode and length as \code{x} with the
  values recoded.
}
\description{
  Given a vector and a recode matrix replace the values in
  the vector that match the first column of the matrix with
  the entries in the second column.
}
\details{
  The values is \code{x} are recoded using the schema
  provided with the matrix \code{mat}.  The values of
  \code{x} tha match the values in \code{mat[,1]} are
  replaced with the corresponding values in \code{mat[,2]}.
}
\examples{
set.seed(12345)
# variables with values 1:4
x <- sample( 1:4, 20, replace=TRUE )
x

# recode 1->2, 3->4
m <- matrix(c( 1,2, 3,4), ncol=2, byrow=TRUE)
m

# recode and show the results
r <- rbind(x=x, recoded=recode(x, m))
colnames(r) <- rep("", ncol(r))
colnames(r)[ r[1,] != r[2,] ] <- "v"
r
}
\seealso{
  \code{\link{match}}
}

