\name{show.keywords}
\alias{show.keywords}
\title{Show the Keywords list}
\description{
This function will show the Keywords list needed for writing R Documentation
files in Rd format.
}
\usage{
show.keywords(file = NULL)
}
\arguments{
  \item{file}{path to file with keywords, defaults to \file{RHOME/doc/KEYWORDS}}
}
\details{
The function list the \code{file}. By default it is taken to be
a \file{KEYWORDS} file in \file{RHOME/doc/KEYWORDS}. The value
of the RHOME variable is taken by \code{Sys.getenv} function.
}
\value{
Nothing, i.e. \code{NULL}.
}
\seealso{\code{\link{file.show}}, \code{\link{Sys.getenv}}}
\examples{
show.keywords()
}
\keyword{file}
\keyword{utilities}
