\docType{methods}
\name{see}
\alias{see}
\alias{see,ANY-method}
\alias{see,data.frame-method}
\alias{see-methods}
\title{Inspect the content of an object}
\arguments{
  \item{o}{object to be inspected}

  \item{\dots}{arguments passed to other methods}
}
\value{
  Either \code{NULL} or the result invisibly.
}
\description{
  Inspect the content of an object, usually in an external
  editor. Methods are two implemented for functions and
  data frames.
}
\details{
  For data frames the function uses \code{\link{edit}}. The
  result is returned invisibly.

  For any other objects \ code is generated via
  \code{\link{dump}} to a temporary file and the file is
  opened in an external editor. The editor is taken from
  \code{getOption("editor")}.

  In case of the data frames the console waits until the
  file is closed.
}
\examples{
\dontrun{
# for function
see(lm)
# for data.frame
see( data.frame(x=1:5, y=5:1) )
}
}
\seealso{
  \code{\link{edit}}
}
\keyword{utilities}

