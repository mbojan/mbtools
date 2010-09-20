\name{see}
\docType{methods}
\alias{see}
\alias{see-methods}
\alias{see,ANY-method}
\alias{see,data.frame-method}
\title{Inspect the content of an object}
\description{
Inspect the content of an object, usually in an external editor. Methods are
two implemented for functions and data frames.
}
\usage{
see(o, ...)
\S4method{see}{ANY}(o, ...)
\S4method{see}{data.frame}(o, ...)
}
\arguments{
  \item{o}{object to be inspected}
  \item{\dots}{arguments passed to other methods}
}
\details{
For data frames the function uses \code{\link{edit}}. The result is returned
invisibly.

For any other objects \R\ code is generated via \code{\link{dump}} to a
temporary file and the file is opened in an external editor. The editor is
taken from \code{getOption("editor")}.

In case of the data frames the \R console waits until the file is closed.
}
\value{
Either \code{NULL} or the result invisibly.
}
\seealso{\code{\link{edit}}}
\examples{
\dontrun{
# for function
see(lm)
# for data.frame
see( data.frame(x=1:5, y=5:1) )
}
}
\keyword{utilities}
