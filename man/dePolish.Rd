\name{dePolish}
\alias{dePolish}
\title{Rip Polish letters of the diacritics}
\description{
The function substitutes all Polish letters in the provided character vector
with ASCII counterparts.
}
\usage{
dePolish(x, encoding=NULL)
}
\arguments{
  \item{x}{character vector, do be processed}
  \item{encoding}{character, encoding used in \code{x}}
}
\details{
The function substitutes (with \code{sub}) case-sensitively all Polish letters
with simple ASCII counterparts.

The \code{encoding} argument defines the way in which Polish letters are encoded
in \code{x}. If \code{NULL} and the platform is Windows the function
uses CP1250 encoding. If not on Windows the function uses \code{\link{iconv}}
to translate the letters to default encoding on current locale (via argument
\code{from=""}.
}
\value{
A character vector after substitutions
}
\seealso{\code{\link{sub}}, \code{\link{iconv}}}
\examples{
\dontrun{
#  some Polish family name
x <- "G\xbfeg\xbf\xf3\xb3ka"
dePolish(x)
rm(x) # cleanup
}
}
\keyword{character}
