\name{workingDir}
\alias{getwd}
\alias{setwd}
\title{Enhancing working directory functionality}
\description{
These functions are enhancements of the original \code{\link[base]{getwd}}
and \code{\link[base]{setwd}} functions from package \pkg{base}. The enhancements
include putting the current directory in the name of the console window.
}
\usage{
getwd()
setwd(dir)
}
\arguments{
  \item{dir}{character string}
}
\details{
See help files of the originals \code{\link[base]{getwd}},
\code{\link[base]{setwd}}.
}
\seealso{
Originals \code{\link[base]{getwd}}, \code{\link[base]{setwd}}.
As well as \code{\link[utils]{setWindowTitle}}.
}
\examples{
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
}
