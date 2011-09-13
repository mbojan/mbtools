\name{cw}
\alias{cw}
\title{
  Reset the number of text columns of R console.
}
\description{
  Reset the number of text columns used by R console. By default reset to the
  current width of the terminal using the \code{COLUMNS} environment variable.
}
\usage{
cw(w = Sys.getenv("COLUMNS"))
}
\arguments{
  \item{w}{numeric, new width}
}
\value{
  List with one component called \code{width} containing the old setting.
}
\seealso{\code{\link{options}}}
