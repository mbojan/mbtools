\name{attributize}
\alias{attributize}
\alias{attributize.default}
\alias{attributize.data.frame}
\title{Map a vector to several plotting attributes}
\description{
  Given a vector with likely high number of unique values create a list of
  vectors which combinations cover the unique values of the input vector.
}
\usage{
attributize(x, ...)
\method{attributize}{default}(x, ..., na.last = FALSE, vcol = ncol(d))
\method{attributize}{data.frame}(x, vec, vcol = ncol(x), ...)
}
\arguments{
  \item{x,vec}{input vector}
  \item{na.last}{logical, passed to \code{sort}}
  \item{vcol}{numeric vector, column id with input vector values}
  \item{\dots}{other arguments passed to/from other methods}
}
\value{
  data frame
}
