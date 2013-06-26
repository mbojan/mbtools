\name{attributize}
\alias{attributize}
\alias{attributize.data.frame}
\alias{attributize.default}
\title{Map a vector to several plotting attributes}
\usage{
  attributize(x, ...)

  \method{attributize}{data.frame} (x, vec, vcol = ncol(x),
    ...)

  \method{attributize}{default} (x, ..., na.last = FALSE,
    vcol = ncol(d))
}
\arguments{
  \item{x,vec}{input vector}

  \item{na.last}{logical, passed to \code{sort}}

  \item{vcol}{numeric vector, column id with input vector
  values}

  \item{\dots}{other arguments passed to/from other
  methods}
}
\value{
  data frame
}
\description{
  Given a vector with likely high number of unique values
  create a list of vectors which combinations cover the
  unique values of the input vector.
}

