\name{grepex}
\alias{grepex}
\title{
Extract portions of string(s) with regular expressions
}
\description{
Given a character vector extract portions of each element with
regular expressions.
}
\usage{
grepex(pattern, x, multiple = FALSE, ...)
}
\arguments{
  \item{pattern}{character vector, regular expressions}
  \item{x}{character vector, strings from which to extract}
  \item{multiple}{logical, currently ignored}
  \item{\dots}{other arguments passed to \code{\link{regexpr}}}
}
\details{
Every element of \code{x} is searched with patterns in \code{pattern}
using \code{\link{regexpr}}. The matches are then extracted.
}
\value{
A matrix \code{r}, say, with \code{length(pattern)} columns and
\code{length(x)} rows. The entry \code{r[i,j]} of this matrix is a portion of
\code{x[i]} which is  \emph{first match} of \code{pattern[i]}.
}
\seealso{
\code{\link{regexpr}}
}
\examples{
x <- c("On 2010-02-01 John Smith had a beer",
        "On 2010-02-02 Adam Smith did not have a beer",
        "On 2010-02-03 Bob Smith had a beer again")

re <- c( isodate="\\\\<[0-9]{4}-[0-9]{2}-[0-9]{2}\\\\>",
        name="\\\\<[A-Z][a-z]+\\\\> \\\\<[A-Z][a-z]+\\\\>",
        beer="\\\\<beer\\\\>")

grepex(re, x)
}
