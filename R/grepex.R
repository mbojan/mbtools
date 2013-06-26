# extracting from 'x' based on results of 'regexpr'
extract <- function(x, v)
{
    rval <- sapply( seq(along=v), function(i)
            substr(x[i], v[i], v[i] + attr(v, "match.length")[i] - 1) )
    rval[ v == -1 ] <- NA
    rval
}




#' Extract portions of string(s) with regular expressions
#' 
#' Given a character vector extract portions of each element with regular
#' expressions.
#' 
#' Every element of \code{x} is searched with patterns in \code{pattern} using
#' \code{\link{regexpr}}. The matches are then extracted.
#' 
#' @param pattern character vector, regular expressions
#' @param x character vector, strings from which to extract
#' @param multiple logical, currently ignored
#' @param \dots other arguments passed to \code{\link{regexpr}}
#' @return A matrix \code{r}, say, with \code{length(pattern)} columns and
#' \code{length(x)} rows. The entry \code{r[i,j]} of this matrix is a portion
#' of \code{x[i]} which is \emph{first match} of \code{pattern[i]}.
#' @seealso \code{\link{regexpr}}
#' @examples
#' 
#' x <- c("On 2010-02-01 John Smith had a beer",
#'         "On 2010-02-02 Adam Smith did not have a beer",
#'         "On 2010-02-03 Bob Smith had a beer again")
#' 
#' re <- c( isodate="\\<[0-9]{4}-[0-9]{2}-[0-9]{2}\\>",
#'         name="\\<[A-Z][a-z]+\\> \\<[A-Z][a-z]+\\>",
#'         beer="\\<beer\\>")
#' 
#' grepex(re, x)
#' 
#' @export grepex
grepex <- function(pattern, x, multiple=FALSE, ...)
{
    r <- sapply(pattern, function(p)
        {
            if(!multiple)
            {
                v <- regexpr(p, x, ...)
                extract(x, v)
            } else
            {
                .NotYetImplemented()
            }
        })
    r
}
