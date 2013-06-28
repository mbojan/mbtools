#' Rescale linearly to new minimum and maximum
#' 
#' Linerarly rescale values of a numeric vector so that the result have new
#' prescribed minimum and maximum.
#' 
#' The function linearly rescales the values of \code{x} so that the minimum
#' and maximum of the result are equal to the values supplied with \code{nmin}
#' and \code{nmax}.
#' 
#' The formula for the transformation is \eqn{a + bx} where the coefficients of
#' the transformation are given by: \deqn{a = nmin - \frac{\min(x) (nmax -
#' nmin)}{\max(x) - \min(x)}}{a = nmin - (min(x) * (nmax - nmin)) / (max(x) -
#' min(x))} and \deqn{b = \frac{nmax - nmin}{\max(x) - \min(x)}}{b = (nmax -
#' nmin) / (omax - omin)}
#' 
#' If \code{x} contains missing data (\code{NA}s) and \code{na.rm} is not
#' \code{TRUE}, the function will return an error.
#' 
#' If \code{coef} is \code{TRUE} then instead of transformed values of \code{x}
#' the coefficients \eqn{a} and \eqn{b} are returned in a list.
#' 
#' @param x numeric vector to rescale
#' @param nmin numeric scalar, new minimum, defaults to 0
#' @param nmax numeric scalar, new maximum, defaults to 1
#' @param na.rm logical, should the missing values be removed, defaults to
#' \code{FALSE}
#' @param coef logical, should a list with coefficients of the transformation
#' be returned instead
#' @return If \code{coef} is \code{FALSE}, which is the default, a vector of
#' transformed values of \code{x} is returned. If it is \code{TRUE} then the
#' list with components \code{a} and \code{b} is returned containing the
#' coefficients of the transformation.
#' @seealso \code{\link{scale}}
#' @keywords arith
#' @example examples/rescale.R
#' @export
rescale <-
function (x, nmin = 0, nmax = 1, na.rm = FALSE, coef = FALSE) 
{
    omin <- min(x, na.rm = na.rm)
    omax <- max(x, na.rm = na.rm)
    b <- (nmax - nmin)/(omax - omin)
    a <- nmin - (omin * (nmax - nmin))/(omax - omin)
    if (coef) {
        return(list(a = a, b = b))
    }
    else {
        return(x * b + a)
    }
}
