#' Rescale linearly to new minimum and maximum
#' 
#' Linerarly rescale values of a numeric vector so that the result have new
#' prescribed minimum and maximum.
#'
#' Imports \code{rescale} from package \pkg{scales}.
#' 
#' @param x numeric vector to rescale
#' @param nmin numeric scalar, new minimum, defaults to 0
#' @param nmax numeric scalar, new maximum, defaults to 1
#' @param na.rm logical, should the missing values be removed, defaults to
#' \code{TRUE}
#' @param from numeric vector of length two, range from which to rescale.
#' Defaults to the range of \code{x}.
#' @param to numeric vector of length two, output range
#' @param coef logical, should a vector with coefficients of the transformation
#' be returned instead
#'
#' @return If \code{coef} is \code{FALSE}, which is the default, a vector of
#' transformed values of \code{x} is returned. If it is \code{TRUE} then the
#' list with components \code{a} and \code{b} is returned containing the
#' coefficients of the transformation.
#' @seealso \code{\link{scale}}
#' @keywords arith
#' @example examples/rescale.R
#' @importFrom scales rescale
#' @export
rescale <- function (x, nmin = 0, nmax = 1, na.rm = TRUE,
                     from = range(x, na.rm=na.rm),
                     to = c(nmin, nmax),
                     coef = FALSE) 
{
  rval <- scales::rescale(x=x, to=to, from=from)
  if (coef) {
      return(coef(lm(rval ~ x)))
  } else
  {
    rval
  }
}
