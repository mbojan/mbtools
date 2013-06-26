#' Compare to sets
#' 
#' This is an wrapper around \code{\link{setdiff}} and \code{\link{intersect}}
#' to quickly compare to sets.
#' 
#' The default method expects atomic vectors.
#' 
#' The method for data frames compares the sets of variable names.
#' 
#' @aliases setcompare setcompare.default setcompare.data.frame
#' @param x,y sets to compare. Implemented methods support atomic vectors and
#' data frames, see Details
#' @return A list with three components \describe{ \item{x}{Elements of
#' \code{x} not fount in \code{y}} \item{int}{Elements present in both \code{x}
#' and \code{y}} \item{y}{Elements in \code{y} not found in \code{x}} }
#' @seealso \code{\link{setdiff}}, \code{\link{intersect}}
#' @examples
#' 
#' # default method for vectors
#' x <- 1:5
#' y <- 3:7
#' setcompare(x, y)
#' setcompare(letters[x], letters[y]) 
#' 
#' # method for data frames
#' d1 <- structure(as.data.frame(matrix(rnorm(25), 5, 5)), names=letters[x])
#' d2 <- structure(as.data.frame(matrix(rnorm(25), 5, 5)), names=letters[y])
#' setcompare(d1, d2)
#' 
#' @export setcompare
setcompare <- function(x, y) UseMethod("setcompare")


setcompare.default <- function(x, y)
{
  list( x=setdiff(x, y),
      int=intersect(x, y),
      y=setdiff(y, x) )
}

setcompare.data.frame <- function(x, y)
{
  setcompare.default( names(x), names(y))
}
