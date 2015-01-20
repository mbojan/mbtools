#' Test for a symmetric matrix
#' 
#' Test whether the supplied matrix is symmetric around diagonal
#' 
#' I the matrix \code{m} is not a square matrix the function returns
#' \code{FALSE} and issues a warning.
#' 
#' The testing boils down to comparing lower and upper triangles of the matrix
#' \code{m} using the function \code{identical}.
#' 
#' @param m a matrix to be tested
#'
#' @return Logical, whether the matrix is symmetric or not.
#'
#' @seealso See \code{\link{identical}}, \code{\link{matrix}},
#' \code{\link{upper.tri}}. Also \code{\link[sna]{symmetrize}} in package
#' \pkg{sna}.
#'
#' @export
#'
#' @example examples/is.symmetric.R
is.symmetric <- function(m)
{
    if( !is.matrix(m) )
	stop("'m' must be a matrix")
    if( dim(m)[1] != dim(m)[2] )
    {
	warning("'m' is not square")
	return(FALSE)
    }
    # compare lower and upper triangles
    ltr <- m[lower.tri(m)]
    tm <- t(m)
    utr <- tm[lower.tri(tm)]
    identical( ltr, utr )
}


