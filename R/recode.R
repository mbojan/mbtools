#' Recoding of variables
#' 
#' Given a vector and a recode matrix replace the values in the vector that
#' match the first column of the matrix with the entries in the second column.
#' 
#' The values is \code{x} are recoded using the schema provided with the matrix
#' \code{mat}.  The values of \code{x} tha match the values in \code{mat[,1]}
#' are replaced with the corresponding values in \code{mat[,2]}.
#' 
#' @param x atomic vector, variable to be recoded
#' @param mat two-column matrix, the recoding scheme, see Details
#' @return Vector of the same mode and length as \code{x} with the values
#' recoded.
#' @seealso \code{\link{match}}
#' @example examples/recode.R
#' 
#' 
#' @export recode
recode <-
function (x, mat) 
{
    i <- match(x, mat[, 1])
    i2 <- which(!is.na(i))
    i <- i[i2]
    rval <- x
    rval[i2] <- mat[, 2][i]
    rval
}
