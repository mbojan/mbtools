#' Carrying over values from previous elements
#' 
#' This function recursively carries over values from previous to subsequent
#' elements of a vector
#' 
#' The function recursively imputes to all values of \code{x} that are equal to
#' \code{empty} the values of the preceding elements of \code{x}.
#' 
#' If the firs value of \code{x} is equal to \code{empty}, then an error is
#' returned. For now you should take care of it by manually imputing the
#' appropriate value.
#' 
#' @param x numeric or character, vector to be processed
#' @param empty numeric or character scalar, value to be replaced
#' @return A vector \code{x} with all elements equal to \code{empty} replaced
#' with last non-\code{empty} values.
#' @seealso \code{\link{approx}} for other ways of filling-in the gaps in
#' vectors
#' @keywords manip
#' @examples
#' 
#' x <- c(1, 2, 2, NA, NA, 1, 4)
#' 
#' # replacing NA's with last non-NA observation
#' carryover(x)
#' 
#' @export carryover
carryover <-
function (x, empty = NA) 
{
    if (is.na(empty)) 
        ind <- which(is.na(x))
    else ind <- which(x == empty)
    if (1 %in% ind) {
        stop("First observation missing, you need to impute it by hand!")
    }
    if (length(ind) != 0) {
        rval <- x
        rval[ind] <- x[ind - 1]
        carryover(rval, empty = empty)
    }
    else return(x)
}
