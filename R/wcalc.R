#' Calculate the column widths of a fixed-width file
#' 
#' Calculate column widths of a plain text fixed-width file from column numbers
#' on which each field begins.
#' 
#' The first element of the \code{p} argument should be 1 as it is the first
#' column of the first field. Subsequent numbers should correspond to the
#' column numbers that begin other fields. The last number of \code{p} should
#' be the end of the last field + 1 or, equivalently, the begining of the next
#' (non-existing) field.
#' 
#' @param p numeric vector of column numbers, should start with 1 and end with
#' the number of the last column in a file+1
#' @return Numeric vector of column widths.
#' @keywords manip utilities
#' @examples
#' 
#' # TODO add examples
#' 
#' @export wcalc
wcalc <-
function (p) 
{
    x <- c(1, p[seq(1, length(p) - 1)])
    p - x
}
