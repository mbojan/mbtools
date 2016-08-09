#' Recoding variables (vectors)
#' 
#' This function has been moved to package \pkg{recode} at \url{https://github.com/mbojan/recode}.
#'
#'
#' 
#' @param x atomic vector, variable to be recoded
#' @param fromto two-column matrix or data frame, or a list
#' @param ... Only used in the default method and expects further elements of
#' the recoding rule set
#'
#' @return Vector of the same mode as second column of \code{fromto} and length
#' as \code{x} with the values recoded.
#'
#' @seealso \code{\link{match}}
#'
#' @example examples/recode.R
#' @export recode


recode <- function(x, fromto, ...)
{
  .Deprecated(
      new = "recode::recode",
      package = "recode",
      msg = paste(
        "Function recode() has been moved to package 'recode'.",
        "See https://github.com/mbojan/recode",
        sep=" "
      )
    )
}
