#' Measuring number of columns of textual output
#'
#' @param width numeric length of the ruler
#'
#' @export
ruler <- function(width=80)
{
  s <- seq(1, width)
  l <- strsplit(as.character(s), "")
  mx <- max(sapply(l, length))
  l <- lapply(l, function(x) append(x, rep(" ", mx - length(x))) )
  m <- do.call("cbind", l)
  apply(m, 1, function(r) cat(r, "\n", sep=""))
  invisible(NULL)
}


