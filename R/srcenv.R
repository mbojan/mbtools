#' Return an environment with objects defined in a script file
#'
#' @param file R file to source
#' @param ... other arguments passed to \code{sys.source}
#' 
#' @export 
srcenv <- function(file, ...) {
  e <- new.env(parent.frame())
  sys.source(file, envir=e, ...)
  e
}
