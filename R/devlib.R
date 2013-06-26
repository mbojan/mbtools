#' Load package from developer's library
#' 
#' Load package from developer's library.
#' 
#' 
#' @param libloc Path to developer's library
#' @param \dots other arguments passed to \code{\link{library}}
#' @return Nothing of interest, but the package is loaded.
#' @seealso \code{\link{library}}, \code{\link{.libPaths}}
#' @export devlib
devlib <- function(..., libloc=getOption("devlib"))
{
  stopifnot(!is.null(libloc))
  library(..., lib.loc=libloc)
}
