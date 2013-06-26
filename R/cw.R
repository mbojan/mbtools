# reset number of text columns used by R, by default to the width of the
# terminal


#' Reset the number of text columns of R console.
#' 
#' Reset the number of text columns used by R console. By default reset to the
#' current width of the terminal using the \code{COLUMNS} environment variable.
#' 
#' 
#' @param w numeric, new width
#' @return List with one component called \code{width} containing the old
#' setting.
#' @seealso \code{\link{options}}
#' @export cw
cw <- function(w=Sys.getenv("COLUMNS"))
{
  options(width=w)
}
