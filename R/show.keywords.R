#' Show the Keywords list
#' 
#' This function will show the Keywords list needed for writing R Documentation
#' files in Rd format.
#' 
#' The function list the \code{file}. By default it is taken to be a
#' \file{KEYWORDS} file in \file{RHOME/doc/KEYWORDS}. The value of the RHOME
#' variable is taken by \code{Sys.getenv} function.
#' 
#' @param file path to file with keywords, defaults to
#' \file{RHOME/doc/KEYWORDS}
#' @return Nothing, i.e. \code{NULL}.
#' @seealso \code{\link{file.show}}, \code{\link{Sys.getenv}}
#' @keywords file utilities
#' @examples
#' 
#' show.keywords()
#' 
#' @export show.keywords
show.keywords <- function( file=NULL )
{
    if( is.null(file) )
    {
        rhome <- Sys.getenv("R_HOME")
        file <- file.path( rhome, "doc", "KEYWORDS" )
    }
    file.show( file, header="Keywords for R documentation")
}

