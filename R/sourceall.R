#' Source all R-code files that match the pattern
#' 
#' This function sources the files in a specified directory that follow the
#' supplied pattern.
#' 
#' All files in the \code{dir} directory are sourced in alphabetical order via
#' \code{\link{source}}.
#' 
#' @param dir character, the directory to look for the files. Defaults to the
#' working directory
#' @param pattern character, the regular expression that defines the pattern of
#' the files to be sourced. Defaults to all files with ".R" extension.
#' @param \dots other arguments passed to \code{source()}
#' @seealso \code{\link{source}}
#' @keywords file programming
#' @export sourceall
sourceall <- function( dir=getwd(), pattern=".*\\.R", ... )
{
	l <- list.files( dir, pattern=pattern, )
	cat( "Sourced from", dir, "\n")
	for( f in l ) {
		cat( f, fill=TRUE )
		source( file.path(dir, f), ... )
	}
	invisible(l)
}
