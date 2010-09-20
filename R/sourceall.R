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
