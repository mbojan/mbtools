# Inspect objects in user-friendly way

setGeneric("see", function(o, ...) standardGeneric("see"))

setMethod("see", "ANY",
function(o, ... )
{
    # create a temporary file and dump the function
    file <- paste( tempfile(), ".R", sep="")
    e <- new.env()
    nam <- deparse(substitute(o))
    assign( nam, o, envir=e )
    dump( nam, file=file, envir=e )
    # open the file in the editor
    r <- try( file.show( file, pager=getOption("editor"), title=nam,
	delete.file=TRUE,... ), silent=TRUE )
    if(inherits(r, "try-error"))
    {
	stop("file showing did not work")
	print(r)
	cat("Check if 'options(editor)' is set correctly, currently:",
	    getOption("editor"), "\n")
	cat("Check if temporary folder is writable, currently:",
	    tempdir(), "\n")
    }
    # clean-up
    invisible(NULL)
} )

setMethod("see", "data.frame",
function(o, ...)
{
    edit(o, ...)
    return(NULL)
} )





f <- function(x)
{
    cat("Mode:", mode(x), "\n")
    cat("is.language:", is.language(x), "\n")
}

