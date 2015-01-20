#' Exporting R objects to MS Office programs
#' 
#' This function exports R objects into Windows clipboard in a format that
#' makes the further work in MS Office easier. Current methods cover matrices
#' and data frames.
#' 
#' There are currently three methods: for matrices, tables, and data frames.
#' Data frames and tables are converted to matrices. \code{tcb} is an alias for
#' \code{toWord}.
#' 
#' For any other objects the function tries to coerce its argument to matrix.
#' 
#' If \code{useNames} is \code{TRUE} then the object is expanded to contain row
#' and column names. The first cell in the first row contain dimension names
#' separated with a backslash.
#' 
#' This function converts the object to a tab-separated table and puts it in
#' the Windows clipboard. The content can then be pasted in to MS Word or MS
#' Excel. For making tables in MS Word you will have to first create an empty
#' table with appropriate number of rows and columns. To aid you in this after
#' the \code{toWord} is called it displays an informative message about the
#' dimensionality of the exported table. As a next step you have to select the
#' whole table in MS Word, preferably via menus: Table|Select|Table.  Then
#' paste in the content of the clipboard.
#' 
#' In MS Excel you are not required to select the appropriate area of the
#' spread sheet. It is sufficient to put the cursor (cell selector) in the
#' upper-left corner of the to-be-table.
#' 
#' @param object object to processed, currently matrix, data frame or table are
#' supported directly. Other objects are coerced to matrix.
#' @param useNames logical, whether to use dimension names
#' @param \dots other arguments to methods
#'
#' @return The converted object is put to clipboard and returned invisibly.
#'
#' @note This function will work only on MS Windows distributions of R
#'
#' @seealso \code{writeClipboard} and package \pkg{xtable}
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # 'showClip' function for testing only: 
#' # print the contents of the clipboard to the console
#' showClip <- function()
#' {
#'   d <- readClipboard( format=1, raw=FALSE )
#'   cat(d, sep="\n")
#'   cat("\n")
#' }
#' 
#' # some matrix
#' m <- matrix(1:4, ncol=2, dimnames=list(r=1:2, c=1:2))
#' m
#' 
#' ### Matrix with names
#' toWord(m)
#' showClip()
#' # clipboard contains:
#' # r\c	1	2
#' # 1	1	3
#' # 2	2	4
#' 
#' ### Matrix without names
#' toWord(m, useNames=FALSE)
#' showClip()
#' # 1	3
#' # 2	4
#' 
#' ### Data frame
#' DF <- as.data.frame(m)
#' DF
#' toWord(DF)
#' showClip()
#' # 	V1	V2
#' # 1	1	3
#' # 2	2	4
#' 
#' ### Table
#' # one-dimensional horizontally
#' toWord( table(1:5) )
#' showClip()
#' # two-dimensional as matrix
#' toWord( table(1:5, 1:5) )
#' showClip()
#' # higer dimensions should return error
#' r <- try( toWord( table(1:5, 1:5, 1:5) ), silent=TRUE )
#' cat(r)
#' stopifnot(inherits(r, "try-error"))
#' 
#' 
#' # 'tcb' is an alias for 'toWord'
#' tcb(m)
#' tcb(DF)
#' }
#' 

toWord <- function(object, ...)
{
  stopifnot( .Platform$OS.type == 'windows' ) # only on Windows at this moment
  UseMethod("toWord")
}



#' @method toWord matrix
#' @export
#' @rdname toWord
toWord.matrix <- function(object, useNames=TRUE, ...)
{
	if( !useNames | is.null(dimnames(object)) ) {
	    m <- object
	}
	else {
	    nams <- dimnames(object)
	    dnames <- names(nams)
	    # make a sequence if dimension names are empty
	    if( is.null(nams[[1]]) ) nams[[1]] <- seq(1, dim(object)[1])
	    if( is.null(nams[[2]]) ) nams[[2]] <- seq(1, dim(object)[2])
	    m <- cbind(nams[[1]], object)
	    m <- rbind( c( paste(dnames, collapse="\\"), nams[[2]]), m)
	}
	rval <- apply( m, 1, paste, collapse="\t")
	rval <- paste(rval, collapse="\n")
	require(utils)
	writeClipboard(rval)
	cat("In clipboard:",
	    paste(dim(m), collapse=" x "),
	    "table\n")
	invisible(rval)
}


#' @method toWord data.frame
#' @export
#' @rdname toWord
toWord.data.frame <- function(object, useNames=TRUE, ...)
{
	# convert factors two character
	isf <- sapply(object, is.factor)
	d <- object
	d[isf] <- lapply( d[isf], as.character )
	# process as character matrix
	toWord( as.matrix(d), useNames=useNames)
}


#' @method toWord table
#' @export
#' @rdname toWord
toWord.table <- function(object, useNames=TRUE, ...)
{
    # only if table is one- or two-dimensional
    ndim <- length(dim(object))
    if( ndim > 2 )
	stop("'object' has ", ndim, " dimensions, only 1 or 2 supported")
    # horizontal table for one dimension
    if( ndim == 1 )
    {
	rval <- toWord( t(as.matrix(object)), useNames=useNames )
    } else
    {
	class(object) <- "matrix"
	rval <- toWord( object, useNames=useNames )
    }
    invisible(rval)
}



#' @method toWord default
#' @export
#' @rdname toWord
toWord.default <- function(object, useNames=TRUE, ...)
{
    toWord( as.matrix(object), useNames=useNames )
}


# @alias cb
cb <- function(object, ...) toWord( object=object, ... )
