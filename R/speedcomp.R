#' Comparing CPU times of several R expressions
#' 
#' This function enables comparing CPU times of several expressions. The times
#' are calculated via \code{\link{system.time}}, additionally time ratios are
#' calculated.
#' 
#' The function uses \code{\link{system.time}} to estimate CPU times needed to
#' evaluate each of the expressions. The output provides raw timings as well as
#' ratios of time elapsed.
#' 
#' The included \code{\link{show}} method prints the results nicely and
#' additionally calculates the ratios of times elapsed.
#' @param \dots a collection of expressions in the form of tag=value
#'
#' @return An object of S4 class \code{speedcomp} with two slots
#' \describe{
#' \item{\code{e}}{list of expressions processed}
#' \item{\code{timings}}{matrix containing the CPU times of the expressions in
#' \code{e} estimated with \code{\link{system.time}}. Rows correspond to
#' expressions and columns to the fields returned by \code{\link{system.time}},
#' i.e. "user cpu", "system cpu", "elapsed", "subproc1" and "subproc2"}
#' }
#' 
#' @seealso \code{\link{system.time}}
#' @keywords programming
#' @examples
#' 
#' # some testing of 'lm' fitting with different data sizes
#' e1 <- expression( lm( I(rnorm(10000)) ~ I(runif(10000)) ) )
#' e2 <- expression( lm( I(runif(20000)) ~ I(rnorm(20000)) ) )
#' 
#' # compare
#' speedcomp(e1, e2)
#' 
#' 
#' @export speedcomp

speedcomp <- function(...)
{
    arg <- list(...)
    if( !all(sapply(arg, is.expression)) )
	stop("all arguments must be R expressions")
    # raw timings
    timings <- sapply(arg, function(x) system.time(eval(x)))
    dnames <- list( c("user cpu", "system cpu", "elapsed", "subproc1",
	    "subproc2"),
	expression=seq(1, length(arg)) )
    dimnames(timings) <- dnames
    new("speedcomp", e=arg, timings=t(timings))
}

#' @method show speedcomp
#' @rdname speedcomp
# showing the results
setClass("speedcomp", representation( e="list", timings="matrix"))

setMethod("show", "speedcomp", function(object)
{
    nl <- 80 * 5 # printed lines chars for each expression
    cat("\n")
    cat("Evaluated expressions\n")
    lapply( seq(1, length(object@e)),
	function(x) cat(x, ": ", substr(object@e[[x]], 1, nl), "\n", sep="") )
    cat("\n")
    cat("Timings\n")
    print(object@timings)
    cat("\n")

    # calculate ratios of elapsed times
    el <- object@timings[,"elapsed"]
    ratios.el <- outer( el, el,
	function(x, y) round(x/y, 1) )
    cat("Ratios of elapsed times [i / j]\n")
    print(ratios.el, quote=FALSE)
    cat("\n")
    invisible(object)
} )

