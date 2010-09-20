
setClass("speedcomp", representation( e="list", timings="matrix"))

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

# showing the results
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

