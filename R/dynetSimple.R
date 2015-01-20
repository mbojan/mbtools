#' Simple visualization of dynamic network
#' 
#' Simple visualization of a dynamics of a network that has constant set of
#' vertices. \emph{This function only works on Windows}.
#' 
#' This function is limited for networks with the same set of vertices. The
#' plotting mechanism uses the recording functionality of the \code{windows}
#' device.  This is also a limitation as all devices share the
#' \code{.SavedPlots} object where the plots are stored.
#' 
#' The main argument \code{g} is assumed to be a three-dimensional array
#' containing a set of square adjacency matrices representing the networks. The
#' first dimension enumerates the networks.
#' 
#' The \code{coords} argument may specify the coordinates of the vertices by a
#' matrix with two columns (for x and y) and number of rows equal to the number
#' of vertices of the network. Alternatively, the placement algorithm specified
#' with \code{layout} is used to calculate them. If \code{coords} is
#' \code{NULL} (default) the placement is calculated based on cumulated
#' network, i.e. network obtained by summing up all graphs in the stack
#' (\code{apply(g, c(2,3), sum)}). If it is a numeric scalar it is assumed to
#' be an index of the graph in \code{g} to be used for getting the coordinates.
#' 
#' The \code{layout} is a name of the vertex placement algorithim found among
#' those supplied in \pkg{sna} package. See \code{\link[sna]{gplot.layout}} for
#' a list. Optional arguments to \code{layout} may be passed as a list through
#' \code{layout.par}.
#' 
#' The function uses \code{\link[sna]{gplot}} function from package \pkg{sna}
#' to do the plotting. Other arguments may be passed through \code{...}.
#' 
#' @param g three-dimensional numeric array of network matrices, first
#' dimension corresponds to network id
#' @param coords \code{NULL}, numeric network position in the stack \code{g} or
#' a numeric matrix with two columns and rows containig coordinates. See
#' Details
#' @param layout character, name of the vertex placement algorithm function
#' @param layout.par list, optional list of arguments to placement function
#' @param \dots other arguments passed to \code{\link[sna]{gplot}}
#'
#' @return The vertex coordinates are returned invisibly. As a side effect a
#' \code{windows} device is opened with the \code{record} option as
#' \code{TRUE}. The plots are created and can be navigated with the device
#' keys. Also, an object \code{.SavedPlots} storing the history is created in
#' the global environment.
#'
#' @note As all \code{windows} devices share \code{.SavedPlots} object to store
#' the history. This should be manually deleted after the device is closed.
#' This directive could not be a part of the function as the object cannot be
#' deleted if the device is still on.
#'
#' @seealso \code{windows} and \code{\link{recordPlot}} for more information on
#' the device and the recording functionality. \code{\link[sna]{gplot}} and
#' \code{\link[sna]{gplot.layout}} for functions in \pkg{sna} package which
#' this function uses.
#'
#' @keywords hplot graphs
#'
#' @export
#'
#' @examples
#' 
#' # only on Windows and 'sna' package needed
#' \dontrun{
#' # generate a stack of random graphs
#' g <- rgraph(10, 9, tprob=.3)
#' # plot using cumulated placement
#' dynetSimple(g)
#' }
#' 
dynetSimple <- function(g, coords=NULL,
    layout="fruchtermanreingold", layout.par=list(), ... )
{
    require(sna)
    # get the layout function
    layout.fun <- try( match.fun(paste("gplot.layout.", layout, sep = "")),
	silent=TRUE)
    if (class(layout.fun) == "try-error") 
	stop("Error in gplot: no layout function for mode ", layout)
    # by default use cumulated network
    if(is.null(coords))
	k <- layout.fun( apply(g, c(2,3), sum), layout.par)
    else {
	# use the specified graph in the stack
	k <- layout.fun( g[coords,,], layout.par)
    }
    windows(record=TRUE)
    for(i in seq(1, dim(g)[1]))
    {
	gplot(g[i,,], coord=k, jitter=FALSE,
	    main=paste("Graph no.", i), ...)
    }
    message("Plots are stored in '.SavedPlots'")
    invisible(k)
}

