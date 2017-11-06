#' Digitizing images
#'
#' @field image object of class "raster", image to be digitized
#' @field xpts,ypts, numeric vectors of length specifying, some well defined
#' length on, respectively, 'x' and 'y' axes.
#' @field xinterval,yinterval numeric scalars, lengths of the intervals marked
#' with \code{xpts} and \code{ypts} points
#'
#' @seealso \code{\link{Locator}}, \code{\link{as.raster}}
#' 
#' @include Locator.R
#' @export Digitize
#' @exportClass Digitize
Digitize <- setRefClass("Digitize", contain="Locator",
                        fields=list( image="ANY",
                                    xmarks = "list",
                                    ymarks = "list",
                                    xcoords = "numeric",
                                    ycoords = "numeric" ),
                        methods = list(
                                       initialize = function(image) {
                                         'initialize'
                                         stopifnot(inherits(image, "raster"))
                                         image <<- image
                                         xmarks <<- ymarks <<- list(x=NULL, y=NULL)
                                         callSuper()
                                       },

                                       show = function() {
                                         'show'
                                         cat("Object of class", class(.self), "\n")
                                         cat("Image:", paste(dim(image), collapse=" x "), "\n")
                                         cat("X-calibration points: ")
                                         if( length(xmarks$x) > 0 ) {
                                           cat("\n")
                                           print( do.call("cbind", xmarks) )
                                         } else {
                                           cat("none\n")
                                         }
                                         cat("X coordinates:", ifelse(length(xcoords) > 0, paste(xcoords, collapse=", "), "none"), "\n")
                                         cat("Y-calibration points: ")
                                         if( length(ymarks$y) > 0 ) {
                                           cat("\n")
                                           print( do.call("cbind", ymarks) )
                                         } else {
                                           cat("none\n")
                                         }
                                         cat("Y coordinates:", ifelse(length(ycoords) > 0, paste(ycoords, collapse=", "), "none"), "\n")
                                         cat("Data points:\n")
                                         callSuper()
                                       },

                                       plot = function(points=TRUE, xpoints=TRUE, ypoints=TRUE, ...) {
                                         'plot'
                                         # plot the image
                                         plot.raster(image)
                                         # plot data points
                                         if(points && length(pts$x) != 0) {
                                           callSuper(...)
                                         }
                                         if(xpoints && length(xmarks$x) != 0) {
                                           points(xmarks, col="blue", pch=4)
                                           if(length(xcoords) > 0)
                                             text(xmarks, label=xcoords, col="blue", pos=1)
                                         }
                                         if(ypoints && length(ymarks$y) != 0) {
                                           points(ymarks, col="blue", pch=4)
                                           if(length(ycoords)>0)
                                             text(ymarks, label=ycoords, col="blue", pos=2)
                                         }
                                       },

                                       start = function(append=TRUE, ...) {
                                         'start digitizing'
                                         .self$plot()
                                         callSuper(append=append, ...)
                                       },

                                       markx = function( col="blue", pch=4, ... ) {
                                         'mark points on X axis'
                                         .self$plot()
                                         xmarks <<- locator(n=2, type="p", col=col, pch=pch, ...)
                                       },

                                       marky = function( col="blue", pch=4, ... ) {
                                         'mark points on Y axis'
                                         .self$plot()
                                         ymarks <<- locator(n=2, type="p", col=col, pch=pch, ...)
                                       },

                                       transform = function() {
                                         'transform data points'
                                         # TODO check data availability
                                         d <- data.frame(xcoords, ycoords, xmarks=xmarks$x, ymarks=ymarks$y)
                                         xmod <- lm( xcoords ~ xmarks, data=d)
                                         ymod <- lm( ycoords ~ ymarks, data=d)
                                         list( x = predict(xmod, data.frame(xmarks=pts$x)),
                                              y = predict(ymod, data.frame(ymarks=pts$y)) )
                                       }

                                       )
                        )

#============================================================================ 
# Auxiliary functions

# Plot raster object with aspect ratio = 1
plot.raster <- function(r, type="n", asp=1, ann=FALSE, ...)
{
  op <- graphics::par(mar=c(2, 2, 0.5, 0.5), mgp=c(1, 0.5, 0), cex.axis=0.7)
  on.exit(graphics::par(op))
  plot(c(1, ncol(r)), c(1, nrow(r)), type=type, asp=asp, ann=ann, ...)
  graphics::rasterImage(r, 1, 1, ncol(r), nrow(r))
}

# Paste point coordinatess for pretty printing
paste_points <- function(l)
{
  m <- do.call("cbind", l)
  paste(apply(m, 1, function(r) paste0("(", r[1], ", ", r[2], ")")), collapse=", ")
}


if(FALSE)
{
  source("R/Locator.R")
  library(png)
  x <- readPNG("inst/extdata/Rlogo-2.png")
  r <- as.raster(x)
  plot(r)

  d <- Digitize(r)
  # some points
  d$pts <- structure(list(x = c(481.531024517023, 540.430742094776,
                                481.531024517023, 311.097799185654,
                                229.640742961102, 303.578686303387,
                                521.63295988911, 511.607476046089), y =
  c(535.765173351815, 414.42520696526, 164.239709261024, 538.267028328857,
    373.144599844061, 176.748984146236, 266.815763319761, 471.967871437235)),
                     .Names = c("x", "y"))
  d$markx()
  d$marky()
  d$xcoords <- c(1,10)
  d$ycoords <- c(2,5)

  # reordering
  d$reorder(1,8,2,7,3)
  d$reorder(5,8,7,6)
}
