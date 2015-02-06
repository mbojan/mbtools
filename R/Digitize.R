#' Digitizing images
#'
#' @field image
#' @field xpts,ypts
#' @field xinterval,yinterval
#' 
#' @include Locator.R
#' @export Digitize
#' @exportClass Digitize
Digitize <- setRefClass("Digitize", contain="Locator",
                        fields=list( image="ANY",
                                    xpts = "numeric",
                                    ypts = "numeric",
                                    xinterval = "numeric",
                                    yinterval = "numeric" ),
                        methods = list(
                                       initialize = function(image) {
                                         'initialize'
                                         stopifnot(inherits(image, "raster"))
                                         image <<- image
                                         callSuper()
                                       },

                                       show = function() {
                                         'show'
                                         cat("Object of class", class(.self), "\n")
                                         cat("Image:", paste(dim(image), collapse=" x "), "\n")
                                         callSuper()
                                       },

                                       plot = function(points=TRUE, ...) {
                                         'plot'
                                         plot.raster(image)
                                         if(points && length(pts$x) != 0) {
                                           callSuper(...)
                                         }
                                       },

                                       start = function(append=TRUE, ...) {
                                         'start digitizing'
                                         .self$plot()
                                         callSuper(append=append, ...)
                                       }

                                       )
                        )

#============================================================================ 
# Auxiliary functions

# Plot raster object with aspect ratio = 1
plot.raster <- function(r, type="n", asp=1, ann=FALSE, ...)
{
  op <- par(mar=c(2, 2, 0.5, 0.5), mgp=c(1, 0.5, 0), cex.axis=0.7)
  on.exit(par(op))
  plot(c(1, ncol(r)), c(1, nrow(r)), type=type, asp=asp, ann=ann, ...)
  rasterImage(r, 1, 1, ncol(r), nrow(r))
}


if(FALSE)
{
  source("R/Locator.R")
  library(png)
  x <- readPNG("inst/extdata/Rlogo-2.png")
  r <- as.raster(x)
  plot(r)

  d <- Digitize(r)
  d$start()
  # some points
  d$pts <- structure(list(x = c(481.531024517023, 540.430742094776,
                                481.531024517023, 311.097799185654,
                                229.640742961102, 303.578686303387,
                                521.63295988911, 511.607476046089), y =
  c(535.765173351815, 414.42520696526, 164.239709261024, 538.267028328857,
    373.144599844061, 176.748984146236, 266.815763319761, 471.967871437235)),
                     .Names = c("x", "y"))
  d$plot(path=TRUE)
  d$trace(reorder, browser)
  d$start()

  # reordering
  d$reorder(1,8,2,7,3)
  d$reorder(5,8,7,6)
}
