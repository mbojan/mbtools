# Plot raster object with correct aspect ratio
plot.raster <- function(r, ...)
{
  op <- par(mar=c(0,0,0,0))
  on.exit(par(op))
  plot(c(1, ncol(r)), c(1, nrow(r)), type="n", asp=1, ann=FALSE, axes=FALSE)
  rasterImage(r, 1, 1, ncol(r), nrow(r))
}

join_pts <- function(l1, l2)
{
  stopifnot(length(l1) == length(l2))
  structure( lapply( seq_along(l1), function(i) c(l1[[i]], l2[[i]]) ),
            names = names(l1) )
}

#============================================================================ 
# Locator

Locator <- setRefClass("Locator",
                       fields = list( pts = "list" ),
                       methods = list(
                                      initialize = function() {
                                        pts <<- list(x=NULL, y=NULL)
                                      },

                                      start = function(..., append=TRUE, col="red", type="p") {
                                        'Starts adding points'
                                        newp <- locator(..., col=col, type="p")
                                        if(append) { 
                                          pts <<- join_pts(pts, newp)
                                        } else { 
                                          pts <<- newp 
                                      } 
                                      },

                                      plot = function(col="red", ...) {
                                        'plot points'
                                        points(pts, col=col, ...)
                                      },

                                      identify = function() {
                                        'identify points'
                                        .self$plot()
                                        i <- identify(pts, atpen=TRUE)
                                        i
                                      },

                                      remove = function(ind) {
                                        'removing points'
                                        pts <<- lapply(pts, function(x) x[-ind])
                                      }

                                      )
                       )


# Image digitizer
Digitize <- setRefClass("Digitize", contain="Locator",
                        fields=list( image="ANY",
                                    xpts = "numeric",
                                    ypts = "numeric",
                                    xinterval = "numeric",
                                    yinterval = "numeric" ),
                        methods = list(
                                       initialize = function(image) {
                                         image <<- image
                                       },

                                       plot = function() {
                                         plot.raster(image)
                                       }

                                       )
                        )

#============================================================================ 

if(FALSE)
{
  plot(1:2)
  l <- Locator()
  l$start()
  l$plot()
  l$remove(1)


  library(png)
  x <- readPNG("~/Desktop/Rlogo-2.png")
  r <- as.raster(x)
  plot(r)

  d <- Digitize(r)
  d$plot()
  d$start()
}
