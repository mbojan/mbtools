# Plot raster object with aspect ratio = 1
plot.raster <- function(r, axes=TRUE, ...)
{
  op <- par(mar=c(1,1,1,1))
  on.exit(par(op))
  plot(c(1, ncol(r)), c(1, nrow(r)), type="n", asp=1, ann=FALSE, axes=axes)
  rasterImage(r, 1, 1, ncol(r), nrow(r))
}

# Join two two-component lists by concatenating 
join_pts <- function(l1, l2)
{
  stopifnot(length(l1) == length(l2))
  structure( lapply( seq_along(l1), function(i) c(l1[[i]], l2[[i]]) ),
            names = names(l1) )
}

isok <- function() {
  input <- readline("OK to add (Y/n):")
  if( input %in% c("Y", "") ) {
    rval <- TRUE
  } else {
    rval <- FALSE
  }
  return(rval)
}


#============================================================================ 
# Reference-class-based locator()

Locator <- setRefClass("Locator",
                       fields = list( pts = "list" ),
                       methods = list(
                                      initialize = function() {
                                        pts <<- list(x=NULL, y=NULL)
                                      },

                                      show = function() {
                                        print(pts)
                                      },

                                      start = function(..., append=TRUE, col="red", type="p") {
                                        'Starts adding points'
                                        newp <- locator(..., col=col, type="p")
                                        ok <- isok()
                                        if(ok) {
                                          if(append) { 
                                            pts <<- join_pts(pts, newp)
                                          } else { 
                                            pts <<- newp 
                                          }
                                        } else {
                                          cat("Stopped\n")
                                        }
                                      },

                                      plot = function(col="red", labels=TRUE, pos=1, ...) {
                                        'plot points'
                                        points(pts, col=col, ...)
                                        if(labels)
                                          text(pts, labels=seq_along(pts$x), pos=pos)
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
                                      },

                                      reorder = function(ind) {
                                        pts <<- lapply(pts, function(x)
                                               {
                                                 x[sort(ind)] <- x[ind]
                                                 x
                                               } )
                                      }

                                      )
                       )

#============================================================================ 
# Image digitizer

Digitize <- setRefClass("Digitize", contain="Locator",
                        fields=list( image="ANY",
                                    xpts = "numeric",
                                    ypts = "numeric",
                                    xinterval = "numeric",
                                    yinterval = "numeric" ),
                        methods = list(
                                       initialize = function(image) {
                                         stopifnot(inherits(image, "raster"))
                                         image <<- image
                                         callSuper()
                                       },

                                       show = function() {
                                         cat("Object of class", class(.self), "\n")
                                         cat("Image:", paste(dim(image), collapse=" x "), "\n")
                                         callSuper()
                                       },

                                       plot = function(points=TRUE, ...) {
                                         plot.raster(image)
                                         if(points && length(pts$x) != 0) {
                                           callSuper(...)
                                         }
                                       },

                                       start = function(append=TRUE, ...) {
                                         .self$plot()
                                         callSuper(append=append, ...)
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
  x <- readPNG("inst/extdata/Rlogo-2.png")
  r <- as.raster(x)
  plot(r)

  d <- Digitize(r)
  d$plot()
  d$start()
}
