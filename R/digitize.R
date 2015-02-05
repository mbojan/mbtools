# Plot raster object with aspect ratio = 1
plot.raster <- function(r, type="n", asp=1, ann=FALSE, ...)
{
  op <- par(mar=c(2, 2, 0.5, 0.5), mgp=c(1, 0.5, 0), cex.axis=0.7)
  on.exit(par(op))
  plot(c(1, ncol(r)), c(1, nrow(r)), type=type, asp=asp, ann=ann, ...)
  rasterImage(r, 1, 1, ncol(r), nrow(r))
}

# Join two two-component lists by concatenating 
join_pts <- function(l1, l2)
{
  stopifnot(length(l1) == length(l2))
  structure( lapply( seq_along(l1), function(i) c(l1[[i]], l2[[i]]) ),
            names = names(l1) )
}

# Ask if OK
isok <- function() {
  input <- readline("OK to add (Y/n):")
  if( input %in% c("Y", "") ) {
    rval <- TRUE
  } else {
    rval <- FALSE
  }
  return(rval)
}

# Draw a path with arrows
path <- function(pts, ...) {
  n <- length(pts$x)
  arrows(pts$x[-n], pts$y[-n], pts$x[-1], pts$y[-1], ...)
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

                                      start = function(..., append=TRUE, ask=FALSE, col="red", type="p") {
                                        'Starts adding points'
                                        newp <- locator(..., col=col, type="p")
                                        if(ask) {
                                          ok <- isok()
                                        } else {
                                          ok <- TRUE
                                        }
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

                                      plot = function(path=FALSE, col="red", labels=TRUE, pos=1, ...) {
                                        'plot points'
                                        if(path) {
                                          path(pts, col=col, length=.1, ...)
                                        }
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

                                      reorder = function(...) {
                                        input <- as.numeric(unlist(list(...))) 
                                        used <- o %in% input
                                        z <- rep(NA, length(o))
                                        z[ seq(which(o==input[1]), which(o==input[1])+length(input)-1) ] <- input
                                        z[is.na(z)] <- o[!used]
                                        pts <<- lapply(pts, function(x) x[z])
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
  d$pts <- structure(list(x = c(481.531024517023, 540.430742094776,
                                481.531024517023, 311.097799185654,
                                229.640742961102, 303.578686303387,
                                521.63295988911, 511.607476046089), y =
  c(535.765173351815, 414.42520696526, 164.239709261024, 538.267028328857,
    373.144599844061, 176.748984146236, 266.815763319761, 471.967871437235)),
                     .Names = c("x", "y"))
  d$plot(path=TRUE)
  d$trace(reorder, browser)
  d$reorder(1,8,2,7,3)
  d$reorder(5,8,7,6)

  # reordering
  o <- c(1,2,3,4,5,6,7,8)
  input <- c(3,6,5,4)
  tgt <- c(1,8,2,7,3,4,5,6)
  used <- o %in% input
  z <- rep(NA, length(o))
  z[ seq(which(o==input[1]), which(o==input[1])+length(input)-1) ] <- input
  z[is.na(z)] <- o[!used]
  z



  

}
