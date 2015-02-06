#' Reference-class implementation of locator()
#'
#' @field pts a list of components 'x' and 'y' containg the coordinates of the points
#'
#' @exportClass Locator
#' @export Locator
Locator <- setRefClass("Locator",
                       fields = list( pts = "list" ),
                       methods = list(
                                      initialize = function() {
                                        'initialize object'
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
                                        'reorder points'
                                        input <- as.numeric(unlist(list(...))) 
                                        o <- seq(1, length(pts$x))
                                        used <- o %in% input
                                        z <- rep(NA, length(o))
                                        z[ seq(which(o==input[1]), which(o==input[1])+length(input)-1) ] <- input
                                        z[is.na(z)] <- o[!used]
                                        pts <<- lapply(pts, function(x) x[z])
                                      }

                                      )
                       )

#============================================================================ 
# Auxiliary functions

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

