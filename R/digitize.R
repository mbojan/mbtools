
join_pts <- function(l1, l2)
{
  stopifnot(length(l1) == length(l2))
  structure( lapply( seq_along(l1), function(i) c(l1[[i]], l2[[i]]) ),
            names = names(l1) )
}

#============================================================================ 
# Locator

Locator <- setRefClass("Locator",
                       fields = list( pts = "list", adds = "list" ),
                       methods = list(
                                      start = function(..., append=TRUE, col="red", type="p") {
                                        'Starts adding points'
                                        newp <- locator(..., col=col, type="p")
                                        if(append) { 
                                          pts <<- join_pts(pts, newp)
                                          adds <<- c(adds, list(newp))
                                        } else { 
                                          pts <<- newp 
                                      } 
                                      },

                                      undo = function() {
                                        'undo last edit() operation'
                                      }
                                      )
                       )





#============================================================================ 
# Digitize

Digitize <- setRefClass("Digitize",
                        fields=list( image = "numeric",
                                    pts = "list",
                                    xpts = "numeric",
                                    ypts = "numeric",
                                    xint = "numeric",
                                    yint = "numeric" ),
                        methods = list(
                                       initialize = function()
                                       )
                        )


Locator$methods(
                initialize = function()
                {
                  pts <<- list(x=NULL, y=NULL)
                },

                start = function(..., append=TRUE, col="red", type="p")
                {
                  newp <- locator(..., col=col, type="p")
                  if(append)
                  {
                    pts <<- join_pts(pts, newp)
                  } else {
                    pts <<- newp
                  }
                },

                plot = function(..., new=FALSE)
                {
                  points(pts)
                }

                )



Digitize <- setRefClass("Digitize", contains="Locator", fields=list(image="numeric"))
o <- Digitize$new( image=readPNG("Rlogo-2.png"))

#============================================================================ 

if(FALSE)
{
  plot(1:2)
  l <- Locator()
  l$start()
  l$plot()
}
