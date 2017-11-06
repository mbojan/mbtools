#' Andrew's plots
#' 
#' Andrew's plot is an exploratory technique for identifying clusters of
#' similar observations.
#' 
#' Andrew's plot shows each observation in a multivariate data set as a curve
#' over \eqn{[-\pi; \pi]}{[-pi; pi]} interval. Formally, each observation
#' \eqn{x = (x_1, x_2, ..., x_p)} is transformed according to the following
#' formula (from Everitt (1993)): \deqn{f(t) = \frac{1}{\sqrt{2}} x_1 + x_2
#' \sin(t) + x_3 }{1/sqrt(2) x_1 + x_2 sin(t) + x_3 cos(t) x_4 sin(2t) + x_5
#' cos(2t) ...}\deqn{\cos(t) + x_4 \sin(2t) + x_5 \cos{2t} ...}{1/sqrt(2) x_1 +
#' x_2 sin(t) + x_3 cos(t) x_4 sin(2t) + x_5 cos(2t) ...} and plotted against
#' the above mentioned interval. The transformation preserves Euclidean
#' distances so if two curves are identical so are the observations.
#' 
#' By default the functins are evaluated on an equally-spaced interval from
#' \eqn{-\pi}{-pi} to \eqn{\pi}{pi} of the length provided by \code{res}.
#' Custom intervals can be constructed via \code{w} argument.
#' 
#' Other arguments are passed to \code{\link{matplot}}.
#' 
#' @param x numeric matrix, vector or data frame that contains only numeric
#' variables
#' @param draw logical, whether the plot should be produced
#' @param res numeric, number of points on which transformed variables are
#' evaluated, see Details
#' @param w numeric vector, sequence of points on which transformed variables
#' are evaluated
#' @param main,xlab,ylab,pch,lty,col arguments passed to \code{\link{matplot}},
#' see \code{\link{par}}
#' @param \dots other arguments passed to other methods, \code{\link{matplot}}
#' in the end
#'
#' @return Depending on the value of the \code{draw} argument the plot is
#' produced (default) or not. In both cases the function returns a matrix of
#' transformed observations invisibly.
#'
#' @export
#'
#' @seealso Package \pkg{cluster}
#'
#' @references Everitt, B. S. (1993) "Cluster Analysis", New York: John Wiley
#' and Sons
#'
#' @example examples/andrews.R

andrews <- function(x, ...) UseMethod("andrews")






#' @method andrews matrix
#' @export
#' @rdname andrews
andrews.matrix <- function(x, draw=TRUE, res=20, w=seq(-pi, pi, length=res),
                           main="Andrew's plot", xlab="t", ylab="f(t)", pch=1,
                           lty=1, col=1, ...)
{
   stopifnot(is.numeric(x))
   # weights within the functions
   ww <- rep( seq(1, ceiling( (ncol(x)-1)/2 )),
       each=2, length.out=ncol(x)-1 )
   # functions
   wf <- rep( c("sin", "cos"), length.out=ncol(x)-1)
   m <- outer(w, ww)
   mm <- cbind( 1/sqrt(2), sapply( seq(along=wf),
       function(i) do.call(wf[i], list(x=m[,i]) ) )  )
   rval <- mm %*% t(x)
   if(draw)
	graphics::matplot( w, rval, type="l", main=main, xlab=xlab, ylab=ylab, pch=pch,
	    lty=lty, col=col, ... )
   invisible(rval)
}





#' @method andrews data.frame
#' @export
#' @rdname andrews
andrews.data.frame <- function(x, ...)
{
   andrews( data.matrix(x), ... )
}



#' @method andrews data.frame
#' @export
#' @rdname andrews
andrews.vector <- function(x, ...)
{
   stopifnot( is.numeric(x) )
   andrews( t(as.matrix(x)) )
}


if(FALSE)
{
### Using artificial data
d <- data.frame( x = c( rnorm(50, 1), rnorm(50, 4), rnorm(50, 7)),
    y = c( rnorm(50,1), rnorm(50, 7), rnorm(50, 1)),
    k = rep(1:3, each=50) )
# plotting
# show data
plot(d$x, d$y, pch=d$k)
# Andrew's plots
layout( matrix(1:4, ncol=2))
andrews( d[1:2], main="Unsupervised x,y")
andrews( d[2:1], main="Unsupervised y,x")
andrews( d[1:2], col=d$k, main="Color-coded x,y")
andrews( d[2:1], col=d$k, main="Color-coded y,x")
# three curves for cluster means
andrews( cbind( tapply(d$x, d$k, mean), tapply(d$y, d$k, mean)),
    col=1:3 )

### Using 'iris' data
d <- iris[1:4]
# Andrew's plots
layout( matrix(1:4, ncol=2) )
# "unsupervised"
andrews(d, lty=1, col=1, main="Andrew's plot of Iris data")
# colored with species
andrews(d, lty=1, col=match( iris$Species, unique(iris$Species)),
    main="Andrew's plot of Iris data\n color-coded species")
# Andrew's plot on standardized data
andrews( scale(d), main="Andrew's plot of standardized Iris data")
# Andrew's plot on principal components
pcad <- princomp(d)
andrews( pcad$scores, main="Andrew's plot of PCA of Iris data")
}




