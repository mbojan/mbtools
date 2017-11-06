#' Interactive Curve Fitting
#' 
#' Interactively enter a set of points and then fit a curve of specified form
#' to get a coefficients.
#' 
#' Frequently there is a need to quickly come up with a particular form of a
#' function. This function enables to quickly fit a curve of certain kind to a,
#' usually small, number of data points.
#' 
#' The function starts with creating an empty plotting region ready for input
#' of \code{n} data points. Every data input (through \code{locator}) results
#' in adding a point to the plot with a subsequent number. After the data are
#' inputed the function uses model-fitting function specified with \code{model}
#' to fit the curve specified with \code{formula} with possibly additional
#' arguments passed through \code{...}.
#' 
#' The \code{formula} argument has a usual form as in almost all model-fitting
#' functions in . It can contain only variables \code{y} and \code{x}.
#' Furthermore, direct transformations of \code{y} are not supported. All
#' transformations of \code{x} should be enclosed in \code{I()} function.
#' 
#' The function was designed to be used with \code{lm} or \code{glm} as
#' model-fitting functions. However, in principle, it should be possible to use
#' any other function that provides similar interface, i.e. \code{formula} and
#' \code{data} arguments, and the corresponding \code{predict} method.
#' 
#' Once the model is fitted the function finds its predicted values for a
#' sequence of values defined by \code{xlim} of length \code{prn}. Then the
#' curve itself is added to the plot. Finally the model object is returned.
#' 
#' @param formula formula for the model to be fitted to the entered data
#' @param n numeric, number of data points to be entered
#' @param model character, name of the model-fitting function to be used for
#' the entered data
#' @param prn numeric, number of points used to evaluate the fitted model,
#' important only for plotting the fitted curve
#' @param xlim numeric of length 2, limits for the horizontal axis
#' @param ylim numeric of length 2, limits for the vertical axis
#' @param ... other arguments passed to the model-fitting function
#'
#' @return A model object created for the supplied data with the model-fitting
#' function specified with \code{model}.
#'
#' @seealso See \code{\link{lowess}} and \code{\link{spline}} for other curve
#' fitting functions, \code{\link{lm}} and \code{\link{glm}} for help with th
#' model-fitting functions supported. Also see \code{\link{predict}} and
#' \code{\link{I}}
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # fitting via 'lm'
#' iCurveFit(formula= y ~ x ) # linear
#' iCurveFit(formula= y ~ I(x^2) + x ) # parabolic
#' iCurveFit(formula= y ~ I(x^3) + I(x^2) + x, n=7) # polynomial of degree 3
#' iCurveFit(formula= y ~ I(sin(x)), n=7) # sinusoidal
#' iCurveFit(formula= y ~ I(log(x)), n=7) # log
#' iCurveFit(formula= y ~ I(1/x), n=7) # hyperbolic
#' }
#' 
iCurveFit <- function(formula=y ~ x, n=5, model="lm",
    prn=100, xlim=c(0,10), ylim=c(0,10), ...)
{
  # create the empty plot to click on
  plot( 1, xlim=xlim, ylim=ylim, type="n", xlab="x", ylab="y",
  main=paste("Waiting for", n, "data points") )
  # empty list to store the data in
  d <- structure(vector(2, mode="list"), names=c("x", "y"))
  # for every point plot it and add to data list 'd'
  for( i in 1:n )
  {
    p <- graphics::locator(1)
    graphics::points(p)
    graphics::text(p, labels=i, pos=1)
    d$x <- c(d$x, p$x)
    d$y <- c(d$y, p$y)
  }
  # fit the model to the data
  m <- do.call(model, list(formula=formula, data=d, ...))
  # construct the 'x' values for plotting the fitted curve
  ox <- seq(min(xlim), max(xlim), length=prn)
  # get the fitted 'y' data
  pr <- stats::predict(m, data.frame(x=ox), type="response" )
  # plot the fitted curve
  graphics::lines(ox, pr)
  # return the model object
  m
}
