#' Component variation in PCA analysis
#' 
#' Calculate percent of explained variance for Principal Component Analysis
#' objects.
#' 
#' The function calculates percentages of variance explained for each component
#' resulting from Principal Component Analysis. The variance is equal to:
#' \deqn{100\frac{\sigma_i^2}{\sum_i \sigma_i^2}}{sigma^2 / sum( sigma^2 ) *
#' 100} where \eqn{\sigma}{sigma} is equal to component's standard deviation.
#' 
#' @param object object of class \code{princomp}
#' @param ... other arguments currently not supported
#'
#' @return A vector of length equal to a number of components existing in
#' \code{object}. Each element containes the percent of variance explained by
#' the corresponding component.
#'
#' @seealso \code{\link[stats]{princomp}}, \code{\link[stats]{anova}}
#'
#' @export
#' @method anova princomp
#'
#' @example examples/anova.princomp.R
anova.princomp <- function(object, ...) {
	object$sdev^2 / sum(object$sdev^2) * 100
}
