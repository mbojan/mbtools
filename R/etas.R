#' Calculate Eta coefficients
#' 
#' This generic function calculates Eta coefficients which are also known as
#' "Correlation ratios" or (the squared value) as the "Proportion of
#' explained variance".
#' 
#' The Eta coefficient (more specifically its squared value) has a
#' interpretation in terms of the proportion of explained variance.
#' 
#' In the decision theory the interpretation is related to the identification
#' problem that involves two variables: \eqn{y} and \eqn{x}. The task is two
#' identify the values of \code{y}.
#' 
#' The value of the \eqn{\eta^2}{Eta^2} is the proportion by which the error of
#' predicting values of \eqn{y} is reduced by using the information contained
#' in \eqn{x}.
#'
#' @return Values of eta and partial eta coefficients.
#' 
#' @param object the R object
#' @param fac vector for conditioning variable
#' @param ... arguments passed to other methods
#'
#' @export
#'
#' @example examples/etas.R
etas <- function( object, ... ) UseMethod("etas")





#' @details For numeric vectors the function requires additional argument: a
#' vector of the same length as the first.  The result is a value of the
#' \eqn{\eta^2}{Eta^2} assuming that we want to predict the values of
#' \code{object} with the values of \code{fac} using the so called ``Type I
#' regression of means''.
#' 
#' For two variables \eqn{y} and \eqn{x} the \eqn{\eta}{Eta} is given by the
#' formula:
#'
#' \deqn{\eta^2 = ( D^2(y) - E[D^2(y|x)] ) / D^2(y)}{Eta^2 = ( D^2(y) - E[D^2(y|x)] ) / D^2(y)}
#'
#' @method etas default
#' @export
#' @rdname etas
etas.default <- function( object, fac, ... ) {
	n <- length(object)
	if( length(fac) != n )
		stop("arguments must be of the same length")
	( stats::var(object) - mean(tapply(object, fac, FUN=stats::var)) ) / stats::var(object)
}



#' @details For objects of class \code{anova} the function calculates the Eta's
#' and Partial Eta Squares for all effects in the given model. In this setting
#' the eta squares for the given effect are equal to:
#'
#' \deqn{\frac{SS_{effect}}{SS_{total}}}{SSeffect / SStotal}
#'
#' where \eqn{SS} are apropriate Sums of Squares.  The ``Partial Eta Squares''
#' for the given effect are equal to:
#'
#' \deqn{\frac{SS_{effect}}{SS_{effect}+SS_{resid}}}{SSeffect / (SSeffect+SSresid)}
#'
#' @method etas anova
#' @export
#' @rdname etas
etas.anova <- function( object, ... ) {
	nam <- row.names(object["Sum Sq"])
	ss <- object[["Sum Sq"]]
	sstotal <- sum(ss)
	ssresid <- ss[nam=="Residuals"]
	ss <- ss[-length(ss)]
	rval <- data.frame( ss/sstotal, ss/(ss+ssresid) )
	structure( rval,	names=c("Eta", "Partial Eta"),
				row.names=nam[-length(nam)] )
}




#' @details For objects of class \code{lm} the function is applied on the
#' result of calling \code{anova}.
#'
#' @method etas lm
#' @export
#' @rdname etas
etas.lm <- function( object, ... ) etas( stats::anova(object) )
