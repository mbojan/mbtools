#' Fit two-level null random intercept model
#' 
#' Fit a null two-level random intercept model using closed-form formulas
#' provided by Snijders nd Bosker (1999)
#' 
#' @param y numeric, dependent variable
#' @param g numeric, grouping factor
#' @param data data frame, optional dataset where the variables should be
#' looked for
#' @param x object of class "nullModel"
#' @param ... other arguments passed to/from other methods
#' 
#' @details The function fits a random effect model of the form:
#'
#' \deqn{Y_{ij} = \mu + U_j + R_{ij}}{Y_ij = mu + U_j + R_ij}
#'
#' where \eqn{Y_{ij}}{Y_ij} is the value of the dependent variable (`y`)
#' for i-th object within j-th group, \eqn{\mu}{mu} is the population mean,
#' \eqn{U_j} is the effect specific to group j, and \eqn{R_{ij}}{R_ij} is the
#' within-grup error.
#' 
#' The function estimates between-group and pooled within-group variances as
#' well as the intra-class correlation coefficient.  The closed-form
#' expressions are provided by Snijders and Bosker (1999, p. 18--21).
#'
#' @return The function returns a list of S3 class "nullModel" which
#' contains the following components:
#' \describe{
#' \item{yname}{character, name of the dependent variable}
#' \item{gname}{character, name of the grouping factor}
#' \item{n}{numeric, number of groups}
#' \item{m}{numeric, total number of observations}
#' \item{wgvar}{numeric, pooled within-group variance}
#' \item{bgvar}{numeric, between-group variance}
#' \item{tau}{numeric, estimate of the population between-group variance}
#' \item{intcor}{numeric, intra-class correlation coefficient}
#' }
#'
#' @seealso `lmer` in package \pkg{lme4} for general fitting of mixed-effects
#'   models.
#'
#' @references Snijders, T.A.B., Bosker, R. (1999) "Multilevel Analysis. An
#' Introduction to Basic and Advanced Multilevel Modelling". London: Sage
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # fit null model with lmer
#' library(lme4)
#' system.time(mm <- lmer( Reaction ~ 1 | Subject, sleepstudy ))
#' # fit with closed-form formulas
#' system.time(m <- nullModel( sleepstudy$Reaction, sleepstudy$Subject ))
#' mm
#' m
#' }
#' 
nullModel <- function( y, g, data=NULL)
# y = dependent var
# g = grouping factor
{
    gname <- deparse(substitute(g))
    yname <- deparse(substitute(y))
    if(!is.null(data))
    {
	y <- data[[yname]]
	g <- data[[gname]]
    }
    if( length(y) != length(g) )
	stop("'y' and 'g' must be of same length")
    # names of the variables
    # trivia
    m <- length(y) # number of cases
    n <- length(unique(g)) # number of groups
    gsizes <- tapply(y, g, length) # group sizes
    gmeans <- tapply(y, g, mean) # group means
    gvars <- tapply(y, g, stats::var) # within-group variances
    # pooled within-group variance
    WGvar <- 1 / (m-n) * sum( (gsizes-1) * gvars )
    # average weighted group size as in (3.7), p. 19 of S&B (1999)
    ntilde <- m/n - stats::var(gsizes) / m
    # between-group variance
    BGvar <- 1 / (ntilde * (n-1)) * sum( gsizes * (gmeans - mean(y))^2 )
    # population BG variance
    tau <- BGvar - WGvar/ntilde
    # intraclass correlation
    intcor <- tau / (tau + WGvar)
    structure(list(yname=yname, gname=gname, n=n, m=m, wgvar=WGvar,
                   bgvar=BGvar, tau=tau, intcor=intcor), class="nullModel")
}


#' @method print nullModel
#' @export
#' @rdname nullModel
print.nullModel <- function(x, ...)
{
  cat("Null two-level random intercept model\n")
  cat( x@yname, "  ~  1 | ", x@gname, "\n", sep="")
  cat("\n")
  cat("Number of cases: ", x@m, ", number of groups: ", x@n, "\n", sep="")
  cat("\n")
  cat("Random effects:\n")
  mat <- matrix( c(x@gname, "(Intercept)", x@tau, sqrt(x@tau),
                   "Residual", "", x@wgvar, sqrt(x@wgvar)),
                ncol=4, nrow=2,
                byrow=TRUE,
                dimnames = list( c("",""), c("Groups", "Name", "Variance", "Std.Dev.")) )
  print(mat, quote=FALSE, digits=max(3, getOption("digits") - 3) )
  cat("\n")
  cat("Intra-class correlation:", x@intcor, "\n")
}
