#' Hellwig's method for choosing subset of independet variables
#' 
#' Hellwig's method selects a subset of independent variables in a linear
#' regression model based on their correlations with some dependent variable as
#' well as correlations between themselves. The goal is to select a subset of
#' variables which are fairly independent from each other but highly correlated
#' with the dependent variable.
#' 
#' Given \eqn{m} independent variables Hellwig's method consists of evaluating
#' all \eqn{2^m - 1} combinations using the following steps:
#' \enumerate{
#' \item Individual capacity of an independent variable in a subset is given
#' by: \deqn{h_{kj} = r_{0j}^2 / \sum_{i \in I} r_{ij}}{h_kj = r_0j^2 / sum_{i
#' \in I} r_ij} where \eqn{r_{0j}}{r_0j} is correlation of j-th independent
#' variable with the dependent variable, \eqn{r_{ij}}{r_ij} is a correlation
#' with i-th and j-th dependent variable, and I is a focal set of independent
#' variables.
#' 
#' \item Integral capacity of information for every combination \eqn{k} is
#' equal to: \deqn{H_k = \sum_j h_{kj}}{H_k = sum_j h_kj}
#' }
#' The subset with the highest value of \eqn{H_k} should be selected.
#' 
#' @param y numeric, dependent variable
#' @param x numeric matrix, independent variables
#' @param method character, type of correlation measures used, passed to
#' \code{\link{cor}}
#'
#' @return Data frame with two columns: \code{k} combination of independent
#' variables in the form of x-y-z where x, y, z... are the indices of columns
#' in \code{x}, and \code{h} the capacity of the subset \eqn{H_k}.
#'
#' @references TODO Add references
#'
#' @export
#'
#' @example examples/hellwig.R
hellwig <- function( y, x, method="pearson")
{
  requireNamespace("utils")
  x <- as.data.frame(x)
  cm <- stats::cor(x, method=method) # correlation matrix among indeps
  cd <- stats::cor(x, y, method=method) # correlations with dependent
  # list of combination vectors
  k <- sapply( seq(2, length(x)), function(i)
              utils::combn(length(x), i, simplify=FALSE) )
  k <- do.call("c", k)
  # function calculating individual capacities
  hfun <- function(v)
  {
    sapply(v, function(i) cd[i]^2 / sum(abs(cm[v,i])) )
  }
  h <- sapply(k, hfun)
  data.frame( k = sapply( k, paste, collapse="-"),
             h = sapply(h, sum),
             stringsAsFactors=FALSE)
}
