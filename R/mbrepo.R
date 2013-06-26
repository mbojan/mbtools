#' Get current URL to my R packages repository
#' 
#' Returns a valid URL to my package repository.
#' 
#' Because I was migrating quite a bit recently this function, when called with
#' the default value of \code{loc}, should give the current and valid address
#' to my R package repository. If the default value is not working alternative
#' \code{loc}ations can be tried.
#' 
#' @param loc character, name of location
#' @return Character scalar with the URL to the top dir of my R package
#' repository. Can be used with \code{\link{download.packages}},
#' \code{\link{install.packages}} and alike.
#' @examples
#' 
#' \dontrun{
#' available.packages(contrib.url(mbrepo()))
#' }
#' 
#' @export mbrepo
mbrepo <- function(loc=c("3e", "icm"))
{
    loc <- switch( match.arg(loc),
            "3e"="http://bojan.3e.pl/R",
            icm="http://www.icm.edu.pl/~mbojan/R")
    loc
}
            
