#' List object names along with classes and sizes
#' 
#' List, and optionally plot, all the objects in the specified environment
#' along with their class attribute and size in bytes.
#' 
#' The function scans the environment supplied by \code{env} with the \code{ls}
#' function for objects. Then the classes of these objects are retrieved with
#' \code{object.size}.
#' 
#' If \code{graph} is \code{TRUE} then additionally a dotchart with showing
#' object sizes is produced.
#' 
#' @param graph logical, should a dotchart of sizes by produced
#' @param env environment to scan for objects, defaults to Global Environment
#' @param ... other arguments passed to \code{ls}
#' @return A data frame with two columns ``class'' and ``size'' containing the
#' class and size (in bytes) of the objects found in the environment
#' \code{env}.
#' @seealso \code{ls} for listing the objects, \code{object.size} for
#' calculating the memory size of an object, \code{dotchart} for making
#' dotcharts
#' @keywords environment
#' @examples
#' 
#' # make some environment
#' e <- new.env()
#' # create some objects in it
#' assign("x", rnorm(10), envir=e)
#' assign("d", data.frame(a=1:10, b=10:1), envir=e)
#' assign("l", letters, envir=e)
#' 
#' # table of contents
#' lss(env=e)
#' 
#' # with a chart
#' lss(TRUE, env=e)
#' 
#' # clean-up
#' rm(e)
#' 
#' @export lss
lss <-
function (graph = FALSE, env = .GlobalEnv, ...) 
{
    enam <- deparse(substitute(env))
    nam <- ls(envir = env, ...)
    cls <- sapply(nam, function(x) class(get(x, envir = env)))
    cls <- sapply(cls, paste, collapse = ", ")
    s <- sapply(ls(envir = env), function(x) object.size(get(x, 
        envir = env)))
    rval <- data.frame(class = cls, size = s)
    if (graph) 
        dotchart(sort(s), main = paste("Objects in environment:", 
            enam), xlab = "Size [bytes]")
    return(rval)
}
