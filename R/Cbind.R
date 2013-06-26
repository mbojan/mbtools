# binding matrices/data.frames matching on row names



#' Combine objects by columns matching the rows on row names
#' 
#' Take a sequence of matrix-like objects and combine the column-wise matching
#' the rows on the common row names.
#' 
#' All the supplied objects have to have row names defined, i.e. calling
#' \code{\link{rownames}} on the should return character vector.
#' 
#' All the objects are converted to data frames prior combining.
#' 
#' The result will have the number of columns equal to the sum of the numbers
#' of columns in the supplied objects. The number of rows will be equal to the
#' number of unique row names in all the arguments.
#' 
#' @param \dots matrices, vectors or data.frames that are to be combined. These
#' are converted to data frames before merging.
#' @return Data frame consisting of merged objects.
#' @seealso \code{\link{cbind}}, \code{\link{rownames}}
#' @examples
#' 
#' # two matrices
#' m1 <- m2 <- matrix(1:4, 2, 2)
#' colnames(m1) <- colnames(m2) <- 1:2
#' rownames(m1) <- c("a", "b")
#' rownames(m2) <- c("b", "c")
#' Cbind(m1, m2)
#' 
#' # matrices and a data frame
#' l <- list(  m1 = m1, 
#'             m2 = m2,
#'             d1 = data.frame( x=1:4, y=2:5,
#'               row.names=letters[1:4]) 
#'   )
#' do.call("Cbind", l)
#' 
#' @export Cbind
Cbind <- function(...)
{
  al <- list(...)
  # all arguments must have rownames defined
  stopifnot( sapply(al, function(x) !is.null(rownames(x))))
  # number of columns per argument
  ncols <- sapply(al, ncol)
  # all unique row names
  rnames <- unique( unlist(lapply(al, rownames)))
  # matching vectors
  rids <- lapply(al, function(m) match(rownames(m), rnames) )
  rl <- lapply( seq(along=ncols), function(i)
    {
      z <- matrix(NA, length(rnames), ncols[i] )
      z <- as.data.frame(z)
      colnames(z) <- colnames( al[[i]] )
      z[ rids[[i]] , ] <- al[[i]]
      z
    } )
  rval <- do.call("cbind", rl)
  rownames(rval) <- rnames
  rval
}




if(FALSE)
{
m1 <- m2 <- matrix(1:4, 2, 2)
colnames(m1) <- colnames(m2) <- 1:2
rownames(m1) <- c("a", "b")
rownames(m2) <- c("b", "c")
l <- list(m1=m1, m2=m2,
  d1 <- data.frame( x=1:4, y=2:5, row.names=letters[1:4]) 
  )

Cbind(m1, m2)
}
