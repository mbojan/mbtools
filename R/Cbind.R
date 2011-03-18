# binding matrices/data.frames matching on row names

Cbind <- function(...)
{
  al <- list(...)
  # convert everything to a data frame
  al <- lapply(al, as.data.frame)
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

m1 <- m2 <- matrix(1:4, 2, 2)
colnames(m1) <- colnames(m2) <- 1:2
rownames(m1) <- c("a", "b")
rownames(m2) <- c("b", "c")
l <- list(m1=m1, m2=m2,
  d1 <- data.frame( x=1:4, y=2:5, row.names=letters[1:4]) 
  )

Cbind(m1, m2)
