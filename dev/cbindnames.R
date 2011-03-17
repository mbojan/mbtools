# binding matrices/data.frames matching on row names

Cbind <- function(...)
{
  # TODO check for type
  # TODO check for row names
  al <- list(...)
  ncols <- sapply(al, ncol)
  rnames <- unique( unlist(lapply(al, rownames)))
  # matching vectors
  rids <- lapply(al, function(m) match(rownames(m), rnames) )
  rl <- lapply( seq(along=ncols), function(i)
    {
      z <- matrix(NA, length(rnames), ncols[i] )
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
Cbind(m1, m2)
