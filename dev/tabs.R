cellParbox <- function(x, width="\\cwd")
{
  content <- paste(x, collapse="\\ ")
  paste("\\parbox{", width, "}{", content, "}", sep="")
}

#============================================================================ 


l <- list( 1, 2, 3, "a", c(1,2), c(3,4) )
dim(l) <- c(3, 2)


