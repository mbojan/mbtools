devlib <- function(..., libloc=getOption("devlib"))
{
  stopifnot(!is.null(libloc))
  library(..., lib.loc=libloc)
}
