# reset number of text columns used by R, by default to the width of the
# terminal
cw <- function(w=Sys.getenv("COLUMNS"))
{
  options(width=w)
}
