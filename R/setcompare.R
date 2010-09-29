setcompare <- function(x, y) UseMethod("setcompare")


setcompare.default <- function(x, y)
{
  list( x=setdiff(x, y),
      int=intersect(x, y),
      y=setdiff(y, x) )
}

setcompare.data.frame <- function(x, y)
{
  setcompare.default( names(x), names(y))
}
