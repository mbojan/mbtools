mbrepo <- function(loc=c("3e", "icm"))
{
    loc <- switch( match.arg(loc),
            "3e"="http://bojan.3e.pl/R",
            icm="http://www.icm.edu.pl/~mbojan/R")
    loc
}
            
