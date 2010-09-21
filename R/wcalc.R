wcalc <-
function (p) 
{
    x <- c(1, p[seq(1, length(p) - 1)])
    p - x
}
