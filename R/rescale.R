rescale <-
function (x, nmin = 0, nmax = 1, na.rm = FALSE, coef = FALSE) 
{
    omin <- min(x, na.rm = na.rm)
    omax <- max(x, na.rm = na.rm)
    b <- (nmax - nmin)/(omax - omin)
    a <- nmin - (omin * (nmax - nmin))/(omax - omin)
    if (coef) {
        return(list(a = a, b = b))
    }
    else {
        return(x * b + a)
    }
}
