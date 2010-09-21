recode <-
function (x, mat) 
{
    i <- match(x, mat[, 1])
    i2 <- which(!is.na(i))
    i <- i[i2]
    rval <- x
    rval[i2] <- mat[, 2][i]
    rval
}
