carryover <-
function (x, empty = NA) 
{
    if (is.na(empty)) 
        ind <- which(is.na(x))
    else ind <- which(x == empty)
    if (1 %in% ind) {
        stop("First observation missing, you need to impute it by hand!")
    }
    if (length(ind) != 0) {
        rval <- x
        rval[ind] <- x[ind - 1]
        carryover(rval, empty = empty)
    }
    else return(x)
}
