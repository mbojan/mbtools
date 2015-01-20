### Test both methods on some simple graphs
testit <- function(d)
{
    rval <- c( eg = isGraphical(d, "eg"),
        tv = isGraphical(d, "tv") )
    if(!all(rval))
    {
        print(rval)
        stop("failed")
    }
    rval
}
# graphs of size 3
testit( c(0,0,0) )
testit( c(0,1,1) )
testit( c(1,2,1) )
testit( c(2,2,2) )

