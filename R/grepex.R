# extracting from 'x' based on results of 'regexpr'
extract <- function(x, v)
{
    rval <- sapply( seq(along=v), function(i)
            substr(x[i], v[i], v[i] + attr(v, "match.length")[i] - 1) )
    rval[ v == -1 ] <- NA
    rval
}


grepex <- function(pattern, x, multiple=FALSE, ...)
{
    r <- sapply(pattern, function(p)
        {
            if(!multiple)
            {
                v <- regexpr(p, x, ...)
                extract(x, v)
            } else
            {
                .NotYetImplemented()
            }
        })
    r
}
