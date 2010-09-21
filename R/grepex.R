grepex <-
function (pattern, x, multiple = FALSE, ...) 
{
    r <- sapply(pattern, function(p) {
        if (!multiple) {
            v <- regexpr(p, x, ...)
            extract(x, v)
        }
        else {
            .NotYetImplemented()
        }
    })
    r
}
