dePolish <-
function (x, encoding = NULL) 
{
    d <- read.table(system.file("polish/letterspl.txt", package = "bojan"), 
        header = TRUE, as.is = TRUE)
    if (is.null(encoding)) {
        if (.Platform$OS.type == "windows") 
            s <- d$cp1250
        else {
            s <- iconv(d$latin2, from = "latin2", to = "")
        }
    }
    else {
        if (encoding == "utf-8" | encoding == "utf8") 
            s <- d$utf8
        else s <- iconv(d$latin2, from = "latin2", to = encoding)
    }
    rval <- x
    for (i in seq(1, nrow(d))) {
        rval <- gsub(s[i], d$ascii[i], rval)
    }
    rval
}
