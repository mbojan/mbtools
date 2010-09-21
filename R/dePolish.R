.readLetterspl <- function(enc=c("utf-8", "latin2", "win1250"))
{
    a <- match.arg(enc)
    fname <- file.path("/home/michal/R/src/mbtools/mbtools-dev/inst/extdata",
                    paste("letterspl", enc, "txt", sep="."))
            # fname <- system.file(file.path("extdata", paste("letterspl", enc, "txt", sep=".")), package="mbtools")
    rval <- read.table(fname, header=FALSE, as.is=TRUE)
    names(rval) <- c("ascii", "nat")
    rval
}

dePolish <-
function (x, encoding = NULL) 
{
    if (is.null(encoding)) {
        if (.Platform$OS.type == "windows") 
            encoding <- "win1250"
        else {
            encoding <- "utf-8"
        }
        d <- .readLetterspl(enc=encoding)
    }
    else {
        if ( encoding == "utf8") 
            encoding <- "utf-8"
        d <- .readLetterspl(enc="latin2")
        d$nat <- iconv(d$nat, from="latin2", to=encoding)
    }
    rval <- x
    for (i in seq(1, nrow(d))) {
        rval <- gsub(d$nat[i], d$ascii[i], rval)
    }
    rval
}

