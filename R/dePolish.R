.readLetterspl <- function(enc=c("utf-8", "latin2", "win1250"))
{
    a <- match.arg(enc)
    fname <- system.file("extdata", paste("letterspl", enc, "txt", sep="."), package="mbtools")
    rval <- read.table(fname, header=FALSE, as.is=TRUE)
    names(rval) <- c("ascii", "nat")
    rval
}



#' Rip Polish letters of the diacritics
#' 
#' The function substitutes all Polish letters in the provided character vector
#' with ASCII counterparts.
#' 
#' The function substitutes (with \code{sub}) case-sensitively all Polish
#' letters with simple ASCII counterparts.
#' 
#' The \code{encoding} argument defines the way in which Polish letters are
#' encoded in \code{x}. If \code{NULL} and the platform is Windows the function
#' uses CP1250 encoding. If not on Windows the function uses
#' \code{\link{iconv}} to translate the letters to default encoding on current
#' locale (via argument \code{from=""}.
#' 
#' @param x character vector, do be processed
#' @param encoding character, encoding used in \code{x}
#' @return A character vector after substitutions
#' @seealso \code{\link{sub}}, \code{\link{iconv}}
#' @examples
#' 
#' \dontshow{
#' dePolish("dupa")
#' }
#' 
#' @export dePolish
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

