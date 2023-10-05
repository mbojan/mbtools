#' Rip Polish letters of the diacritics
#' 
#' The function substitutes all Polish letters in the provided character
#' vector with ASCII counterparts.
#'
#' @param x character; vector to be processed
#' @param encoding character; encoding used in `x`
#'
#' The function substitutes (with [sub()]) case-sensitively all Polish
#' letters with simple ASCII counterparts.
#' 
#' The `encoding` argument defines the way in which Polish letters are
#' encoded in `x`. If `NULL` and the platform is Windows the function
#' uses CP1250 encoding. If not on Windows the function uses
#' [iconv()] to translate the letters to default encoding on current
#' locale (via argument `from=""`).
#' 
#' @return A character vector after substitutions.
#' 
#' @seealso [sub()], [iconv()]
#' 
#' @export
dePolish <- function (x, encoding = NULL) {
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




# Read vectors of Polish characters from the package-embedded file
#
# @param enc character; which encoding to use
#
# @return Data frame of two columns `ascii` and `nat` ("native") with the
#   characters.
.readLetterspl <- function(enc=c("utf-8", "latin2", "win1250"))
{
  a <- match.arg(enc)
  fname <- system.file("extdata", paste("letterspl", enc, "txt", sep="."), 
                       package="mbtools")
  rval <- utils::read.table(fname, header=FALSE, as.is=TRUE)
  names(rval) <- c("ascii", "nat")
  rval
}
