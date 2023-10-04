#' @name github
#' @title Various GitHub tools

#' @rdname github
#' @description - `github_details()` - Wrap input with markup proper for
#'   the "Details" element of a GitHub issue etc.
#'
#' @param x character; text to be wrapped in Details
#' @param summary_text character; the title of the Details
#' 
#' @export
#' @examples
#' ch <- c("a", "b")
#' cat(github_details(ch))

github_details <- function(x, summary_text = "Summary") {
  paste(
    "<details>",
    paste0("<summary>", summary_text, "</summary>"),
    "",
    paste(paste0(paste0(rep(" ", 6), collapse=""), x), collapse="\n"),
    "",
    "</details>",
    sep = "\n"
  )
}


#' @rdname github
#' @description - `session_info()` - Run [sessioninfo::session_info()],
#'   format the output and copy to clipboard for easy pasting on GitHub.
#' 
#' @importFrom utils capture.output
#' @export

session_info <- function() {
  requireNamespace("sessioninfo")
  requireNamespace("clipboard")
  z <- capture.output(sessioninfo::session_info())
  ch <- github_details(z, summary_text = "Session info")
  clipboard::write_cb(ch)
  
}




