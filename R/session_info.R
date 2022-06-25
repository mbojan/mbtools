#' Session info for GitHub
#'
#' Run `sessioninfo::session_info()`, format the output and copy to clipboard
#' for easy pasting on GitHub.
#' 
#' @importFrom utils capture.output
#' @export

session_info <- function() {
  requireNamespace("sessioninfo")
  requireNamespace("clipboard")
  z <- capture.output(sessioninfo::session_info())
  ch <- paste(
    "<details>",
    "<summary>Session info</summary>",
    "",
    paste(paste0(paste0(rep(" ", 6), collapse=""), z), collapse="\n"),
    "",
    "</details>",
    sep = "\n"
  )
  clipboard::write_cb(ch)
  
}
