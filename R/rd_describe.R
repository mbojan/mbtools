#' RD generator: describe
#'
#' Generate RD markup for the definition list via the `\describe` command.
#' 
#' @param object R object
#' @param ... other arguments passed to/from other methods
#' 
#' @return Nothing, but RD syntax is [cat()]ed.
#' 
#' @family RD generators
#' @export
rd_describe <- function(object, ...) UseMethod("rd_describe")


#' @describeIn rd_describe The default method expects character vector of
#'   "terms" and a character vector `description` of the same length containing
#'   the term descriptions.
#' 
#' @param description Character vector of term descriptions
#' 
#' @export
rd_describe.default <- function(object, description, ...) {
  stopifnot(identical(length(object), length(description)))
  defs <- as.character(object)
  descs <- as.character(description)
  cat("\\describe{\n")
  for(i in seq(along=names(defs))) {
    cat("  \\item{", defs[i], "}{", descs[i], "}\n", sep="")
  }
  cat("}\n\n")
}




#' @describeIn rd_describe Generate a `\describe{}` markup with a list of
#'   variable names together with variable labels defined (as in the
#'   **labelled** package).
#'
#' @export
rd_describe.data.frame <- function(object, ...) {
  stopifnot(requireNamespace("labelled"))
  # Data frame of variable labels
  varlabel <- labelled::var_label(object)
  isnull <- sapply(varlabel, is.null)
  varlabel[isnull] <- list("")
  varlabel <- unlist(varlabel)
  nams <- names(object)
  rd_describe.default(nams, varlabel, ...)
}

if(FALSE) {
  d <- data.frame(
    x = 1:5,
    y = 5:1
  ) |>
    labelled::set_variable_labels(
      x = "This is variable `x`",
      y = "This is variable `y`"
    )
  rd_describe(d)
}