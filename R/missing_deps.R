#' Check whether there are missing dependencies
#'
#' @param deps character vector of elements from "Depends", "Suggests",
#'   "Imports", "Enhances", or "LinkingTo"
#' @param ... arguments passed to [cranet::pkgnet()]
#'
#' @details First [installed.packages()] is called to get a matrix of all the
#'   installed packages. One can use `...` to pass e.g. `lib.loc` to
#'   [installed.packages()] to restrict attention to selected library tree.
#'   Obtained output is passed to [cranet::pkgnet()] to build the dependency
#'   graph. Use `deps` argument to control what kind of dependency relations are
#'   to be used. By default only "hard" dependencies are looked at. Finally the
#'   function compares the packages in the graph to the list of installed
#'   packages. Any packages present in the first but absent in the second should
#'   be installed but are not. The vector of names of these is returned.
#'
#' @return Character vector of names of missing packages with and igraph object
#'   as a `"graph"` attribute.
#'
#' @seealso [cranet::pkgnet()]
missing_deps <- function(deps = c("Depends", "Imports", "LinkingTo"), ...) {
  g <- cranet::pkgnet(
    installed.packages(...),
    vnams = c("Version"),
    enams = deps
  )
  g <- igraph::delete_vertices(g, igraph::V(g)[name == "R"])
  p <- .packages(TRUE)
  structure(
    setdiff(igraph::V(g)$name, p),
    graph = g
  )
}


