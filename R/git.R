# Querying git history ----------------------------------------------------

#' @name git
#' @title Git querying
#' 
#' @description Various tools for querying Git history as a Directed Acyclic
#'   Graph (DAG).
NULL

#' @rdname git
#' 
#' @description - `git_log()` - a low-level interface
#' 
#' @param dir directory with git repo, defaults to current directory
#' @param format_log character vector of `git log` format options
#' @param delim,... passed to [readr::read_delim()] usually `col_names` or
#'   `col_types` need to be specified too
#'   
#' @details Function `git_log()` runs `git log` in `dir` passing `format_log`
#'   collapsed with white spaces to the `--format` option. The command is run
#'   with `--all` option to include all the branches.
#'   
#' @return Function `git_log()` returns a tibble with as many columns as there
#'   are fields requested with `format_log` and parsed by [readr::read_delim()]
#'   using `delim`.
#' 
#' @export
git_log <- function(dir = ".", format_log, delim = " ", ...) {
  withr::with_dir(dir, {
    l <- system2(
      "git", 
      paste0("log --all --format=\"", paste(format_log, collapse=" "), "\""), 
      stdout=TRUE
    )
  })
  readr::read_delim(l, delim = delim, ...)
}

#' @rdname git
#' 
#' @description - `git_commit_edgelist()` - assemble an edgelist of the commit
#'   graph in which a directed edge connects a commit to its parent.
#'   
#' @details Do note, wrt `git_commit_edgelist()`, that a commit can be a parent
#'   of more than one commit (i.e., when the history "forks") and a commit can
#'   have multiple parents (i.e. in case of merge commits).
#'   
#' @return Function `git_commit_edgelist()` returns a two-column tibble with
#'   columns:
#' - `.commit` - hash of a commit
#' - `.parent` - hash of a parent of the `.commit`
#' 
#' @importFrom magrittr %>%
#' @export
git_commit_edgelist <- function(dir = ".") {
  git_log(
    dir, 
    format_log = c("%H-%P"), 
    col_names=c(".commit", ".parent"),
    col_types = "cc", 
    delim = "-"
  ) %>%
    dplyr::filter(!is.na(.parent)) %>%
    dplyr::mutate(
      .parent = strsplit(.parent, " ")
    ) %>%
    tidyr::unnest(".parent")
}

globalVariables(".parent")

#' @rdname git
#' 
#' @description - `git_commits()` - assemble a database of git commits with hash
#'   and author date time
#' 
#' @param col_types passed to [readr::read_delim()], defaults to `"ciT"`
#' 
#' @details For `git_commits()` if `col_types` is missing (default) it is
#'   assumed to be `"ciT"`
#' 
#' @return Function `git_commits()` returns a tibble with columns:
#' - `.commit` - commit hash
#' - `author_timestamp` - Linux timestamp of author date
#' - `author_datetime` - author date in strict ISO 8601 format
#' 
#' @export
git_commits <- function(dir = ".", col_types) {
  if(missing(col_types)) col_types <- "ciT"
  git_log(
    dir,
    format_log = c("%H %at %aI"),
    col_names = c(".commit", "author_timestamp", "author_datetime"),
    col_types = col_types
  )
}

#' @rdname git
#' 
#' @description - `git_refs()` - fetch information about Git refs
#' 
#' @return Function `git_refs()` returns a tibble with a row for each ref and
#'   the following columns:
#'   - `.commit` - commit hash
#'   - `ref` - full name of the ref, e.g. `refs/heads/master` or
#'     `refs/remotes/origin/HEAD`
#' 
#' @export
git_refs <- function(dir = ".") {
  withr::with_dir(dir, {
    l <- system2("git", "show-ref", stdout = TRUE)
  })
  readr::read_delim(
    l, 
    delim = " ",
    col_names = c(".commit", "ref"),
    col_types = "cc"
  )
}

#' @rdname git
#' 
#' @description - `git_commit_graph()` - create an igraph object with the
#'   complete history of the repository. 
#'   
#' @return The igraph object returned by `git_commit_graph()` has vertices
#'   correspond to commits and edges point from commits to their parents. It has
#'   additionally the following attributes defined:
#'   - `name` - vertex attribute with commit hash
#'   - `author_timestamp` - vertex attribute with author date timestamp
#'   - `author_datetime` - vertex attribute with author date in ISO  8601 format
#'   - `refs` - vertex attribute with a list of either `NULL` or character
#'   vector of refs pointing to the particular commit
#' 
#' @export
git_commit_graph <- function(dir = ".") {
  edb <- git_commit_edgelist(dir)
  vdb <- git_commits(dir, col_types = "ccc")
  g <- igraph::graph_from_data_frame(
    as.data.frame(edb),
    directed=TRUE,
    vertices = as.data.frame(vdb)
  )
  reflist <- with(git_refs(dir), tapply(ref, .commit, unique))
  set_vertex_attr(g, name = "refs", index = igraph::V(g)[names(reflist)], value = reflist)
}

#' @rdname git
#' 
#' @description - `Vref()` - custom creation of vertex sequences
#' 
#' @param g a graph built with `git_commit_graph()`
#' @param refs commit refs
#' 
#' @return Function `Vref()` works similarly to [igraph::V()] returning vertex
#'   sequence for vertices in `g` corresponding to refs specifed by `refs`.
#' 
#' @export
Vref <- function(g, refs=NULL, ...) {
  stopifnot(inherits(g, "igraph"))
  stopifnot("refs" %in% igraph::vertex_attr_names(g))
  refvec <- unlist(igraph::V(g)$refs)
  if(is.null(refs)) {
    i <- grep()

  } else {
    mv <- match(refs, g$refs$ref)
    bad_ref <- is.na(mv)
    if(any(bad_ref))
      stop("can't find these refs: ", paste(dQuote(refs[bad_ref]), collapse=", ") )
    igraph::V(g)[g$refs$.commit[mv]]
  }
}

grep_search <- function(pat, x, ...) {
  
}




# Layout curved branches --------------------------------------------------

# procedure curved_branches(C)
#   Initialize an empty list of active branches B
#   for c in C from lowest i-coordinate to largest
#     if c.branchChildren is not empty
#       select d in c.branchChildren
#       replace d by c in B
#     else
#       insert c in B
#     for d' in c.branchChildren \ {d}
#                 remove d' from B
#     c.j = index of c in B

# Active branch = a ref that has no children

curved_branches <- function(g) {
  refs <- Vref(g)
  active_branches <- refs[igraph::degree(g, v = refs, mode = "in") == 0]
  active_branches
}
