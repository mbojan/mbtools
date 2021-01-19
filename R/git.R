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
#' @param dir directory with git repo
#' @param format_log character vector of `git log` format options
#' @param delim,... passed to [readr::read_delim()] usually `col_names` or `col_types`
#'   need to be specified too
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
#' @return Function `git_commit_edgelist()` returns a two-column tibble with columns:
#' - `.commit` - hash of a commit
#' - `.parent` - hash of a parent of the `.commit`
#' 
#' @importFrom %>% dplyr
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
#' @description - `git_commits` - assemble a database of git commits with hash and author date time
#' 
#' @param col_types passed to [readr::read_delim()]
#' 
#' @details For `git_commits()` if `col_types` is missing (default) it is assumed to be `"ciT"`
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
#' @export
git_commit_graph <- function(dir = ".") {
  edb <- git_commit_edgelist(dir)
  vdb <- git_commits(dir, col_types = "ccc")
  g <- igraph::graph_from_data_frame(
    as.data.frame(edb),
    directed=TRUE, 
    vertices = as.data.frame(vdb)
  )
  igraph::set_graph_attr(g, "refs", git_refs(dir))
}

#' @rdname git
#' 
#' @param g a graph built with `git_commit_graph()`
#' @param refs commit refs
#' 
#' @export
Vref <- function(g, refs) {
  stopifnot(inherits(g, "igraph"))
  stopifnot("refs" %in% igraph::list.graph.attributes(g))
  mv <- match(refs, g$refs$ref)
  bad_ref <- is.na(mv)
  if(any(bad_ref))
    stop("can't find these refs: ", paste(dQuote(refs[bad_ref]), collapse=", ") )
  igraph::V(g)[g$refs$.commit[mv]]
}

if(FALSE) {
  g <- git_commit_graph()
  dir <- "F:/michal/Documents/R/network"
  g <- git_commit_graph(dir)
  xy <- layout_with_sugiyama(g, layers = order(V(g)$author_timestamp))
  xy <- graphlayouts::layout_with_stress(g)
  xy <- cmdscale(distances(g, mode = "all")) %>% rescale()
  
  
  plot(
    g, 
    # layout=xy$layout, 
    layout = xy,
    vertex.label=NA,
    vertex.size = 3,
    edge.arrow.size = .5
  )
  
  library(igraph)
  sg <- make_ego_graph(g, order=5, nodes = Vref(g, "refs/remotes/origin/i172-predict-ergm"))[[1]]
  plot(
    sg, 
    
    vertex.color = V(sg) == Vref(sg, "refs/remotes/origin/i172-predict-ergm"),
    vertex.label = NA,
    edge.arrow.size = 0.5
  )
  
  library(tidygraph)
  library(ggraph)
  as_tbl_graph(g) %>%
    ggraph(layout="sugiyama") +
    geom_node_point() +
    geom_edge_link()
}




# Drawing commit histories ------------------------------------------------

# https://pvigier.github.io/2019/05/06/commit-graph-drawing-algorithms.html
