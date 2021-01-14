# Drawing commit histories
# 
# https://pvigier.github.io/2019/05/06/commit-graph-drawing-algorithms.html

if(FALSE) {
  
  library(igraph)
  
  withr::with_dir("~/R/src/network", {
    l <- system2("git", "log --all --format='%H %P'", stdout=TRUE)
  })
  edb <- strsplit(l, " ") %>%
    lapply(function(x) tibble::tibble(.commit = x[1], .parent = x[-1])) %>%
    dplyr::bind_rows()
  
  g <- graph_from_data_frame(edb, directed=TRUE)
  xy <- graphlayouts::layout_with_stress(g)
  plot(xy, pch=".")
  plot(
    g, 
    vertex.label=NA, 
    layout=xy,
    vertex.size=3, 
    edge.arrow.size=.5
  )
  
}
