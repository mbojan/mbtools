edb <- git_commit_edgelist()


g <- git_commit_graph()
dir <- "../network"
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







# Drawing commit histories ------------------------------------------------

# https://pvigier.github.io/2019/05/06/commit-graph-drawing-algorithms.html

library(igraph)
dir <- "../network"
g <- git_commit_graph(dir)
oy <- order(V(g)$author_timestamp)
ox <- runif(vcount(g))
xy <- cbind(ox, oy)
plot(
  g, 
  layout = xy,
  vertex.label=NA,
  vertex.size = 3,
  edge.arrow.size = .5
)

# Get giant component
k <- clusters(g)
s <- induced_subgraph(g, V(g)[k$membership == which.max(k$csize)])
# Topological sort
tsort <- pooh::tsort(
  edb[,1],
  edb[,2], 
  domain = as.integer(V(s))[order(V(s)$author_timestamp, decreasing = TRUE)]
)
oy <- tsort
ox <- runif(vcount(s))
xy <- scale(cbind(ox, oy))
plot(
  s, 
  layout = xy,
  vertex.label=NA,
  vertex.size = 3,
  edge.arrow.size = .5,
  rescale = FALSE
)

plot(tsort, order(V(s)$author_timestamp, decreasing = TRUE))
