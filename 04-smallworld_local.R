library(igraph)
source("R/swi.R")
source("R/ran.R")

vocab_vids <- readRDS(file = "data/asd_na-osg-2023_08_14-vocab_vid.rds")
g <- igraph::upgrade_graph(readRDS(file = "network/child_net_graph.rds"))
m <- readRDS(file = "data/cdi-metadata-pos_vid.rds")

x <- lapply(vocab_vids[1:100], function(v) {
    swi(induced_subgraph(g, vids = v), methods = "propensity")
})
