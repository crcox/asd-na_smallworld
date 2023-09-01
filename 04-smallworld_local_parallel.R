library(purrr)
library(furrr)
library(igraph)
source("R/swi.R")
source("R/ran.R")

vocab_vids <- readRDS(file = "data/asd_na-osg-2023_08_14-vocab_vid.rds")
g <- igraph::upgrade_graph(readRDS(file = "network/child_net_graph.rds"))
m <- readRDS(file = "data/cdi-metadata-pos_vid.rds")

plan("multicore")

x <- future_map(vocab_vids, function(v) {
    swi(igraph::induced_subgraph(g, vids = v), methods = c("propensity", "telesford"))
}, .options = furrr_options(seed = TRUE))

saveRDS(x, file = "sw_p_t.rds")
