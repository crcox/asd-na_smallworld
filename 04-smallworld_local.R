library(purrr)
library(furrr)
library(igraph)
source("R/swi.R")
source("R/ran.R")

vocab_vids <- readRDS(file = "data/asd_na-osg-2023_08_14-vocab_vid.rds")
g <- igraph::upgrade_graph(readRDS(file = "network/child_net_graph.rds"))
m <- readRDS(file = "data/cdi-metadata-pos_vid.rds")
x <- lapply(vocab_vids, function(v) {
    swi(induced_subgraph(g, vids = v), methods = "propensity")
})
saveRDS(x, file = "results/swpropensity.rds")

x <- readRDS("results/swpropensity.rds")

swp <- vapply(x, function(y) {y[1]}, numeric(1))
deltaC <- vapply(x, function(y) {y["deltaC"]}, numeric(1))
deltaL <- vapply(x, function(y) {y["deltaL"]}, numeric(1))
pos_counts <- readRDS("data/asd_na-osg-2023_08_14-pos_counts.rds")
pos_counts
pos_counts$swp <- swp
pos_counts$nproduced <- pos_counts$noun + pos_counts$verb + pos_counts$other
library(ggplot2)
ggplot(pos_counts, aes(x = nproduced, y = swp, color = group)) +
    geom_smooth()

ggplot(pos_counts, aes(x = nproduced, y = swp_unscaled, color = group)) +
    geom_smooth()

pos_counts$swp_unscaled <- 1 - sqrt((deltaC^2 + deltaL^2) / 2)
