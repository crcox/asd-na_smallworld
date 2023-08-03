#!/usr/bin/env Rscript --vanilla --default-packages=methods,utils,stats,graphics

args <- commandArgs(trailingOnly = TRUE)

subject_id <- as.integer(args[1])
niter <- as.integer(args[2])
network_path <- args[3]
vocab_vid_path <- args[4]
pos_counts_path <- args[5]
meta_path <- args[6]

str(list(subject_id, niter, network_path, vocab_vid_path, pos_counts_path, meta_path))

library(igraph)

pos_counts_df <- readRDS(file = pos_counts_path)[subject_id, ]
vocab_vids <- readRDS(file = vocab_vid_path)[[subject_id]]
g <- igraph::upgrade_graph(readRDS(file = network_path))
m <- readRDS(file = metadata_path)

print(pos_counts_df)
pos_counts <- unlist(pos_counts[c("noun", "verb", "other")])

results <- list(
    orig = swi(induced_subgraph(g, vids = vocab_vids)),
    ran = replicate(niter, swi(balanced_ran(g, x$n, m$pos)), simplify = TRUE)
)

saveRDS(results, file = sprintf("%03d_smallworld.rds", subject_id))
