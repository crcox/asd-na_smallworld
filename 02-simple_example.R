library(dplyr)
library(igraph)

source("R/ran.R")
source("R/swi.R")

pos_counts <- readRDS(file = "data/asd_na-osg-2023_06_30-pos_counts.rds")
g <- upgrade_graph(readRDS(file = "network/child_net_graph.rds"))
m <- readRDS(file = "data/cdi-metadata-pos_vid.rds")

# Worked example for one subject ----
x <- pos_counts %>%
    filter(subjectkey == "129245")

# Sanity check on balanced_ran()
n <- unlist(x[c("noun", "verb", "other")])
gran <- balanced_ran(g, n, m$pos)
y <- m %>%
    filter(word %in% names(V(gran))) %>%
    count(pos) %>%
    as_tibble()
all.equal(unname(n), y$n)

# Example of computing small world metrics over multiple RANs
system.time(replicate(10, swi(balanced_ran(g, n, m$pos)), simplify = TRUE))

# Notes on time:
# Computing small world metrics for 10 random networks takes ~10s on CRC laptop.
# Extrapolating, that would mean about 3 hours for 1000 networks. This is
# reasonable for the open science grid.
