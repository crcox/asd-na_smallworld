library(dplyr)
library(purrr)
library(igraph)
library(tidyr)

part_of_speech <- function(categories) {
    noun_cat <- c("animals", "vehicles", "toys", "people", "places", "food_drink",
                  "clothing", "body_parts", "household", "furniture_rooms",
                  "outside")
    verb_cat <- c("action_words")
    return(ifelse(categories %in% noun_cat, "noun",
                  ifelse(categories %in% verb_cat, "verb", "other")))
}

d <- list(
    new = readRDS(file = "data/asd_na-osg-2023_08_14.rds") %>% mutate(dset = "new"),
    old = readRDS(file = "data/asd_na-osg-2023_06_30.rds") %>% mutate(dset = "old")
) %>%
    bind_rows()

d %>%
    group_by(subjectkey, dset) %>%
    summarize(n = sum(produced)) %>%
    group_by(subjectkey) %>%
    filter(n[1] != n[2])

g <- upgrade_graph(readRDS(file = "network/child_net_graph.rds"))
v <- data.frame(vid = seq_len(igraph::vcount(g)), word = names(igraph::V(g)))
m <- readRDS(file = "data/cdi-metadata.rds") %>%
    mutate(
        pos = part_of_speech(category),
        pos = factor(pos, levels = c("noun", "verb", "other"))
    ) %>%
    select(num_item_id, word = cue_CoxHae, pos) %>%
    left_join(v, by = "word") %>%
    filter(!is.na(vid)) %>%
    arrange(vid)
d_vid <- d %>%
    left_join(m, by = "num_item_id") %>%
    filter(!is.na(vid)) %>%
    arrange(group, subjectkey, vid)

pos_counts <- d_vid %>%
    filter(produced == TRUE) %>%
    count(group, subjectkey, pos) %>%
    pivot_wider(
        id_cols = c(group, subjectkey),
        names_from = pos,
        values_from = n,
        values_fill = 0
    )

vocab_vid <- d_vid %>%
    filter(produced == TRUE) %>%
    group_by(group, subjectkey) %>%
    group_split() %>%
    map(~{pull(.x, vid)})

saveRDS(pos_counts, file = "data/asd_na-osg-2023_08_14-pos_counts.rds")
saveRDS(vocab_vid, file = "data/asd_na-osg-2023_08_14-vocab_vid.rds")
saveRDS(m, file = "data/cdi-metadata-pos_vid.rds")
