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
    v3 = readRDS(file = "data/asd_na-osg-2023_01_05_2024.rds") %>% mutate(dset = "d01_05_2024"),
    v2 = readRDS(file = "data/asd_na-osg-2023_08_14.rds") %>% mutate(dset = "d08_14_2023"),
    v1 = readRDS(file = "data/asd_na-osg-2023_06_30.rds") %>% mutate(dset = "d06_30_2023")
) %>%
    bind_rows() %>%
    mutate(dset = factor(dset, levels=c("d01_05_2024", "d08_14_2023", "d06_30_2023")))

d %>%
    filter(dset == "d01_05_2024" | dset == "d08_14_2023", group == "ASD") %>%
    group_by(subjectkey, dset) %>%
    summarize(n = sum(produced)) %>%
    group_by(subjectkey) %>%
    pivot_wider(id_cols = "subjectkey", names_from = dset, values_from = n) %>%
    filter(d01_05_2024 != d08_14_2023 | is.na(d01_05_2024) | is.na(d08_14_2023)) %>%
    summary()
    #print(n=54)

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
    filter(dset == "d01_05_2024") %>%
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

saveRDS(pos_counts, file = "data/asd_na-osg-2024_01_05-pos_counts.rds")
saveRDS(vocab_vid, file = "data/asd_na-osg-2024_01_05-vocab_vid.rds")
#saveRDS(m, file = "data/cdi-metadata-pos_vid.rds")
