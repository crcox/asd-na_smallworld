library(dplyr)
library(purrr)
library(tidyr)
library(igraph)
library(ggplot2)

pos_counts <- readRDS("data/asd_na-osg-2023_06_30-pos_counts.rds")

swi_list <- map(seq_len(nrow(pos_counts)), ~{
    p <- file.path("results", sprintf("%04d_smallworld.rds", .x))
    readRDS(p)
})

swi_df <- map(swi_list, ~{
    ran_mean <- rowMeans(.x$ran, na.rm = TRUE)
    ran_sdev <- apply(.x$ran, 1, sd, na.rm = TRUE)
    z <- as.list((.x$orig - ran_mean) / ran_sdev)
    names(z) <- paste(names(z), "z", sep = "_")
    x <- as.list(.x$orig)
    names(x) <- paste(names(x), "o", sep = "_")
    as_tibble(c(x, z))
}) %>%
    list_rbind() %>%
    bind_cols(pos_counts, .)

d <- swi_df %>%
    #drop_na() %>%
    mutate(nproduced = noun + verb + other)
    filter(nproduced > 59)

summary(d)

d %>%
    group_by(group) %>%
    summarize(across(c(index_o,index_z,propensity_o,propensity_z,telesford_o,telesford_z), list(m=mean,s=sd)))

dplot <- pivot_longer(
    d,
    cols = c(ends_with("_o"), ends_with("_z")),
    names_to = c("metric", "ran_std"),
    names_sep = "_",
    values_to = "value"
) %>%
    mutate(
        ran_std = factor(ran_std, levels = c("o", "z"), labels = c("original values", "RAN standardized values"))
    )

ggplot(dplot, aes(x = nproduced, y = value, color = group)) +
    geom_smooth() +
    facet_wrap(~ metric, scales = "free_y") +
    ggtitle("unadjusted values")

ggsave("smallworld_preliminary_GAM_orig.png", width = 7, height = 6, units = "in", dpi = 300)

ggplot(dplot %>% filter(ran_std == "RAN standardized values"), aes(x = nproduced, y = value, color = group)) +
    geom_smooth() +
    facet_wrap( ~ metric) +
    ggtitle("RAN standardized values")

ggsave("smallworld_preliminary_GAM_ran-stand.png", width = 7, height = 6, units = "in", dpi = 300)

# poly reg ----


orth_poly <- poly(d$nproduced, 3)
d <- bind_cols(d, as_tibble(orth_poly)) %>%
    rename(linear = "1", quadradic = "2", cubic = "3")

models <- list(
    index_o = lm(index_o ~ (linear + quadradic + cubic) * group, data = d),
    propensity_o = lm(propensity_o ~ (linear + quadradic + cubic) * group, data = d),
    telesford_o = lm(telesford_o ~ (linear + quadradic + cubic) * group, data = d, subset = !is.infinite(telesford_o)),
    index_z = lm(index_z ~ (linear + quadradic + cubic) * group, data = d),
    propensity_z = lm(propensity_z ~ (linear + quadradic + cubic) * group, data = d),
    telesford_z = lm(telesford_z ~ (linear + quadradic + cubic) * group, data = d)
)

map(models, summary)
