library(dplyr)
library(purrr)
library(tidyr)
library(igraph)
library(ggplot2)

meta <- readRDS("data/cdi-metadata-pos_vid.rds")
n_pos <- table(meta$pos)
pos_counts <- readRDS("data/asd_na-osg-2024_01_05-pos_counts.rds") %>%
    mutate(
        nproduced = noun + verb + other,
        p_noun = noun / n_pos["noun"],
        p_verb = verb / n_pos["verb"],
        p_other = other / n_pos["other"],
        r_noun = (noun / nproduced) * (sum(n_pos) / n_pos["noun"]),
        r_verb = (verb / nproduced) * (sum(n_pos) / n_pos["verb"]),
        r_other = (other / nproduced) * (sum(n_pos) / n_pos["other"])
    ) %>%
    rename(
        n_noun = noun,
        n_verb = verb,
        n_other = other
    )
pos_counts_long <- pos_counts %>%
    pivot_longer(
        c(starts_with("n_"), starts_with("p_"), starts_with("r_")),
        names_to = c("metric", "word_type"),
        names_sep = "_",
        values_to = "value"
    ) %>%
    mutate(
        word_type = factor(word_type, levels = c("noun", "verb", "other")),
        metric = factor(metric, levels = c("n", "p", "r"))
    )

df_hline = expand_grid(
    metric = levels(pos_counts_long$metric),
    word_type = levels(pos_counts_long$word_type)
)
df_hline$hline <- if_else(df_hline$metric == "r", 1, NA)

ggplot(pos_counts_long %>% filter(!(metric == "r" & value > 3)), aes(x = nproduced, y = value, color = group)) +
    geom_point(alpha = .3) +
    geom_smooth() +
    geom_hline(data = df_hline, aes(yintercept = hline)) +
    facet_grid(
        metric~word_type,
        scales = "free_y",
        labeller = labeller(
            metric = c(
                n = "count",
                p = "count / total",
                r = "p(type|vocab) : p(type)"
            )
        )
    ) +
    theme_bw(base_size = 18)

ggsave(filename = "pos_by_group.pdf", width = 11, height = 8, units = "in", dpi = 300)
ggsave(filename = "pos_by_group.png", width = 11, height = 8, units = "in", dpi = 300)

swi_list <- map(seq_len(nrow(pos_counts)), ~{
    p <- file.path("results", "asd-na_smallworld_2024_01_05", sprintf("%04d_smallworld_2024_01_05.rds", .x))
    readRDS(p)
})
swi_list <- readRDS("results/sw_p_t_2024_01_05.rds")

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
    # filter(nproduced > 59) %>%
    mutate(nproduced = n_noun + n_verb + n_other)

d_long <- d %>%
    pivot_longer(
        c(ends_with("_o"), ends_with("_z")),
        names_to = c("sw_metric", "sw_scale"),
        names_sep = "_",
        values_to = "sw_value"
    )

ggplot(d_long %>% filter(sw_metric == "propensity"), aes(x = nproduced, y = sw_value, color = group)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(
        ~sw_scale,
        scales = "free_y",
        labeller = labeller(sw_scale = c(o = "original values", z = "RAN standardized"))
    ) +
    theme_bw(base_size = 18)

ggsave("sw_propensity.png", width = 11, height = 8, units = "in", dpi = 300)

ggplot(d_long %>% filter(sw_metric == "propensity", nproduced > 300), aes(x = nproduced, y = sw_value, color = group)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(
        ~sw_scale,
        scales = "free_y",
        labeller = labeller(sw_scale = c(o = "original values", z = "RAN standardized"))
    ) +
    theme_bw(base_size = 18)

ggsave("sw_propensity_gt300.png", width = 11, height = 8, units = "in", dpi = 300)

summary(d)

d %>%
    group_by(group) %>%
    summarize(across(c(index_o,index_z,propensity_o,propensity_z,telesford_o,telesford_z), list(m=mean,s=sd)))

ggplot(d, aes(x = nproduced, y = propensity_o, color = group)) +
    geom_point() +
    geom_smooth()

ggplot(filter(d, nproduced >= 100), aes(x = nproduced, y = propensity_o, color = group)) +
    geom_point() +
    geom_smooth()

ggplot(filter(d, nproduced >= 100), aes(x = nproduced, y = propensity_z, color = group)) +
    geom_point() +
    geom_smooth()

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


dplot %>%
    group_by(metric, ran_std) %>%
    summarize(
        min = min(value)
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
