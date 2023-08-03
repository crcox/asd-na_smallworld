library(dplyr)
library(igraph)

#' Sample from lexical-semantic environment balanced over parts of speech
#'
#' @param g An igraph object representing the lexical-semantic environment
#' @param n A numeric vector, representing the size of the sample for each part
#'   of speech (POS)
#' @param pos A fector with length `vcount(g)` (number of words in environment)
#'   with a level for each POS.
#' @returns a numeric vector of vertex ids (NOT CDI numeric item ids!)
#'
#' @export
balanced_ran <- function(g, n, pos = rep(1, igraph::vcount(g)) / igraph::vcount(g)) {
    env_size <- igraph::vcount(g)
    assertthat::are_equal(env_size, length(pos))
    assertthat::are_equal(nlevels(pos), length(n))
    ix <- stratified_sample(n, seq_len(env_size), pos, as_list = FALSE)
    return(igraph::induced_subgraph(g, ix))
}


#' Sample from multiple strata from one data set
#'
#' @param n a numeric vector, giving the number to sample from each stratum
#' @param x A data set to sample from
#' @param strat a factor of labels for the data. The number of labels must equal
#'   the length of n.
#' @param as_list Logical. Should the sample from each strata be segregated into
#'   list elements. [default: FALSE]
#'
#' @returns A sample drawn from \code{x}. Format depends on \code{x} and whether
#'   to return \code{as_list}.
#'
#' @export
stratified_sample <- function(n, x, strat, as_list = FALSE) {
    y <- mapply(sample, split(x, strat), n, SIMPLIFY = FALSE)
    return(if (as_list) y else unname(do.call(c, y)))
}


#' Map CDI categories to parts of speech
#'
#' @param categories A factor or character vector
#' @returns A character vector
part_of_speech <- function(categories) {
    noun_cat <- c("animals", "vehicles", "toys", "people", "places", "food_drink",
                  "clothing", "body_parts", "household", "furniture_rooms",
                  "outside")
    verb_cat <- c("action_words")
    return(ifelse(categories %in% noun_cat, "noun",
                  ifelse(categories %in% verb_cat, "verb", "other")))
}
