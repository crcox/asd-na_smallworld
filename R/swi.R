#' Small world indices for an igraph network
#'
#' Networks have small world structure when the average shortest paths between
#' nodes are small, while local clustering remains high. That is, there is a
#' collection of interconnected communities. This concept can be quantified many
#' ways, three of which are discussed by Hills (XXXX). Those three methods are
#' implemented within this function.
#'
#' @param g An igraph object representing the network
#' @param iter Number of random samples from which to estimate random and
#'   lattice clustering and path length.
#' @param methods A character vector method-names. A small world estimate would
#'   be provided for each method listed.
#' @param local_clust Logical. If TRUE, then the local clustering coefficient is
#'   computed at each node, and these are averaged. If false, the global
#'   clustering coeffcient for the network is computed. These have slightly
#'   different definitions; local clustering could be considered more in line
#'   with the spirit of small worldness and may be more well behaved (but these
#'   are currently untested assumptions). [default: TRUE].
#' @returns A numeric vector of small world metrics, one for each of the
#'   requested \code{methods}.
#'
#' @details
#' Small worldness is estimated with respect to networks that preserve some
#' qualities of the original network, like the number of nodes and average
#' degree, but are either 1) randomly generated (i.e., via an Erdos-Renyi game)
#' or 2) closely approximating a ring lattice. Since there is no single perfect
#' comparison network, and each random or lattice network will differ, several
#' of each are generated and the average clustering coefficient and average
#' shortest path lengths are recorded for each. The number of these networks to
#' generate is determined by \code{iter}. These stats are then averaged, and
#' these parameter estimates are then passed into the equations for computing
#' small worldness (along with the values obtained from the original network).
#'
#' Each of the three implemented small world metrics rely on some combination of
#' these same six essential values:
#
#' 1. Clustering coefficient (CC) from the observed network. \eqn{C_{obs}}
#' 2. Average shortest path length (ASPL) from the observed network. \eqn{L_{obs}}
#' 3. Parameter estimate of CC for random networks. \eqn{C_{rand}}
#' 4. Parameter estimate of ASPL for random networks. \eqn{L_{rand}}
#' 5. Parameter estimate of CC for lattice networks. \eqn{C_{latt}}
#' 6. Parameter estimate of ASPL for lattice networks. \eqn{L_{latt}}
#'
#' Small World Index (method: "index")
#' \deqn{SWI = \frac{ \frac{C_{obs}}{C_{rand}} }{ \frac{L_{obs}}{L_{rand}} }}
#'
#' Small World Propensity (method: "propensity")
#' \deqn{\phi = 1 - \sqrt{ \frac{\Delta^2 C + \Delta^2 L}{2}}}
#' where
#' \deqn{\Delta C = \frac{C_{latt}-C_{obs}}{C_{latt}-C_{rand}}}
#' and
#' \deqn{\Delta L = \frac{L_{obs}-L_{rand}}{L_{latt}-L_{rand}}}
#'
#' Telesford's small world measure (method: "telesford")
#' \deqn{\omega = \frac{L_{rand}}{L_{obs}} - \frac{C_{obs}}{C_{latt}}}
#'
#' @references
#' Hills, T. (XXXX). Network Science Book.
#'
#' @export
swi <- function(g, iter = 100, methods = c("index", "propensity", "telesford"), local_clust = TRUE, truncate_delta = TRUE) {
    nv <- igraph::vcount(g)
    ne <- igraph::ecount(g)
    target_degree <- mean(degree(g))

    Grand <- replicate(iter, igraph::erdos.renyi.game(nv, ne, type = "gnm"))
    Glatt <- replicate(iter, make_ring_with_degree(nv, target_degree))

    if (local_clust) {
        Cobs <- mean(igraph::transitivity(g, type = "local", isolates = "zero"))
        Crand <- mean(vapply(Grand, igraph::transitivity, numeric(nv), type = "local", isolates = "zero"))
        Clatt <- mean(vapply(Glatt, igraph::transitivity, numeric(nv), type = "local", isolates = "zero"))
    } else {
        Cobs <- igraph::transitivity(g, type = "global")
        Crand <- mean(vapply(Grand, igraph::transitivity, numeric(1), type = "global"))
        Clatt <- mean(vapply(Glatt, igraph::transitivity, numeric(1), type = "global"))
    }

    Lobs <- igraph::mean_distance(g)
    Lrand <- mean(vapply(Grand, igraph::mean_distance, numeric(1)))
    Llatt <- mean(vapply(Glatt, igraph::mean_distance, numeric(1)))

    if (length(methods) == 1 && methods == "propensity") {
       return(sw_propensity(Cobs, Lobs, Crand, Lrand, Clatt, Llatt, truncate_delta))
    } else {
        vapply(methods, function(x) {
            switch(x,
                   index = sw_index(Cobs, Lobs, Crand, Lrand),
                   propensity = sw_propensity(Cobs, Lobs, Crand, Lrand, Clatt, Llatt, truncate_delta)[1],
                   telesford = sw_telesford(Cobs, Lobs, Crand, Lrand, Clatt, Llatt)
            )
        }, FUN.VALUE = numeric(1), USE.NAMES = TRUE)
    }
}

make_ring_with_degree <- function(nv, target_degree) {
    init_degree <- ceiling(target_degree) + (ceiling(target_degree) %% 2)
    g <- igraph::make_lattice(c(nv), nei = init_degree / 2, circular = TRUE)
    # Same as Thomas Hills' implementation, I think:
    # g <- sample_smallworld(1, nv, init_degree / 2, p = 0)
    ne <- igraph::ecount(g)
    x <- floor(((init_degree * nv) - (target_degree * nv)) / 2)
    z <- sample(ne, size = x, replace = FALSE)
    return(igraph::delete_edges(g, z))
}

sw_index <- function(Cobs, Lobs, Crand, Lrand) {
    return((Cobs / Crand) * (Lrand / Lobs))
}

sw_propensity <- function(Cobs, Lobs, Crand, Lrand, Clatt, Llatt, truncate_delta = TRUE) {
    deltaC_orig <- (Clatt - Cobs) / (Clatt - Crand)
    deltaL_orig <- (Lobs - Lrand) / (Llatt - Lrand)
    if (truncate_delta) {
        deltaC <- pmin(pmax(deltaC_orig, 0), 1)
        deltaL <- pmin(pmax(deltaL_orig, 0), 1)
    } else {
        deltaC <- deltaC_orig
        deltaL <- deltaL_orig
    }
    swp <- 1 - sqrt((deltaC^2 + deltaL^2) / 2)
    return(c("swp" = swp, "deltaC" = deltaC_orig, "deltaL" = deltaL_orig))
}

sw_telesford <- function(Cobs, Lobs, Crand, Lrand, Clatt, Llatt) {
    return((Lrand / Lobs) - (Cobs / Clatt))
}
