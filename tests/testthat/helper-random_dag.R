### Ensure random.dag.matrix function is available to all tests
library(pcalg)
random.dag.matrix <- function(pp, nedge){
    g <- randomDAG(n = pp, prob = 2 * nedge / (pp * (pp - 1)), lB = -5.0, uB = 5.0) # Note that the edge weights are selected at random here!
    pi <- sample(1:pp) # permutation ordering
    g <- as(g, "matrix")
    m <- g[pi, pi]
    colnames(m) <- rownames(m) <- as.character(1:pp)

    m
}
