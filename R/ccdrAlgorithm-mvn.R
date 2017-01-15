#' @export
generate_mvn_data <- function(coefs, vars, n = 1){
    stopifnot(nrow(coefs) == ncol(coefs))

    if(is.null(colnames(coefs))){
        stop("Input 'coefs' requires node names!")
    } else{
        stopifnot(rownames(coefs) == colnames(coefs))
    }

    # if(is_matrix_type(vars)){
        if(is.null(colnames(vars))){
            stop("Input 'vars' requires node names!")
        } else{
            stopifnot(colnames(coefs) == colnames(vars))
            stopifnot(rownames(coefs) == rownames(vars))
        }

        vars <- Matrix::diag(vars)
        names(vars) <- colnames(coefs)
    # }
    stopifnot(length(vars) == ncol(coefs))

    if(is.null(names(vars))){ # vars is now a vector
        stop("Input 'vars' requires node names!")
    }

    ### Need this to ensure the output has the same order as the input
    ###  after things get shuffled around
    original_node_order <- colnames(coefs)

    topsort <- names(igraph::topo_sort(igraph::graph.adjacency(coefs)))
    edgelist <- matrix_to_edgeWeightList(coefs)
    nodes <- names(edgelist) # this will be sorted according to the topological order

    x <- replicate(n, generate_mvn_vector(edgelist, nodes, topsort, vars))

    ### Permute columns back to original ordering
    x <- t(x)[, original_node_order]
    x
}

#' @export
generate_mvn_vector <- function(edgelist, nodes, topsort, vars = NULL){
    normal_seed <- sapply(vars, function(x) rnorm(n = 1, mean = 0, sd = sqrt(x)))
    gen_dag_vector_R(edgelist, nodes, topsort, seed = normal_seed)
}

gen_dag_vector_R <- function(edgelist, nodes, topsort, seed){
    nnode <- length(edgelist)
    x <- numeric(nnode)
    names(x) <- nodes

    for(j in seq_along(topsort)){
            child <- topsort[j]
            parents <- edgelist[[child]]$parents
            weights <- edgelist[[child]]$weights
            nparents <- length(parents)
            if(nparents > 0){
                ### Iterate over parents and add associated effects
                for(i in seq_along(parents)){
                    this.par <- parents[i]
                    x[child] <- x[child] + weights[i] * x[this.par]
                    # x[child] <- x[child] + weights[i] * x[index[i]] # equivalent to above line
                }
            }

            ### Add noise: This is a crucial step. If nothing is added here, the
            ###            output will be all zeroes since the root node(s) will
            ###            have x[child] = 0 at this point.
            ###
            ### Gaussian model: This is random error ~ N(0, vars[j])
            ### Logistic model: This a (deterministic) bias term
            x[child] <- x[child] + seed[child]
    }

    x
}

matrix_to_edgeWeightList <- function(x){
    sp <- sparsebnUtils::as.sparse(x) # NOTE: no longer a bottleneck under sparsebnUtils v0.0.4

    nodes <- colnames(x)
    stopifnot(sp$dim[1] == sp$dim[2])

    out <- lapply(vector("list", length = sp$dim[1]), function(z) list(parents = character(0), index = integer(0), weights = numeric(0)))
    names(out) <- nodes
    for(j in seq_along(sp$cols)){
        child <- sp$cols[[j]]
        parent <- sp$rows[[j]]
        weight <- sp$vals[[j]]
        parents <- c(out[[child]]$parents, nodes[parent]) # !!! THIS IS SLOW
        index <- c(out[[child]]$index, parent) # !!! THIS IS SLOW
        weights <- c(out[[child]]$weights, weight) # !!! THIS IS SLOW
        out[[nodes[child]]] <- list(parents = parents, index = index, weights = weights)
    }

    out
}
