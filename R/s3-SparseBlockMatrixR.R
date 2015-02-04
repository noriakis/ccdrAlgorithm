#
#  s3-SparseBlockMatrixR.R
#  ccdrpkg
#
#  Created by Bryon Aragam (local) on 4/24/14.
#  Copyright (c) 2014 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# SparseBlockMatrixR S3 Class for R
#------------------------------------------------------------------------------#

#
# A convenience class to make easier sharing data between R and C++ easier. This class mimics the structure
#   of the C++ class 'SparseBlockMatrix' (note the name difference to differentiate the two) as a list in R,
#   which makes it easy to use Rcpp to pass a sparse structure between R and C++. This class is NOT intended
#   to be a general purpose sparse data structure for R; instead, it simply streamlines the connection between
#   R and C++.
#
# A SparseBlockMatrixR object consists of four main components:
#   1) rows - a list of integer vectors
#   2) vals - a list of numeric vectors
#   3) blocks - a list of integer vectors
#   4) sigmas - a numeric vector
#
# There is also a fifth component which identifies whether the indexing in the object begins at 0 or 1.
#   This is needed for bookkeeping and ensuring coherent translation between R and C++.
#   5) start - 0 or 1
#
# These components all exactly reflect their purpose in SparseBlockMatrix.h; see that file for more details.
#
#   NOTES:
#       1) Since C++ begins indexing at 0, we have included two functions for re-indexing between the two conventions,
#          defined in reIndexR and reIndexC. The 'start' flag ensures we keep track of where indexing begins.
#
#

# Delegation
SparseBlockMatrixR <- function(x) UseMethod("SparseBlockMatrixR", x)
as.SparseBlockMatrixR <- function(x) UseMethod("as.SparseBlockMatrixR", x)

is.SparseBlockMatrixR <- function(sbm){
    inherits(sbm, "SparseBlockMatrixR")
}

# Re-indexing TO C for SparseBlockMatrixR objects
reIndexC.SparseBlockMatrixR <- function(sbm){
    #
    # Using lapply does NOT work here: if one of the list elements is an empty vector, adding 1 will
    #  mysteriously coerce it to a numeric vector (should be integer!). Not sure why this happens, but
    #  we shouldn't allow R to secretly change our data types without our permission. Use a for loop
    #  instead.
    #
    # UPDATE 05/13/14: Using '1L' keeps everything as an integer. This makes sense since '1' is a numeric
    #                   literal in R, while the corresponding literal for an integer is '1L'.
    #
    sbm$rows <- lapply(sbm$rows, function(x){ x - 1L})
    if(length(sbm$blocks) > 0) sbm$blocks <- lapply(sbm$blocks, function(x){ x - 1L})

# OLD CODE
#     if(length(sbm$blocks) > 0){
#         for(j in 1:length(sbm$rows)){
#             if(length(sbm$rows[[j]]) > 0) sbm$rows[[j]] <- sbm$rows[[j]] - 1
#             if(length(sbm$blocks[[j]]) > 0) sbm$blocks[[j]] <- sbm$blocks[[j]] - 1
#         }
#     } else{
#         for(j in 1:length(sbm$rows)){
#             if(length(sbm$rows[[j]]) > 0) sbm$rows[[j]] <- sbm$rows[[j]] - 1
#         }
#     }

    sbm$start <- 0

    sbm
}

# Re-indexing TO R for SparseBlockMatrixR objects
reIndexR.SparseBlockMatrixR <- function(sbm){
    #
    # Using lapply does NOT work here: if one of the list elements is an empty vector, adding 1 will
    #  mysteriously coerce it to a numeric vector (should be integer!). Not sure why this happens, but
    #  we shouldn't allow R to secretly change our data types without our permission. Use a for loop
    #  instead.
    #
    # UPDATE 05/13/14: Using '1L' keeps everything as an integer. This makes sense since '1' is a numeric
    #                   literal in R, while the corresponding literal for an integer is '1L'.
    #
    sbm$rows <- lapply(sbm$rows, function(x){ x + 1L})
    if(length(sbm$blocks) > 0) sbm$blocks <- lapply(sbm$blocks, function(x){ x + 1L})

# OLD CODE
#     if(length(sbm$blocks) > 0){
#         for(j in 1:length(sbm$rows)){
#             if(length(sbm$rows[[j]]) > 0) sbm$rows[[j]] <- sbm$rows[[j]] + 1
#             if(length(sbm$blocks[[j]]) > 0) sbm$blocks[[j]] <- sbm$blocks[[j]] + 1
#         }
#     } else{
#         for(j in 1:length(sbm$rows)){
#             if(length(sbm$rows[[j]]) > 0) sbm$rows[[j]] <- sbm$rows[[j]] + 1
#         }
#     }
#
    sbm$start <- 1

    sbm
}

# List constructor
SparseBlockMatrixR.list <- function(li){

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 5 || !setequal(names(li), c("rows", "vals", "blocks", "sigmas", "start"))){
        stop("Input is not coercable to an object of type SparseBlockMatrixR, check list for the following elements: rows (list), vals (list), blocks (list), sigmas (numeric), start (integer)")
    } else if( length(li$rows) != length(li$vals)){
        #
        # We enforce that rows and vals must have the same length, but relax this assumption for blocks and sigmas
        #  since they are mostly internal to the CCDr algorithm, and once the algorithm has run we may want to free
        #  up the memory associated with these elements
        #
        stop("rows and vals have different sizes; should all have the same length (pp)!!")
    }

    structure(li, class = "SparseBlockMatrixR")
}

# sparse object constructor
SparseBlockMatrixR.sparse <- function(sp){

    if( !is.sparse(sp)){
        stop("Input must be a sparse object!")
    } else if(sp$dim[1] != sp$dim[2]){
        stop("Input must be square!")
    }

    pp <- sp$dim[1]
    if(sp$start == 0) sp <- reIndexR(sp) # re-index rows and cols to start at 1 if necessary

    sbm.rows <- vector("list", length = pp)
    sbm.vals <- vector("list", length = pp)
    sbm.blocks <- vector("list", length = pp)
    sbm.sigmas <- rep(0, pp)

    # how to vectorize this???
    for(j in 1:pp){
        # Clear out the jth entry in the lists to be an empty vector
        sbm.rows[[j]] <- integer(0)
        sbm.vals[[j]] <- numeric(0)
        sbm.blocks[[j]] <- integer(0)
    }

    for(j in 1:pp){

        thisColIdx <- which(sp$cols == j)
        rows <- as.integer(sp$rows[thisColIdx])
        for(k in seq_along(rows)){
            row <- rows[k]

            sbm.rows[[j]] <- c(sbm.rows[[j]], row)
            sbm.rows[[row]] <- c(sbm.rows[[row]], j)

            sbm.vals[[j]] <- c(sbm.vals[[j]], sp$vals[thisColIdx[k]])
            sbm.vals[[row]] <- c(sbm.vals[[row]], 0)

            # vals[rows[j][k]][block[j][k]] = beta_ji
            sbm.blocks[[j]] <- c(sbm.blocks[[j]], length(sbm.rows[[row]]))
            sbm.blocks[[row]] <- c(sbm.blocks[[row]], length(sbm.rows[[j]]))
        }

    }

    names(sbm.rows) <- names(sbm.vals) <- names(sbm.blocks) <- as.character(1:pp)

    #
    # NOTE: We use R-indexing by default. This can be changed by using reIndexC if necessary.
    #
    SparseBlockMatrixR.list(list(rows = sbm.rows, vals = sbm.vals, blocks = sbm.blocks, sigmas = sbm.sigmas, start = 1))
}

# matrix constructor
SparseBlockMatrixR.matrix <- function(m){

    if(nrow(m) != ncol(m)) stop("Input matrix must be square!")

    sp <- as.sparse(m)

    SparseBlockMatrixR.sparse(sp)
}

# Convert FROM list TO SparseBlockMatrixR
as.SparseBlockMatrixR.list <- function(li){
    SparseBlockMatrixR.list(li)
}

# Convert FROM sparse TO SparseBlockMatrixR
as.SparseBlockMatrixR.sparse <- function(sp){
    SparseBlockMatrixR.sparse(sp)
}

# Convert FROM matrix TO SparseBlockMatrixR
as.SparseBlockMatrixR.matrix <- function(m){
    SparseBlockMatrixR.matrix(m)
}

# Convert FROM SparseBlockMatrixR TO list
#
#  Even though internally the SBM object is a list, we must still manually define this function
#
as.list.SparseBlockMatrixR <- function(sbm){
    list(rows = sbm$rows, vals = sbm$vals, blocks = sbm$blocks, sigmas = sbm$sigmas, start = sbm$start)
}

# Convert FROM SparseBlockMatrixR TO matrix
as.matrix.SparseBlockMatrixR <- function(sbm){
    pp <- length(sbm$rows)
    m <- diag(rep(0, pp))

    if(sbm$start == 0) sbm <- reIndexR(sbm)

    for(j in 1:pp){
        m[sbm$rows[[j]], j] <- sbm$vals[[j]]
    }

    attributes(m)$dim <- c(pp, pp)
    attributes(m)$dimnames <- list()
    attributes(m)$dimnames[[1]] <- as.character(1:nrow(m))
    attributes(m)$dimnames[[2]] <- as.character(1:ncol(m))

    m
}

#
# Print function for SparseBlockMatrixR
#
#  By default, coerce the object to a Matrix object (from the Matrix package) and print it using the default
#    print.Matrix method. Optionally, set pretty = FALSE to print the SBM object as a list.
#
print.SparseBlockMatrixR <- function(sbm, pretty = TRUE){
    if(pretty){
        print(Matrix::Matrix(as.matrix(sbm)))
    } else{
        print(as.list(sbm))
    }

}
