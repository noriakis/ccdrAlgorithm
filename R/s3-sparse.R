#
#  s3-sparse.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# sparse S3 Class for R
#------------------------------------------------------------------------------#

#
# An alternative data structure for storing sparse matrices in R using the (row, column, value)
#   format. Internally it is stored as a list with three components, each vectors, that contain
#   the rows / columns / values of the nonzero elements.
#
# Its main purpose is to serve as an intermediary between the standard R dense matrix class and the
#   internal SparseBlockMatrixR class. That is, to convert from matrix to SBM, we do
#
#       matrix -->> sparse -->> SparseBlockMatrixR
#
# In theory, this class can be used externally as a useful data structure for storing sparse matrices
#   as an alternative to the Matrix class provided by the Matrix package. Currently, however, the class
#   structure is fairly limited, so there isn't much a reason to do this.
#
#

is.sparse <- function(sp){
    inherits(sp, "sparse")
} # END IS.SPARSE

# Re-indexing TO C for sparse objects
reIndexC.sparse <- function(sp){
    if(sp$start == 0){
        warning("This object already uses C-style indexing!")
        return(sp)
    }

    sp$rows <- sp$rows - 1
    sp$cols <- sp$cols - 1
    sp$start <- 0

    sp
} # END REINDEXC.SPARSE

# Re-indexing TO R for sparse objects
reIndexR.sparse <- function(sp){
    if(sp$start == 1){
        warning("This object already uses R-style indexing!")
        return(sp)
    }

    sp$rows <- sp$rows + 1
    sp$cols <- sp$cols + 1
    sp$start <- 1

    sp
} # END REINDEXR.SPARSE

# List constructor
sparse.list <- function(li){

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 5 || names(li) != c("rows", "cols", "vals", "dim", "start") || is.null(names(li))){
        stop("Input is not coercable to an object of type SparseBlockMatrixR, check list for the following (named) elements: rows, vals, blocks, sigmas, start")
    } else if( length(unique(lapply(li[1:3], length))) > 1){
        stop("rows / cols / vals elements have different sizes; should all have the same length (pp)!!")
    } else if(length(li$dim) != 2){
        stop("dim attribute must have length 2!")
    } else if(li$start != 0 && li$start != 1){
        stop("start attribute must be 0 (C-style) or 1 (R-style)!")
    }

    structure(li, class = "sparse")
} # END SPARSE.LIST

#
# Convert FROM list TO sparse
#
as.sparse.list <- function(li){
    sparse.list(li)
} # END AS.SPARSE.LIST

# Convert FROM matrix TO sparse
#
# By default, return the object using R indexing. If desired, the method can return C-style indexing by setting
#   index = "C".
as.sparse.matrix <- function(m, index = "R"){

    if( nrow(m) != ncol(m)) stop("Input matrix must be square!")

    if(index != "R" && index != "C") stop("Invalid entry for index parameter: Must be either 'R' or 'C'!")

    pp <- nrow(m)

    nnz <- which(abs(m) > .MACHINE_EPS) - 1
    vals <- double(length(nnz))
    rows <- integer(length(nnz))
    cols <- integer(length(nnz))
    for(k in seq_along(nnz)){
        col <- trunc(nnz[k] / pp)
        row <- nnz[k] - (pp * col)
        vals[k] <- as.vector(m)[nnz[k] + 1]
        rows[k] <- row
        cols[k] <- col
    }

    sp <- sparse.list(list(rows = rows, cols = cols, vals = vals, dim = c(pp, pp), start = 0))

    if(index == "R"){
        reIndexR(sp)
    } else{
        sp
    }
} # END AS.SPARSE.MATRIX

# Convert FROM SparseBlockMatrixR TO sparse
#
# By default, return the object using R indexing. If desired, the method can return C-style indexing by setting
#   index = "C".
as.sparse.SparseBlockMatrixR <- function(sbm, index = "R"){

    if(index != "R" && index != "C") stop("Invalid entry for index parameter: Must be either 'R' or 'C'!")

    pp <- length(sbm$rows)

    sp.rows <- integer(0)
    sp.cols <- integer(0)
    sp.vals <- numeric(0)

    sp.idx <- 0
    for(j in 1:pp){
        these.rows <- sbm$rows[[j]]
        these.vals <- sbm$vals[[j]]
        for(k in seq_along(these.rows)){

            # Only include nonzero values
            if(these.vals[k] != 0){
                sp.idx <- sp.idx + 1

                sp.rows <- c(sp.rows, these.rows[k])
                sp.cols <- c(sp.cols, j)
                sp.vals <- c(sp.vals, these.vals[k])
            }
        }
    }

    sp <- sparse.list(list(rows = sp.rows, cols = sp.cols, vals = sp.vals, dim = c(pp, pp), start = 1))

    if(index == "R"){
        sp
    } else{
        sp$start <- 0
        reIndexC(sp)
    }
} # END AS.SPARSE.SPARSEBLOCKMATRIXR

# Convert FROM sparse TO matrix
as.matrix.sparse <- function(sp){

    if( !is.sparse(sp)){
        stop("Input must be a sparse object!")
    }

    if(sp$start == 0) sp <- reIndexR(sp) # if indexing starts at 0, adjust to start 1 instead

    m.dim <- sp$dim
    m <- matrix(0, nrow = m.dim[1], ncol = m.dim[2])

    for(k in seq_along(sp$vals)){
        m[sp$rows[k], sp$cols[k]] <- sp$vals[k]
    }

    attributes(m)$dim <- sp$dim
    attributes(m)$dimnames <- list()
    attributes(m)$dimnames[[1]] <- as.character(1:nrow(m))
    attributes(m)$dimnames[[2]] <- as.character(1:ncol(m))

    m
} # END AS.MATRIX.SPARSE

# Convert FROM sparse TO list
as.list.sparse <- function(sp){

    list(rows = sp$rows, cols = sp$cols, vals = sp$cols, dim = sp$dim, start = sp$start)
} # END AS.LIST.SPARSE

#
# Print function for sparse objects
#
# By default, format the output as a three-column matrix [cols | rows | vals] ordered by increasing columns.
#   Optionally, set pretty = FALSE to print the sparse object as a list.
print.sparse <- function(sp, pretty = TRUE){
    if(pretty){
        out <- cbind(sp$cols, sp$rows, sp$vals)
        colnames(out) <- c("cols", "rows", "vals")
        print(out)
    } else{
        print(as.list(sp))
    }

} # END PRINT.SPARSE
