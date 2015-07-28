#
#  s3-edgeList.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 7/27/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# edgeList S3 Class for ccdr package
#------------------------------------------------------------------------------#

#
# edgeList S3 class skeleton
#
# Data
# * list edges           // edge list as a list
#
# Methods
#

#' edgeList class
#'
#' Convenience wrapper class for a (column-major) edge list. Each component of the list
#' corresponds to a node, and each component is an integer vector whose components are the parents
#' of this node in the graph. Only intended for internal use.
#'
#' @section Slots:
#' \describe{
#' \item{\code{edges}}{Edge list representation of a graph sorted by children.}
#' }
#'
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}}
#'
#' @docType S3class
#' @name edgeList-class
NULL

#' @export
is.edgeList <- function(edgeL){
    inherits(edgeL, "edgeList")
} # END IS.EDGELIST

#' @export
edgeList.list <- function(li){
    ### Minimal consistency checks for this class
    if(!is.list(li)){
        stop("Input must be a list!")
    }

    structure(li, class = "edgeList")
} # END EDGELIST.LIST

#' @export
print.edgeList <- function(edgeL){
    ### Assumes the DAG has at most 1000 nodes: Output will be cramped and illegible if the graph is larger than this
    ### We shouldn't be printing this when pp > 1000 anyway!
    edgeL.out <- mapply(function(x, y){
        prefix <- paste0("[", x, "]")
        prefix <- sprintf("%-5s", prefix)
        paste0(prefix, paste(sprintf("%4d", sort(y)), collapse = ""))
    }, 1L:length(edgeL), edgeL)
    edgeL.out <- unlist(edgeL.out)
    edgeL.out <- paste(edgeL.out, collapse = " \n")

    cat(edgeL.out, "\n")
}

#' @export
#' @describeIn get.adjacency.matrix Convert internal \code{edgeList} representation to an adjacency matrix
get.adjacency.matrix.edgeList <- function(edgeL){
    numnode <- length(edgeL)
    Matrix.out <- Matrix::Matrix(0, nrow = numnode, ncol = numnode)

    ### This loop is pretty slow!
    for(j in 1:numnode){
        for(i in edgeL[[j]]){
            Matrix.out[i, j] <- 1
        }
    }

    Matrix.out
} # END GET.ADJACENCY.MATRIX.EDGELIST

#' @export
num.nodes.edgeList <- function(edgeL){
    length(edgeL)
} # END NUM.NODES.EDGELIST

#' @export
num.edges.edgeList <- function(edgeL){
    sum(unlist(lapply(edgeL, length)))
} # END NUM.EDGES.EDGELIST

#' @export
is.zero.edgeList <- function(edgeL){
    (num.edges(edgeL) == 0)
} # END IS.ZERO.EDGELIST
