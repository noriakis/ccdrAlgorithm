#
#  s3-ccdrFit.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# ccdrFit S3 Class for R
#------------------------------------------------------------------------------#

#
# ccdrFit S3 class skeleton
#
# Data
# * SparseBlockMatrixR sbm  // adjacency matrix as SparseBlockMatrix
# * numeric lambda          // regularization parameter
# * integer nedge           // number of edges
# * integer pp              // number of nodes
# * integer nn              // number of observations
# * numeric time            // time to run CCDr algorithm
#
# Methods
# * as.graphCompare
# * get.adjacency.matrix
# * get.sigmas
#

#' ccdrFit class
#'
#' Convenience wrapper class for output of CCDr algorithm: Represents a single DAG estimate in the solution path.
#'
#' @section Slots:
#' \describe{
#' \item{\code{sbm}}{Should eventually be a graph object (probably).}
#' \item{\code{lambda}}{(numeric) Value of lambda for this estimate.}
#' \item{\code{nedge}}{(integer) Number of edges in this estimate.}
#' \item{\code{pp}}{(integer) Number of nodes.}
#' \item{\code{nn}}{(integer) Number of observations this estimate was based on.}
#' \item{\code{time}}{(numeric) Time in seconds to generate this estimate.}
#' }
#'
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}}, \code{\link{get.sigmas}}
#'
#' @docType S3class
#' @name ccdrFit-class
NULL

#' @export
is.ccdrFit <- function(cf){
    inherits(cf, "ccdrFit")
} # END IS.CCDRFIT

# ccdrFit constructor
ccdrFit.list <- function(li){

    #
    # Need to be careful when using this constructor directly since it allows the nedge
    #  component to be different from the actual number of edges stored in the SBM object.
    #  This is allowed for efficiency reasons while running the main algorithm.
    #
    # UPDATE: An explicit check has been added for now.
    #

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 6 || !setequal(names(li), c("sbm", "lambda", "nedge", "pp", "nn", "time"))){
        stop("Input is not coercable to an object of type ccdrFit, check list for the following elements: sbm (SparseBlockMatrixR), lambda (numeric), nedge (integer), pp (integer), nn (integer), time (numeric or NA)")
    } else if( !is.SparseBlockMatrixR(li$sbm)){
        stop("'sbm' component must be a valid SparseBlockMatrixR object!")
    } else if(.num_edges(li$sbm) != li$nedge){
        stop("Attempting to set nedge to an improper value: Must be equal to the number of nonzero values in sbm.")
    }

    structure(li, class = "ccdrFit")
} # END CCDRFIT.LIST

#' @export
as.list.ccdrFit <- function(cf){
    list(sbm = cf$sbm, lambda = cf$lambda, nedge = cf$nedge, pp = cf$pp, nn = cf$nn, time = cf$time)
} # END AS.LIST.CCDRFIT

#' get.adjacency.matrix.ccdrFit
#'
#' Extracts the adjacency matrix from a \code{\link{ccdrFit-class}} object.
#'
#' @return
#' \code{matrix}
#'
#' @export
get.adjacency.matrix.ccdrFit <- function(cf){
    as.matrix(cf$sbm)
} # END GET.ADJACENCY.MATRIX.CCDRFIT

#' num.nodes.ccdrFit
#'
#' @export
num.nodes.ccdrFit <- function(cf){
    cf$pp
} # END NUM.NODES.CCDRFIT

#' num.edges.ccdrFit
#'
#' @export
num.edges.ccdrFit <- function(cf){
    cf$nedge
} # END NUM.EDGES.CCDRFIT

#' num.samples.ccdrFit
#'
#' @export
num.samples.ccdrFit <- function(cf){
    cf$nn
} # END NUM.SAMPLES.CCDRFIT

#' @export
print.ccdrFit <- function(cf){
    MAX_NODES <- 20

    cat("CCDr estimate\n",
        cf$pp, " nodes with ", cf$nedge, " edges\n",
        cf$nn, " observations\n",
        "lambda = ", cf$lambda, "\n",
        sep = "")

    cat("\nPhi: \n")
    if(cf$pp < MAX_NODES) {
        print(get.adjacency.matrix(cf))
    } else{
        cat("<Adjacency matrix has more than ", MAX_NODES, " nodes: suppressing output>\n", sep = "")
    }
} # END PRINT.CCDRFIT

#------------------------------------------------------------------------------#
# to_B.ccdrFit
# Internal function to convert estimates from the (Rho, R) parametrization to
#  the standard (B, Omega) parametrization.
#
to_B.ccdrFit <- function(cf){
    cf$sbm <- to_B(cf$sbm)

    cf
}
