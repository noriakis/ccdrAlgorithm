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

#' @export
is.ccdrFit <- function(cf){
    inherits(cf, "ccdrFit")
} # END IS.CCDRFIT

# Constructor
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

#' @export
get.adjacency.matrix.ccdrFit <- function(cf){
    as.matrix(cf$sbm)
} # END GET.ADJACENCY.MATRIX.CCDRFIT

#' @export
get.sigmas.ccdrFit <- function(cf){
    cf$sbm$sigmas
} # END GET.SIGMAS.CCDRFIT

# Operates on a list of ccdrFit objects
#' @export
get.times <- function(li){
    # check.class <- all(unlist(lapply(li, function(x){ is.ccdrFit(x)})))
    if(!check_list_class(li, "ccdrFit")){
        stop("Some component is not of type ccdrFit -- this function only works on lists of ccdrFit objects.")
    }

    times <- unlist(lapply(li, function(x){ x$time}))
    names(times) <- NULL

    times
} # END GET.TIMES

# Also operates on a list of ccdrFit objects
#' @export
get.lambdas <- function(li){
    # check.class <- all(unlist(lapply(li, function(x){ is.ccdrFit(x)})))
    if(!check_list_class(li, "ccdrFit")){
        stop("Some component is not of type ccdrFit -- this function only works on lists of ccdrFit objects.")
    }

    times <- unlist(lapply(li, function(x){ x$lambda}))
    names(times) <- NULL

    times
} # END GET.LAMBDAS

#' @export
print.ccdrFit <- function(cf){
    cat("CCDr estimate\n",
        cf$pp, " nodes with ", cf$nedge, " edges\n",
        cf$nn, " observations\n",
        "lambda = ", cf$lambda, "\n",
        sep = "")

    cat("\nPhi: \n")
    print(as.matrix(cf$sbm))

    cat("\nRho: \n")
    print(cf$sbm$sigmas)
} # END PRINT.CCDRFIT
