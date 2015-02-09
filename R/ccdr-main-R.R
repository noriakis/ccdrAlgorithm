#
#  ccdr-main-R.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Main CCDr methods
#
#   CONTENTS:
#     ccdr.run
#     .ccdr_call
#     .ccdr_gridR
#     .ccdr_singleR
#

###--- These two lines are necessary to import the auto-generated Rcpp methods in RcppExports.R---###
#' @useDynLib ccdr
#' @importFrom Rcpp sourceCpp
NULL

#' ccdr.run
#'
#' Placeholder for what will eventually be the main function exported from this package
#' @export
ccdr.run <- function(data,
                     betas,
                     lambdas,
                     lambdas.length = 20,
                     gamma = 2.0,
                     error.tol = 1e-4,
                     lambda.ratio = 1e-2,
                     max.iters,
                     alpha = 10,
                     verbose = FALSE
){
    ### This is just a wrapper for the internal implementation given by .ccdr_call
    .ccdr_call(X = data,
               betas = betas,
               lambdas = lambdas,
               nlam = lambdas.length,
               gamma = gamma,
               eps = error.tol,
               rlam = lambda.ratio,
               maxIters = max.iters,
               alpha = alpha,
               verbose = verbose)
}

# .ccdr_call
#
#   Handles most of the bookkeeping for CCDr. Sets default values and prepares arguments for
#    passing to .ccdr_gridR and .ccdr_singleR. Some type-checking as well, although most of
#    this is handled internally by .ccdr_gridR and .ccdr_singleR.
#
.ccdr_call <- function(X,
                       betas,
                       lambdas,
                       nlam,
                       gamma,
                       eps,
                       rlam,
                       maxIters,
                       alpha,
                       verbose = FALSE
){
    ### Check data X
    if(!.check_if_data_matrix(X)) stop("Data must be either a data.frame or a numeric matrix!")
    if(.count_nas(X) > 0) stop(paste0(.count_nas(X), " missing values detected!"))

    ### Get the dimensions of the data matrix
    nn <- as.integer(nrow(X))
    pp <- as.integer(ncol(X))

    ### Use default values for lambda if not specified
    if(missing(lambdas)){
        if(missing(nlam)){
            stop("Both lambdas and nlam unspecified: Must specify a value for at least one of these arguments!")
        } else{
            ### Check nlam if specified
            if(!is.numeric(nlam)) stop("nlam must be numeric!")
            if(nlam <= 0) stop("nlam must be positive!")
        }

        if(missing(rlam)){
            stop("rlam must be specified if lambdas is not explicitly specified.")
        } else{
            ### Check rlam if specified
            if(!is.numeric(rlam)) stop("rlam must be numeric!")
            if(rlam < 0) stop("rlam must be >= 0!")
        }

        # If no grid of lambdas is passed, then use the standard log-scale that starts at
        #  max.lam = sqrt(nn) and descends to min.lam = rlam * max.lam
        lambdas <- gen.lambdas(nn, rlam = rlam, nlam = as.integer(nlam))
    }

    ### Check lambdas
    if(!is.numeric(lambdas)) stop("lambdas must be a numeric vector!")
    if(any(lambdas < 0)) stop("lambdas must contain only nonnegative values!")

    if(length(lambdas) != nlam){
        warning("Length of lambdas vector does not match nlam. The specified lambdas vector will be used and nlam will be overwritten.")
    }

    ### By default, set the initial guess for betas to be all zeroes
    if(missing(betas)){
        betas <- matrix(0, nrow = pp, ncol = pp)
        betas <- SparseBlockMatrixR(betas)

        # If the initial matrix is the zero matrix, indexing does not matter so we don't need to use reIndexC here
        #   Still need to set start = 0, though.
        betas$start <- 0
    } # Type-checking for betas happens in .ccdr_singleR

    # This parameter can be set by the user, but in order to prevent the algorithm from taking too long to run
    #  it is a good idea to keep the threshold used by default which is O(sqrt(pp))
    if(missing(maxIters)){
        maxIters <- 2 * max(10, sqrt(pp))
    }

    t1.cor <- proc.time()[3]
    #     cors <- cor(X)
    #     cors <- cors[upper.tri(cors, diag = TRUE)]
    cors <- .cor_vector(X)
    t2.cor <- proc.time()[3]

    .ccdr_gridR(cors,
                as.integer(pp),
                as.integer(nn),
                betas,
                as.numeric(lambdas),
                as.numeric(gamma),
                as.numeric(eps),
                as.integer(maxIters),
                as.numeric(alpha),
                verbose)
}

# .ccdr_gridR
#
#   Main subroutine for running the CCDr algorithm on a grid of lambda values.
.ccdr_gridR <- function(cors,
                        pp, nn,
                        betas,
                        lambdas,
                        gamma,
                        eps,
                        maxIters,
                        alpha,
                        verbose
){

    ### Check alpha
    if(!is.numeric(alpha)) stop("alpha must be numeric!")
    if(alpha < 0) stop("alpha must be >= 0!")

    ### nlam is now set automatically
    nlam <- length(lambdas)

    ccdr.out <- list()
    for(i in 1:nlam){
        if(verbose) cat("Working on lambda = ", lambdas[i], " [", i, "/", nlam, "]\n", sep = "")

        t1.ccdr <- proc.time()[3]
        ccdr.out[[i]] <- .ccdr_singleR(cors,
                                       pp, nn,
                                       betas,
                                       lambdas[i],
                                       gamma = gamma,
                                       eps = eps,
                                       maxIters = maxIters,
                                       alpha = alpha,
                                       verbose = verbose
        )
        t2.ccdr <- proc.time()[3]

        betas <- ccdr.out[[i]]$sbm
        betas <- reIndexC(betas) # use C-friendly indexing

        # 07/16/14: Added code below to check edge threshold via alpha parameter
        if(ccdr.out[[i]]$nedge > alpha * pp) break
    }

    # The internal code for computing the run time does not include the time to compute correlations (t2.cor - t1.cor above),
    #   but as with our implementation of the PC algorithm, we do not include it in the timing anyway. For small models
    #   with p <= 500, this time is negligible anyway.

    ccdr.out[1:(i-1)] # only return up to i - 1 since the last (ith) model would not have finished running anyway
} # END CCDR_GRIDR

# .ccdr_singleR
#
#   Internal subroutine for handling calls to singleCCDr: This is the only place where C++ is directly
#    called. Type-checking is strongly enforced here.
.ccdr_singleR <- function(cors,      # 2-8-15: renamed from 'c' to 'cors'
                          pp, nn,
                          betas,
                          lambda,
                          gamma,
                          eps,
                          maxIters,
                          alpha,     # 2-9-15: No longer necessary in .ccdr_singleR, but needed since the C++ call asks for it
                          verbose = FALSE
){

    ### Check cors
    if(!is.numeric(cors)) stop("cors must be a numeric vector!")
    if(length(cors) != pp*(pp+1)/2) stop(paste0("cors has incorrect length: Expected length = ", pp*(pp+1)/2, " input length = ", length(cors)))

    ### Check dimension parameters
    if(!is.integer(pp) || !is.integer(nn)) stop("Both pp and nn must be integers!")
    if(pp <= 0 || nn <= 0) stop("Both pp and nn must be positive!")

    ### Check betas
    if(.check_if_matrix(betas)){ # if the input is a matrix, convert to SBM object
        betas <- SparseBlockMatrixR(betas)
        betas <- reIndexC(betas) # use C-friendly indexing
    } else if(!is.SparseBlockMatrixR(betas)){ # otherwise check that it is an object of class SparseBlockMatrixR
        stop("Incompatible data passed for betas parameter: Should be either matrix or list in SparseBlockMatrixR format.")
    }

    ### Check lambda
    if(!is.numeric(lambda)) stop("lambda must be numeric!")
    if(lambda < 0) stop("lambda must be >= 0!")

    ### Check gamma
    if(!is.numeric(gamma)) stop("gamma must be numeric!")
    if(gamma < 0 && gamma != -1) stop("gamma must be >= 0 (MCP) or = -1 (Lasso)!")

    ### Check eps
    if(!is.numeric(eps)) stop("eps must be numeric!")
    if(eps <= 0){
        if(eps < 0) stop("eps must be >= 0!")
        if(eps == 0) warning("eps is set to zero: This may cause the algorithm to fail to converge, and maxIters will be used to terminate the algorithm.")
    }

    ### Check maxIters
    if(!is.integer(maxIters)) stop("maxIters must be an integer!")
    if(maxIters <= 0) stop("maxIters must be > 0!")

    ### alpha check is in .ccdr_gridR

    if(verbose) cat("Opening C++ connection...")
    t1.ccdr <- proc.time()[3]
    ccdr.out <- singleCCDr(cors,
                           betas,
                           nn,
                           lambda,
                           c(gamma, eps, maxIters, alpha),
                           verbose = verbose)
    t2.ccdr <- proc.time()[3]
    if(verbose) cat("C++ connection closed. Total time in C++: ", t2.ccdr-t1.ccdr, "\n")

    #
    # Convert output back to SBM format
    #
    ccdr.out <- list(sbm = SparseBlockMatrixR(list(rows = ccdr.out$rows, vals = ccdr.out$vals, blocks = ccdr.out$blocks, sigmas = ccdr.out$sigmas, start = 0)),
                     lambda = ccdr.out$lambda,
                     nedge = ccdr.out$length,
                     pp = pp,
                     nn = nn,
                     time = t2.ccdr - t1.ccdr)
    ccdr.out$sbm <- reIndexR(ccdr.out$sbm)

    ccdrFit(ccdr.out)
} # END .CCDR_SINGLER
