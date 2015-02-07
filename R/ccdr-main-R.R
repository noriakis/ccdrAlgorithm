#
#  ccdr-main-R.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Functions
#
#   CONTENTS:
#     ccdr.gridR
#     ccdr.singleR
#

#' ccdr.gridR
#'
#' @param X data matrix
#' @param betas OPTIONAL beta matrix / DEFAULT = zeroes
#' @param lambdas OPTIONAL lambda grid / DEFAULT = derived from data
#' @param nlam OPTIONAL number of lambdas / DEFAULT = 50
#' @param gamma OPTIONAL gamma parameter for MCP / DEFAULT = 2.0
#' @param eps OPTIONAL error threshold / DEFAULT = 1e-4
#' @param rlam OPTIONAL ratio between min.lam : max.lam / DEFAULT = 1e-4
#' @param maxIters OPTIONAL maximum number of iterations / DEFAULT = 2 * max(10, sqrt(pp))
#' @param alpha OPTIONAL stopping criterion / DEFAULT = 2.0
#' @param verbose OPTIONAL flag for printing out progress / DEFAULT = FALSE
#' @param as.mat OPTIONAL convert results to R Matrix class or not?
ccdr.gridR <- function(X,
                       betas,
                       lambdas,
                       nlam = 50,
                       gamma = 2.0,
                       eps = 1e-4,
                       rlam = 1e-4,
                       maxIters,
                       alpha = 2.0,
                       verbose = FALSE,
                       as.mat = TRUE
){
    # get the dimensions of the data matrix
    nn <- dim(X)[1] # this number corresponds to the total number of samples collected
    pp <- dim(X)[2] # this number is the total number of predictors in the model

    # by default, set the initial guess for betas to be all zeroes
    if(missing(betas)){
        betas <- matrix(0, nrow = pp, ncol = pp)
        betas <- SparseBlockMatrixR(betas)

        # If the initial matrix is the zero matrix, indexing does not matter so we don't need to use reIndexC here
        #   Still need to set start = 0, though.
        betas$start <- 0
    } else if(is.matrix(betas)){ # if the input is a matrix, convert to SBM object

        betas <- SparseBlockMatrixR(betas)
        betas <- reIndexC(betas) # use C-friendly indexing
    } else if(!is.SparseBlockMatrixR(betas)){ # otherwise check that it is an object of class SparseBlockMatrixR
        stop("Incompatible data passed for betas parameter: Should be either matrix or list in SparseBlockMatrixR format.")
    }

    # if no grid of lambdas is passed, then use the standard log-scale that starts at
    # max.lam = sqrt(nn) and descends to min.lam = rlam * max.lam
    if(missing(lambdas)){
        lambdas <- gen.lambdas(nn, rlam = rlam, nlam = nlam)
    } else{
        if(length(lambdas) != nlam){
            warning("Length of lambdas vector does not match nlam. The supplied lambdas vector will be used and nlam will be overwritten.")
        }
        nlam <- length(lambdas) # overwrite the passed value of nlam if it does not match the length of the lambdas vector
    }

    # this parameter can be set by the user, but in order to prevent the algorithm from taking too long to run
    # it is a good idea to keep the threshold used by default which is O(sprt(pp))
    if(missing(maxIters)){
        maxIters <- 2 * max(10, sqrt(pp))
    }

    t1.cor <- proc.time()[3]
    cors <- cor(X)
    cors <- cors[upper.tri(cors, diag = TRUE)]
    t2.cor <- proc.time()[3]

    ccdr.out <- list()
    for(i in 1:nlam){
        if(verbose) cat("Working on lambda = ", lambdas[i], " [", i, "/", nlam, "]\n", sep = "")

        t1.ccdr <- proc.time()[3]
        ccdr.out[[i]] <- ccdr.singleR(cors,
                                      pp, nn,
                                      betas,
                                      lambdas[i],
                                      gamma = gamma,
                                      eps = eps,
                                      rlam = rlam,
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
    #  with p <= 500, this time is negligible anyway.

    ccdr.out[1:(i-1)] # only return up to i - 1 since the last (ith) model would not have finished running anyway
} # END CCDR.GRIDR

#' ccdr.singleR
#'
#' @param c correlation vector
#' @param pp
#' @param nn
#' @param betas OPTIONAL beta matrix / DEFAULT = zeroes
#' @param gamma OPTIONAL gamma parameter for MCP / DEFAULT = 2.0
#' @param eps OPTIONAL error threshold / DEFAULT = 1e-4
#' @param rlam OPTIONAL ratio between min.lam : max.lam / DEFAULT = 1e-4
#' @param maxIters OPTIONAL maximum number of iterations / DEFAULT = 2 * max(10, sqrt(pp))
#' @param alpha OPTIONAL stopping criterion / DEFAULT = 2.0
#' @param verbose OPTIONAL flag for printing out progress / DEFAULT = FALSE
ccdr.singleR <- function(c,
                         pp, nn,
                         betas,
                         lambda,
                         gamma = 2.0,
                         eps = 1e-4,
                         rlam = 1e-4,
                         maxIters,
                         alpha = 2.0,
                         verbose = FALSE
){
    # get the dimensions of the data matrix
    #     nn <- dim(X)[1] # this number corresponds to the total number of samples collected
    #     pp <- dim(X)[2] # this number is the total number of predictors in the model

    # by default, set the initial guess for betas to be all zeroes
    if(missing(betas)){
        betas <- matrix(0, nrow = pp, ncol = pp)
        betas <- SparseBlockMatrixR(betas)

        # If the initial matrix is the zero matrix, indexing does not matter so we don't need to use reIndexC here
        #   Still need to set start = 0, though.
        betas$start <- 0
    } else if(is.matrix(betas)){ # if the input is a matrix, convert to SBM object

        betas <- SparseBlockMatrixR(betas)
        betas <- reIndexC(betas) # use C-friendly indexing
    } else if(!is.SparseBlockMatrixR(betas)){ # otherwise check that it is an object of class SparseBlockMatrixR
        stop("Incompatible data passed for betas parameter: Should be either matrix or list in SparseBlockMatrixR format.")
    }

    if(missing(lambda)){
        stop("Warning: Must specify value for lambda!")
    }

    # this parameter can be set by the user, but in order to prevent the algorithm from taking too long to run
    # it is a good idea to keep the threshold used by default which is O(sprt(pp))
    if(missing(maxIters)){
        maxIters <- 2 * max(10, sqrt(pp))
    }

    #     t1.cor <- proc.time()[3]
    #     cors <- cor(X)
    #     cors <- cors[upper.tri(cors, diag = TRUE)]
    #     t2.cor <- proc.time()[3]

    if(verbose) cat("Opening C++ connection...")
    t1.ccdr <- proc.time()[3]
    ccdr.out <- singleCCDr(c,
                           betas,
                           nn,
                           lambda,
                           c(gamma, eps, maxIters, alpha),
                           verbose = verbose)
    t2.ccdr <- proc.time()[3]
    if(verbose) cat("C++ connection closed. Total time in C++: ", t2.ccdr-t1.ccdr, "\n")

    # * SparseBlockMatrixR sbm  // adjacency matrix as SparseBlockMatrix
    # * numeric lambda          // regularization parameter
    # * integer nedge           // number of edges
    # * integer pp              // number of nodes
    # * integer nn              // number of observations

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
} # END CCDR.SINGLER
