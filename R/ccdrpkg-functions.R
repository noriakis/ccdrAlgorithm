#
#  ccdrpkg-functions.R
#  ccdrpkg
#
#  Created by Bryon Aragam (local) on 4/8/14.
#  Copyright (c) 2014 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Functions
#
#   CONTENTS:
#     random.sparse
#     gen.lambdas
#

# UNNECESSARY TO IMPORT LIBRARIES HERE
# require("Matrix")
# require("Rgraphviz")
# require("igraph")

# Utility functions to adapt indexing of data structures between C-style (starting at 0) and R-style (starting at 1)
reIndexC <- function(x) UseMethod("reIndexC", x)
reIndexR <- function(x) UseMethod("reIndexR", x)

#
# DEPRECATED
#
# generate a random sparse matrix (returned as a matrix object)
# random.sparse <- function(dim, s, diag = TRUE){
#     if(length(dim) == 1){
#         nrow <- ncol <- dim
#     } else if(length(dim) == 2){
#         nrow <- dim[1]
#         ncol <- dim[2]
#     } else{
#         stop("dim must have either one or two components!")
#     }
#     
#     m <- matrix(0, nrow = nrow, ncol = ncol)
#     for(i in 1:s){
#         m[sample(1:nrow, 1), sample(1:ncol, 1)] <- rnorm(1)
#     }
#     
#     if(!diag) diag(m) <- rep(0, ncol)
#     
#     m
# }

# default / usual function for creating a grid of lambdas
gen.lambdas <- function(nn,
                        rlam = 0.001, 
                        nlam = 50
){
    max.lam <- max(sqrt(nn))
    min.lam <- rlam*max.lam
    lambda <- exp(seq(log(max.lam), log(min.lam), log(rlam)/(nlam-1)))
    
    return(lambda)
}