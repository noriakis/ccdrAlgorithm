#
#  s3-generics.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Generics
#
#   CONTENTS:
#

# Generics for SparseBlockMatrixR
SparseBlockMatrixR <- function(x) UseMethod("SparseBlockMatrixR", x)
as.SparseBlockMatrixR <- function(x) UseMethod("as.SparseBlockMatrixR", x)

# Generics for ccdrFit
ccdrFit <- function(x) UseMethod("ccdrFit", x)
as.ccdrFit <- function(x) UseMethod("as.ccdrFit", x)

# Generics for sparse
sparse <- function(x) UseMethod("sparse", x)
as.sparse <- function(x) UseMethod("as.sparse", x)

# Generics for various utility functions
reIndexC <- function(x) UseMethod("reIndexC", x)
reIndexR <- function(x) UseMethod("reIndexR", x)
get.adjacency.matrix <- function(x) UseMethod("get.adjacency.matrix", x)
get.sigmas <- function(x) UseMethod("get.sigmas", x)
