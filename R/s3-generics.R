#
#  s3-generics.R
#  ccdrAlgorithm
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE CCDRALGORITHM: Generics
#
#   CONTENTS:
#

# Generics for SparseBlockMatrixR ------------------------------------------------------------
SparseBlockMatrixR <- function(x) UseMethod("SparseBlockMatrixR", x)
as.SparseBlockMatrixR <- function(x) UseMethod("as.SparseBlockMatrixR", x)
to_B <- function(x) UseMethod("to_B", x)

# Taken from sparsebnUtils -------------------------------------------------------------------
# num.nodes <- function(x) UseMethod("num.nodes", x)
# num.edges <- function(x) UseMethod("num.edges", x)
# is.zero <- function(x) UseMethod("is.zero", x)

# Imported from sparsebnUtils ----------------------------------------------------------------
#' @importFrom sparsebnUtils reIndexC
#' @export
sparsebnUtils::reIndexC

#' @importFrom sparsebnUtils reIndexR
#' @export
sparsebnUtils::reIndexR

#' @importFrom sparsebnUtils get.adjacency.matrix
#' @export
sparsebnUtils::get.adjacency.matrix

#' @importFrom sparsebnUtils num.nodes
#' @export
sparsebnUtils::num.nodes

#' @importFrom sparsebnUtils num.edges
#' @export
sparsebnUtils::num.edges

#' @importFrom sparsebnUtils is.zero
#' @export
sparsebnUtils::is.zero
