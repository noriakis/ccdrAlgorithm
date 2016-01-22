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
to_B <- function(x) UseMethod("to_B", x)
