#
#  ccdrAlgorithm-bwlist.R
#  ccdrAlgorithm
#
#  Created by Bryon Aragam (local) on 8/11/17.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#
# PACKAGE CCDRALGORITHM: Helper methods for black/white lists
#
#   CONTENTS:
#     names_to_indices
#     rows_to_list
#     bwlist_check
#     bwlist_to_weights
#

### Just a wrapper for match with a better name
names_to_indices <- function(v, names){
    match(v, names)
} # END NAMES_TO_INDICES

rows_to_list <- function(m){
    lapply(1:nrow(m), function(j) m[j,])
} # END ROWS_TO_LIST

bwlist_check <- function(bwlist, names){
    if(!is.matrix(bwlist) || ncol(bwlist) != 2){
        stop("Input must be a matrix with exactly 2 columns!")
    }

    if(any(is.na(bwlist))){
        stop("Input cannot have missing values!")
    }

    if(is.character(bwlist)){
        bwlist <- as.vector(bwlist)
        bwlist <- names_to_indices(bwlist, names)
        bwlist <- matrix(bwlist, ncol = 2)
    }

    storage.mode(bwlist) <- "integer"
    rows_to_list(bwlist)
} # END BWLIST_CHECK

bwlist_to_weights <- function(black, white, nnode){
    weights <- matrix(1L, ncol = nnode, nrow = nnode)

    if(!is.null(white)){
        for(k in 1:length(white)){
            weights[white[[k]][1], white[[k]][2]] <- 0L
        }
    }

    if(!is.null(black)){
        for(k in 1:length(black)){
            weights[black[[k]][1], black[[k]][2]] <- -1L
        }
    }

    weights
} # END BWLIST_TO_WEIGHTS
