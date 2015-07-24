#
#  s3-ccdrPath.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 7/24/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# ccdrPath S3 Class for R
#------------------------------------------------------------------------------#

#
# ccdrPath S3 class skeleton
#
# Data
#
# Methods
#

#' @export
is.ccdrPath <- function(cp){
    inherits(cp, "ccdrPath")
} # END IS.CCDRPATH

# ccdrPath constructor
ccdrPath.list <- function(li){
    if(!check_list_class(li, "ccdrFit")){
        stop("Some component is not of type ccdrPath -- ccdrPath objects must consist of ccdrFit components only.")
    }

    ### Note that we still allow these objects to inherit from the base list class
    structure(li, class = c("ccdrPath", "list"))
}

#' @export
print.ccdrPath <- function(cp){
    cat("CCDr solution path\n",
        length(cp), " estimates for lambda in [", min(lambda.grid(cp)), ",", max(lambda.grid(cp)), "]\n",
        "Number of edges per solution: ", paste(num.edges(cp), collapse = "-"), "\n",
        num.nodes(cp), " nodes\n",
        num.samples(cp), " observations\n",
        sep = "")
}

#' num.nodes.ccdrPath
#'
#' @export
num.nodes.ccdrPath <- function(cp){
    unique(unlist(lapply(cp, function(x) x$pp)))
} # END NUM.NODES.CCDRPATH

#' num.edges.ccdrPath
#'
#' @export
num.edges.ccdrPath <- function(cp){
    ### unique(.) not needed since different estimates should have different # of edges
    unlist(lapply(cp, function(x) x$nedge))
} # END NUM.EDGES.CCDRPATH

#' num.samples.ccdrPath
#'
#' @export
num.samples.ccdrPath <- function(cp){
    unique(unlist(lapply(cp, function(x) x$nn)))
} # END NUM.SAMPLES.CCDRPATH

#' lambda.grid.ccdrPath
#'
#' @export
lambda.grid.ccdrPath <- function(cp){
    lambdas <- unlist(lapply(cp, function(x){ x$lambda}))
    names(lambdas) <- NULL

    lambdas
} # END LAMBDA.GRID.CCDRPATH

#' get.adjacency.matrix.ccdrPath
#'
#' @export
get.adjacency.matrix.ccdrPath <- function(cp){
    lapply(cp, get.adjacency.matrix)
} # END GET.ADJACENCY.MATRIX.CCDRPATH

#------------------------------------------------------------------------------#
# DEPRECATED METHODS
#------------------------------------------------------------------------------#

# Operates on a list of ccdrPath objects
#' @export
get.times <- function(li){
    .Deprecated()

    # check.class <- all(unlist(lapply(li, function(x){ is.ccdrPath(x)})))
    if(!check_list_class(li, "ccdrFit")){
        stop("Some component is not of type ccdrFit -- this function only works on lists of ccdrFit objects.")
    }

    times <- unlist(lapply(li, function(x){ x$time}))
    names(times) <- NULL

    times
} # END GET.TIMES

# Also operates on a list of ccdrPath objects
get.lambdas <- function(li){
    .Deprecated()

    # check.class <- all(unlist(lapply(li, function(x){ is.ccdrPath(x)})))
    if(!check_list_class(li, "ccdrFit")){
        stop("Some component is not of type ccdrFit -- this function only works on lists of ccdrFit objects.")
    }

    times <- unlist(lapply(li, function(x){ x$lambda}))
    names(times) <- NULL

    times
} # END GET.LAMBDAS
