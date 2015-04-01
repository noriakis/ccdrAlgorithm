#
#  ccdr-functions.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Functions
#
#   CONTENTS:
#     gen.lambdas
#

#' generate.lambdas
#'
#' Default / usual function for creating a grid of lambdas
#'
#' @param lambdas.ratio Ratio between the maximum lambda value and the minimum lambda value in the solution
#'                      path. Note that by default, the maximum value is \code{sqrt(nn)}.

#' @export
generate.lambdas <- function(lambda.max,
                             lambdas.ratio = 1e-3,
                             lambdas.length = 50,
                             scale = "linear"
){
    lambda.min <- lambdas.ratio * lambda.max
    .gen_lambdas(lambda.max = lambda.max,
                 lambda.min = lambda.min,
                 lambdas.length = lambdas.length,
                 scale = scale)
} # END GENERATE.LAMBDAS

# .gen_lambdas
#  Internal implementation of generate.lambdas
.gen_lambdas <- function(lambda.max,
                         lambda.min,
                         lambdas.length = 50,
                         scale = "linear"
){
#     lambda.max <- max(sqrt(nn))
#     lambda.min <- lambdas.ratio * lambda.max

    if(scale == "linear"){
        lambdas <- seq(lambda.max, lambda.min, length.out = lambdas.length)
    } else if(scale == "log"){
        lambdas.ratio <- lambda.min / lambda.max
        lambdas <- exp(seq(log(lambda.max), log(lambda.min), log(lambdas.ratio)/(lambdas.length-1)))
    } else{
        stop("Invalid input for scale argument! Must be either 'log' or 'linear'.")
    }

    lambdas
} # END .GEN_LAMBDAS

# .gen_lambdas <- function(nn,
#                          lambdas.ratio = 0.001,
#                          lambdas.length = 50,
#                          scale = "linear"
# ){}
