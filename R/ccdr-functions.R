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
#' @export
generate.lambdas <- function(nn,
                             lambdas.ratio = 0.001,
                             lambdas.length = 50,
                             scale = "linear"
){
    .gen_lambdas(nn = nn,
                 lambdas.ratio = lambdas.ratio,
                 lambdas.length = lambdas.length,
                 scale = scale)
} # END GENERATE.LAMBDAS

# .gen_lambdas
#  Internal implementation of generate.lambdas
.gen_lambdas <- function(nn,
                         lambdas.ratio = 0.001,
                         lambdas.length = 50,
                         scale = "linear"
){
    max.lam <- max(sqrt(nn))
    min.lam <- lambdas.ratio * max.lam

    if(scale == "linear"){
        lambdas <- seq(max.lam, min.lam, length.out = lambdas.length)
    } else if(scale == "log"){
        lambdas <- exp(seq(log(max.lam), log(min.lam), log(lambdas.ratio)/(lambdas.length-1)))
    } else{
        stop("Invalid input for scale argument! Must be either 'log' or 'linear'.")
    }

    lambdas
} # END .GEN_LAMBDAS
