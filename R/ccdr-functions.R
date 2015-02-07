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

# default / usual function for creating a grid of lambdas
gen.lambdas <- function(nn,
                        rlam = 0.001,
                        nlam = 50
){
    max.lam <- max(sqrt(nn))
    min.lam <- rlam*max.lam
    lambda <- exp(seq(log(max.lam), log(min.lam), log(rlam)/(nlam-1)))

    return(lambda)
} # END GEN.LAMBDAS
