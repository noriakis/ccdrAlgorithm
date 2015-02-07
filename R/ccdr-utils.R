#
#  ccdr-utils.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 5/28/14.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#
#     .check_if_matrix
#     .check_if_graph
#     .cor_vector
#     .generate_key

# Special function to check if an object is EITHER matrix or Matrix object
.check_if_matrix <- function(m){
    is.matrix(m) || inherits(m, "Matrix")
}

# Convenience wrapper for checking if an object is a graphNEL object
# .check_if_graph <- function(g){
#     inherits(g, "graphNEL")
# }

.cor_vector <- function(X){
    if( !is.data.frame(X) && !is.matrix(X)){
        stop("Input must either be a data.frame or a matrix!")
    } else if( any(dim(X) < 2)){
        stop("Input must have at least 2 rows and columns!")
    }

    cors <- cor(X)
    cors <- cors[upper.tri(cors, diag = TRUE)]

    cors
}

# .generate_key <- function(len = 15){
#     chars <- c(1:10, LETTERS)
#     key <- paste(sample(chars, len), sep="", collapse="")
#
#     key
# }
