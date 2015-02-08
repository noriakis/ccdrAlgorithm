#
#  ccdr-utils.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 5/28/14.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#
#     .check_if_matrix
#     .cor_vector
#     .col_classes
#

# Special function to check if an object is EITHER matrix or Matrix object
.check_if_matrix <- function(m){
    is.matrix(m) || inherits(m, "Matrix")
}

# Convenience wrapper for checking if an object is a graphNEL object
# .check_if_graph <- function(g){
#     inherits(g, "graphNEL")
# }

.cor_vector <- function(X){
# This is now implicitly checked via .col_classes
#     if( !is.data.frame(X) && !is.matrix(X)){
#         stop("Input must either be a data.frame or a matrix!")
#     }

    check.numeric <- (.col_classes(X) != "numeric")
    if( any(check.numeric)){
        not.numeric <- which(check.numeric)
        stop(paste0("Input columns must be numeric! Columns ", paste(not.numeric, collapse = ", "), " are non-numeric."))
    }

    if( any(dim(X) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    cors <- cor(X)
    cors <- cors[upper.tri(cors, diag = TRUE)]

    cors
}

.col_classes <- function(X){
    if( !is.data.frame(X) && !is.matrix(X)){
        stop("Input must be a data.frame or a matrix!")
    }

    apply(X, 2, class)
}

# .generate_key <- function(len = 15){
#     chars <- c(1:10, LETTERS)
#     key <- paste(sample(chars, len), sep="", collapse="")
#
#     key
# }
