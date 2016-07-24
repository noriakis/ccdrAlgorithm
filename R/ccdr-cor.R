## returns TRUE if ivn_list is a list of vectors or NULL elements,
## and when nn and pp provided:
## additionally check if all components in vectors are appropriate labels
check_if_ivn_list <- function(X, nn, pp) {
    ## check if it is a list
    if(!is.list(X)) return(FALSE)

    ## check if every component is a vector of NULL
    if(!any(sapply(X, is.vector) | sapply(X, is.null))) return(FALSE)

    ## check if length matches with nn
    if(length(X) != nn) return(FALSE)

    ## check if labels are integers and fall in 1...pp
    labels <- unlist(X)
    if(any(!is.integer(labels))) return(FALSE) ## non-integer labels
    if(any(labels < 1) | any(labels > pp)) return(FALSE) ## labels out of range

    return(TRUE)
} # END CHECK_IF_IVN_LIST

## to do:
## to be included in `sparsebn` package
cor_vector_ivn <- function(X, ivn){
    check.numeric <- (sparsebnUtils::col_classes(X) != "numeric")
    if( any(check.numeric)){
        not.numeric <- which(check.numeric)
        stop(paste0("Input columns must be numeric! Columns ", paste(not.numeric, collapse = ", "), " are non-numeric."))
    }

    if( any(dim(X) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    pp <- ncol(X)
    ivnlabels <- unique(unlist(ivn))
    if(is.null(ivnlabels)) {
        ## i.e. purely observational
        cors <- stats::cor(X)
        cors <- cors[upper.tri(cors, diag = TRUE)]
        return(list(cors = cors, indexj = rep(0L, pp + 1)))
    } else {
        ## so there are at least some interventions
        ivnj <- as.integer(c(ivnlabels, pp + 1))
        # get all the j's that has interventions
        # including pp+1 for observational rows (compatible with purely observational data)
        # is this necessary if most are balanced designs where all nodes get intervention?
        # any possible optimization?
        len <- length(ivnj)
        cors <- vector("list", len)
        indexj <- as.integer(rep(len - 1, pp + 1)) # zero-based index for C compatibility
        for(j in 1:(len - 1)) {
            jj <- ivnj[j]
            indexj[jj] <- j - 1
            ## extract rows where node jj has no intervetion
            corsjj <- cor(X[!sapply(lapply(ivn, is.element, jj), any), ])
            cors[[j]] <- corsjj[upper.tri(corsjj, diag = TRUE)]
        }
        corsjj <- cor(X[!sapply(lapply(ivn, is.element, pp + 1), any), ])
        ## do not change above line to cor(X[sapply(ivn, is.null), ])
        ## why?
        cors[[len]] <- corsjj[upper.tri(corsjj, diag = TRUE)]
        cors <- unlist(cors)
        return(list(cors = cors, indexj = indexj))
    }
} # END COR_VECTOR_INTERVETION
