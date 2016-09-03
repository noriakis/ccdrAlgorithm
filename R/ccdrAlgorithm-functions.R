## returns TRUE if ivn_list is a list of vectors or NULL elements,
check_if_ivn_list <- function(ivn) {
    ## check if it is a list
    if(!is.list(ivn)) return(FALSE)

    ## check if every component is a vector of NULL
    return(all(sapply(ivn, is.vector) | sapply(ivn, is.null)))
} # END CHECK_IF_IVN_LIST

## returns TRUE if ivn_list has length nn, the number of sample rows
check_ivn_size <- function(ivn, data) {
    ## check if length matches with nn
    return(length(ivn) == nrow(data))
} # END CHECK_IF_IVN_SIZE

## returns TRUE if a vector component of 'ivn' is NULL,
## or has all correct labels of nodes under intervention in this sample:
## 1) integer, 2) between 1 and pp, and 3) no duplicates
check_vector_label <- function(vec, pp) {

    if(is.null(vec)) return(TRUE)

    ## Note: If a vector has only integers and NAs, is.integer returns all TRUE
    ## e.g.: c(NA, 1L, NA, 3L, NA, 5L)
    ## However, c(1L, NA, 3L, 4, NA) returns all FALSE
    ## check if labels are integers
    if(any(is.na(vec)) || !is.integer(vec)) {
        stop("Non-integer label(s) found in one or more components in ivn.")
        return(FALSE)
    }

    ## check if labels are in 1..pp
    if(any(vec < 1) | any(vec > pp)) {
        stop(sprintf("Labels should all be between 1 and %d to refer to the columns of data.", pp))
        return(FALSE)
    }

    ## check if labels are unique
    if(anyDuplicated(vec)) {
        stop("Duplicated label(s) found in one component in ivn.")
        return(FALSE)
    }

    return(TRUE)
} # END CHECK_VECTOR_LABEL

## returns TRUE if every vector in 'ivn' is NULL,
## or has correct labels: integer, between 1 and pp, and no duplicates
check_ivn_label <- function(ivn, data) {
    sapply(ivn, check_vector_label, ncol(data))
} # END CHECK_IVN_LABEL

## to do:
## to be included in `sparsebn` package
cor_vector_ivn <- function(data, ivn){
    check.numeric <- (sparsebnUtils::col_classes(data) != "numeric")
    if( any(check.numeric)){
        not.numeric <- which(check.numeric)
        stop(paste0("Input columns must be numeric! Columns ", paste(not.numeric, collapse = ", "), " are non-numeric."))
    }

    if( any(dim(data) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    pp <- ncol(data)
    ## unique(unlist(list(NULL, NULL, ...))) returns NULL without error
    ## so checking list(NULL, NULL, ...) is equivalent to checking if ivnlabels is NULL
    ivnlabels <- unique(unlist(ivn))
    if(is.null(ivnlabels)) {
        ## i.e. purely observational
        cors <- stats::cor(data)
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
            corsjj <- stats::cor(data[!sapply(lapply(ivn, is.element, jj), any), ])
            cors[[j]] <- corsjj[upper.tri(corsjj, diag = TRUE)]
        }
        corsjj <- stats::cor(data[!sapply(lapply(ivn, is.element, pp + 1), any), ])
        ## do not change above line to cor(data[sapply(ivn, is.null), ])
        ## why?
        cors[[len]] <- corsjj[upper.tri(corsjj, diag = TRUE)]
        cors <- unlist(cors)
        return(list(cors = cors, indexj = indexj))
    }
} # END COR_VECTOR_INTERVETION
