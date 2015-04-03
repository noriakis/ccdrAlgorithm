#
#  ccdr-test_script.R
#

#
# Test script for ccdr package
#

status <- numeric(0) # Nothing has happened yet

### Get session info
sessionInfo()

{
    ### Install devtools and check for Rtools installation (needed for Windows)
    tryCatch({
        if(!require("devtools")) install.packages("devtools", repos = "http://cran.rstudio.com/")
        library("devtools")
    }, error = function(c){ stop(c)})

    if(!find_rtools()){
        stop("WARNING: Rtools is required to build R packages, but is not currently installed.\n",
             "Please download and install Rtools 3.1 from http://cran.r-project.org/bin/windows/Rtools/ and then run devtools::find_rtools().")
    }

    status["devtools"] <- 1
}

{
    ### Load bioconductor
    tryCatch({
        source("http://bioconductor.org/biocLite.R")
        biocLite(suppressUpdates = TRUE, suppressAutoUpdate = TRUE)

        ### Install bioconductor dependencies
        if(!require("graph")) biocLite("graph", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
        if(!require("RBGL")) biocLite("RBGL", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
    }, error = function(c){ stop(c)})

    status["bioconductor"] <- 1
}

{
    ### Install pcalg package and all dependencies (there are a bunch!)
    tryCatch({
        if(!require("pcalg")) install.packages("pcalg", repos = "http://cran.rstudio.com/")
        library("pcalg")
    }, error = function(c){ stop(c)})

    status["pcalg"] <- 1
}

{
    ### Install and load dev package
    tryCatch({
        install_url("http://www.stat.ucla.edu/~naragam/pkg/ccdr.tar.gz")
        library("ccdr")
    }, error = function(c){ stop(c)})

    status["ccdr"] <- 1
}

### Check package install
if(exists("ccdr.run")){
    message("Package install successful!")

    test_run <- function(pp, nn, ss){

        ### Generate some random data
        beta.min <- 0.5
        beta.max <- 2
        edge.pr <- 2 * ss / (pp - 1)
        g <- pcalg::randomDAG(n = pp, prob = edge.pr, lB = beta.min, uB = beta.max) # Note that the edge weights are selected at random here!

        # d <- generate_data(g, nn)
        pi <- sample(1:pp)
        X <- pcalg::rmvDAG(n = nn, dag = g, errDist = "normal")
        X <- X[, pi] ## permute the columns to randomize node ordering

        ### Use lambdas
        t1 <- proc.time()[3]
        lambdas <- generate.lambdas(lambda.max = sqrt(nn), lambdas.ratio = 0.1, lambdas.length = 20, scale = "linear")
        final1 <- ccdr.run(data = X, lambdas = lambdas, alpha = 3, verbose = FALSE)
        t2 <- proc.time()[3]

        ## Use lambdas.length
        final2 <- ccdr.run(data = X, lambdas.length = 10, alpha = 10, verbose = FALSE)

        class1 <- lapply(final1, function(z) class(z))
        class2 <- lapply(final2, function(z) class(z))
        if(all(class1 == "ccdrFit") && all(class2 == "ccdrFit")){
            t2 - t1 # if successful, return runtime as result
        } else{
            NA
        }
    }

    ### Small graphs
    status["(10, 10, 1)"] <- test_run(10, 10, 1)
    status["(10, 100, 1)"] <- test_run(10, 100, 1)
    status["(10, 1000, 1)"] <- test_run(10, 1000, 1)

    status["(10, 10, 0.5)"] <- test_run(10, 10, 0.5)
    status["(10, 100, 0.5)"] <- test_run(10, 100, 0.5)
    status["(10, 1000, 0.5)"] <- test_run(10, 1000, 0.5)

    status["(10, 10, 2)"] <- test_run(10, 10, 2)
    status["(10, 100, 2)"] <- test_run(10, 100, 2)
    status["(10, 1000, 2)"] <- test_run(10, 1000, 2)

    ### Large graphs
    status["(100, 1000, 0.5)"] <- test_run(100, 1000, 0.5)
    status["(100, 1000, 1)"] <- test_run(100, 1000, 1)
    status["(100, 1000, 2)"] <- test_run(100, 1000, 2)

    status["(200, 100, 0.5)"] <- test_run(200, 100, 0.5)
    status["(200, 100, 1)"] <- test_run(200, 100, 1)
    status["(200, 100, 2)"] <- test_run(200, 100, 2)

    status["(500, 50, 0.5)"] <- test_run(500, 50, 0.5)
    status["(500, 50, 1)"] <- test_run(500, 50, 1)
    status["(500, 50, 2)"] <- test_run(500, 50, 2)
}

### Show results
status

### Remove dev package
remove.packages("ccdr")
