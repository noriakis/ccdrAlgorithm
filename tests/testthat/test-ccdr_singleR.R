context("ccdr_singleR")

pp <- 10L
nn <- 5L
indexj <- rep(0L, pp + 1)
nj <- as.integer(rep(nn, pp))
cors.length <- pp*(pp+1)/2
cors.test <- runif(cors.length)
betas.test <- matrix(runif(pp*pp), ncol = pp)
lambda.test <- pi
gamma.test <- 2.0
eps.test <- 0.1
maxIters.test <- 1000L
alpha.test <- 10

### The call for ccdr_singleR
# ccdr_singleR <- function(cors,
#                           pp, nn,
#                           betas,
#                           lambda,
#                           gamma,
#                           eps,
#                           maxIters,
#                           alpha,
#                           verbose = FALSE)

test_that("ccdr_singleR runs as expected", {
    ### Throw error if parameter and initial values not explicitly specified
    expect_error(ccdr_singleR(cors = cors.test))

    ### No error
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test), NA)

    ### No error
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = nj, indexj = indexj, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test), NA)

})

test_that("Check input: cors", {

    ### Throw error if cors has length != pp*(pp+1)/2
    expect_error(ccdr_singleR(cors = cors.test[-1], pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: pp", {

    ### pp is not an integer
    expect_error(ccdr_singleR(cors = cors.test, pp = pi, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### pp is not > 0
    expect_error(ccdr_singleR(cors = cors.test, pp = -1L, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: nn", {

    ### nn is not an integer
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = pi, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### nn is not > 0
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = -1L, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: indexj", {
    ### indexj is defined to be a vector containing the start position of the correlation matrix for node j in 'cors'

    ### indexj is not a vector
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, indexj = matrix(0L, nrow = 1, ncol = pp + 1), betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### indexj is of wrong size
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, indexj = rep(0L, pp + 2), betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### indexj has non-integer
    indexj1 <- indexj

    indexj1[1] <- pi
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, indexj = indexj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    indexj1[1] <- NA
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, indexj = indexj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### indexj out of bound
    indexj1[1] <- 0
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, indexj = indexj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    indexj1[1] <- pp + 2
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, indexj = indexj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: nj", {
    ### nj is defined to be a vector containing the number of times each node is free of intervention (to replace nn)

    ### nj is not a vector
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = matrix(nn, nrow = 1, ncol = pp), betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### nj is of wrong size
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = as.integer(rep(nn, pp + 1)), betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### nj has non-integer
    nj1 <- nj

    nj1[1] <- pi
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = nj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    nj1[1] <- NA
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = nj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### nj out of bound
    nj1 <- rep(-1L, pp)
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = nj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    nj1 <- as.integer(rep(nn + 1, pp))
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, nj = nj1, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: betas", {

    ### betas is not a matrix or SparseBlockMatrixR
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = as.numeric(betas.test), lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### If betas = zeroes and lambda = sqrt(n), then output should be zero
    final <- ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = matrix(0, nrow = pp, ncol = pp), lambda = sqrt(nn), gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test)
    expect_true(is.zero(final$sbm))
})

test_that("Check input: lambda", {

    ### lambda is negative
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = -lambda.test, gamma = gamma.test, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: gamma", {

    ### gamma is negative
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = -5, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test))

    ### gamma = -1 is OK
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = -1, eps = eps.test, maxIters = maxIters.test, alpha = alpha.test), NA)

})

test_that("Check input: eps", {

    ### eps is negative
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = -5, maxIters = maxIters.test, alpha = alpha.test))

    ### Output warning if user sets eps = 0
    expect_warning(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = 0, maxIters = maxIters.test, alpha = alpha.test))
})

test_that("Check input: maxIters", {

    ### maxIters is negative
    expect_error(ccdr_singleR(cors = cors.test, pp = pp, nn = nn, betas = betas.test, lambda = lambda.test, gamma = gamma.test, eps = eps.test, maxIters = -5, alpha = alpha.test))
})

test_that("Check input: alpha", {

})
