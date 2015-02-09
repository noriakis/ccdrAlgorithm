context(".ccdr.singleR")

pp <- 10
nn <- 5
cors.length <- pp*(pp+1)/2
cors.test <- runif(cors.length)
betas.test <- matrix(runif(pp*pp), ncol = pp)

test_that(".ccdr.singleR runs as expected", {
    ### No error / Also tests default values for betas, gamma, eps, maxIters, alpha
    expect_error(not(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi)))
})

test_that("Check input: cors", {

    ### Throw error if cors is integer
    expect_error(.ccdr.singleR(cors = as.integer(cors.test), pp = pp, nn = nn, lambda = pi))

    ### Throw error if cors has length != pp*(pp+1)/2
    expect_error(.ccdr.singleR(cors = cors.test[-1], pp = pp, nn = nn, lambda = pi))
})

test_that("Check input: pp", {
    ### pp is not an integer
    expect_error(.ccdr.singleR(cors = cors.test, pp = pi, nn = nn, lambda = pi))

    ### pp is not > 0
    expect_error(.ccdr.singleR(cors = cors.test, pp = -1L, nn = nn, lambda = pi))
})

test_that("Check input: nn", {
    ### nn is not an integer
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = pi, lambda = pi))

    ### nn is not > 0
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = -1L, lambda = pi))
})

test_that("Check input: betas", {
    ### betas is not a matrix or SparseBlockMatrixR
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, betas = as.numeric(betas.test), lambda = pi))

    ### If betas = zeroes and lambda = sqrt(n), then output should be zero
    final <- .ccdr.singleR(cors = cors.test, pp = pp, nn = nn, betas = matrix(0, nrow = pp, ncol = pp), lambda = sqrt(nn))
    expect_true(is.zero(final$sbm))
})

test_that("Check input: gamma", {
    ### gamma is non-numeric
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, gamma = 1L))

    ### gamma is negative
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, gamma = -5))

    ### gamma = -1 is OK
    expect_error(not(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, gamma = -1)))

})

test_that("Check input: eps", {
    ### eps is non-numeric
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, eps = 1L))

    ### eps is negative
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, eps = -5))

    ### Output warning if user sets eps = 0
    expect_warning(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, eps = 0))
})

test_that("Check input: maxIters", {
    ### maxIters is not an integer
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, maxIters = pi))

    ### maxIters is negative
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, maxIters = -5))
})

test_that("Check input: alpha", {
    ### alpha is negative
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, alpha = -5))
})

test_that("Check input: verbose", {
    ### Works when verbose = TRUE
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, verbose = TRUE))

    ### Works when verbose = FALSE
    expect_error(.ccdr.singleR(cors = cors.test, pp = pp, nn = nn, lambda = pi, verbose = FALSE))
})
