context("ccdr.run with ivn")

suppressMessages({
    pp <- 4
    nn <- 10
    X.test <- matrix(rnorm(4 * nn * pp), 4 * nn, pp)
    ivnvector <- as.integer(c(rep(1, nn), rep(2, nn), rep(3, nn), rep(4, nn)))
    dat.test <- sparsebnUtils::sparsebnData(X.test, type = "c", ivn = as.list(ivnvector))
    betas.test <- matrix(runif(pp*pp), ncol = pp)
    lambdas.test <- c(sqrt(97), 50, 6, pi, exp(1), 1, 0.05)
    lambdas.length.test <- 10
})

# ccdr.run <- function(data,
#                      betas,
#                      lambdas,
#                      lambdas.length = NULL,
#                      gamma = 2.0,
#                      error.tol = 1e-4,
#                      max.iters = NULL,
#                      alpha = 10,
#                      verbose = FALSE
# )

test_that("ccdr.run runs as expected", {
    ### No error / Also tests default values for other parameters
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test), NA)
})

test_that("Check input: ivn", {
    ### ivn must be a list
    dat.test.notlist <- dat.test
    dat.test.notlist$ivn <- ivnvector
    expect_error(ccdr.run(data = dat.test.notlist, lambdas.length = lambdas.length.test), "'ivn' argument must be a list of NULLs or vectors!")
    rm("dat.test.notlist")

    ### each component of ivn must be either NULL or vectors
    dat.test.ivncomp <- dat.test
    dat.test.ivncomp$ivn[[1]] <- matrix(1:4, 1, 4)
    expect_error(ccdr.run(data = dat.test.ivncomp, lambdas.length = lambdas.length.test), "'ivn' argument must be a list of NULLs or vectors!")
    rm("dat.test.ivncomp")

    ### ivn must have the length same as the number of sample rows
    dat.test.length <- dat.test
    dat.test.length$ivn <- as.list(1:4)
    expect_error(ccdr.run(data = dat.test.length, lambdas.length = lambdas.length.test), "Length of 'ivn' is 4, expected 40!")
    rm("dat.test.length")

    ### labels in each must be appropriate
    dat.test.notint <- dat.test
    dat.test.notint$ivn[[1]] <- c(1L, "2")
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), "Intervention labels incorrect. Probably non-integer, out of range, or duplicates.")

    dat.test.notint$ivn[[1]] <- c(1L, 1.5)
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), "Intervention labels incorrect. Probably non-integer, out of range, or duplicates.")

    dat.test.notint$ivn[[1]] <- c(3L, 1L)
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), NA)
    ## labels do not have to be sorted

    dat.test.notint$ivn[[1]] <- c(1L, NA, 3L)
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), "Intervention labels incorrect. Probably non-integer, out of range, or duplicates.")

    dat.test.notint$ivn[[1]] <- c(0L, 1L)
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), "Intervention labels incorrect. Probably non-integer, out of range, or duplicates.")

    dat.test.notint$ivn[[1]] <- c(100L, 1L)
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), "Intervention labels incorrect. Probably non-integer, out of range, or duplicates.")

    dat.test.notint$ivn[[1]] <- c(1L, 2L, 1L)
    expect_error(ccdr.run(data = dat.test.notint, lambdas.length = lambdas.length.test), "Intervention labels incorrect. Probably non-integer, out of range, or duplicates.")

    rm("dat.test.notint")
})

test_that("Check input: lambdas", {
    ### No error
    expect_error(ccdr.run(data = dat.test, lambdas = lambdas.test), NA)

    ### Negative values not allowed
    lambdas.test.neg <- lambdas.test
    lambdas.test.neg[3] <- -1 * lambdas.test.neg[3]
    expect_error(ccdr.run(data = dat.test, lambdas = lambdas.test.neg), "only nonnegative values!")

    #   ### Integers not allowed
    #     lambdas.test.int <- as.integer(lambdas.test)
    #     expect_error(ccdr.run(data = dat.test, lambdas = lambdas.test.int), "lambdas must be a numeric vector!")
})

test_that("Check input: lambdas.length", {
    ### Negative values not allowed
    expect_error(ccdr.run(data = dat.test, lambdas.length = -5), "must be positive!")

    ### Zero not allowed
    expect_error(ccdr.run(data = dat.test, lambdas.length = 0), "must be positive!")

    ### Cannot leave both lambdas and lambdas.length empty
    expect_error(ccdr.run(data = dat.test), "Both lambdas and lambdas.length unspecified")
})

test_that("Check input: max.iters", {
    ### maxIters is not an integer
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, maxx.ters = pi))

    ### maxIters is negative
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, maxx.ters = -5))
})

test_that("Check input: error.tol", {
    ### error.tol cannot be negative
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, error.tol = -1), "must be >= 0!")

    ### warning if error.tol is zero
    expect_warning(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, error.tol = 0), "eps is set to zero")
})

test_that("Check input: gamma", {
    ### gamma = -1 is OK
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, gamma = -1), NA)

    ### gamma cannot be arbitrary negative number
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, gamma = -5), "gamma must be >= 0")
})

test_that("Check input: alpha", {
    ### alpha is negative
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, alpha = -5))
})

test_that("Check input: verbose", {
    ### Works when verbose = TRUE
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, verbose = TRUE), NA)

    ### Works when verbose = FALSE
    expect_error(ccdr.run(data = dat.test, lambdas.length = lambdas.length.test, verbose = FALSE), NA)
})

