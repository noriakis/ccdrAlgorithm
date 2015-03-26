context("CCDr test cases")

test_that("If cors = 0 and betas = 0, ouput should be all zeroes", {

    #
    # In the special case when the data and the initial guess are both zero, the result
    #  of the CCDr should be _exactly_ beta_ij = 0 and sigma_j = sqrt(n) for all i and j
    #
    # For details, see Section 5.2 of Aragam and Zhou (2015), in particular equations
    #  (33) and (34)
    #

    ### Low-dimensions
    pp <- 5
    nn <- 10
    cors <- rep(0, pp*(pp+1)/2)
    final <- ccdr::.ccdr_gridR(cors = cors,
                               pp = as.integer(pp), nn = as.integer(nn),
                               betas = matrix(0, nrow = pp, ncol = pp),
                               alpha = 10, gamma = 2, eps = 1e-4, maxIters = 10L, lambdas = 10:1, verbose = FALSE)

    check_zeroes <- check_vars <- TRUE
    for(k in seq_along(final)){
        check_zeroes <- check_zeroes & all(get.adjacency.matrix(final[[k]]) == 0)
        check_vars <- check_vars & isTRUE(all.equal(get.sigmas(final[[k]]), rep(sqrt(nn), pp)))
    }

    expect_true(check_zeroes)
    expect_true(check_vars)

    ### High-dimensions
    pp <- 50
    nn <- 10
    cors <- rep(0, pp*(pp+1)/2)
    final <- ccdr::.ccdr_gridR(cors = cors,
                               pp = as.integer(pp), nn = as.integer(nn),
                               betas = matrix(0, nrow = pp, ncol = pp),
                               alpha = 10, gamma = 2, eps = 1e-4, maxIters = 10L, lambdas = 10:1, verbose = FALSE)

    check_zeroes <- check_vars <- TRUE
    for(k in seq_along(final)){
        check_zeroes <- check_zeroes & all(get.adjacency.matrix(final[[k]]) == 0)
        check_vars <- check_vars & isTRUE(all.equal(get.sigmas(final[[k]]), rep(sqrt(nn), pp)))
    }

    expect_true(check_zeroes)
    expect_true(check_vars)
})
