context("ccdrFit get.___")

m <- rbind(c(0,  0,  0),
           c(1,  0,  0),
           c(0, 2.1, 0))
sbm <- SparseBlockMatrixR(m)

li <- list(sbm = sbm, lambda = pi, nedge = 2, pp = 1, nn = 10, time = exp(1))
cf <- ccdrFit(li)
cf.li <- list(cf, cf, cf)

test_that("get.___ methods", {
    expect_that(get.adjacency.matrix(cf), is_equivalent_to(m))
    expect_that(get.rhos(cf), is_equivalent_to(rep(0, ncol(m))))

    ### Test methods that act on ccdrFit lists
    expected_result <- rep(exp(1), length(cf.li))
    expect_that(get.times(cf.li), is_equivalent_to(expected_result))

    expected_result <- rep(pi, length(cf.li))
    expect_that(get.lambdas(cf.li), is_equivalent_to(expected_result))
})
