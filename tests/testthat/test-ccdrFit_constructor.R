context("ccdrFit.list")

m <- rbind(c(0,  0,  0),
           c(1,  0,  0),
           c(0, 2.1, 0))
sbm <- SparseBlockMatrixR(m)

li <- list(sbm = sbm, lambda = pi, nedge = 2, pp = 1, nn = 10, time = exp(1))
cf <- ccdrFit(li)

test_that("ccdrFit is consistent with different ways of accessing nedge", {
    matrix.nedge <- sum(as.matrix(cf$sbm) != 0)
    sbm.nedge <- .num_edges(sbm)

    ### Note that sbm.nedge and cf$nedge are equal by construction since nedge is set manually above
    expect_equal(sbm.nedge, matrix.nedge, cf$nedge)
})
