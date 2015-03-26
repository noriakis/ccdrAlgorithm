context("is.___ s3 methods")

### S3 sparse
test_that("is.sparse works as expected", {
    li <- list(rows = integer(0), cols = integer(0), vals = numeric(0), dim = c(1,1), start = 1)
    sp <- sparse(li)
    expect_is(sp, "sparse")

    expect_true(is.sparse(sp))
    expect_false(is.sparse(list(0)))
})

### S3 SparseBlockMatrixR
test_that("is.SparseBlockMatrixR works as expected", {
    # "rows", "vals", "blocks", "sigmas", "start"
    li <- list(rows = list(integer(0)), vals = list(numeric(0)), blocks = list(integer(0)), sigmas = numeric(0), start = 1)
    sbm <- SparseBlockMatrixR(li)
    expect_is(sbm, "SparseBlockMatrixR")

    expect_true(is.SparseBlockMatrixR(sbm))
    expect_false(is.SparseBlockMatrixR(list(0)))
})

### S3 ccdrFit
test_that("is.ccdrFit works as expected", {
    # "sbm", "lambda", "nedge", "pp", "nn", "time"
    li <- list(rows = list(integer(0)), vals = list(numeric(0)), blocks = list(integer(0)), sigmas = numeric(0), start = 1)
    sbm <- SparseBlockMatrixR(li)

    li <- list(sbm = sbm, lambda = pi, nedge = 0, pp = 1, nn = 10, time = runif(1))
    cf <- ccdrFit(li)
    expect_is(cf, "ccdrFit")

    expect_true(is.ccdrFit(cf))
    expect_false(is.ccdrFit(list(0)))
})
