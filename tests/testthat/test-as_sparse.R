context("sparse to matrix conversion")

test_that("as.matrix -> as.sparse -> as.matrix makes no changes", {

    ### Test the zero matrix
    m <- matrix(rep(0, 1), ncol = 1)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)
    m <- matrix(rep(0, 4), ncol = 2)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)
    m <- matrix(rep(0, 12), ncol = 3) # non-square case
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)

    ### Test a randomly generated matrix
    m <- random.sparse(5, 5)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)

    ### Test a randomly generated DAG
    m <- random.dag.matrix(10, 10)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)
})

test_that("as.matrix -> as.sparse -> as.matrix makes no changes to REALLY BIG matrices", {

    ### Test the zero matrix
    m <- matrix(rep(0, 100*100), ncol = 100)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)

    ### Test a randomly generated matrix
    m <- random.sparse(100, 5*100)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)

    ### Test a randomly generated DAG
    m <- random.dag.matrix(100, 5*100)
    expect_equal(as.matrix(as.sparse(as.matrix(m))), m)
})

test_that("as.sparse converts matrix objects correctly", {})
