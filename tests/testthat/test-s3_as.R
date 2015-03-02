context("sparse to matrix conversion")

#
# NOTE: Need to use is_equivalent_to in order to ignore attributes!
#

test_that("as.matrix -> as.sparse -> as.matrix makes no changes", {

    ### Test the zero matrix
    m <- matrix(rep(0, 1), ncol = 1)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))
    m <- matrix(rep(0, 4), ncol = 2)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))

    ### 2-27-2015: sparse currently not allowed to handle non-square matrices: Why though?
#     m <- matrix(rep(0, 12), ncol = 3) # non-square case
#     expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))

    ### Test a randomly generated matrix
    m <- random.sparse(5, 5)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))

    ### Test a randomly generated DAG
    m <- random.dag.matrix(10, 10)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))
})

test_that("as.matrix -> as.sparse -> as.matrix makes no changes to REALLY BIG matrices", {

    ### Test the zero matrix
    m <- matrix(rep(0, 100*100), ncol = 100)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))

    ### Test a randomly generated matrix
    m <- random.sparse(100, 5*100)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))

    ### Test a randomly generated DAG
    m <- random.dag.matrix(100, 5*100)
    expect_that(as.matrix(as.sparse(as.matrix(m))), is_equivalent_to(m))
})

test_that("as.sparse converts matrix objects correctly", {})
