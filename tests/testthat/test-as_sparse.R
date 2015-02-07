context("as.sparse")

test_that("as.sparse converts matrix objects correctly", {
    m <- random.dag.matrix(10, 10)
    sp <- as.sparse(m)

    expect_equal(as.matrix(sp), m)
    # sum(abs(m-as.matrix(sp)))
})
