context("reIndex method for sparse objects")

test_that("reIndexC <-> reIndexR returns original", {
    ### R -> C
    sp <- sparse.list(list(rows = c(1L), cols = c(1L), vals = pi, dim = c(2, 2), start = 1))
    expect_equal(reIndexR(reIndexC(sp)), sp)

    ### C -> R
    sp[["start"]] <- 0 # pretend like this is uses C-style indexing
    expect_equal(reIndexC(reIndexR(sp)), sp)
})

test_that("reIndexC works as expected", {
    spR <- sparse.list(list(rows = c(1L), cols = c(1L), vals = pi, dim = c(2, 2), start = 1))
    spC <- reIndexC(spR)
    expect_equal(spC, sparse.list(list(rows = c(0L), cols = c(0L), vals = pi, dim = c(2, 2), start = 0)))
})

test_that("reIndexR works as expected", {
    spC <- sparse.list(list(rows = c(0L), cols = c(0L), vals = pi, dim = c(2, 2), start = 0))
    spR <- reIndexR(spC)
    expect_equal(spR, sparse.list(list(rows = c(1L), cols = c(1L), vals = pi, dim = c(2, 2), start = 1)))
})
