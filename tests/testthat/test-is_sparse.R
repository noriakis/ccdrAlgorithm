context("is_sparse")

test_that("is.sparse works as expected", {
    li <- list(rows = integer(0), cols = integer(0), vals = numeric(0), dim = c(1,1), start = 1)
    sp <- sparse(li)
    expect_is(sp, "sparse")

    expect_true(is.sparse(sp))
    expect_false(is.sparse(list(0)))
})
