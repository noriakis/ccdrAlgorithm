context("black/white lists")

dat <- generate_fixed_data_frame()
sbdata <- suppressMessages(sparsebnData(dat, type = "c"))

### Get all blocks at once
nodes <- names(sbdata$data)
blocks <- lapply(nodes, function(x){
    # Allow all off-diagonal entries since we are no longer using the block decomposition
    row <- (nodes)[nodes != x]
    col <- rep(x, length(col))
    cbind(row, col)
})
blocks <- do.call("rbind", blocks)

pp <- ncol(dat)
len_saturate <- pp*(pp-1)/2
nlambda <- 20

test_that("White lists work OK", {
    dags <- ccdr.run(sbdata, lambdas.length = nlambda, whitelist = blocks)
    expect_equal(num.edges(dags), rep(len_saturate, nlambda))
})

test_that("Black lists work OK", {
    dags <- ccdr.run(sbdata, lambdas.length = nlambda, blacklist = blocks)
    expect_equal(num.edges(dags), rep(0, nlambda))
})
