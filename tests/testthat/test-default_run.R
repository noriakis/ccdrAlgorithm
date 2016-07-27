context("Default run")

# library("mvtnorm")

### TEST CCDR ---------------------------------------------

### Generate data
data <- suppressMessages(sparsebnUtils::sparsebnData(generate_fixed_data_frame(), type = "c"))
# data <- suppressWarnings(sparsebnUtils::sparsebnData(X, type = "continuous"))

test_that("Testing default behaviour of ccdr.run", {
    final <- ccdr.run(data = data, lambdas.length = 20)

    expect_is(final, "list")

    ### Check output types
    check_sbf <- check_list_class(final, "sparsebnFit")
    expect_true(check_sbf)
    # for(i in seq_along(final)){
    #     expect_is(final[[i]], "sparsebnFit")
    # }

    ### Check consistency of nedge
    for(i in seq_along(final)){
        matrix.nedge <- Matrix::nnzero(get.adjacency.matrix(final[[i]]$edges))
        edgeL.nedge <- num.edges(final[[i]]$edges)
        expect_equal(final[[i]]$nedge, edgeL.nedge, matrix.nedge)
    }
})

test_that("Testing ccdr.run with manual settings", {
    lambdas <- sparsebnUtils::generate.lambdas(lambda.max = sqrt(nrow(data$data)), lambdas.ratio = 0.1, lambdas.length = 20, scale = "linear")
    final <- ccdr.run(data = data, lambdas = lambdas, alpha = 3, max.iters = 10, verbose = FALSE)

    expect_is(final, "list")

    ### Check output types
    check_sbf <- check_list_class(final, "sparsebnFit")
    expect_true(check_sbf)
    # for(i in seq_along(final)){
    #     expect_is(final[[i]], "sparsebnFit")
    # }

    ### Check consistency of nedge
    for(i in seq_along(final)){
        matrix.nedge <- Matrix::nnzero(get.adjacency.matrix(final[[i]]$edges))
        edgeL.nedge <- num.edges(final[[i]]$edges)
        expect_equal(final[[i]]$nedge, edgeL.nedge, matrix.nedge)
    }
})

test_that("Bugfix: ccdr.run returns the correct number of solutions", {
    ### Edge threshold not met, return all solutions
    final <- ccdr.run(data = data, lambdas.length = 5)
    expect_equal(length(final), 5)

    ### Edge threshold met, only return subpath of complete solutions
    final <- ccdr.run(data = data, lambdas.length = 5, alpha = 0.1)
    expect_equal(length(final), 1)
})

test_that("ccdr.run with intervention, case 1:", {
    pp <- 4
    nn <- 10
    x1 <- rnorm(4 * nn)
    x2 <- 2 * x1; x2[(nn+1):(2*nn)] <- rnorm(nn)
    x3 <- rnorm(4 * nn)
    x4 <- x2 + x3; x4[(3*nn+1):(4*nn)] <- rnorm(nn)
    X <- cbind(x1, x2, x3, x4)
    o <- sample(1:pp)
    o1 <- c(o, pp + 1)
    q <- order(o) ## o[q] == q[o] == 1:pp
    q1 <- order(o1)
    X1 <- X[, o] ## permute the columns to randomize node ordering
    ivnvector <- as.integer(as.vector(sapply(1:pp, rep, nn)))
    ivnvector1 <- q1[ivnvector]
    data1 <- sparsebnUtils::sparsebnData(X1, type = "c", ivn = as.list(ivnvector1))

    V <- as.character(1:pp)
    edL <- vector("list", pp)
    names(edL) <- V
    edL[[1]] <- list(edges = 2, weights = 2)
    edL[[2]] <- list(edges = 4, weights = 1)
    edL[[3]] <- list(edges = 4, weights = 1)
    edL[[4]] <- list(edges = numeric(0))
    ## g0 <- graph::graphNEL(nodes = as.character(1:4), edgeL = edL, edgemode = "directed")
    edL1 <- permutenodes.edgeL(edL, o)
    ##g1 <- permutenodes(g0, o)

    final <- ccdr.run(data = data1, lambdas.length = 10) ## use most of the default settings
    compare.path <- sapply(lapply(final, getElement, "edges"), compare.sFg, edL1)
    shd.val <- compare.path[7, ]
    expect_true(min(shd.val) < 2) ## ideally this should be zero
})

test_that("ccdr.run with intervention, case 2:", {
    pp <- 6
    nn <- 20
    x1 <- rnorm(6 * nn)
    x2 <- rnorm(6 * nn)
    x3 <- 0.5 * x1; x3[(2*nn+1):(3*nn)] <- rnorm(nn)
    x4 <- rnorm(6 * nn)
    x5 <- 0.7 * x2 + 0.5 * x3; x5[(4*nn+1):(5*nn)] <- rnorm(nn)
    x6 <- 0.4 * x1 + 0.8 * x5; x6[(5*nn+1):(6*nn)] <- rnorm(nn)
    X <- cbind(x1, x2, x3, x4, x5, x6)
    o <- sample(1:pp)
    o1 <- c(o, pp + 1)
    q <- order(o) ## o[q] == q[o] == 1:pp
    q1 <- order(o1)
    X1 <- X[, o] ## permute the columns to randomize node ordering
    ivnvector <- as.integer(as.vector(sapply(1:pp, rep, nn)))
    ivnvector1 <- q1[ivnvector]
    data1 <- sparsebnUtils::sparsebnData(X1, type = "c", ivn = as.list(ivnvector1))

    V <- as.character(1:pp)
    edL <- vector("list", pp)
    names(edL) <- V
    edL[[1]] <- list(edges = c(3,6), weights = c(0.5, 0.4))
    edL[[2]] <- list(edges = 5, weights = 0.7)
    edL[[3]] <- list(edges = 5, weights = 0.5)
    edL[[4]] <- list(edges = numeric(0))
    edL[[5]] <- list(edges = 6, weights = 0.8)
    edL[[6]] <- list(edges = numeric(0))
    ## g0 <- graph::graphNEL(nodes = as.character(1:4), edgeL = edL, edgemode = "directed")
    edL1 <- permutenodes.edgeL(edL, o)
    ##g1 <- permutenodes(g0, o)

    final <- ccdrAlgorithm::ccdr.run(data = data1, lambdas.length = 10) # use most of the default settings
    compare.path <- sapply(lapply(final, getElement, "edges"), compare.sFg, edL1)
    shd.val <- compare.path[7, ]
    expect_true(min(shd.val) < 2) ## ideally this should be zero
})
