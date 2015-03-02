context(".gen_lambdas method for generating a grid of lambdas")

test_that(".gen_lambdas is the same as generate.lambdas", {
    nn <- 10
    expect_equal(.gen_lambdas(nn), generate.lambdas(nn))
})

test_that(".gen_lambdas produces values in the expected range", {
    nn <- 10

    ### Linear scale
    expect_true(all(.gen_lambdas(nn, scale = "linear") >= 0))
    expect_true(all(.gen_lambdas(nn, scale = "linear") <= sqrt(nn)))
    expect_equal(max(.gen_lambdas(nn, scale = "linear")), sqrt(nn))
    expect_equal(min(.gen_lambdas(nn, lambdas.ratio = 0.1, scale = "linear")), 0.1*sqrt(nn))

    ### Log scale
    expect_true(all(.gen_lambdas(nn, scale = "log") >= 0))
    expect_true(all(.gen_lambdas(nn, scale = "log") <= sqrt(nn)))
    expect_equal(max(.gen_lambdas(nn, scale = "log")), sqrt(nn))
    expect_equal(min(.gen_lambdas(nn, lambdas.ratio = 0.1, scale = "log")), 0.1*sqrt(nn))
})

test_that(".gen_lambdas produces the exact output expected on test cases", {
    vec <- .gen_lambdas(100, lambdas.ratio = 0.1, lambdas.length = 10, scale = "linear") # should be 10:1
    expect_equal(vec, 10:1)

    vec <- .gen_lambdas(100, lambdas.ratio = 0.1, lambdas.length = 3, scale = "log") # should be {10.000000, 3.162278, 1.000000}
    expect_true(sum(abs(vec - c(10.000000, 3.162278, 1.000000))) < 1e-6) # need to check error instead of equality since values are approximate
})
