library("ccdr")
context("Default run")

source('~/Dropbox/PhD Research/Programming Projects/bncompare_dev/bncompare/R/bncompare-generate.R')
# depends on
source('~/Dropbox/PhD Research/Programming Projects/bncompare_dev/bncompare/R/s3-trueGraph.R')
# depends on
source('~/Dropbox/PhD Research/Programming Projects/bncompare_dev/bncompare/R/bncompare-utils.R')
# depends on
source('~/Dropbox/PhD Research/Programming Projects/bncompare_dev/bncompare/R/bncompare-functions.R')

R_DEBUG_ON <<- FALSE # This is just a hack since we aren't using load_bncompare.R to load the package

### Generate some random data
pp <- 10
nn <- 100
ss <- 2

g <- generate_ordered_dag(pp, ss)
d <- generate_data(g, nn)
final <- ccdr.run(data = d$dat, lambdas.length = 20, lambda.ratio =  0.1, alpha = 3, verbose = FALSE)

test_that("Testing default behaviour of ccdr.gridR", {
    expect_is(final, "list")
    for(i in seq_along(final)){
        expect_is(final[[i]], "ccdrFit")
    }
})
