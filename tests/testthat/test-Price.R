op <- option("european", "vanilla", "call", 20, 0.75)
op.env <- option.env(S = 20, r = 0.03, q = 0.01, sigma = 0.05)

test_that("mc works", {
    expect_equal(price.option(op, op.env, n = 100000, steps = 10, all = FALSE, plot = FALSE),
                 0.41,
                 tolerance = 1e-1)
})

test_that("binomial works", {
    expect_equal(price.option(op, op.env, n = 100, u = 1.2, d = 0.8, method = "binomial", all = FALSE),
                 13.77929,
                 tolerance = 1e-5)
})

test_that("binomial works", {
    expect_equal(price.option(op, op.env, method = "bs", all = FALSE),
                 0.5083162,
                 tolerance = 1e-7)
})
