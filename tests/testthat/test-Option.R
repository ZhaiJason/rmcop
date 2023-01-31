test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("create vanilla option", {
    expect_equal(
        option("european", "vanilla", "call", K = 20, t = 1),
        list(
            "style" = "european",
            "type" = "call",
            "K" = 20,
            "t" = 1
        )
    )
})



# Create an European vanilla option with strike price 20, expiry in 1 year.
vanilla1 <- option("european", "vanilla", "call", K = 20, t = 1)

# Create European  average strike Asian option with strike price 20, expiry in 1 year.
asian1 <- option("european", "asian", "call", K = 20, t = 1, is.avg_price = FALSE)

# Create European knock-out barrier option with strike price 20, barrier price 21, expiry in 1 year.
barrier1 <- option("european", "barrier", "call", K = 20, t = 1, barrier = 21, is.knockout = TRUE)

# Create European binary option with strike price 20, fixed payout 5, expiry in 1 year.
binary1 <- option("european", "binary", "call", K = 20, t = 1, payout = 5)

# Create European fixed strike lookback option with strike price 20, expiry in 1 year.
lookback1 <- option("european", "lookback", "call", K = 20, t = 1, is.fixed = TRUE)
