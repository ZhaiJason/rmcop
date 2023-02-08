test_that("create vanilla option", {
    obj <- list(
        "style" = "european",
        "type" = "call",
        "K" = 20,
        "t" = 1
    )
    attr(obj, "class") <- c("vanilla", "option")

    expect_equal(
        option("european", "vanilla", "call", K = 20, t = 1),
        obj
    )
})

test_that("create asian option", {
    obj <- list(
        "style" = "european",
        "type" = "call",
        "K" = 20,
        "t" = 1,
        "is.avg_price" = FALSE
    )
    attr(obj, "class") <- c("asian", "option")

    expect_equal(
        option("european", "asian", "call", K = 20, t = 1, is.avg_price = FALSE),
        obj
    )
})

test_that("create barrier option", {
    obj <- list(
        "style" = "european",
        "type" = "call",
        "K" = 20,
        "t" = 1,
        barrier = 21,
        is.knockout = TRUE
    )
    attr(obj, "class") <- c("barrier", "option")

    expect_equal(
        option("european", "barrier", "call", K = 20, t = 1, barrier = 21, is.knockout = TRUE),
        obj
    )
})

test_that("create binary option", {
    obj <- list(
        "style" = "european",
        "type" = "call",
        "K" = 20,
        "t" = 1,
        payout = 5
    )
    attr(obj, "class") <- c("binary", "option")

    expect_equal(
        option("european", "binary", "call", K = 20, t = 1, payout = 5),
        obj
    )
})

test_that("create lookback option", {
    obj <- list(
        "style" = "european",
        "type" = "call",
        "K" = 20,
        "t" = 1,
        is.fixed = TRUE
    )
    attr(obj, "class") <- c("lookback", "option")

    expect_equal(
        option("european", "lookback", "call", K = 20, t = 1, is.fixed = TRUE),
        obj
    )
})
