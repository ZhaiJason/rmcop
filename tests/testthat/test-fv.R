test_that("fv() computes future value", {
    expect_equal(fv(100, 0.05, 1), 105)
})
