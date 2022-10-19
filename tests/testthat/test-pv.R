test_that("pv() computes present value", {
  expect_equal(pv(105, 0.05, 1), 100)
})
