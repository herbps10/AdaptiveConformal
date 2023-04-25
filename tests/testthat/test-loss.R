test_that("interval loss well defined", {
  expect_equal(interval_loss(0, -1, 1, 0.5), 2)
  expect_equal(interval_loss(2, -1, 1, 0.5), 6)
  expect_equal(interval_loss(-2, -1, 1, 0.5), 6)

  expect_equal(interval_loss(-2, -1, 1, 0.5), 6)
})
