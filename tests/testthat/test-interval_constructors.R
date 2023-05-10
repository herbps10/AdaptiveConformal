#test_that("interval_constructor_conformity works", {
#})

test_that("interval_constructor_linear with symmetry", {
  f <- interval_constructor_linear(symmetric = TRUE)

  expect_equal(f(0, 1, list()), c(-1, 1))
  expect_equal(f(0, 0.25, list()), c(-0.25, 0.25))

  expect_equal(f(c(0, 1), 1), c(-1, 2))
  expect_equal(f(c(-2, 5), 0.25), c(-2.25, 5.25))
  expect_equal(f(c(0, 1), -2), c(0.5, 0.5))
})

test_that("interval_constructor_linear with asymmetry", {
  f <- interval_constructor_linear(symmetric = FALSE)

  expect_equal(f(0, c(1, 1), list()), c(-1, 1))
  expect_equal(f(0, c(0.25, 0.25), list()), c(-0.25, 0.25))

  expect_equal(f(c(0, 1), c(1, 1)), c(-1, 2))
  expect_equal(f(c(-2, 5), c(0.25, 0.25)), c(-2.25, 5.25))
  expect_equal(f(c(0, 1), c(-2, -2)), c(0.5, 0.5))
})
