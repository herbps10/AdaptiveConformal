method <- "SAOCP"
test_that("SAOCP: default initialization works", {
  result <- aci(method = method)

  expect_equal(result$method, method)
  expect_equal(result$alpha, 0.95)
  expect_equal(result$parameters$interval_constructor, "linear")
  expect_equal(result$parameters$conformity_score, "absolute_error")
  expect_equal(result$parameters$conditional, FALSE)
  expect_equal(result$parameters$D, 1)
  expect_equal(result$parameters$g, 8)
})

test_that("SAOCP: works with data at initialization", {
  result <- aci(c(1, 2, 3), c(0, 1, 2), method = method)

  expect_equal(result$Y, c(1, 2, 3))
  expect_true(is.matrix(result$predictions))
  expect_equal(result$predictions[, 1], c(0, 1, 2))
  expect_equal(result$covered, c(0, 0, 1))

  # Check that metrics are calculated
  expect_equal(result$metrics$coverage, 1/3)
  expect_equal(result$metrics$mean_interval_loss, 20.023155)
  expect_equal(result$metrics$mean_width, 1.05449196)
})

test_that("SAOCP: updating with new values", {
  result <- aci(method = method)

  result <- update(result, newY = 1, newpredictions = 0)
  result <- update(result, newY = 2, newpredictions = 1)
  result <- update(result, newY = 3, newpredictions = 2)

  expect_equal(result$Y, c(1, 2, 3))
  expect_true(is.matrix(result$predictions))
  expect_equal(result$predictions[, 1], c(0, 1, 2))
  expect_equal(result$covered, c(0, 0, 1))

  # Check that metrics are calculated
  expect_equal(result$metrics$coverage, 1/3)
  expect_equal(result$metrics$mean_interval_loss, 20.023155)
  expect_equal(result$metrics$mean_width, 1.05449196)
})

test_that("SAOCP: X at initialization", {
  X <- matrix(c(1, 0, 0, 1, 1, 1, 1, 0, 1), ncol = 3, nrow = 3)
  result <- aci(c(1, 2, 3), c(0, 1, 2), X = X, method = "SF-OGD", parameters = list(gamma = 1))

  expect_equal(result$X, X)
  expect_equal(result$metrics$conditional_coverage, matrix(c(0, 1/3, 0), nrow = 1))
})

test_that("SAOCP: updates with X", {
  X <- matrix(c(1, 0, 0, 1, 1, 1, 1, 0, 1), ncol = 3, nrow = 3)
  result <- aci(X = matrix(ncol = 3, nrow = 0), method = "SF-OGD", parameters = list(gamma = 1))

  result <- update(result, newY = 1, newpredictions = 0, newX = X[1, ])
  result <- update(result, newY = 2, newpredictions = 1, newX = X[2, ])
  result <- update(result, newY = 3, newpredictions = 2, newX = X[3, ])

  expect_equal(result$X, X)
  expect_equal(result$metrics$conditional_coverage, matrix(c(0, 1/3, 0), nrow = 1))
})


test_that("SAOCP: supplying values all at once is equivalent to supplying values one at a time", {
  Y <- rnorm(200, 0, 1)
  predictions <- rnorm(200, 0, 1)
  result1 <- aci(Y = Y, predictions = predictions, method = "SAOCP", alpha = 0.9)

  result2 <- aci(method = "SAOCP", alpha = 0.9)
  for(i in 1:200) {
    result2 <- update(result2, newY = Y[i], newpredictions = predictions[i])
  }

  expect_equal(result1$Y, result2$Y)
  expect_equal(result1$intervals[, 1], result2$intervals[, 1])
  expect_equal(result1$intervals[, 2], result2$intervals[, 2])
})
