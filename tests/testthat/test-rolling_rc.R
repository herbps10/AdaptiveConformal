test_that("RollingRC: default initialization", {
  result <- aci(method = "RollingRC")

  expect_equal(result$method, "RollingRC")
  expect_equal(result$alpha, 0.95)
  expect_equal(result$parameters$gamma, 0.01)
  expect_equal(result$parameters$interval_constructor, "conformal")
  expect_equal(result$parameters$conformity_score, "absolute_error")

  # By default, theta should start at alpha
  expect_equal(result$internal$theta[1], 0.95)
})

test_that("RollingRC: works with data at initialization", {
  result <- aci(c(1, 2, 3), c(0, 1, 2), method = "RollingRC")

  expect_equal(result$Y, c(1, 2, 3))
  expect_true(is.matrix(result$predictions))
  expect_equal(result$predictions[, 1], c(0, 1, 2))
  expect_equal(result$covered, c(0, 1, 1))

  # Check that metrics are calculated
  expect_equal(result$coverage, 2/3)
  expect_equal(result$mean_interval_loss, 14 + 2/3)
  expect_equal(result$mean_width, 1 + 1/3)

  # Prediction
  expect_equal(unname(predict(result)), c(-1, 1))
  expect_equal(unname(predict(result, prediction = 1)), c(0, 2))
})

test_that("RollingRC: updating with new values", {
  result <- aci(method = "RollingRC")

  result <- update(result, newY = 1, newpredictions = 0)
  result <- update(result, newY = 2, newpredictions = 1)
  result <- update(result, newY = 3, newpredictions = 2)

  expect_equal(result$Y, c(1, 2, 3))
  expect_true(is.matrix(result$predictions))
  expect_equal(result$predictions[, 1], c(0, 1, 2))
  expect_equal(result$covered, c(0, 1, 1))

  # Check that metrics are calculated
  expect_equal(result$coverage, 2/3)
  expect_equal(result$mean_interval_loss, 14 + 2/3)
  expect_equal(result$mean_width, 1 + 1/3)

  # Prediction
  expect_equal(unname(predict(result)), c(-1, 1))
  expect_equal(unname(predict(result, prediction = 1)), c(0, 2))
})

test_that("RollingRC: X at initialization", {
  X <- matrix(c(1, 0, 0, 1, 1, 1, 1, 0, 1), ncol = 3, nrow = 3)
  result <- aci(c(1, 2, 3), c(0, 1, 2), X = X, method = "RollingRC")

  expect_equal(result$X, X)
  expect_equal(result$conditional_coverage, matrix(c(0, 2/3, 0.5), nrow = 1))
})

test_that("RollingRC: updates with X", {
  X <- matrix(c(1, 0, 0, 1, 1, 1, 1, 0, 1), ncol = 3, nrow = 3)
  result <- aci(X = matrix(ncol = 3, nrow = 0), method = "RollingRC")

  result <- update(result, newY = 1, newpredictions = 0, newX = X[1, ])
  result <- update(result, newY = 2, newpredictions = 1, newX = X[2, ])
  result <- update(result, newY = 3, newpredictions = 2, newX = X[3, ])

  expect_equal(result$X, X)
  expect_equal(result$conditional_coverage, matrix(c(0, 2/3, 0.5), nrow = 1))
})

test_that("Test multiple iterations of RollingRC with conformity scores", {
  result <- aci(method = "RollingRC")

  # Check that theta starts at the right place
  expect_equal(result$internal$theta[1], 0.95)
  expect_equal(predict(result, prediction = 0), c("2.5%" = 0, "97.5%" = 0))

  # Update with a new observation that falls outside the prediction interval
  result <- update(result, newY = 1, newpredictions = 0)

  expect_equal(result$Y[1], 1)
  expect_equal(result$covered[1], 0)
  expect_equal(result$internal$theta[2], 0.95 + 0.01 * 0.95)
  expect_equal(predict(result, prediction = 0), c("2.5%" = -1, "97.5%" = 1))

  # Update with a new observation that falls inside the prediction interval
  result <- update(result, newY = 0.5, newpredictions = 0)
  expect_equal(result$Y[2], 0.5)
  expect_equal(result$covered[2], 1)
  expect_equal(result$internal$theta[3], result$internal$theta[2] + 0.01 * (-0.05))
  expect_equal(predict(result, prediction = 0), c("2.5%" = -1, "97.5%" = 1))

  # Prediction
  expect_equal(unname(predict(result)), c(-1, 1))
  expect_equal(unname(predict(result, prediction = 1)), c(0, 2))
})
