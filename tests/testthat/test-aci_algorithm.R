test_that("ACI: default initialization", {
  result <- aci(method = "ACI")

  expect_equal(result$method, "ACI")
  expect_equal(result$alpha, 0.95)
  expect_equal(result$parameters$gamma, 0.01)
  expect_equal(result$parameters$interval_constructor, "conformal")
  expect_equal(result$parameters$conformity_score, "absolute_error")

  # By default, theta should start at alpha
  expect_equal(result$internal$theta[1], 0.95)
})

test_that("ACI: checks parameters", {
  expect_no_error(aci(method = "ACI", parameters = list(interval_constructor = "linear")))
  expect_no_error(aci(method = "ACI", parameters = list(interval_constructor = "conformal")))
  expect_error(aci(method = "ACI", parameters = list(interval_constructor = "conformity")))

  expect_no_error(aci(method = "ACI", parameters = list(conformity_score = "absolute_error")))
  expect_error(aci(method = "ACI", parameters = list(conformity_score = "test")))
})

test_that("ACI: works with data at initialization", {
  result <- aci(c(1, 2, 3), c(0, 1, 2), method = "ACI")

  expect_equal(result$Y, c(1, 2, 3))
  expect_true(is.matrix(result$predictions))
  expect_equal(result$predictions[, 1], c(0, 1, 2))
  expect_equal(result$covered, c(0, 1, 1))

  # Check that metrics are calculated
  expect_equal(result$metrics$coverage, 2/3)
  expect_equal(result$metrics$mean_interval_loss, 14 + 2/3)
  expect_equal(result$metrics$mean_width, 1 + 1/3)

  # Prediction
  expect_equal(unname(predict(result)), c(-1, 1))
  expect_equal(unname(predict(result, prediction = 1)), c(0, 2))
})

test_that("ACI: updating with new values", {
  result <- aci(method = "ACI")

  result <- update(result, newY = 1, newpredictions = 0)
  result <- update(result, newY = 2, newpredictions = 1)
  result <- update(result, newY = 3, newpredictions = 2)

  expect_equal(result$Y, c(1, 2, 3))
  expect_true(is.matrix(result$predictions))
  expect_equal(result$predictions[, 1], c(0, 1, 2))
  expect_equal(result$covered, c(0, 1, 1))

  # Check that metrics are calculated
  expect_equal(result$metrics$coverage, 2/3)
  expect_equal(result$metrics$mean_interval_loss, 14 + 2/3)
  expect_equal(result$metrics$mean_width, 1 + 1/3)

  # Prediction
  expect_equal(unname(predict(result)), c(-1, 1))
  expect_equal(unname(predict(result, prediction = 1)), c(0, 2))
})

test_that("ACI: X at initialization", {
  X <- matrix(c(1, 0, 0, 1, 1, 1, 1, 0, 1), ncol = 3, nrow = 3)
  result <- aci(c(1, 2, 3), c(0, 1, 2), X = X, method = "ACI")

  expect_equal(result$X, X)
  expect_equal(result$metrics$conditional_coverage, matrix(c(0, 2/3, 0.5), nrow = 1))
})

test_that("ACI: updates with X", {
  X <- matrix(c(1, 0, 0, 1, 1, 1, 1, 0, 1), ncol = 3, nrow = 3)
  result <- aci(X = matrix(ncol = 3, nrow = 0), method = "ACI")

  result <- update(result, newY = 1, newpredictions = 0, newX = X[1, ])
  result <- update(result, newY = 2, newpredictions = 1, newX = X[2, ])
  result <- update(result, newY = 3, newpredictions = 2, newX = X[3, ])

  expect_equal(result$X, X)
  expect_equal(result$metrics$conditional_coverage, matrix(c(0, 2/3, 0.5), nrow = 1))
})

test_that("ACI: test multiple iterations with conformity scores", {
  result <- aci(method = "ACI")

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

test_that("ACI: accepts linear interval constructors", {
  result <- aci(c(1, 2, 3), c(0, 0, 0), method = "ACI", parameters = list(interval_constructor = "linear", gamma = 1))

  expect_equal(result$intervals[, 1], result$predictions[, 1] - result$internal$theta[1:3, 1])
  expect_equal(result$intervals[, 2], result$predictions[, 1] + result$internal$theta[1:3, 1])
})

test_that("ACI: generates linear asymmetric intervals", {
  result <- aci(c(1, 2, 3), c(0, 0, 0), method = "ACI", parameters = list(interval_constructor = "linear", symmetric = FALSE, gamma = 1))

  expect_equal(result$intervals[, 1], result$predictions[, 1] - result$internal$theta[1:3, 1])
  expect_equal(result$intervals[, 2], result$predictions[, 1] + result$internal$theta[1:3, 2])
})


test_that("ACI: supplying values all at once is equivalent to supplying values one at a time", {
  Y <- rnorm(200, 0, 1)
  predictions <- rnorm(200, 0, 1)
  result1 <- aci(Y = Y, predictions = predictions, method = "ACI", alpha = 0.9)

  result2 <- aci(method = "ACI", alpha = 0.9)
  for(i in 1:200) {
    result2 <- update(result2, newY = Y[i], newpredictions = predictions[i])
  }

  expect_equal(result1$Y, result2$Y)
  expect_equal(result1$intervals[, 1], result2$intervals[, 1])
  expect_equal(result1$intervals[, 2], result2$intervals[, 2])
})
