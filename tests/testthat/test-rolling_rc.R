test_that("RollingRC default initialization works", {
  result <- aci(method = "RollingRC")

  expect_equal(result$method, "RollingRC")
  expect_equal(result$alpha, 0.95)
  expect_equal(result$parameters$gamma, 0.01)
  expect_equal(result$parameters$interval_constructor, "conformal")
  expect_equal(result$parameters$conformity_score, "absolute_error")

  # By default, theta should start at alpha
  expect_equal(result$internal$theta[1], 0.95)
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
})
