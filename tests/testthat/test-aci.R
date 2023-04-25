test_that("Methods are tested", {
  expect_error(aci(method = "xxx"))

  for(method in c("RollingRC", "AgACI", "FACI")) {
    x <- aci(method = method)
    expect_equal(x$method, method)
  }
})

test_that("Initialization works", {
  x <- aci(method = "AgACI")
  expect_equal(x$alpha, 0.95)

  x <- aci(method = "AgACI", alpha = 0.9)
  expect_equal(x$alpha, 0.9)
})
