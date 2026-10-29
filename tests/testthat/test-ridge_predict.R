
test_that("ridge_predict returns correct predictions", {

  X_new <- matrix(c(4, 5), ncol = 1)
  beta <- c(2, 3)

  predictions <- ridge_predict(X_new, beta)

  expect_equal(predictions, c(14, 17))
})

test_that("ridge_predict rejects incompatible dimensions", {

  X_new <- matrix(c(1, 2, 3, 4), ncol = 2)
  beta <- c(1, 2)

  expect_error(ridge_predict(X_new, beta))
})

test_that("ridge_predict rejects invalid coefficients", {

  X_new <- matrix(c(1, 2), ncol = 1)
  beta <- c(NA_real_, 2)

  expect_error(ridge_predict(X_new, beta))
})
