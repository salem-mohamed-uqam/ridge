test_that("ridge_fit returns the correct number of coefficients", {

  set.seed(123)

  X <- matrix(
    rnorm(50),
    nrow = 10,
    ncol = 5
  )

  y <- rnorm(10)

  beta <- ridge_fit(X, y, lambda = 1)

  expect_length(beta, 6)
})
test_that("ridge_fit does not penalize the intercept", {

  set.seed(123)

  X <- matrix(
    rnorm(50),
    nrow = 10,
    ncol = 5
  )

  y <- rnorm(10)

  lambda <- 1

  beta <- ridge_fit(X, y, lambda)

  X_intercept <- cbind(1, X)

  P <- diag(ncol(X) + 1)
  P[1, 1] <- 0

  beta_expected <- solve(
    t(X_intercept) %*% X_intercept +
      lambda * P
  ) %*%
    t(X_intercept) %*% y

  expect_equal(
    beta,
    as.numeric(beta_expected)
  )
})

