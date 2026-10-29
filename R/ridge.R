#' Estimate Ridge regression coefficients
#'
#' Estimates the coefficients of a Ridge regression model
#' for a fixed penalty parameter lambda.
#' The intercept is not penalized.
#'
#' @param X A numeric matrix or data frame of predictors.
#' @param y A numeric response vector.
#' @param lambda A single non-negative penalty parameter.
#' Defaults to 1.
#'
#' @return A numeric vector containing the estimated coefficients.
#' The intercept is the first coefficient.
#'
#' @export
ridge <- function(X, y, lambda = 1) {

  X <- as.matrix(X)
  y <- as.numeric(y)

  # Check the inputs
  if (!is.numeric(X) || !is.numeric(y)) {
    stop("X and y must be numeric.")
  }

  if (nrow(X) != length(y)) {
    stop("The number of rows in X must equal the length of y.")
  }

  if (length(lambda) != 1 || !is.finite(lambda) || lambda < 0) {
    stop("lambda must be a single non-negative number.")
  }

  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("X must have at least one row and one column.")
  }

  if (any(!is.finite(X)) || any(!is.finite(y))) {
    stop("X and y must contain only finite values.")
  }

  # Add the intercept column
  X_intercept <- cbind(Intercept = 1, X)

  # Create the penalty matrix
  # The intercept is not penalized
  penalty <- diag(ncol(X_intercept))
  penalty[1, 1] <- 0

  # Estimate the coefficients using the Ridge formula
  beta <- solve(
    crossprod(X_intercept) + lambda * penalty,
    crossprod(X_intercept, y)
  )

  # Return the estimated coefficients
  as.numeric(beta)
}

