
#' Select the Ridge penalty parameter using cross-validation
#'
#' Performs k-fold cross-validation to select the lambda value
#' that minimizes the mean squared prediction error.
#'
#' @param X A numeric matrix or data frame of predictors.
#' @param y A numeric response vector.
#' @param lambdas A numeric vector of non-negative lambda values to test.
#' Defaults to 10 values between 0 and 10.
#' @param k Number of folds. Defaults to 5.
#' @param seed Random seed for reproducibility. Defaults to 123.
#'
#' @return A list containing the best lambda and the cross-validation
#' errors for all tested lambda values.
#'
#' @examples
#' X <- matrix(1:20, ncol = 1)
#' y <- 2 + 3 * X[, 1] + rnorm(20)
#' ridge_cv(X, y, k = 5)
#'
#' @export
ridge_cv <- function(X, y, lambdas = seq(0, 10, length.out = 10),
                     k = 5, seed = 123) {

  X <- as.matrix(X)
  y <- as.numeric(y)
  lambdas <- as.numeric(lambdas)

  # Check inputs
  if (!is.numeric(X) || !is.numeric(y)) {
    stop("X and y must be numeric.")
  }

  if (nrow(X) != length(y)) {
    stop("The number of rows in X must equal the length of y.")
  }

  if (any(!is.finite(X)) || any(!is.finite(y))) {
    stop("X and y must contain only finite values.")
  }

  if (length(lambdas) == 0 ||
      any(!is.finite(lambdas)) || any(lambdas < 0)) {
    stop("lambdas must contain non-negative finite values.")
  }

  if (length(k) != 1 || !is.finite(k) ||
      k != as.integer(k) || k < 2 || k > length(y)) {
    stop("k must be an integer between 2 and the number of observations.")
  }

  # Create folds
  set.seed(seed)
  folds <- sample(rep(seq_len(k), length.out = length(y)))

  # Store mean squared errors
  cv_errors <- numeric(length(lambdas))

  for (i in seq_along(lambdas)) {

    fold_errors <- numeric(k)

    for (j in seq_len(k)) {

      test_idx <- which(folds == j)
      train_idx <- which(folds != j)

      X_train <- X[train_idx, , drop = FALSE]
      y_train <- y[train_idx]
      X_test <- X[test_idx, , drop = FALSE]
      y_test <- y[test_idx]

      beta <- ridge(X_train, y_train, lambda = lambdas[i])
      predictions <- ridge_predict(X_test, beta)

      fold_errors[j] <- mean((y_test - predictions)^2)
    }

    cv_errors[i] <- mean(fold_errors)
  }

  best_index <- which.min(cv_errors)

  return(list(
    best_lambda = lambdas[best_index],
    cv_errors = data.frame(
      lambda = lambdas,
      mse = cv_errors
    )
  ))
}

