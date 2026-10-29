
#' Predict using a Ridge regression model
#'
#' Computes predicted response values using the coefficients
#' estimated by the ridge function.
#'
#' @param X A numeric matrix or data frame of predictors.
#' @param beta A numeric vector of coefficients returned by ridge().
#' The first coefficient is the intercept.
#'
#' @return A numeric vector of predicted values.
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4), ncol = 1)
#' beta <- c(0.5, 1.8)
#' ridge_predict(X, beta)
#'
#' @export
ridge_predict <- function(X, beta) {

  X <- as.matrix(X)
  beta <- as.numeric(beta)

  # Check the inputs
  if (!is.numeric(X) || !is.numeric(beta)) {
    stop("X and beta must be numeric.")
  }

  if (ncol(X) + 1 != length(beta)) {
    stop("beta must contain one intercept and one coefficient per column of X.")
  }

  if (any(!is.finite(X)) || any(!is.finite(beta))) {
    stop("X and beta must contain only finite values.")
  }

  # Add the intercept column
  X_intercept <- cbind(Intercept = 1, X)

  # Calculate predictions
  y_pred <- as.vector(X_intercept %*% beta)

  return(y_pred)
}
