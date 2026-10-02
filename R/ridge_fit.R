#' Fit a ridge regression model
#'
#' Fits a ridge regression model using a penalty parameter
#' lambda. The intercept is not penalized.
#'
#' @param X A numeric matrix of predictors.
#' @param y A numeric response vector.
#' @param lambda A non-negative penalty parameter.
#'
#' @return A numeric vector containing the estimated coefficients.
#'
#' @export
ridge_fit <- function(X, y, lambda = 1) {

  ridge(X, y, lambda)
}
