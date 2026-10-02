ridge <- function(X, y, lambda = 1) {

  X <- as.matrix(X)
  y <- as.numeric(y)

  p <- ncol(X)

  # Ajouter l'intercept
  X_intercept <- cbind(1, X)

  # Matrice de pénalisation
  P <- diag(p + 1)

  # Ne pas pénaliser beta_0
  P[1, 1] <- 0

  # Estimateur Ridge
  beta <- solve(
    t(X_intercept) %*% X_intercept +
      lambda * P
  ) %*%
    t(X_intercept) %*% y

  as.numeric(beta)
}
