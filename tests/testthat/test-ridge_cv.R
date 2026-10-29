
test_that("ridge_cv retourne un lambda optimal et les erreurs", {
  X <- matrix(1:6, ncol = 1)
  y <- c(2, 4, 5, 8, 10, 11)

  resultat <- ridge_cv(
    X = X,
    y = y,
    lambdas = c(0.1, 1, 10),
    k = 3,
    seed = 123
  )

  expect_true(is.numeric(resultat$best_lambda))
  expect_length(resultat$best_lambda, 1)
  expect_true(all(c("lambda", "mse") %in%
                    names(resultat$cv_errors)))
})

test_that("ridge_cv retourne une erreur pour un k invalide", {
  X <- matrix(1:6, ncol = 1)
  y <- c(2, 4, 5, 8, 10, 11)

  expect_error(
    ridge_cv(X, y, lambdas = c(0.1, 1), k = 1)
  )
})

test_that("ridge_cv_cpp retourne un lambda optimal et les erreurs", {
  X <- matrix(1:6, ncol = 1)
  y <- c(2, 4, 5, 8, 10, 11)

  resultat <- ridge_cv_cpp(
    X = X,
    y = y,
    lambdas = c(0.1, 1, 10),
    k = 3,
    seed = 123
  )

  expect_true(is.numeric(resultat$best_lambda))
  expect_length(resultat$best_lambda, 1)
  expect_true(all(c("lambda", "mse") %in%
                    names(resultat$cv_errors)))
})

test_that("ridge_cv_cpp rejette un k invalide", {
  X <- matrix(1:6, ncol = 1)
  y <- c(2, 4, 5, 8, 10, 11)

  expect_error(
    ridge_cv_cpp(X, y, lambdas = c(0.1, 1), k = 1)
  )
})

test_that("ridge_plot accepte un résultat valide", {
  resultat <- list(
    best_lambda = 1,
    cv_errors = data.frame(
      lambda = c(0.1, 1, 10),
      mse = c(0.5, 0.2, 0.8)
    )
  )

  expect_no_error(ridge_plot(resultat))
})

test_that("ridge_plot rejette un résultat invalide", {
  expect_error(ridge_plot(list()), "cv_errors")
})
