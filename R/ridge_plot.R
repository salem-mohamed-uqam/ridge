
#' Tracer les erreurs de validation croisée Ridge
#'
#' Affiche l'erreur quadratique moyenne en fonction
#' des valeurs de lambda et indique le lambda optimal.
#'
#' @param resultat Objet retourné par ridge_cv() ou ridge_cv_cpp().
#' @return Un graphique des erreurs de validation croisée.
#' @examples
#' \dontrun{
#' X <- matrix(1:10, ncol = 1)
#' y <- c(2, 3, 5, 4, 6, 7, 8, 9, 10, 12)
#' resultat <- ridge_cv_cpp(X, y, c(0.1, 1, 10), k = 5)
#' ridge_plot(resultat)
#' }
#' @export
ridge_plot <- function(resultat) {

  if (is.null(resultat$cv_errors)) {
    stop("L'objet doit contenir cv_errors.")
  }

  erreurs <- resultat$cv_errors

  if (!all(c("lambda", "mse") %in% names(erreurs))) {
    stop("cv_errors doit contenir les colonnes lambda et mse.")
  }

  if (nrow(erreurs) < 1) {
    stop("Aucune erreur de validation croisée à afficher.")
  }

  lambda_optimal <- resultat$best_lambda

  plot(
    erreurs$lambda,
    erreurs$mse,
    type = "b",
    log = if (all(erreurs$lambda > 0)) "x" else "",
    xlab = "Lambda",
    ylab = "Erreur quadratique moyenne (MSE)",
    main = "Validation croisée Ridge"
  )

  if (!is.null(lambda_optimal) && length(lambda_optimal) == 1) {
    graphics::abline(
      v = lambda_optimal,
      col = "red",
      lty = 2
    )
  }

  invisible(erreurs)
}

