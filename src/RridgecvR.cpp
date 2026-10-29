
#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <random>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List ridge_cv_cpp(
    const arma::mat& X,
    const arma::colvec& y,
    const arma::colvec& lambdas,
    int k = 5,
    int seed = 123
) {
  const int n = X.n_rows;
  const int p = X.n_cols;

  // Vérification des données
  if (n != static_cast<int>(y.n_elem)) {
    Rcpp::stop("The number of rows in X must equal the length of y.");
  }

  if (n < 2 || p < 1) {
    Rcpp::stop("X must have at least two rows and one column.");
  }

  if (k < 2 || k > n) {
    Rcpp::stop("k must be between 2 and the number of observations.");
  }

  if (lambdas.n_elem < 1) {
    Rcpp::stop("At least one lambda value is required.");
  }

  if (!X.is_finite() || !y.is_finite() || !lambdas.is_finite()) {
    Rcpp::stop("Inputs must contain only finite values.");
  }

  if (arma::any(lambdas < 0)) {
    Rcpp::stop("Lambda values must be non-negative.");
  }

  // Créer des plis reproductibles et équilibrés
  std::vector<int> indices(n);
  std::iota(indices.begin(), indices.end(), 0);

  std::mt19937 generator(seed);
  std::shuffle(indices.begin(), indices.end(), generator);

  std::vector<int> folds(n);
  for (int i = 0; i < n; ++i) {
    folds[indices[i]] = i % k;
  }

  arma::colvec errors(lambdas.n_elem, arma::fill::zeros);

  // Évaluer chaque valeur de lambda
  for (arma::uword a = 0; a < lambdas.n_elem; ++a) {
    double total_error = 0.0;
    int total_test = 0;

    for (int fold = 0; fold < k; ++fold) {
      std::vector<int> train;
      std::vector<int> test;

      for (int i = 0; i < n; ++i) {
        if (folds[i] == fold) {
          test.push_back(i);
        } else {
          train.push_back(i);
        }
      }

      arma::mat X_train(train.size(), p + 1);
      arma::colvec y_train(train.size());

      for (int i = 0; i < static_cast<int>(train.size()); ++i) {
        X_train(i, 0) = 1.0;
        y_train[i] = y[train[i]];

        for (int j = 0; j < p; ++j) {
          X_train(i, j + 1) = X(train[i], j);
        }
      }

      // Ne pas pénaliser l'intercept
      arma::mat penalty = arma::eye(p + 1, p + 1);
      penalty(0, 0) = 0.0;

      arma::mat A =
        X_train.t() * X_train + lambdas[a] * penalty;
      arma::colvec rhs = X_train.t() * y_train;
      arma::colvec beta;

      bool solved = arma::solve(beta, A, rhs);

      if (!solved) {
        Rcpp::stop("Could not solve the Ridge regression system.");
      }

      // Prédictions sur le pli de test
      for (int idx : test) {
        double prediction = beta[0];

        for (int j = 0; j < p; ++j) {
          prediction += X(idx, j) * beta[j + 1];
        }

        double error = y[idx] - prediction;
        total_error += error * error;
        ++total_test;
      }
    }

    errors[a] = total_error / total_test;
  }

  // Sélection du lambda ayant la plus petite erreur
  arma::uword best_index = errors.index_min();

  Rcpp::DataFrame cv_errors = Rcpp::DataFrame::create(
    Rcpp::Named("lambda") = Rcpp::wrap(lambdas),
    Rcpp::Named("mse") = Rcpp::wrap(errors)
  );

  return Rcpp::List::create(
    Rcpp::Named("best_lambda") = lambdas[best_index],
                                        Rcpp::Named("cv_errors") = cv_errors
  );
}


