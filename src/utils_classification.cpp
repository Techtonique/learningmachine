#include <Rcpp.h>
#include <algorithm>

/* find empirical quantiles of a vector*/
// [[Rcpp::export]]
double empirical_quantile_cpp(Rcpp::NumericVector x, double q) {
  
  // Sort the input vector
  Rcpp::NumericVector sorted_x = Rcpp::clone(x).sort();
  
  // Calculate the position in the sorted array
  int n = sorted_x.size();
  double h = (n - 1) * q;
  int h_floor = std::floor(h);
  int h_ceil = std::ceil(h);
  
  // Handle case where h is an integer
  if (h_floor == h_ceil) {
    return sorted_x[h_floor];
  }
  
  // Interpolate between the two closest ranks
  double fraction = h - h_floor;
  return (1 - fraction) * sorted_x[h_floor] + fraction * sorted_x[h_ceil];
}


/* find empirical quantiles by row*/
// [[Rcpp::export]]
Rcpp::NumericVector row_quantiles_cpp(Rcpp::NumericMatrix mat, double q) {
  int n_rows = mat.nrow();
  Rcpp::NumericVector quantiles(n_rows);
  
  for (int i = 0; i < n_rows; ++i) {
    quantiles[i] = empirical_quantile_cpp(mat(i, Rcpp::_), q);
  }
  
  return quantiles;
}

// [[Rcpp::export]]
Rcpp::List calculate_quantiles_cpp(Rcpp::List sims, double level, int n_classes) {
  double q_lower = 0.5 * (1 - level / 100);
  double q_upper = 1 - q_lower;
  
  Rcpp::List preds_lower(n_classes);
  Rcpp::List preds_upper(n_classes);
  
  for (int i = 0; i < n_classes; ++i) {
    Rcpp::NumericMatrix sim = sims[i];
    preds_lower[i] = row_quantiles_cpp(sim, q_lower);
    preds_upper[i] = row_quantiles_cpp(sim, q_upper);
  }
  
  return Rcpp::List::create(
    Rcpp::Named("preds_lower") = preds_lower,
    Rcpp::Named("preds_upper") = preds_upper
  );
}


// [[Rcpp::export]]
Rcpp::NumericMatrix simulate_gaussian_mixture_cpp(Rcpp::NumericVector x, 
                                                  unsigned long int n, 
                                                  unsigned int p, 
                                                  double width)
{
  Rcpp::NumericMatrix result(n, p);
  for (unsigned int j = 0; j < p; ++j) {
    result(Rcpp::_, j) = Rcpp::sample(x, n, true) + Rcpp::rnorm(n, 0.0, width);
  }
  return(result);
}