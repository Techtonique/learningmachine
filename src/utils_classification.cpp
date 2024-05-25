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
