#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fastSampleCpp(NumericVector resids, int n) {
  int n_resids = resids.size();
  NumericVector result(n);
  for (int i = 0; i < n; i++) {
    result[i] = resids[rand() % n_resids];
  }
  return result;
}