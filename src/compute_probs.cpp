#include <Rcpp.h>
using namespace Rcpp;

// https://gallery.rcpp.org/articles/parallel-distance-matrix/

/* 0 - utils */

// [[Rcpp::export]]
List compute_probs_loop_cpp(unsigned long int n_x, 
                        unsigned long int n,
                        unsigned long int B,
                        ListOf<NumericMatrix> res, 
                        ListOf<NumericMatrix> x)
{
  double sum_k; 
  for (unsigned long int key = 0; key < n_x; key++) {
    for (unsigned long int i = 0; i < n; i++) {
      for (unsigned long int j = 0; j < B; j++) {
        sum_k = 0; 
        for (unsigned long int k = 0; k < n_x; k++)
        {
          sum_k += x[k](i, j); 
        }
        res[key](i, j) = x[key](i, j) / sum_k;
      }
    }
  }
  return (res);
}

/*

 compute_probs_list <- function(x) {
 
# do this in Rcpp /!\
 n_x <- length(x)
 n <- dim(x[[1]])[1]
 B <- dim(x[[1]])[2]
 res <- x
 
 for (key in seq_len(n_x)) {
 for (i in seq_len(n)) {
 for (j in seq_len(B)) {
 res[[key]][i, j] <-
 x[[key]][i, j] / sum(sapply(seq_len(n_x), function(k)
 x[[k]][i, j]))
 }
 }
 }
 
 debug_print(res)
 
 debug_print(compute_probs_loop_cpp(n_x, n, B, res, x))
 
 names_x <- try(names(x), silent = TRUE)
 if(!inherits(names_x, "try-error"))
 names(res) <- names_x
 
#res$sims <- x
 return(res)
 }

  */