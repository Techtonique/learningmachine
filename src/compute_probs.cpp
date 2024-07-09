#include <Rcpp.h>
using namespace Rcpp;

// https://gallery.rcpp.org/articles/parallel-distance-matrix/

/* 0 - utils */

// List compute_probs_loop_cpp(unsigned long int n_x, 
//                         unsigned long int n,
//                         unsigned long int B,
//                         ListOf<NumericMatrix> res, 
//                         ListOf<NumericMatrix> x)
// {
//   double sum_k; 
//   for (unsigned long int key = 0; key < n_x; key++) {
//     for (unsigned long int i = 0; i < n; i++) {
//       for (unsigned long int j = 0; j < B; j++) {
//         sum_k = 0; 
//         for (unsigned long int k = 0; k < n_x; k++)
//         {
//           sum_k += x[k](i, j); 
//         }
//         res[key](i, j) = x[key](i, j) / sum_k;
//       }
//     }
//   }
//   return (res);
// }