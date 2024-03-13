// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// compute_probs_loop_cpp
List compute_probs_loop_cpp(unsigned long int n_x, unsigned long int n, unsigned long int B, ListOf<NumericMatrix> res, ListOf<NumericMatrix> x);
RcppExport SEXP _learningmachine_compute_probs_loop_cpp(SEXP n_xSEXP, SEXP nSEXP, SEXP BSEXP, SEXP resSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned long int >::type n_x(n_xSEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type n(nSEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type B(BSEXP);
    Rcpp::traits::input_parameter< ListOf<NumericMatrix> >::type res(resSEXP);
    Rcpp::traits::input_parameter< ListOf<NumericMatrix> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_probs_loop_cpp(n_x, n, B, res, x));
    return rcpp_result_gen;
END_RCPP
}
// crossprod_cpp
double crossprod_cpp(NumericVector x, NumericVector y);
RcppExport SEXP _learningmachine_crossprod_cpp(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(crossprod_cpp(x, y));
    return rcpp_result_gen;
END_RCPP
}
// l2_norm
double l2_norm(NumericVector x);
RcppExport SEXP _learningmachine_l2_norm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(l2_norm(x));
    return rcpp_result_gen;
END_RCPP
}
// na_matrix
NumericMatrix na_matrix(unsigned int n, unsigned int p);
RcppExport SEXP _learningmachine_na_matrix(SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(na_matrix(n, p));
    return rcpp_result_gen;
END_RCPP
}
// weighted_l2_norm
double weighted_l2_norm(NumericVector x, NumericVector l);
RcppExport SEXP _learningmachine_weighted_l2_norm(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_l2_norm(x, l));
    return rcpp_result_gen;
END_RCPP
}
// matern32_kxx_cpp
NumericMatrix matern32_kxx_cpp(NumericMatrix x, NumericVector l);
RcppExport SEXP _learningmachine_matern32_kxx_cpp(SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(matern32_kxx_cpp(x, l));
    return rcpp_result_gen;
END_RCPP
}
// matern32_kxstar_cpp
NumericMatrix matern32_kxstar_cpp(NumericMatrix newx, NumericMatrix x, NumericVector l);
RcppExport SEXP _learningmachine_matern32_kxstar_cpp(SEXP newxSEXP, SEXP xSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type newx(newxSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(matern32_kxstar_cpp(newx, x, l));
    return rcpp_result_gen;
END_RCPP
}
// derivs
List derivs(NumericMatrix x, NumericVector c, double l);
RcppExport SEXP _learningmachine_derivs(SEXP xSEXP, SEXP cSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(derivs(x, c, l));
    return rcpp_result_gen;
END_RCPP
}
// inters
NumericMatrix inters(NumericMatrix x, unsigned long int j1, unsigned long int j2, NumericVector c, double l);
RcppExport SEXP _learningmachine_inters(SEXP xSEXP, SEXP j1SEXP, SEXP j2SEXP, SEXP cSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type j1(j1SEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type j2(j2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(inters(x, j1, j2, c, l));
    return rcpp_result_gen;
END_RCPP
}
// inters2
NumericVector inters2(NumericMatrix x, unsigned long int j1, unsigned long int j2, NumericVector c, double l);
RcppExport SEXP _learningmachine_inters2(SEXP xSEXP, SEXP j1SEXP, SEXP j2SEXP, SEXP cSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type j1(j1SEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type j2(j2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(inters2(x, j1, j2, c, l));
    return rcpp_result_gen;
END_RCPP
}
// solve_eigen
List solve_eigen(NumericMatrix Eigenvectors, const NumericVector Eigenvalues, const NumericVector y, const double lambda);
RcppExport SEXP _learningmachine_solve_eigen(SEXP EigenvectorsSEXP, SEXP EigenvaluesSEXP, SEXP ySEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Eigenvectors(EigenvectorsSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type Eigenvalues(EigenvaluesSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(solve_eigen(Eigenvectors, Eigenvalues, y, lambda));
    return rcpp_result_gen;
END_RCPP
}
// find_lam_eigen
double find_lam_eigen(NumericMatrix Eigenvectors, const NumericVector Eigenvalues, const NumericVector y, NumericVector lambda_vector);
RcppExport SEXP _learningmachine_find_lam_eigen(SEXP EigenvectorsSEXP, SEXP EigenvaluesSEXP, SEXP ySEXP, SEXP lambda_vectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Eigenvectors(EigenvectorsSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type Eigenvalues(EigenvaluesSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lambda_vector(lambda_vectorSEXP);
    rcpp_result_gen = Rcpp::wrap(find_lam_eigen(Eigenvectors, Eigenvalues, y, lambda_vector));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _learningmachine_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_learningmachine_compute_probs_loop_cpp", (DL_FUNC) &_learningmachine_compute_probs_loop_cpp, 5},
    {"_learningmachine_crossprod_cpp", (DL_FUNC) &_learningmachine_crossprod_cpp, 2},
    {"_learningmachine_l2_norm", (DL_FUNC) &_learningmachine_l2_norm, 1},
    {"_learningmachine_na_matrix", (DL_FUNC) &_learningmachine_na_matrix, 2},
    {"_learningmachine_weighted_l2_norm", (DL_FUNC) &_learningmachine_weighted_l2_norm, 2},
    {"_learningmachine_matern32_kxx_cpp", (DL_FUNC) &_learningmachine_matern32_kxx_cpp, 2},
    {"_learningmachine_matern32_kxstar_cpp", (DL_FUNC) &_learningmachine_matern32_kxstar_cpp, 3},
    {"_learningmachine_derivs", (DL_FUNC) &_learningmachine_derivs, 3},
    {"_learningmachine_inters", (DL_FUNC) &_learningmachine_inters, 5},
    {"_learningmachine_inters2", (DL_FUNC) &_learningmachine_inters2, 5},
    {"_learningmachine_solve_eigen", (DL_FUNC) &_learningmachine_solve_eigen, 4},
    {"_learningmachine_find_lam_eigen", (DL_FUNC) &_learningmachine_find_lam_eigen, 4},
    {"_learningmachine_rcpp_hello_world", (DL_FUNC) &_learningmachine_rcpp_hello_world, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_learningmachine(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
