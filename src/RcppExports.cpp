// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

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
List solve_eigen(NumericMatrix Eigenvectors, const NumericVector Eigenvalues, const NumericVector y, const double reg_lambda);
RcppExport SEXP _learningmachine_solve_eigen(SEXP EigenvectorsSEXP, SEXP EigenvaluesSEXP, SEXP ySEXP, SEXP reg_lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Eigenvectors(EigenvectorsSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type Eigenvalues(EigenvaluesSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double >::type reg_lambda(reg_lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(solve_eigen(Eigenvectors, Eigenvalues, y, reg_lambda));
    return rcpp_result_gen;
END_RCPP
}
// find_lam_eigen
double find_lam_eigen(NumericMatrix Eigenvectors, const NumericVector Eigenvalues, const NumericVector y, NumericVector reg_lambda_vector);
RcppExport SEXP _learningmachine_find_lam_eigen(SEXP EigenvectorsSEXP, SEXP EigenvaluesSEXP, SEXP ySEXP, SEXP reg_lambda_vectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Eigenvectors(EigenvectorsSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type Eigenvalues(EigenvaluesSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type reg_lambda_vector(reg_lambda_vectorSEXP);
    rcpp_result_gen = Rcpp::wrap(find_lam_eigen(Eigenvectors, Eigenvalues, y, reg_lambda_vector));
    return rcpp_result_gen;
END_RCPP
}
// fastSampleCpp
NumericVector fastSampleCpp(NumericVector resids, int n);
RcppExport SEXP _learningmachine_fastSampleCpp(SEXP residsSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type resids(residsSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(fastSampleCpp(resids, n));
    return rcpp_result_gen;
END_RCPP
}
// empirical_quantile_cpp
double empirical_quantile_cpp(Rcpp::NumericVector x, double q);
RcppExport SEXP _learningmachine_empirical_quantile_cpp(SEXP xSEXP, SEXP qSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type q(qSEXP);
    rcpp_result_gen = Rcpp::wrap(empirical_quantile_cpp(x, q));
    return rcpp_result_gen;
END_RCPP
}
// row_quantiles_cpp
Rcpp::NumericVector row_quantiles_cpp(Rcpp::NumericMatrix mat, double q);
RcppExport SEXP _learningmachine_row_quantiles_cpp(SEXP matSEXP, SEXP qSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< double >::type q(qSEXP);
    rcpp_result_gen = Rcpp::wrap(row_quantiles_cpp(mat, q));
    return rcpp_result_gen;
END_RCPP
}
// calculate_quantiles_cpp
Rcpp::List calculate_quantiles_cpp(Rcpp::List sims, double level, int n_classes);
RcppExport SEXP _learningmachine_calculate_quantiles_cpp(SEXP simsSEXP, SEXP levelSEXP, SEXP n_classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type sims(simsSEXP);
    Rcpp::traits::input_parameter< double >::type level(levelSEXP);
    Rcpp::traits::input_parameter< int >::type n_classes(n_classesSEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_quantiles_cpp(sims, level, n_classes));
    return rcpp_result_gen;
END_RCPP
}
// simulate_gaussian_mixture_cpp
Rcpp::NumericMatrix simulate_gaussian_mixture_cpp(Rcpp::NumericVector x, unsigned long int n, unsigned int p, double width);
RcppExport SEXP _learningmachine_simulate_gaussian_mixture_cpp(SEXP xSEXP, SEXP nSEXP, SEXP pSEXP, SEXP widthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned long int >::type n(nSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    rcpp_result_gen = Rcpp::wrap(simulate_gaussian_mixture_cpp(x, n, p, width));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
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
    {"_learningmachine_fastSampleCpp", (DL_FUNC) &_learningmachine_fastSampleCpp, 2},
    {"_learningmachine_empirical_quantile_cpp", (DL_FUNC) &_learningmachine_empirical_quantile_cpp, 2},
    {"_learningmachine_row_quantiles_cpp", (DL_FUNC) &_learningmachine_row_quantiles_cpp, 2},
    {"_learningmachine_calculate_quantiles_cpp", (DL_FUNC) &_learningmachine_calculate_quantiles_cpp, 3},
    {"_learningmachine_simulate_gaussian_mixture_cpp", (DL_FUNC) &_learningmachine_simulate_gaussian_mixture_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_learningmachine(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
