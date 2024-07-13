#include <Rcpp.h>
using namespace Rcpp;

// https://gallery.rcpp.org/articles/parallel-distance-matrix/

/* 0 - utils */

// [[Rcpp::export]]
double crossprod_cpp(NumericVector x, NumericVector y)
{
  unsigned long int n = x.size();
  if (y.size() != n) {
    ::Rf_error("you must have x.size() == y.size()");
  }
  
  double res = 0;
  
  for(unsigned long int i = 0; i < n; i++) {
    res += x(i)*y(i);
  }
  
  return(res);
}


// [[Rcpp::export]]
double l2_norm(NumericVector x)
{
  unsigned long int n = x.size();
  double res = 0;
  
  for(unsigned long int i = 0; i < n; i++) {
    res += pow(x(i), 2);
  }
  
  return(sqrt(res));
}


// [[Rcpp::export]]
NumericMatrix na_matrix(unsigned int n, unsigned int p)
{
  NumericMatrix m(n,p) ;
  std::fill(m.begin(), m.end(), NumericVector::get_na()) ;
  return m ;
}


// [[Rcpp::export]]
double weighted_l2_norm(NumericVector x, NumericVector l)
{
  unsigned long int n = x.size();
  if (l.size() != n) {
    ::Rf_error("you must have x.size() == l.size()");
  }
  double res = 0;
  for(unsigned long int i = 0; i < n; i++) {
    res += pow(x(i), 2)/pow(l(i), 2);
  }
  return(sqrt(res));
}


/* 1 - Matérn 3/2 kernel */

// [[Rcpp::export]]
NumericMatrix matern32_kxx_cpp(NumericMatrix x, 
                               NumericVector l)
{
  unsigned long int n = x.nrow();
  NumericMatrix res(n, n);
  double sqrt3 = sqrt(3);
  double temp = 0;
  
  for(unsigned long int i = 0; i < n; i++) {
    for(unsigned long int j = i; j < n; j++) {
      temp = sqrt3*weighted_l2_norm(x(i, _) - x(j, _), l);
      res(i , j) = (1 + temp)*exp(-temp);
      res(j , i) = res(i , j);
    }
  }
  
  return(res);
}


// [[Rcpp::export]]
NumericMatrix matern32_kxstar_cpp(NumericMatrix newx, 
                                  NumericMatrix x,
                                  NumericVector l)
{
  unsigned long int m = newx.nrow();
  unsigned long int n = x.nrow();
  NumericMatrix res(m, n);
  double temp = 0;
  double sqrt3 = sqrt(3);
  
  for(unsigned long int i = 0; i < m; i++) {
    for(unsigned long int j = 0; j < n; j++) {
      temp = sqrt3*weighted_l2_norm(newx(i, _) - x(j, _), l);
      res(i , j) = (1 + temp)*exp(-temp);
    }
  }
  
  return(res);
}



/* 2 - Derivatives */

/* 2 - 1 Derivatives Eq.8 and Eq.9 */
// [[Rcpp::export]]
List derivs(NumericMatrix x, // matrix of inputs
            NumericVector c, // coefficients (y = K*c)
            double l) //lengthscale parameter 
{
  
  unsigned long int n = x.nrow();
  unsigned long int p = x.ncol();
  
  if (c.size() != n) {
    ::Rf_error("in derivs: you must have c.size() == x.nrow()");
  }
  
  double r = 0;
  NumericVector vec(n);
  double n2 = pow(n, 2);
  double temp = sqrt(3)/l;
  double const_mult = pow(temp, 2);
  NumericMatrix deriv1 = na_matrix(n2, p);
  NumericMatrix deriv2 = na_matrix(n2, p);
  double temp2 = 0;
  //unsigned long int i = 0;
  
  /*
   for(unsigned long int i0 = 0; i0 < n; i0++){
   for(unsigned long int k = 0; k < n; k++){ // There is something to optimize here (?)
   vec = x(i0, _) - x(k, _);
   r = l2_norm(vec);
   temp2 = c(k)*exp(-temp*r); // temp = sqrt(3)/l;
   deriv1(i, _) = temp2*vec; // first derivative
   //Rcout << "The value of deriv1(i, _): " << deriv1(i, 0) << std::endl;
   deriv2(i, _) = temp2*((temp/r)*pow(vec, 2) - 1); // second derivative
   i++;
   }
   }
   */
  
  // https://gallery.rcpp.org/articles/parallel-distance-matrix/
  for(unsigned long int k = 0; k < n; k++){ // There is something to optimize here (?)
    for(unsigned long int i0 = k; i0 < n; i0++){
      vec = x(i0, _) - x(k, _);
      r = l2_norm(vec);
      temp2 = c(k)*exp(-temp*r); // temp = sqrt(3)/l;
      deriv1(i0*n+k, _) = temp2*vec; // first derivative
      deriv1(k*n+i0, _) = deriv1(i0*n+k, _);
      deriv2(i0*n+k, _) = temp2*((temp/r)*pow(vec, 2) - 1); // second derivative
      deriv2(k*n+i0, _) = deriv2(i0*n+k, _);
    }
  }
  
  
  return List::create(Rcpp::Named("deriv1") = const_mult*deriv1,
                      Rcpp::Named("deriv2") = const_mult*deriv2);
}

/* 2 - 2 Derivatives Eq.10 */
//[[Rcpp::export]]
NumericMatrix inters(NumericMatrix x, // matrix of covariates
                     unsigned long int j1, // index of first column
                     unsigned long int j2, // index of second column
                     NumericVector c, // coefficients (so that y = K*c)
                     double l) //lengthscale parameter 
{
  
  unsigned long int n = x.nrow();
  unsigned long int p = x.ncol();
  j1 = j1 - 1; //!!! beware of R indices starting at 0
  j2 = j2 - 1; //!!! beware of R indices starting at 0
  
  if (c.size() != n) {
    ::Rf_error("in inters: you must have c.size() == x.nrow()");
  }
  
  if (j1 == j2) {
    ::Rf_error("in inters: you must have j1 != j2"); //Different columns indices
  }
  
  if (j1 >= p || j2 >= p) {
    ::Rf_error("in inters: you must have j1 < x.ncol() and j2 < x.ncol()"); //!!! beware of R indices starting at 0 
  }
  
  double r = 0;
  double temp = sqrt(3)/l;
  double temp2 = 0;
  double const_mult = pow(temp, 3);
  NumericMatrix res(n, n);
  
  for(unsigned long int i0 = 0; i0 < n; i0++){
    for(unsigned long int k = i0; k < n; k++){ 
      temp2 = (x(i0, j1) - x(k, j1))*(x(i0, j2) - x(k, j2));
      r = l2_norm(x(i0, _) - x(k, _));
      res(i0, k) = (c(k)/r)*exp(-temp*r)*temp2; // 2nd derivative
      res(k, i0) = res(i0, k);
    }
  }
  
  return (const_mult*res);
}


//[[Rcpp::export]]
NumericVector inters2(NumericMatrix x, // matrix of covariates
                      unsigned long int j1, // index of first column
                      unsigned long int j2, // index of second column
                      NumericVector c, // coefficients (so that y = K*c)
                      double l) //lengthscale parameter 
{
  
  unsigned long int n = x.nrow();
  unsigned long int p = x.ncol();
  j1 = j1 - 1; //!!! beware of R indices starting at 0
  j2 = j2 - 1; //!!! beware of R indices starting at 0
  
  if (c.size() != n) {
    ::Rf_error("in inters2: you must have c.size() == x.nrow()");
  }
  
  if (j1 == j2) {
    ::Rf_error("in inters2: you must have j1 != j2"); //Different columns indices
  }
  
  if (j1 >= p || j2 >= p) {
    ::Rf_error("in inters2: you must have j1 < x.ncol() and j2 < x.ncol()"); //!!! beware of R indices starting at 0 
  }
  
  double r = 0;
  double temp = sqrt(3)/l;
  double temp2 = 0;
  double const_mult = pow(temp, 3);
  NumericVector res(pow(n, 2));
  
  for(unsigned long int i0 = 0; i0 < n; i0++){
    for(unsigned long int k = i0; k < n; k++){ // There is something to optimize here (?)
      temp2 = (x(i0, j1) - x(k, j1))*(x(i0, j2) - x(k, j2));
      r = l2_norm(x(i0, _) - x(k, _));
      res(i0*n + k) = (c(k)/r)*exp(-temp*r)*temp2; // 2nd derivative // byrow=TRUE
      res(k*n + i0) = res(i0*n + k);
    }
  }
  
  return (const_mult*res);
}



/* 3 - Eigen decomp... */


//[[Rcpp::export]]
List solve_eigen(NumericMatrix Eigenvectors,
                 const NumericVector Eigenvalues,
                 const NumericVector y,
                 const double reg_lambda)
{
  
  unsigned long int N = Eigenvectors.nrow(); //Number observations
  unsigned long int K = Eigenvectors.ncol(); //Number of eigen vectors 
  if (Eigenvalues.size() != K) {
    ::Rf_error("you must have Eigenvalues.size() == Eigenvectors.ncol()");
  }
  
  // K at most N. Typically smaller (based on user eigentruncation input)
  NumericVector loocv(N); // leave one out error loss
  // coefficients
  NumericVector coeffs(N);
  //Ginv_diag
  NumericVector Ginv_diag(N);
  // temporary line
  NumericVector temp(N);
  
  for(unsigned long int i = 0; i < N; ++i){
    for(unsigned long int j = 0; j < N; ++j){
      temp(j) = sum(Eigenvectors(i, _)*Eigenvectors(j, _)/(reg_lambda + Eigenvalues));
      if (i == j){
        Ginv_diag(j) = temp[j];
      }
    }
    coeffs(i) = crossprod_cpp(temp, y);
    loocv(i) = coeffs(i)/Ginv_diag(i);
  }
  
  return List::create(Rcpp::Named("loocv") = loocv,
                      Rcpp::Named("coeffs") = coeffs);
}


//[[Rcpp::export]]
double find_lam_eigen(NumericMatrix Eigenvectors,
                      const NumericVector Eigenvalues,
                      const NumericVector y,
                      NumericVector reg_lambda_vector)
{
  
  unsigned long int N = Eigenvectors.nrow(); //Number observations
  unsigned long int K = Eigenvectors.ncol(); //Number of eigen vectors 
  if (Eigenvalues.size() != K) {
    ::Rf_error("you must have Eigenvalues.size() == Eigenvectors.ncol()");
  }
  
  // K at most N. Typically smaller (based on user eigentruncation input)
  NumericVector loocv(N); // leave one out error loss
  // coefficients
  NumericVector coeffs(N);
  //Ginv_diag
  NumericVector Ginv_diag(N);
  // temporary line
  NumericVector temp(N);
  // loocv error
  double rmse_loocv = 0;
  double rmse_loocv_prev = 1000000;
  //optimal index;
  unsigned long int i_opt;
  // number of reg_lambdas
  unsigned long int n_reg_lambdas = reg_lambda_vector.size();
  
  for(unsigned long int i = 0; i < n_reg_lambdas; ++i)
  {
    loocv = solve_eigen(Eigenvectors, Eigenvalues,
                        y, reg_lambda_vector(i))(1);
    
    rmse_loocv = sqrt(sum(pow(loocv, 2)));
    
    if (rmse_loocv <= rmse_loocv_prev)
    {
      i_opt = i;
    }
    
    rmse_loocv_prev = rmse_loocv;
  }
  
  return (reg_lambda_vector(i_opt));
}