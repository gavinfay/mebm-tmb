#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  
  DATA_VECTOR(obs);  //Counts of pups
  DATA_IVECTOR(year);  //year relative to start year
  DATA_INTEGER(nyr);
  int nobs = obs.size();

  PARAMETER(log_lamda);  //growth rate
  PARAMETER(log_sigma);  //observation error sd
  PARAMETER(log_tau);    //process error sd
  PARAMETER(log_N0);   //number of pups in first year
  PARAMETER_VECTOR(u);  //pups
  
  Type nll = 0.; // initialize negative log likelihood
  
  // process model:
  Type lamda = exp(log_lamda);
  Type tau = exp(log_tau);
  Type N0 = exp(log_N0);  //initial state
  nll -= dnorm(u(0), log_lamda + log_N0, tau, true);
  for(int i = 1; i < nyr; i++){
    Type m = log_lamda + u(i-1); // population model
    nll -= dnorm(u(i), m, tau, true);
  }

  // // observation model:
   Type sigma = exp(log_sigma);
  vector<Type> pred(nobs);   //storage vector
  for(int i = 0; i < nobs; i++){
    if(year(i)==0) pred(i) = log_N0;
    if(year(i)!=0) pred(i) = u(year(i)-1);
  }
  nll -= sum(dnorm(log(obs), pred, sigma, true));

  //store time series of pups
  vector<Type> predN(nyr+1);   //storage vector
  predN(0) = N0;
  for(int i = 1; i <= nyr; i++){
    predN(i) = exp(u(i-1));
  }


  REPORT(pred);
  ADREPORT(lamda);
  ADREPORT(tau);
  ADREPORT(sigma);
  ADREPORT(predN);
    
  return nll;
  

  /* Quick Reference
     ===============

     ** Macros to read data and declare parameters:

     _Template_Syntax_              _C++_type_                     _R_type_
     DATA_VECTOR(name)              vector<Type>                   vector
     DATA_MATRIX(name)              matrix<Type>                   matrix
     DATA_SCALAR(name)              Type                           numeric(1)
     DATA_INTEGER(name)             int                            integer(1)
     DATA_FACTOR(name)              vector<int>                    factor
     DATA_SPARSE_MATRIX(name)       Eigen::SparseMatrix<Type>      dgTMatrix
     DATA_ARRAY(name)               array<Type>                    array
     PARAMETER_MATRIX(name)         matrix<Type>                   matrix
     PARAMETER_VECTOR(name)         vector<Type>                   vector
     PARAMETER_ARRAY(name)          array<Type>                    array
     PARAMETER(name)                Type                           numeric(1)

     ** Macro to report intermediate expressions back to R:

     REPORT(x)
     ADREPORT(x)

     ** Basic constructors:

     vector<Type> v(n1);
     matrix<Type> m(n1,n2);
     array<Type> a(n1,n2,n3)

     ** Basic operations:

     v+v,v-v,v*v,v/v                Pointwise binary operations
     m*v                            Matrix-vector multiply
     a.col(i)                       R equivalent of a[,,i]
     a.col(i).col(j)                R equivalent of a[,j,i]
     a(i,j,k)                       R equivalent of a[i,j,k]
     exp(v)                         Pointwise math
     m(i,j)                         R equivalent of m[i,j]
     v.sum()                        R equivalent of sum(v)
     m.transpose()                  R equivalent of t(m)

     ** Distributions:

     Type dnbinom2(const Type &x, const Type &mu, const Type &var, int give_log=0)
     Type dpois(const Type &x, const Type &lambda, int give_log=0)
     Type dlgamma(Type y, Type shape, Type scale, int give_log=0)
     Type dnorm(Type x, Type mean, Type sd, int give_log=0)

     ** Parallel accumulator declaration (only methods "+=" and "-="):
     
     parallel_accumulator<Type> res(this);

  */

}
