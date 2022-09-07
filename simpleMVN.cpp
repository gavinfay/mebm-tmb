#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Minimal example */
  DATA_MATRIX(X);
  int nobs = X.cols();
  int n = X.rows();
  
  PARAMETER_VECTOR(mu);
  PARAMETER_VECTOR(logSigma);
  PARAMETER(rhopar);

  Type rho = 2.0 / (1.0 + exp(-rhopar)) - 1.0;
  //rho = (2.0*exp(rhopar)/(1.0+exp(rhopar)))-1.0;
  
  
  vector<Type> sd = exp(logSigma);
  matrix<Type> Sigma(n,n);
  
  Sigma.row(0) << sd[0]*sd[0], sd[0]*sd[1]*rho, sd[0]*sd[2]*rho;
  Sigma.row(1) << sd[1]*sd[0]*rho, sd[1]*sd[1], sd[1]*sd[2]*rho;
  Sigma.row(2) << sd[2]*sd[0]*rho, sd[2]*sd[1]*rho, sd[2]*sd[2];
  
  Type nll = 0.0;
  
  using namespace density;
  MVNORM_t<Type> neg_log_dmvnorm(Sigma);
  
  vector<Type> residual(n);
  for (int iobs=0;iobs<nobs;iobs++) {
    residual = vector<Type>(X.row(iobs))-mu;
    nll += neg_log_dmvnorm(residual);
  }  
  
  REPORT(Sigma);
  REPORT(sd);
  REPORT(rho);
  
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
