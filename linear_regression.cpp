#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

template<class Type>
Type objective_function<Type>::operator() ()
{
  //data section
  DATA_VECTOR(x);    //data vector, passed in list object from R
  DATA_VECTOR(y);    //data vector, passed in list object from R
  int n = y.size();    //define an integer n that is the length of the y vector
  
  //parameter section
  PARAMETER(b0);    //model parameter to be estimated, real number
  PARAMETER(b1);    //model parameter to be estimated, real number
  PARAMETER(logSigma);    //model parameter to be estimated, real number
  
  vector<Type> ypred(n);   //object that will contain our model predictions
                           // note this is a function of model parameters, 
                           // therefore needs to be differntiable
                           // we also specify the size of the vector
                           
  Type neglogL = 0.0;    //objective function value. Initialize to 0.
  
  // specify the model & objective function

  ypred = b0 + b1*x;    // model predictions
  
  neglogL = -sum(dnorm(y, ypred, exp(logSigma), true));  //objective function value
  SIMULATE {
    y = rnorm(ypred,exp(logSigma));
    REPORT(y);
  }
  
  
  Type sigma2 = square(exp(logSigma));
  ADREPORT(sigma2);
  
  //adding priors
  //vector<Type> mu= log(0.2);
  //vector<Type> obs= Type(logSigma);
  //neglogL -= dnorm(obs,mu,0.1);
  //neglogL -= square((logSigma-log(0.2))/0.1);
  
  return neglogL;  //return the objective function
}
