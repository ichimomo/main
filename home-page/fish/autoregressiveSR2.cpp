// ridge vpa

#include <TMB.hpp>
#include <iostream>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data section
  DATA_VECTOR(rec);
  DATA_VECTOR(ssb);
  DATA_INTEGER(SR); 
  DATA_SCALAR(gamma);
  
  // Parameter section
  PARAMETER(rec_loga);
  PARAMETER(rec_logb);
  PARAMETER(log_sigma); 
  PARAMETER(logit_rho); //autoregressive coefficient
  
  Type rho=1/(1+exp(-logit_rho));
  int n = rec.size(); // number of years

  using namespace density;
  vector<Type> x(n);           // Evaluation point
  x.fill(0.0);                 // Point of evaluation: x = (0,0,...,0)
  
  vector<Type> predR(n);  // predicter recruitment from HS (log-scale)¤
  vector<Type> recResid(n); // residuals between predicted and observed values
  predR.fill(0.0);
  recResid.fill(0.0);
  
  Type res = 0; // negative log-likelihood
  for (int i=0; i<n; i++) {
    // Hockey-stick
    if (SR==0) predR(i) += rec_loga+log(ssb(i)+pow((pow(exp(rec_logb),-2.0)+pow(gamma,2.0)/4.0),0.5)-pow((pow((ssb(i)-(1.0/exp(rec_logb))),2.0)+pow(gamma,2.0)/4.0),0.5));
    // Beverton-Holt
    if (SR==1) predR(i) += rec_loga+log(ssb(i))-log(1+exp(rec_logb)*ssb(i));
    // Ricker
    if (SR==2) predR(i) += rec_loga+log(ssb(i))-exp(rec_logb)*ssb(i);
    x(i) += log(rec(i))-predR(i);
  }
  res += SCALE(AR1(rho),exp(log_sigma))(x);           // Evaluate negative log-density of AR1 process at point x 
  
  return res;
}
