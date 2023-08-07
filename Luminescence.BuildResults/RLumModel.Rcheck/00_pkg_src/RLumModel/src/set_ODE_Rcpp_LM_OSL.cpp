//set_ODE_Rcpp_LM_OSL.cpp
//author: Johannes Friedrich, University of Bayreuth (Germany)
//version: 0.1.0 [2016-03-18]
//Function calculates the ODEs for LM-OSL for all quartz luminescence models iterativly
//


// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

//  [[Rcpp::export(".set_ODE_Rcpp_LM_OSL")]]
List set_ODE_Rcpp_LM_OSL(double t, arma::vec n, Rcpp::List parameters) {

  //unpack parameters for ODEs

  arma::vec N = parameters["N"];
  arma::vec E = parameters["E"];
  arma::vec s = parameters["s"];
  arma::vec A = parameters["A"];
  arma::vec B = parameters["B"];
  arma::vec Th = parameters["Th"];
  arma::vec E_th = parameters["E_th"];

  double k_B = parameters["k_B"];

  double R = parameters["R"];
  double P = parameters["P"];
  double temp = parameters["temp"];
  double b = parameters["b"];
  double a = parameters["a"];
  arma::vec dn(N.size()+2);

  int j = 0;
  int jj = 0;
  for (std::size_t i = 0; i < N.size(); ++i){
    if (B[i] == 0)    {//use recombination propability of recombination centers to identify electron traps, because they had no recombination propability to recomibnation centers from conduction band
      j++;
      jj++;

      dn[i] = n[N.size()]*(N[i]-n[i])*A[i] - n[i]*P*a*t*Th[i]*exp(-E_th[i]/(k_B*(273+temp+b*t))) - n[i]*s[i]*exp(-E[i]/(k_B*(273+temp+b*t)));
    } else {//calculate recombination centers
      jj++;
      dn[i] = n[N.size()+1]*(N[i]-n[i])*A[i] - n[i]*s[i]*exp(-E[i]/(k_B*(273+temp+b*t))) - n[N.size()]*n[i]*B[i];
    }
  }

  //build sub-vectors for conduction/valenece band calculation
  arma::vec temp_dn1 = dn.subvec(0,j-1);
  arma::vec temp_dn2 = dn.subvec(j,jj-1);

  arma::vec temp_n = n[N.size()]*n.subvec(j,jj-1);
  arma::vec temp_B = B.subvec(j,jj-1);

  //conduction band
  dn[N.size()] = R - sum(temp_dn1) - sum(arma::trans(temp_n) * temp_B);

  //valence band
  dn[N.size()+1] = R - sum(temp_dn2) - sum(arma::trans(temp_n) * temp_B);

  return(Rcpp::List::create(dn));
}
