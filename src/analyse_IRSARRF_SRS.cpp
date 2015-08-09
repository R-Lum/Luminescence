//analyse_IRSARRF_SRS.cpp
//author: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//version: 0.1 [2015-03-08]
//Function calculates the residual squares in for the R function analyse_IRSAR.RF()
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(".analyse_IRSARRF_SRS")]]
NumericVector analyse_IRSARRF_SRS(NumericVector values_regenerated_limited,
                                    NumericVector values_natural_limited){


  //pre-define variables
  NumericVector residuals = values_natural_limited.length();
  NumericVector results = values_regenerated_limited.size() - values_natural_limited.size();

  //calculate residuals
  for (int i=0; i<results.length(); ++i){

    //squared residuals
    for (int j=0; j<values_natural_limited.length(); ++j){
      residuals[j] = pow((values_regenerated_limited[j+i] - values_natural_limited[j]),2);

    }

    //sum up the residuals
    results[i] = sum(residuals);


  }

  return results;
}
