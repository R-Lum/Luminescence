//analyse_IRSARRF_SRS.cpp
//author: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//version: 0.2.0 [2015-10-10]
//Function calculates the squared residuals for the R function analyse_IRSAR.RF()
//including MC runs for the obtained minimum
//
#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export(".analyse_IRSARRF_SRS")]]
RcppExport SEXP analyse_IRSARRF_SRS(NumericVector values_regenerated_limited,
                                    NumericVector values_natural_limited,
                                    int n_MC
                                    ){


  //pre-define variables
  NumericVector residuals = values_natural_limited.length();
  NumericVector results = values_regenerated_limited.size() - values_natural_limited.size();
  NumericVector results_vector_min_MC = n_MC;


  //(1) calculate sum of the squared residuals
  // this will be used to find the best fit of the curves (which is the minimum)
  for (int i=0; i<results.length(); ++i){

    //squared residuals
    for (int j=0; j<values_natural_limited.length(); ++j){
      residuals[j] = pow((values_regenerated_limited[j+i] - values_natural_limited[j]),2);

    }

    //sum up the residuals
    results[i] = sum(residuals);

  }

  //(2) error calculation
  //use this values to bootstrap and find minimum values and to account for the variation
  //that may result from this method itself (the minimum lays within a valley of minima)
  //
  //using the obtained sliding vector and the function RcppArmadillo::sample() (which equals the
  //function sample() in R, but faster)
  //http://gallery.rcpp.org/articles/using-the-Rcpp-based-sample-implementation/
  for (int i=0; i<results_vector_min_MC.length(); ++i){
    results_vector_min_MC[i] = min(
      RcppArmadillo::sample(
        results,
        results.length(),
        TRUE,
        NumericVector::create()
     )
    );
  }

  //build list with two elements
  //sliding_vector: the original results_vector (this can be used to reproduced the results in R)
  //sliding_vector_min_MC: minimum values based on bootstrapping
  List results_list;
    results_list["sliding_vector"] = results;
    results_list["sliding_vector_min_MC"] = results_vector_min_MC;

  return results_list;
}
