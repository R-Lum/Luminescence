//analyse_IRSARRF_SRS.cpp
//author: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//version: 0.2.0 [2015-10-10]
//Function calculates the squared residuals for the R function analyse_IRSAR.RF()
//including MC runs for the obtained minimum
//
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export(".analyse_IRSARRF_SRS")]]
RcppExport SEXP analyse_IRSARRF_SRS(IntegerVector values_regenerated_limited,
                                    IntegerVector values_natural_limited,
                                    IntegerVector vslide_range,
                                    int n_MC
                                    ){


  //pre-define variables
  int32_t m_index;
  IntegerVector residuals = values_natural_limited.length();
  IntegerVector results_vector_min_MC = n_MC;
  NumericMatrix results(values_regenerated_limited.size() - values_natural_limited.size(), vslide_range.length());
  NumericMatrix temp_results(residuals.length(),results.nrow());

  //(1) calculate sum of the squared residuals
  // this will be used to find the best fit of the curves (which is the minimum)
  for (int v=0;v<vslide_range.length(); v++){
    for (int i=0; i<results.nrow(); ++i){

      //squared residuals
      for (int j=0; j<values_natural_limited.length(); ++j){
         temp_results(j,i) = pow((values_regenerated_limited[j+i] - (values_natural_limited[j] + vslide_range[v])),2);

      }

    }

    //sum up residuals ... its gives use 10 % performance this way
    results(_,v) = colSums(temp_results);

  }

  //(2) error calculation
  //use this values to bootstrap and find minimum values and to account for the variation
  //that may result from this method itself (the minimum lays within a valley of minima)
  //
  //using the obtained sliding vector and the function RcppArmadillo::sample() (which equals the
  //function sample() in R, but faster)
  //http://gallery.rcpp.org/articles/using-the-Rcpp-based-sample-implementation

    //set vector for error calculation
    NumericVector x = results(_,0);

    //overwrite for the case that we have a range,
    //in this case we just want to have selected the column where this value is included
    if(vslide_range.length() > 1){
     m_index = which_min(results) / results.nrow();
     x = results(_,m_index);
    }

  //this follows the way described in Frouin et al., 2017 ... still ...
  for (int i=0; i<results_vector_min_MC.length(); ++i){
    results_vector_min_MC[i] = min(
      RcppArmadillo::sample(
        x,
        results.nrow(),
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
