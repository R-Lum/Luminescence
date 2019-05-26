// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_find_first_value()
// Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//          based on an R script written by Pierre Guibert, IRAMAT-CRP2A,
//          Universite Bordeaux Montaigne (France)
// Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// Version: 0.1.0 [2019-05-15]
// @description: Find the first positions where the values in x are great than in y
//               Where x and y are numerical vectors
//
// @param: x input vector for which we test the values
// @param: y input vector for which the values of x are tested against
//
// @return: the output is an index vector with the same length as x
//
// @note: this version is ca. 10 times faster than the previous R implementation
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector src_find_first_larger_value(
    NumericVector x,
    NumericVector y
    ) {

    //initialise variable
    int ic = 0;
    int ic_max = y.size() - 1;
    NumericVector cur_mean_id(x.size());

    for (int i = 0; i < x.size(); i++){
      while (x[i] > y[ic] && ic < ic_max)
        ic++;

      //fill vector
      cur_mean_id[i] = ic + 1;

    }

    return(cur_mean_id);

}


