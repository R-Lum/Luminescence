// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_get_XSYG_curve_values()
// Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
// Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// Version: 0.1.0 [2017-07-07]
// Usage:   used within the function read_XSYG2R() to extract curve values more efficiently
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <string>
#include <algorithm>
#include <sstream>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix src_get_XSYG_curve_values(std::string s) {

  //00: count pairs
  int pairs = std::count(s.begin(), s.end(), ';') + 1;

  //01: replace all ; by ,
  std::replace(s.begin(), s.end(), ';', ',');

  //02: set needed matrix
  NumericMatrix m(pairs, 2);

  //03: set variables
  std::istringstream ss(s);
  std::string value;
  int i = 0;
  int sw = 0;

  //04: loop over string and convert to double
  while (std::getline(ss, value,  ',')) {
    if (sw % 2 == 0){
      m(i,0) = atof(value.c_str());

    }else{
      m(i,1) = atof(value.c_str());
      i++;

    }
    sw++;

   }
 return m;

}

