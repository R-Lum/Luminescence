//create_UID.cpp
//author: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//version: 0.1.0 [2016-01-26]
// -------------------------------------------------------------------------------------------------
//The purpose of this function is to create a unique ID for RLum objects based on the system time
//and random number. In contrast to the previously used hash function from the package 'digits'
//it is around 70 times faster and reduces the overhead for producing and RLum-object significantly.

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(".create_UID")]]
CharacterVector create_UID() {

  //set variables
  std::string timestamp;
  std::string random;

    //get timestamp in seconds
    timestamp =  std::to_string(std::time(nullptr));

    //get random number
    random = std::to_string(rand());

  return timestamp + random;
}
