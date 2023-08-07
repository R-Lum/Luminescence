// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   create_UID()
// Author:  Sebastian Kreutzer, Geography & Earth Science, Aberystwyth University (United Kingdom)
// Contact: sebastian.kreutzer@aber.ac.uk
// Version: 0.1.0 [2016-01-26]
// Purpose: The purpose of this function is to create a unique ID for RLum
// objects based on the system time and a random number.
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <Rcpp.h>
#include <time.h>

using namespace Rcpp;

// [[Rcpp::export("create_UID")]]
CharacterVector create_UID() {

  //define variables
  CharacterVector random;
  time_t rawtime;
  struct tm * timeinfo;
  char timestamp [80];

  //set date + timestamp (code snippet taken from C++ reference page)
  time (&rawtime);
  timeinfo = localtime (&rawtime);
  strftime (timestamp,80,"%Y-%m-%d-%I:%M.",timeinfo);

  //get time information and add a random number
  //according to the CRAN policy the standard C-function, rand(), even sufficient here, is not allowed
  random = runif(1);

  //combine and return results
  return timestamp + Rcpp::as<std::string>(random);
}
