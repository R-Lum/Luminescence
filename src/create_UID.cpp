// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   create_UID()
// Author:  Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
// Contact: sebastian.kreutzer@uni-heidelberg.de
// Version: 0.1.1 [2025-03-14]
// Purpose: The purpose of this function is to create a unique ID for RLum
// objects based on the system time and a random number.
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <Rcpp.h>
#include <time.h>
#include <functional>

using namespace Rcpp;

// [[Rcpp::export("create_UID")]]
CharacterVector create_UID() {
   //This code was optimised using
   //Le Chat, Mistral AI. (2025). AI Assistant.
   //Version 1.0. Paris, France: Mistral AI.

  //define variables
  time_t rawtime;
  struct tm * timeinfo;
  char timestamp [80];

  //set date + timestamp (code snippet taken from C++ reference page)
  time (&rawtime);
  timeinfo = localtime (&rawtime);
  strftime (timestamp,80,"%Y-%m-%d-%I:%M.",timeinfo);

  //get time information and add a random number
  //according to the CRAN policy the standard C-function, rand(),
  //even sufficient here, is not allowed
  double random = R::runif(0, 1);

  // combine timestamp and random number
  std::ostringstream oss;
  oss << timestamp << random;
  std::string combined = oss.str();

  // hash the combined string using std::hash
  std::hash<std::string> hasher;
  size_t hash_value = hasher(combined);

  // Convert hash to hex string directly with fixed length
  std::ostringstream hash_oss;
  hash_oss << std::hex << std::setw(sizeof(size_t) * 2) << std::setfill('0') << hash_value;
  return hash_oss.str();
}
