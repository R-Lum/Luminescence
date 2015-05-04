### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Rcpp
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-04-29
### ===============================================================================================

if(!require("Rcpp"))
  install.packages("Rcpp")

library(Rcpp)


##compile new attributes
Rcpp::compileAttributes()
