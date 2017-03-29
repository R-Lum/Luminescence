### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Rcpp
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2019-03-29
### ===============================================================================================

if(!require("Rcpp"))
  install.packages("Rcpp")

library(Rcpp)


##compile new attributes
Rcpp::compileAttributes()

