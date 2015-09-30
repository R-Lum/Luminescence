### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### roxygen2
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-09-30
### ===============================================================================================

if(!require("devtools"))
  install.packages("devtools")

library(devtools)

document(pkg = ".", roclets = NULL)
