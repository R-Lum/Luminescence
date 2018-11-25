### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### roxygen2
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2018-11-25
### ===============================================================================================
##updated using the suggestions in
##https://github.com/klutometis/roxygen/issues/822


# Load packages -------------------------------------------------------------------------------
if(!require("devtools"))
  install.packages("devtools")

if(!require("pkgbuild"))
  install.packages("pkgbuild")

# Create documentation  -----------------------------------------------------------------------
pkgbuild::compile_dll()
devtools::document()
