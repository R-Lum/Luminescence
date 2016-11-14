### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Reverese Dependency Check
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2016-11-14
### ===============================================================================================
## This script should be run manually after the package has been built

devtools::revdep_check()
devtools::revdep_check_save_summary()
devtools::revdep_check_print_problems()
