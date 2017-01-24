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


##keep the old one ... just in case, as the devtools function is a little bit broken ..
####check reverse dependencies
results <-
  tools::check_packages_in_dir(
    "~/GitHub/R_Luminescence/RLum.BuildResults/",
    reverse = list(repos = getOption("repos")["CRAN"],
                   which = "most"),
    clean = TRUE

  )

##show results
print(summary(results))

##remove unwanted data
unlink("RLum.BuildResults/Library/", recursive = TRUE)
unlink("RLum.BuildResults/Outputs/", recursive = TRUE)
file.remove("RLum.BuildResults/PACKAGES")
file.remove("RLum.BuildResults/PACKAGES.gz")
