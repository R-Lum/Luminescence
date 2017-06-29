## Release summary

Bugfix release.

## Test environments
* local macOS Sierra 10.12.5, R version 3.4.1 RC (2017-06-22 r72859); currently no R-devel available
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.4.0 (2017-04-21)
    * i386-w64-mingw32/i386 (32-bit), R 3.4.0 (2017-04-21)
* on Travis CI
  * Ubuntu 12.04.5 LTS, R-devel
  * MacOSX, 10.11-xcode7.3, R 3.4.0

## R CMD check results
There were no ERRORs or WARNINGs.

## CRAN Package Check Results

NOTEL 
Version: 0.7.4 
Check: package dependencies 
Result: NOTE 
    Packages suggested but not available for checking: ‘rjags’ ‘devtools’ 
Flavor: r-release-osx-x86_64

This is not releated to our package, but to missing resources on the particular check environment.

## Reverse package dependency checks 

Carried out for: 

* RLumShiny
* RLumModel
* TLDating

Results: No ERRORS or WARNINGs
