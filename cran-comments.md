## Release summary


## Addressed CRAN issues

> Undeclared package ‘shiny’ in Rd xrefs 

We removed the link to the 'shiny' package, since it is not used direclty 
by 'Luminescence'.

## Win-Builder

Multiple errors for URL https://doi.org/10.1515/geochr-2015-0022
The URL is correct and works as expected, the problem appears to be, however, 
related to the server behind the DOI forwarding and is thus beyond our reach. 

## R CMD check --as-cran results

0 errors | 0 warnings | 0 note

## Other notes or warnings

* *winbuilder* 

* old: claims that `https://doi.org/10.1515/geochr-2015-0022` is wrong, the URL is correct.
* release: OK
* devel: did not return results after two hours

R-old and R-release also complained about invalid URLs, however, we double 
checked thous URLs and found them accessible. 

## Reverse dependency checks

Reverse depends 4: 

* 3/4 are ok. 
* `RLumShiny`: The note here is the same as on CRAN and not related to 'Luminescence'

## Test environments
* local macOS High Sierra 10.14.6, Xcode 10.3, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.6.1 (2019-07-20)
    * i386-w64-mingw32/i386 (32-bit), R 3.6.1 (2019-07-20)
* on Travis CI
    * Ubuntu 16.04.6 LTS, oldrel
    * Ubuntu 16.04.6 LTS, release
    * Ubuntu 16.04.6 LTS, devel
    * macOS Sierra 10.13.3, Xcode 9.4.1, release
