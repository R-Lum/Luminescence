## Release summary

This release addresses a request received from CRAN.
We also fixed some bugs.

## Addressed CRAN issues received via mail

> From: Kurt Hornik, 2019-08-02
> Issue: datalist not correct 

We rebuilt our datalist as requested. 
Thanks to Kurt Hornik for pointing out this issue!

## R CMD check --as-cran results

0 errors | 0 warnings | 0 note

## CRAN incoming message

>Found the following (possibly) invalid URLs:
>   URL: https://doi.org/10.1515/geochr-2015-0022
>     From: inst/doc/HowTo_analyse_Al2O3.html
>     Status: Error
>     Message: libcurl error code 60:
>       	server certificate verification failed. CAfile: none CRLfile: none
>       	(Status without verification: OK)

The URL is correct. 

## CRAN messages

> Version: 0.9.3 
> Check: installed package size 
> Result: NOTE 
>     installed size is 6.0Mb
>     sub-directories of 1Mb or more:
>     R 1.5Mb
>     help 1.1Mb
>     libs 1.7Mb 

We are aware of it, however, this is the package size and there is nothing we can 
do about.

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
