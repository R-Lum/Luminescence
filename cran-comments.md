## Release summary

This is a bugfix release addressing the issue raised by Tomas Kalibera (see below).
We also run reverse dependency checks to assure that our changes have no site effect. 

## Addressed CRAN issues

* Fix the issue raised via email by Tomas Kalibera (2019-12-18)

>  passing argument endian="litte" (with the typo) to readBin (repeated on 5 source lines). 
> It should be endian="little" for little endian. 

## Further CAN messages

> Version: 0.9.6
> Check: installed package size 
> Result: NOTE 
>     installed size is 6.0Mb
>     sub-directories of 1Mb or more:
>     R 1.6Mb
>     help 1.1Mb
>     libs 1.6Mb 

We are aware of it, however, this is the package size and there is nothing we can 
do about.

## Win-Builder

Multiple errors for URL https://doi.org/10.1515/geochr-2015-0022 for example
"Message: libcurl error code 35:". 

* The URL is correct and the SSL certificate is valid. 
* We cannot reproduce this error. Indeed there was a problem in the past with that particular server, 
but it is not the case anymore.

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
