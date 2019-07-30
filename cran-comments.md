## Release summary

This is a minor bug fix release after 0.9.1 did not pass the CRAN pre-checks. 

## CRAN check message

>Found the following (possibly) invalid file URI:
>   URI: **required**
>     From: man/apply_EfficiencyCorrection.Rd

Fixed and my apologies. Although I have no idea why none of 
the other tests before showed this wrong 'URL'. 

## CRAN messages

> Version: 0.9.0.110 
> Check: installed package size 
> Result: NOTE 
>     installed size is 5.8Mb
>     sub-directories of 1Mb or more:
>     R 1.5Mb
>     help 1.1Mb
>     libs 1.5Mb 

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

* 2/4 are ok. 
* `RLumModel` crashed but this is not related to 'Luminescence' but something that has 
changed in 'deSolve'
* `RLumShiny` showed a note not related to 'Luminescence'

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
