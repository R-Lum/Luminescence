## Release summary

This is a major release with new features and bug fixes. 

## Addressed CRAN issues

* We removed the unconditional stripping in response to the email by 
Brian Ripley from 2019-04-10 (subject: CRAN packages stripping unconditionally).

This necessarily encreased the library size on unix platforms.

## R CMD check --as-cran results

0 errors | 0 warnings | 0 note

## Other notes or warnings

* *winbuilder* (oldrel and stable) returned the note "Possibly mis-spelled words in DESCRIPTION: deconvolution (42:38)". The 
word is spelled correctly. 

* *winbuilder* (devel) returned a note on the package size. There is nothing we can do anymore for the moment. 

## Reverse dependency checks

Reverse depends 4: all OK.

## Test environments
* local macOS High Sierra 10.13.3, Xcode 9.2, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.4.0 (2017-04-21)
    * i386-w64-mingw32/i386 (32-bit), R 3.4.0 (2017-04-21)
* on Travis CI
    * Ubuntu 14.04.5 LTS, oldrel
    * Ubuntu 14.04.5 LTS, release
    * Ubuntu 14.04.5 LTS, devel
    * macOS Sierra 10.12, Xcode8.3, release
