## Release summary

Bugfix release, intended to replace version 0.8.4 on CRAN.

## Other notes or warnings

* *winbuilder* (oldrel and stable) returned the note "Possibly mis-spelled words in DESCRIPTION: deconvolution (42:38)". The 
word is spelled correctly. 

* *winbuilder* (devel) returned a note on the package size. There is nothing we can do anymore for the moment. 

## Reverse dependency checks

Reverse depends 4: all OK.

## Test environments
* local macOS High Sierra 10.13.5, Xcode 9.4, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-release
    * i386-w64-mingw32/i386 (32-bit), R-stable
* on Travis CI
    * Ubuntu 14.04.5 LTS, oldrel
    * Ubuntu 14.04.5 LTS, release
    * Ubuntu 14.04.5 LTS, devel
    * macOS Sierra 10.12, Xcode8.3, release
