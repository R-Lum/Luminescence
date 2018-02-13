## Release summary

This minor release replaces the version 0.8.0 submitted to 
CRAN recently. We had not been aware of the problems with 
the package 'data.table' on CRAN and we thus now lowered required package version number for 'data.table'.

We furthermore reduced the size of the example data and lowered the required R version to 3.3.0 
(3.4.0 previously).

## R CMD check --as-cran results

0 errors | 0 warnings | 1 note

* Size of tarball: 5614541 bytes. We tried to reduce the size of example data, vignettes and included 
libraries to a minimum, the rest is R code and comments.

## Addressed CRAN requests 

### Email by Kurt Hornik (2017-08-03)
> [Kurt Hornik via e-mail (2017-08-03)] "These have package CITATION files with author fields using BibTeX style
> {...} brace grouping, which will not work as intended for bibentries
> processed by R." Specific case: "Luminescence6  "Mercier, Norbert and Kreutzer, Sebastian and 
> Christophe, Claire and Gu{'e}rin [...]"

The character had already a proper escape character, i.e. {\'e}. The backslash, however, 
appears to be mistakenly removed in the e-mail by Prof Hornik. Anyway, to avoid further problems, 
we followed the alternative suggestion and set a package encoding (UTF-8) and provided the 
accented character.

> [Kurt Hornik via e-mail (2017-08-03)] "These have package CITATION files with author fields using BibTeX style
> Family_1, Given_1
> Family_1, Given_1 and Family_2, Given_2

Changed as requested.

## Addressed CRAN issues

* "Dependence on R version ‘3.3.2’ not with patchlevel 0": Corrected and required version set to 3.3.0.

* "Check: installed package size" (r-devel-linux-x86 & 64-fedora-gcc): Using the sugesstion by Dirk Eddebuettel 
to reduce the shared library size (http://dirk.eddelbuettel.com/blog/2017/08/14/#009_compact_shared_libraries). 

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
