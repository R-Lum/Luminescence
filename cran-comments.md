## Release summary

This is a major release, inlcuding various new features.

## Addressed CRAN requests 

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

## Test environments
* local macOS Sierra 10.12.6, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.4.0 (2017-04-21)
    * i386-w64-mingw32/i386 (32-bit), R 3.4.0 (2017-04-21)
* on Travis CI
  * Ubuntu 12.04.5 LTS, R-devel
  * MacOSX, 10.11-xcode7.3, R 3.4.0

## R CMD check results

