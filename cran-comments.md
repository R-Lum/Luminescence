## Release summary

This version updates 0.9.11 on CRAN and fixes issues flagged by the CRAN team.
This submission addresses a warning flagged for the submission 0.9.12.
`partial argument match of 'rol' to 'role'`; fixed

## Addressed CRAN issues

* `Error in xtfrm.default(x) : cannot xtfrm 'x'` found in two 
files related to one function and fixed.

* `invalid value 0 for 'digits' argument` + CRAN email from 2021-05-24; fixed.

* `closeAllConnections()` issue flagged by CRAN email from 2021-05-11; fixed.

## Win-Builder

0 errors | 0 warnings | 0 note

## R CMD check --as-cran results

0 errors | 0 warnings | 0 note

## Reverse dependency checks

Reverse depends 3: 

* 2/3 are OK. 
* `RLumShiny`: The note here is the same as on CRAN and not related to 'Luminescence'
