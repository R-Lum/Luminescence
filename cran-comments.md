## Release summary

This a bugfix release intend to replace Luminescence version 0.9.7 on CRAN.

## Addressed CRAN issues

> Undeclared package ‘shiny’ in Rd xrefs 

We removed the link to the 'shiny' package, since it is not used directly
by 'Luminescence'.

## Win-Builder (old, release, devel)

Multiple errors for URL https://doi.org/10.1515/geochr-2015-0022
The URL is correct and works as expected, the problem appears to be, however, 
related to the server behind the DOI forwarding and is thus beyond our reach. 

## R CMD check --as-cran results

0 errors | 0 warnings | 1 note

The note concerns the change of the maintainer's email address. 

## Other notes or warnings

* *winbuilder* 

* old: claims that `https://doi.org/10.1515/geochr-2015-0022` is wrong, the URL is correct.
* release: OK
* devel: did not return results after two hours


## Reverse dependency checks

Reverse depends 3: 

* 2/3 are ok. 
* `RLumShiny`: The note here is the same as on CRAN and not related to 'Luminescence'
