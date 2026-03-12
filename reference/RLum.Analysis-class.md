# Class `"RLum.Analysis"`

Object class to represent analysis data for protocol analysis, i.e. all
curves, spectra etc. from one measurements. Objects from this class are
produced, by e.g.
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md),
[read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.md)

## Slots

- `protocol`:

  Object of class [character](https://rdrr.io/r/base/character.html)
  describing the applied measurement protocol

- `records`:

  Object of class [list](https://rdrr.io/r/base/list.html) containing
  objects of class
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)

## Note

The method
[structure_RLum](https://r-lum.github.io/Luminescence/reference/structure_RLum.md)
is currently just available for objects containing
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md).

## Objects from the Class

Objects can be created by calls of the form
`set_RLum("RLum.Analysis", ...)`.

## Class version

0.4.19

## See also

[Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## Examples

``` r
## show method
showClass("RLum.Analysis")
#> Class "RLum.Analysis" [package "Luminescence"]
#> 
#> Slots:
#>                                                                         
#> Name:    protocol    records originator       info       .uid       .pid
#> Class:  character       list  character       list  character  character
#> 
#> Extends: "RLum"

##set an empty object
set_RLum(class = "RLum.Analysis")
#> 
#>  [RLum.Analysis-class]
#>   originator: eval()
#>   protocol: NA
#>   additional info elements:  0
#>   number of records: 0
#>   >> This is an empty object, which cannot be used for further analysis! <<

## use example data
##load data
data(ExampleData.RLum.Analysis, envir = environment())

##show curves in object
get_RLum(IRSAR.RF.Data)
#> [[1]]
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF (NA)
#>   curveType: NA
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 1423.15 1437.8 
#>   additional info elements: 0 
#> 
#> [[2]]
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF (NA)
#>   curveType: NA
#>   measured values: 524
#>   .. range of x-values: 0.3768403 715.4821
#>   .. range of y-values: 1379.947 2103.4 
#>   additional info elements: 0 
#> 

##show only the first object, but by keeping the object
get_RLum(IRSAR.RF.Data, record.id = 1, drop = FALSE)
#> 
#>  [RLum.Analysis-class]
#>   originator: NA()
#>   protocol: IRSAR
#>   additional info elements:  0
#>   number of records: 1
#>   .. : RLum.Data.Curve : 1
#>   .. .. : #1 RF (NA)

## subsetting with SAR sample data
data(ExampleData.BINfileData, envir = environment())
sar <- object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)

## get
get_RLum(sar, subset = "NPOINTS == 250")
#> [[1]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[2]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[3]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[4]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[5]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[6]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[7]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[8]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[9]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[10]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[11]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[12]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[13]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[14]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[15]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[16]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[17]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[18]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[19]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[20]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[21]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[22]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[23]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 
#> [[24]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL (PMT) | #2 TL (PMT) | #3 TL (PMT) | #4 TL (PMT) | #5 TL (PMT) | #6 TL (PMT) | #7 TL (PMT)
#>   .. .. : #8 TL (PMT) | #9 TL (PMT) | #10 TL (PMT) | #11 TL (PMT) | #12 TL (PMT) | #13 TL (PMT) | #14 TL (PMT)
#>   .. .. : #15 TL (PMT)
#> 

## remove
remove_RLum(sar, subset = "NPOINTS == 250")
#> [[1]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[2]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[3]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[4]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[5]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[6]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[7]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[8]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[9]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[10]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[11]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[12]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[13]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[14]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[15]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[16]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[17]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[18]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[19]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[20]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[21]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[22]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[23]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
#> [[24]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL (PMT) | #2 OSL (PMT) | #3 OSL (PMT) | #4 OSL (PMT) | #5 OSL (PMT) | #6 OSL (PMT) | #7 OSL (PMT)
#>   .. .. : #8 OSL (PMT) | #9 OSL (PMT) | #10 OSL (PMT) | #11 OSL (PMT) | #12 OSL (PMT) | #13 OSL (PMT) | #14 OSL (PMT)
#>   .. .. : #15 IRSL (PMT)
#> 
```
