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

0.4.18

## See also

[Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

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
#>   recordType: RF
#>   curveType: NA
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 1423.15 1437.8 
#>   additional info elements: 0 
#> 
#> [[2]]
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
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
#>   .. .. : #1 RF

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
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[2]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[3]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[4]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[5]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[6]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[7]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[8]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[9]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[10]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[11]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[12]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[13]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[14]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[15]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[16]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[17]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[18]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[19]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[20]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[21]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[22]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[23]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
#> 
#> [[24]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL | #6 TL | #7 TL
#>   .. .. : #8 TL | #9 TL | #10 TL | #11 TL | #12 TL | #13 TL | #14 TL
#>   .. .. : #15 TL
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
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[2]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[3]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[4]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[5]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[6]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[7]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[8]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[9]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[10]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[11]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[12]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[13]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[14]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[15]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[16]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[17]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[18]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[19]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[20]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[21]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[22]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[23]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
#> [[24]]
#> 
#>  [RLum.Analysis-class]
#>   originator: Risoe.BINfileData2RLum.Analysis()
#>   protocol: unknown
#>   additional info elements:  0
#>   number of records: 15
#>   .. : RLum.Data.Curve : 15
#>   .. .. : #1 OSL | #2 OSL | #3 OSL | #4 OSL | #5 OSL | #6 OSL | #7 OSL
#>   .. .. : #8 OSL | #9 OSL | #10 OSL | #11 OSL | #12 OSL | #13 OSL | #14 OSL
#>   .. .. : #15 IRSL
#> 
```
