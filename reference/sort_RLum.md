# Sort data for RLum-class and Risoe.BINfileData-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md) and
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
objects. Depending on the input object, the corresponding function will
be selected.

## Usage

``` r
sort_RLum(object, ...)

# S4 method for class 'list'
sort_RLum(object, ...)

# S4 method for class 'RLum.Analysis'
sort_RLum(object, slot = NULL, info_element = NULL, decreasing = FALSE, ...)

# S4 method for class 'Risoe.BINfileData'
sort_RLum(object, info_element, decreasing = FALSE, ...)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  or
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  (**required**): S4 object of class `RLum.Analysis` or
  `Risoe.BINfileData`.

- ...:

  further arguments passed to the specific class method

- slot:

  [character](https://rdrr.io/r/base/character.html) (*optional*): slot
  name to use in sorting.

- info_element:

  [character](https://rdrr.io/r/base/character.html) (*optional*): names
  of the `info` field to use in sorting. The order of the names sets the
  sorting priority. Regardless of available info elements, the following
  elements always exist because they are calculated from the record
  `XY_LENGTH`, `NCOL`, `X_MIN`, `X_MAX`, `Y_MIN`, `Y_MAX`.

- decreasing:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether the sort order should be decreasing (`FALSE` by default). It
  can be provided as a vector to control the ordering sequence for each
  sorting element.

## Value

An object of the same type as the input object provided.

## Functions

- `sort_RLum(list)`: Returns a list of sorted
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects.

- `sort_RLum(RLum.Analysis)`: Sorting of `RLum.Data` objects contained
  in this `RLum.Analysis` object. At least one of `slot` and
  `info_element` must be provided. If both are given, ordering by `slot`
  always takes priority over `info_element`. Only the first element in
  each `slot` and each `info_element` is used for sorting. Example:
  `.pid` can contain multiple values, however, only the first is taken.
  Please note that the
  [`show()`](https://r-lum.github.io/Luminescence/reference/show.md)
  method does some structuring, which may lead to the impression that
  the sorting did not work.

- `sort_RLum(Risoe.BINfileData)`: Sort method for
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects.

## Function version

0.1.0

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)

## Author

Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## How to cite

Colombo, M., 2025. sort_RLum(): Sort data for RLum-class and
Risoe.BINfileData-class objects. Function version 0.1.0. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data
data(ExampleData.XSYG, envir = environment())
obj <- OSL.SARMeasurement$Sequence.Object[1:9]

sort_RLum(obj, slot = "recordType")
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  0
#>   number of records: 9
#>   .. : RLum.Data.Curve : 9
#>   .. .. : #1 OSL (NA) <> #2 OSL (NA) <> #3 OSL (NA) <> #4 OSL (NA) <> #5 OSL (UVVIS) 
#>   .. .. : #6 TL (NA) <> #7 TL (NA) <> #8 TL (UVVIS)
#>   .. .. : #9 irradiation (NA)
sort_RLum(obj, info_element = "curveDescripter")
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  0
#>   number of records: 9
#>   .. : RLum.Data.Curve : 9
#>   .. .. : #1 TL (NA) <> #2 TL (NA) 
#>   .. .. : #3 OSL (NA) <> #4 OSL (NA) 
#>   .. .. : #5 TL (UVVIS) 
#>   .. .. : #6 OSL (UVVIS) <> #7 OSL (NA) <> #8 OSL (NA)
#>   .. .. : #9 irradiation (NA)

data(ExampleData.XSYG, envir = environment())
sar <- OSL.SARMeasurement$Sequence.Object[1:5]
sort_RLum(sar, solt = "recordType", info_element = c("startDate"))
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  0
#>   number of records: 5
#>   .. : RLum.Data.Curve : 5
#>   .. .. : #1 TL (UVVIS) <> #2 TL (NA) <> #3 TL (NA) 
#>   .. .. : #4 OSL (UVVIS) <> #5 OSL (NA)
```
