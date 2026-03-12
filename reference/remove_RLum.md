# Strips records from RLum-class objects

Remove records from an RLum-class object in a convenient way using
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)
for the selection.

## Usage

``` r
remove_RLum(object, ...)

# S4 method for class 'list'
remove_RLum(object, ...)

# S4 method for class 'RLum.Analysis'
remove_RLum(object, ...)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): object with records to be removed.

- ...:

  parameters to be passed to
  [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).
  The arguments `get.index` and `drop` are preset and have no effect
  when provided

## Value

An object of the same type as the input provided with records removed;
it can result in an empty object.

## Functions

- `remove_RLum(list)`: Returns a list of
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects where the selected records are stripped

- `remove_RLum(RLum.Analysis)`: Method to remove records from an
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object.

## Function version

0.1.0

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2026. remove_RLum(): Strips records from RLum-class
objects. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A.,
2026. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.2.0. https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data
data(ExampleData.XSYG, envir = environment())
sar <- OSL.SARMeasurement$Sequence.Object[1:9]

## strop only OSL curves
sar <- remove_RLum(sar, recordType = "OSL")
sar
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  0
#>   number of records: 4
#>   .. : RLum.Data.Curve : 4
#>   .. .. : #1 TL (UVVIS) <> #2 _TL (NA) <> #3 _TL (NA) 
#>   .. .. : #4 irradiation (NA)
```
