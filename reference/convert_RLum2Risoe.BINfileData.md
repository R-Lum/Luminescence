# Converts RLum.Analysis and RLum.Data.Curve objects to RLum2Risoe.BINfileData objects

The functions converts
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
and
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
objects (or a [list](https://rdrr.io/r/base/list.html) of such objects)
to
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
objects. The function intends to provide a minimum of compatibility
between both formats. The created
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object can be later exported to a BIN-file using function
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md).

## Usage

``` r
convert_RLum2Risoe.BINfileData(object, keep.position.number = FALSE)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  or
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  (**required**): input object to be converted

- keep.position.number:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): keeps
  the original position number or re-calculate the numbers to avoid
  doubling

## Value

The function returns a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object.

## Note

The conversion can be never perfect. The `RLum` objects may contain
information which are not part of the
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
definition.

## Function version

0.1.4

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. convert_RLum2Risoe.BINfileData(): Converts
RLum.Analysis and RLum.Data.Curve objects to RLum2Risoe.BINfileData
objects. Function version 0.1.4. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##simple conversion using the example dataset
data(ExampleData.RLum.Analysis, envir = environment())
convert_RLum2Risoe.BINfileData(IRSAR.RF.Data)
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     0
#>  Object date:          20251212
#>  User:                 0
#>  System ID:            0 (unknown)
#>  Overall records:      2
#>  Records type:         RL    (n = 2)
#>  Position range:       0 : 0
#>  Run range:            1 : 2
#>  Set range:            0 : 0
```
