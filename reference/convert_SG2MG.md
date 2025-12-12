# Converts Single-Grain Data to Multiple-Grain Data

Conversion of single-grain data to multiple-grain data by adding signals
from grains belonging to one disc (unique pairs of position, set and
run).

## Usage

``` r
convert_SG2MG(object, write_file = FALSE, ...)
```

## Arguments

- object:

  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  [character](https://rdrr.io/r/base/character.html) (**required**):
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  object or BIN/BINX-file name

- write_file:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  the input was a path to a file, the output can be written to a file if
  `TRUE`. The multiple grain file will be written into the same folder
  and with extension `-SG` to the file name.

- ...:

  further arguments passed down to
  [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)
  if input is file path

## Value

[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object and if `write_file = TRUE` and the input was a file path, a file
is written to origin folder.

## Function version

0.1.0

## See also

[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Norbert Mercier, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux
Montaigne (France) , RLum Developer Team

## How to cite

Kreutzer, S., Mercier, N., 2025. convert_SG2MG(): Converts Single-Grain
Data to Multiple-Grain Data. Function version 0.1.0. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## simple run
## (please not that the example is not using SG data)
data(ExampleData.BINfileData, envir = environment())
convert_SG2MG(CWOSL.SAR.Data)
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     03
#>  Object date:          060920, 070920, 080920, 090920, 100920
#>  User:                 Default
#>  System ID:            0 (unknown)
#>  Overall records:      720
#>  Records type:         IRSL  (n = 24)
#>                        OSL   (n = 336)
#>                        TL    (n = 360)
#>  Position range:       1 : 24
#>  Run range:            1 : 8
#>  Set range:            2 : 6
```
