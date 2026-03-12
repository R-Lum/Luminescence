# Import Luminescence Data from Helios Luminescence Reader

Import of files with `.osl` extension produced by the zero rad Helios
luminescence reader and conversion to
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects.

## Usage

``` r
read_HeliosOSL2R(file, verbose = TRUE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html),
  [list](https://rdrr.io/r/base/list.html) (**required**): name of one
  or multiple `.osl` files (URLs are supported); it can be the path to a
  directory, in which case the function tries to detect and import all
  `.osl` files found in the directory.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  not in use, for compatibility reasons only

## Value

A
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object. Results are returned as a list when multiple files are processed
or `file` is a list.

## Note

Thanks to Krzysztof Maternicki for providing example data.

## Function version

0.1.1

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2026. read_HeliosOSL2R(): Import Luminescence Data from
Helios Luminescence Reader. Function version 0.1.1. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
Bluszcz, A., 2026. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.2.0. https://r-lum.github.io/Luminescence/

## Examples

``` r
file <- system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence")
read_HeliosOSL2R(file)
#> 
#> [read_HeliosOSL2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  HeliosOSL_Example.osl
#> 
#>  [RLum.Analysis-class]
#>   originator: read_HeliosOSL2R()
#>   protocol: NA
#>   additional info elements:  0
#>   number of records: 6
#>   .. : RLum.Data.Curve : 6
#>   .. .. : #1 OSL (J) <> #2 OSL (Jcorr) <> #3 OSL (Jphd) <> #4 OSL (dt.s.) <> #5 OSL (T.K.) <> #6 OSL (ILed.A.)
```
