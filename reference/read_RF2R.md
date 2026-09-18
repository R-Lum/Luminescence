# Import RF-files to R

Import files produced by the IR-RF 'ImageJ' macro (`SR-RF.ijm`;
Mittelstraß and Kreutzer, 2021) into R and create a list of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects.

## Usage

``` r
read_RF2R(file, verbose = TRUE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of one or multiple RF files (URLs are supported); it can be the
  path to a directory, in which case the function tries to detect and
  import all RF files found in the directory.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  not used, only for compatible reasons

## Value

Returns an S4
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object containing
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
objects for each curve. Results are returned as a list when multiple
files are processed or `file` is a list. The header may contain
`key = "value"` pairs, which will be stored in the `info` slot;
incomplete pairs are silently dropped.

## Details

The results of spatially resolved IR-RF data are summarised in so-called
RF-files (Mittelstraß and Kreutzer, 2021). The output of the function
can be passed to function
[analyse_IRSAR.RF](https://r-lum.github.io/Luminescence/reference/analyse_IRSAR.RF.md).

## Function version

0.1.4

## How to cite

Kreutzer, S., Colombo, M., 2026. read_RF2R(): Import RF-files to R.
Function version 0.1.4. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., de Boer, A., Bluszcz, A., 2026.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.3.1. https://r-lum.github.io/Luminescence/

## References

Mittelstraß, D., Kreutzer, S., 2021. Spatially resolved infrared
radiofluorescence: single-grain K-feldspar dating using CCD imaging.
Geochronology 3, 299–319.
[doi:10.5194/gchron-3-299-2021](https://doi.org/10.5194/gchron-3-299-2021)

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[analyse_IRSAR.RF](https://r-lum.github.io/Luminescence/reference/analyse_IRSAR.RF.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany)  
, RLum Developer Team

## Examples

``` r
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
object <- read_RF2R(file)
#> 
#> [read_RF2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  RF_file.rf
```
