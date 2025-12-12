# Import RF-files to R

Import files produced by the IR-RF 'ImageJ' macro (`SR-RF.ijm`;
Mittelstraß and Kreutzer, 2021) into R and create a list of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects

## Usage

``` r
read_RF2R(file, verbose = TRUE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  path and file name of the RF file. Alternatively a list of file names
  can be provided.

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
objects for each curve.

## Details

The results of spatially resolved IR-RF data are summarised in so-called
RF-files (Mittelstraß and Kreutzer, 2021). This functions provides an
easy import to process the data seamlessly with the R package
'Luminescence'. The output of the function can be passed to function
[analyse_IRSAR.RF](https://r-lum.github.io/Luminescence/reference/analyse_IRSAR.RF.md).

## Function version

0.1.1

## How to cite

Kreutzer, S., 2025. read_RF2R(): Import RF-files to R. Function version
0.1.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

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

Sebastian Kreutzer, Geography & Earth Science, Aberystwyth University
(United Kingdom) , RLum Developer Team

## Examples

``` r
##Import
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
temp <- read_RF2R(file)
#> 
#> [read_RF2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  RF_file.rf
```
