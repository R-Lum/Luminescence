# Import Luminescence Data into R

Convenience wrapper function to provide a quicker and more standardised
way of reading data into R by looping through all in the package
available data import functions starting with `read_`. Import data types
can be mixed.

## Usage

``` r
import_Data(file, ..., fastForward = TRUE, verbose = FALSE)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  file to be imported, can be a [list](https://rdrr.io/r/base/list.html)
  or a [character](https://rdrr.io/r/base/character.html) vector

- ...:

  arguments to be further passed down to supported functions (please
  check the functions to determine the correct arguments)

- fastForward:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  option to create
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects during import or a [list](https://rdrr.io/r/base/list.html) of
  such objects

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

## Value

Always returns a [list](https://rdrr.io/r/base/list.html); empty or
filled with
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects

## Function version

0.1.6

## See also

[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[read_BINXLOG2R](https://r-lum.github.io/Luminescence/reference/read_BINXLOG2R.md),
[read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.md),
[read_HeliosOSL2R](https://r-lum.github.io/Luminescence/reference/read_HeliosOSL2R.md),
[read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md),
[read_RF2R](https://r-lum.github.io/Luminescence/reference/read_RF2R.md),
[read_SPE2R](https://r-lum.github.io/Luminescence/reference/read_SPE2R.md),
[read_TIFF2R](https://r-lum.github.io/Luminescence/reference/read_TIFF2R.md),
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2026. import_Data(): Import Luminescence Data into R.
Function version 0.1.6. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A., 2026.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.2.1. https://r-lum.github.io/Luminescence/

## Examples

``` r
## import BINX/BIN
file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- import_Data(file)

## RF data
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
temp <- import_Data(file)
```
