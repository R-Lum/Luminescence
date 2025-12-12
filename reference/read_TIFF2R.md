# Import TIFF Image Data into R

Simple wrapper around
[tiff::readTIFF](https://rdrr.io/pkg/tiff/man/readTIFF.html) to import
TIFF images and TIFF image stacks to be further processed within the
package `'Luminescence'`

## Usage

``` r
read_TIFF2R(file, merge2stack = FALSE, verbose = TRUE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  file name, can be of type [list](https://rdrr.io/r/base/list.html) for
  automated processing of many images

- merge2stack:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  `file` is a [list](https://rdrr.io/r/base/list.html) it merges the
  individual images into one image stack. Please note that the smallest
  image dimension determines pixel dimension of the output stack.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal

- ...:

  not in use, for compatibility reasons only

## Value

[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
object

## Function version

0.2.0

## See also

[tiff::readTIFF](https://rdrr.io/pkg/tiff/man/readTIFF.html),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. read_TIFF2R(): Import TIFF Image Data into R.
Function version 0.2.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## use system file
file <- system.file("extdata", "TIFFfile.tif", package = "Luminescence")

## import image
image <- read_TIFF2R(file)
#> 
#> [read_TIFF2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  TIFFfile.tif
```
