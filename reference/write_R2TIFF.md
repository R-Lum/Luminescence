# Export RLum.Data.Image and RLum.Data.Spectrum objects to TIFF Images

Simple wrapper around
[tiff::writeTIFF](https://rdrr.io/pkg/tiff/man/writeTIFF.html) to export
suitable
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects to TIFF images. Per default 16-bit TIFF files are exported.

## Usage

``` r
write_R2TIFF(object, file = tempfile(), norm = 65535, ...)
```

## Arguments

- object:

  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  or
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  object (**required**): input object, can be a
  [list](https://rdrr.io/r/base/list.html) of such objects.

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of the output file.

- norm:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  normalisation value. Usually, in imaging applications the pixel values
  are integer count values, but values in TIFF files must be in the 0-1
  range. Normalising to the to the highest 16-bit integer values - 1
  ensures that the numerical values are retained in the exported image.
  If `1` nothing is normalised.

- ...:

  further arguments to be passed to
  [tiff::writeTIFF](https://rdrr.io/pkg/tiff/man/writeTIFF.html).

## Value

A TIFF file

## Function version

0.1.2

## See also

[tiff::writeTIFF](https://rdrr.io/pkg/tiff/man/writeTIFF.html),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. write_R2TIFF(): Export RLum.Data.Image and
RLum.Data.Spectrum objects to TIFF Images. Function version 0.1.2. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
data(ExampleData.RLum.Data.Image, envir = environment())
write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile(fileext = ".tiff"))
```
