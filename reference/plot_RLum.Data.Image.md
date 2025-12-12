# Plot function for an `RLum.Data.Image` S4 class object

The function provides very basic plot functionality for image data of an
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
object. For more sophisticated plotting it is recommended to use other
very powerful packages for image processing.

**Details on the plot functions**

Supported plot types:

**`plot.type = "plot.raster"`**

Uses the standard plot function of R
[graphics::image](https://rdrr.io/r/graphics/image.html). If wanted, the
image is enhanced, using the argument `stretch`. Possible values are
`hist`, `lin`, and `NULL`. The latter does nothing. The argument
`useRaster = TRUE` is used by default, but can be set to `FALSE`.

**`plot.type = "contour"`**

This uses the function
[graphics::contour](https://rdrr.io/r/graphics/contour.html)

## Usage

``` r
plot_RLum.Data.Image(
  object,
  frames = NULL,
  par.local = TRUE,
  plot.type = "plot.raster",
  ...
)
```

## Arguments

- object:

  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  (**required**): S4 object of class `RLum.Data.Image`

- frames:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): sets the
  frames to be set, by default all frames are plotted. Can be sequence
  of numbers, as long as the frame number is valid.

- par.local:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): use
  local graphical parameters for plotting, e.g. the plot is shown in one
  column and one row. If `par.local = FALSE` global parameters are
  inherited.

- plot.type:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  plot types. Supported types are `plot.raster`, `contour`

- ...:

  further arguments and graphical parameters that will be passed to the
  specific plot functions. Standard supported parameters are `xlim`,
  `ylim`, `zlim`, `xlab`, `ylab`, `main`, `mtext`, `legend` (`TRUE` or
  `FALSE`), `col`, `cex`, `axes` (`TRUE` or `FALSE`), `zlim_image`
  (adjust the z-scale over different images), `stretch`, `digits`,
  scientific (`TRUE` or `FALSE`).

## Value

Returns a plot

## Note

The axes limitations (`xlim`, `zlim`, `zlim`) work directly on the
object, so that regardless of the chosen limits the image parameters can
be adjusted for best visibility. However, in particular for z-scale
limitations this is not always wanted, please use `zlim_image` to
maintain a particular value range over a series of images.

## Function version

0.2.2

## See also

[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[plot](https://rdrr.io/r/graphics/plot.default.html),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md),
[graphics::image](https://rdrr.io/r/graphics/image.html),
[graphics::contour](https://rdrr.io/r/graphics/contour.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. plot_RLum.Data.Image(): Plot function for an
RLum.Data.Image S4 class object. Function version 0.2.2. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##load data
data(ExampleData.RLum.Data.Image, envir = environment())

##plot data
plot_RLum.Data.Image(ExampleData.RLum.Data.Image)

```
