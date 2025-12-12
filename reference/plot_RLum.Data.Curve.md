# Plot function for an RLum.Data.Curve S4 class object

The function provides a standardised plot output for curve data of an
`RLum.Data.Curve` S4-class object.

## Usage

``` r
plot_RLum.Data.Curve(
  object,
  par.local = TRUE,
  norm = FALSE,
  smooth = FALSE,
  auto_scale = FALSE,
  interactive = FALSE,
  ...
)
```

## Arguments

- object:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  (**required**): S4 object of class `RLum.Data.Curve`

- par.local:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): use
  local graphical parameters for plotting, e.g. the plot is shown in one
  column and one row. If `par.local = FALSE`, global parameters are
  inherited.

- norm:

  [logical](https://rdrr.io/r/base/logical.html)
  [character](https://rdrr.io/r/base/character.html) (*with default*):
  whether curve normalisation should occur (`FALSE` by default).
  Alternatively, the function offers modes `"max"` (used with `TRUE`),
  `"last"` and `"huot"`, see details.

- smooth:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  provides automatic curve smoothing based on the internal function
  `.smoothing`

- auto_scale:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  activated, auto scales `xlim` or `ylim` to the extent of the other. If
  both are set, the auto-scaling is skipped.

- interactive:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enables/disables interactive plotting mode using
  [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html)

- ...:

  further arguments and graphical parameters that will be passed to
  [graphics::plot.default](https://rdrr.io/r/graphics/plot.default.html)
  and [graphics::par](https://rdrr.io/r/graphics/par.html)

## Value

Returns a plot.

## Details

Only single curve data can be plotted with this function. Arguments
according to [plot](https://rdrr.io/r/graphics/plot.default.html).

**Curve normalisation**

The argument `norm` normalises all count values. To date the following
options are supported:

`norm = TRUE` or `norm = "max"`: Curve values are normalised to the
highest count value in the curve

`norm = "last"`: Curve values are normalised to the last count value
(this can be useful in particular for radiofluorescence curves)

`norm = "huot"`: Curve values are normalised as suggested by Sébastien
Huot via GitHub: \$\$ y = (observed - median(background)) /
(max(observed) - median(background)) \$\$

The background of the curve is defined as the last 20% of the count
values of a curve.

## Note

Not all arguments of
[plot](https://rdrr.io/r/graphics/plot.default.html) will be passed!

## Function version

0.4.0

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. plot_RLum.Data.Curve(): Plot function for an
RLum.Data.Curve S4 class object. Function version 0.4.0. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##plot curve data

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum.Data.Curve object
plot_RLum.Data.Curve(temp)

```
