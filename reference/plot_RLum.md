# General plot function for RLum S4 class objects

The function calls object specific plot functions for RLum S4 class
objects.

## Usage

``` r
plot_RLum(object, ...)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): object of class
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  or a list of such objects. If a list is provided, the function tries
  to plot every object in the list according to its `RLum` class, after
  removing non-RLum objects.

- ...:

  further arguments and graphical parameters to pass to the specific
  plot functions. The only arguments that are supported directly are
  `main` (plot title) and `mtext` (subtitle). They may be lists, in
  which case each element is dispatched to the plots (with recycling )if
  `object` is also a list.

## Value

Produces a plot depending on the input object.

## Details

The function provides a generalised access point for plotting various
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding plot function
will be selected. Allowed arguments can be found in the documentations
of each plot function.

|  |  |  |
|----|----|----|
| **object** | **corresponding plot function** | [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md) |
| [plot_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Curve.md) | [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md) | [plot_RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Spectrum.md) |
| [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md) | [plot_RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Image.md) | [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md) |
| [plot_RLum.Analysis](https://r-lum.github.io/Luminescence/reference/plot_RLum.Analysis.md) | [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md) | [plot_RLum.Results](https://r-lum.github.io/Luminescence/reference/plot_RLum.Results.md) |

## Function version

0.6

## See also

[plot_RLum.Analysis](https://r-lum.github.io/Luminescence/reference/plot_RLum.Analysis.md),
[plot_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Curve.md),
[plot_RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Spectrum.md),
[plot_RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Image.md),
[plot_RLum.Results](https://r-lum.github.io/Luminescence/reference/plot_RLum.Results.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany)  
, RLum Developer Team

## How to cite

Kreutzer, S., Colombo, M., 2026. plot_RLum(): General plot function for
RLum S4 class objects. Function version 0.6. In: Kreutzer, S., Burow,
C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., de Boer, A.,
Bluszcz, A., 2026. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.3.1. https://r-lum.github.io/Luminescence/

## Examples

``` r
#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum object
plot_RLum(temp)

```
