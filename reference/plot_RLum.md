# General plot function for RLum S4 class objects

Function calls object specific plot functions for RLum S4 class objects.

## Usage

``` r
plot_RLum(object, ...)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum`. Optional a
  [list](https://rdrr.io/r/base/list.html) containing objects of class
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  can be provided. In this case the function tries to plot every object
  in this list according to its `RLum` class. Non-RLum objects are
  removed.

- ...:

  further arguments and graphical parameters that will be passed to the
  specific plot functions. The only argument that is supported directly
  is `main` (setting the plot title). In contrast to the normal
  behaviour `main` can be here provided as
  [list](https://rdrr.io/r/base/list.html) and the arguments in the list
  will dispatched to the plots if the `object` is of type `list` as
  well.

## Value

Returns a plot.

## Details

The function provides a generalised access point for plotting specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects.  
Depending on the input object, the corresponding plot function will be
selected. Allowed arguments can be found in the documentations of each
plot function.

|                                                                                                  |     |                                                                                                      |
|--------------------------------------------------------------------------------------------------|-----|------------------------------------------------------------------------------------------------------|
| **object**                                                                                       |     | **corresponding plot function**                                                                      |
| [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)       | :   | [plot_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Curve.md)       |
| [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md) | :   | [plot_RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Spectrum.md) |
| [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)       | :   | [plot_RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Image.md)       |
| [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)           | :   | [plot_RLum.Analysis](https://r-lum.github.io/Luminescence/reference/plot_RLum.Analysis.md)           |
| [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)             | :   | [plot_RLum.Results](https://r-lum.github.io/Luminescence/reference/plot_RLum.Results.md)             |

## Note

The provided plot output depends on the input object.

## Function version

0.4.4

## See also

[plot_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Curve.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[plot_RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Spectrum.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[plot_RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Image.md),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[plot_RLum.Analysis](https://r-lum.github.io/Luminescence/reference/plot_RLum.Analysis.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[plot_RLum.Results](https://r-lum.github.io/Luminescence/reference/plot_RLum.Results.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. plot_RLum(): General plot function for RLum S4 class
objects. Function version 0.4.4. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum object
plot_RLum(temp)

```
