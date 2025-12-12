# Cosmic-ray removal and spectrum smoothing for RLum.Data.Spectrum objects

The function provides several methods for cosmic-ray removal and
spectrum smoothing for
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
objects, and those embedded in [list](https://rdrr.io/r/base/list.html)
or
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects.

## Usage

``` r
apply_CosmicRayRemoval(
  object,
  method = "smooth",
  method.Pych.smoothing = 2,
  method.Pych.threshold_factor = 3,
  MARGIN = 2,
  verbose = FALSE,
  plot = FALSE,
  ...
)
```

## Arguments

- object:

  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  or
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): input object to be treated, which can be also provided
  as [list](https://rdrr.io/r/base/list.html). If an
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object is provided, only its
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  objects are treated. Please note: this mixing of objects does not work
  for a list of `RLum.Data` objects.

- method:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  Defines method that is applied for cosmic ray removal. Allowed methods
  are `smooth`, the default,
  ([stats::smooth](https://rdrr.io/r/stats/smooth.html)),
  `smooth.spline`
  ([stats::smooth.spline](https://rdrr.io/r/stats/smooth.spline.html)),
  `smooth_RLum`
  ([smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md))
  and `Pych`. See details for further information.

- method.Pych.smoothing:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  Smoothing parameter for cosmic ray removal according to Pych (2003).
  The value defines how many neighbouring values in each frame are used
  for smoothing (e.g., `2` means that the two previous and two following
  values are used).

- method.Pych.threshold_factor:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Threshold for zero-bins in the histogram. Small values mean that more
  peaks are removed, but signal might be also affected by this removal.

- MARGIN:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*): on
  which part the function cosmic ray removal should be applied on:

  - 1 = along the time axis (line by line),

  - 2 = along the wavelength axis (column by column).

  **Note:** This argument only affects methods `smooth`, `smooth.spline`
  and `smooth_RLum`.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): If
  `TRUE` the histograms used for the cosmic-ray removal are returned as
  plot including the used threshold. Note: A separate plot is returned
  for each frame! Currently only for `method = "Pych"` a graphical
  output is provided.

- ...:

  further arguments and graphical parameters that will be passed to the
  [stats::smooth](https://rdrr.io/r/stats/smooth.html),
  [stats::smooth.spline](https://rdrr.io/r/stats/smooth.spline.html) or
  [smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md).
  See details for more information.

## Value

Returns same object as input.

## Details

**`method = "Pych"`**

This method applies the cosmic-ray removal algorithm described by Pych
(2003). There are some differences with respect to the publication:

- For interpolation between neighbouring values, the median is used
  instead of the mean.

- The number of breaks in the histogram is set to half the number of the
  input values.

For further details see references below.

**Other methods** Arguments supported through `...`

|                   |                      |                                                    |                                                                                  |
|-------------------|----------------------|----------------------------------------------------|----------------------------------------------------------------------------------|
| METHOD            | ARGUMENT             | TYPE                                               | REMARKS                                                                          |
| `"smooth"`        | `kind`               | [character](https://rdrr.io/r/base/character.html) | see [stats::smooth](https://rdrr.io/r/stats/smooth.html)                         |
|                   | `twiceit`            | [logical](https://rdrr.io/r/base/logical.html)     | see [stats::smooth](https://rdrr.io/r/stats/smooth.html)                         |
| `"smooth.spline"` | `spar`               | [numeric](https://rdrr.io/r/base/numeric.html)     | see [stats::smooth.spline](https://rdrr.io/r/stats/smooth.spline.html)           |
| `"smooth_RLum"`   | `k`                  | [numeric](https://rdrr.io/r/base/numeric.html)     | see [smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md) |
|                   | `fill`               | [numeric](https://rdrr.io/r/base/numeric.html)     | see [smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md) |
|                   | `align`              | [character](https://rdrr.io/r/base/character.html) | see [smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md) |
|                   | `method_smooth_RLum` | [character](https://rdrr.io/r/base/character.html) | see [smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md) |

**Best practice**

There is no single silver-bullet strategy for cosmic-ray removal, as it
depends on the characteristic of the detector and the chosen settings.
For instance, high values for pixel binning will improve the light
output, but also cause multiple pixels to be affected by a single cosmic
ray. The same is valid for longer integration times. The best strategy
is to combine methods and ensure that the spectrum is not distorted on a
case-to-case basis.

**How to combine methods?**

Different methods can be combined by applying the method repeatedly to
the dataset (see example).

## Function version

0.4.1

## How to cite

Kreutzer, S., 2025. apply_CosmicRayRemoval(): Cosmic-ray removal and
spectrum smoothing for RLum.Data.Spectrum objects. Function version
0.4.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## References

Pych, W., 2004. A Fast Algorithm for Cosmic-Ray Removal from Single
Images. The Astronomical Society of the Pacific 116 (816), 148-153.
[doi:10.1086/381786](https://doi.org/10.1086/381786)

## See also

[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[stats::smooth](https://rdrr.io/r/stats/smooth.html),
[stats::smooth.spline](https://rdrr.io/r/stats/smooth.spline.html),
[smooth_RLum](https://r-lum.github.io/Luminescence/reference/smooth_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##(1) - use with your own data and combine (uncomment for usage)
## run two times the default method and smooth with another method
## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "smooth")
```
