# Smoothing of data for RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding function will
be selected. The smoothing is performed in the internal function
`.smoothing()`.

## Usage

``` r
smooth_RLum(object, ...)

# S4 method for class 'list'
smooth_RLum(object, ...)

# S4 method for class 'RLum.Analysis'
smooth_RLum(object, ...)

# S4 method for class 'RLum.Data.Curve'
smooth_RLum(
  object,
  k = NULL,
  fill = NA,
  align = "right",
  method = "mean",
  p_acceptance = 1e-07
)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum`

- ...:

  further arguments passed to the specific class method

- k:

  `smooth_RLum`; [integer](https://rdrr.io/r/base/integer.html) (*with
  default*): window for the rolling mean or median. If `NULL`, this set
  automatically (ignored if `method = "Carter_etal_2018"`).

- fill:

  `smooth_RLum`; [numeric](https://rdrr.io/r/base/numeric.html) (*with
  default*): value used to pad the result so to have the same length as
  the input.

- align:

  `smooth_RLum`; [character](https://rdrr.io/r/base/character.html)
  (*with default*): one of `"right"`, `"center"` or `"left"`, specifying
  whether the index of the result should be right-aligned (default),
  centred, or left-aligned compared to the rolling window of
  observations (ignored if `method = "Carter_etal_2018"`).

- method:

  `smooth_RLum`; [character](https://rdrr.io/r/base/character.html)
  (*with default*): smoothing method to be applied: one of `"mean"`,
  `"median"` or `"Carter_etal_2018"`.

- p_acceptance:

  `smooth_RLum`; [numeric](https://rdrr.io/r/base/numeric.html) (*with
  default*): probability threshold of accepting a value to be a sample
  from a Poisson distribution (only used for
  `method = "Carter_etal_2018"`). Values that have a Poisson probability
  below the threshold are replaced by the average over the four
  neighbouring values.

## Value

An object of the same type as the input object provided.

## Functions

- `smooth_RLum(list)`: Returns a list of
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects that had been passed to smooth_RLum

- `smooth_RLum(RLum.Analysis)`: Smoothing of `RLum.Data` records
  contained in the input object.

- `smooth_RLum(RLum.Data.Curve)`: Smoothing of
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  objects using a rolling mean or median. For methods `"mean"` and
  `"median"`, smoothing is performed by rolling mean and rolling median
  with window of size `k`. Method `"Carter_etal_2018"` implements a
  Poisson smoother for dark-background signals measured by a
  photomultiplier tube.

## Note

Currently only `RLum` objects of class `RLum.Data.Curve` and
`RLum.Analysis` (with curve data) are supported.

## Function version

0.1.0

## How to cite

Kreutzer, S., 2025. smooth_RLum(): Smoothing of data for RLum-class
objects. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Carter, J., Cresswell, A.J., Kinnaird, T.C., Carmichael, L.A., Murphy,
S. & Sanderson, D.C.W., 2018. Non-Poisson variations in photomultipliers
and implications for luminescence dating. Radiation Measurements 120,
267-273.
[doi:10.1016/j.radmeas.2018.05.010](https://doi.org/10.1016/j.radmeas.2018.05.010)

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
## load example data
data(ExampleData.CW_OSL_Curve, envir = environment())

## create RLum.Data.Curve object from this example
curve <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )

## plot data without and with smoothing
plot_RLum(curve)

plot_RLum(smooth_RLum(curve))

```
