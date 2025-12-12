# Channel binning for RLum.Data-class objects

The function provides a generalised access point for specific
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
objects. Depending on the input object, the corresponding function will
be selected.

## Usage

``` r
bin_RLum.Data(object, ...)

# S4 method for class 'RLum.Data.Curve'
bin_RLum.Data(object, bin_size = 2)

# S4 method for class 'RLum.Data.Spectrum'
bin_RLum.Data(object, bin_size.col = 1, bin_size.row = 1)
```

## Arguments

- object:

  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  (**required**): S4 object of class `RLum.Data`

- ...:

  further arguments passed to the specific class method

- bin_size:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of channels used for each bin, e.g. `bin_size = 2` means that
  two channels are binned.

- bin_size.col:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of channels used for each bin, e.g. `bin_size.col = 2` means
  that two channels are binned. Note: The function does not check the
  input, very large values mean a full column binning (a single sum)

- bin_size.row:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of channels used for each bin, e.g. `bin_size.row = 2` means
  that two channels are binned. Note: The function does not check the
  input, very large values mean a full row binning (a single sum)

## Value

An object of the same type as the input provided after binning is
applied.

## Functions

- `bin_RLum.Data(RLum.Data.Curve)`: Allows binning of RLum.Data.Curve
  data.

- `bin_RLum.Data(RLum.Data.Spectrum)`: Allows binning of
  RLum.Data.Spectrum data. Count values and values on the x-axis are
  summed up; for wavelength/energy values, the mean is calculated.

## Note

Currently only `RLum.Data` objects of class
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
and
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
are supported.

## Function version

0.2.0

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. bin_RLum.Data(): Channel binning for RLum.Data-class
objects. Function version 0.2.0. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

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

## plot data without and with 2 and 4 channel binning
plot_RLum(curve)

plot_RLum(bin_RLum.Data(curve, bin_size = 2))

plot_RLum(bin_RLum.Data(curve, bin_size = 4))

```
