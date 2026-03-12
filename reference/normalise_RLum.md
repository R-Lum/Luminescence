# Normalisation of RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding function will
be selected. The normalisation is performed in the internal function
`.normalise_curve()`.

## Usage

``` r
normalise_RLum(object, norm = TRUE, ...)

# S4 method for class 'list'
normalise_RLum(object, norm = TRUE, ...)

# S4 method for class 'RLum.Analysis'
normalise_RLum(object, norm = TRUE, ...)

# S4 method for class 'RLum.Data.Curve'
normalise_RLum(object, norm)

# S4 method for class 'RLum.Data.Image'
normalise_RLum(object, norm = TRUE, global = TRUE)

# S4 method for class 'RLum.Data.Spectrum'
normalise_RLum(object, norm = TRUE)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum`

- norm:

  [logical](https://rdrr.io/r/base/logical.html)
  [character](https://rdrr.io/r/base/character.html) (**required**): if
  logical, whether curve normalisation should occur; alternatively, one
  of `"max"` (used with `TRUE`), `"min"`, `"first"`, `"last"`, `"huot"`,
  `"intensity"` or a positive number (e.g., 2.2).

- ...:

  further arguments passed to the specific class method

- global:

  [logical](https://rdrr.io/r/base/logical.html) (\*with default): this
  defines whether the normalisation is applied globally (same to all) or
  locally, in which case each frame has its own normalisation. If
  `global = TRUE` the arguments for `norm = 'first'` and `norm = 'last'`
  work as expected and consider either the first or the last frame for
  the normalisation.

  Normalise
  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  objects to value set via the argument `norm`

## Value

An object of the same type as the input object provided.

## Details

The `norm` argument normalises all count values. The following options
are supported:

`norm = TRUE` or `norm = "max"`: Curve values are normalised to the
highest count value in the curve

`norm = "min"`: Curve values are normalised to the smallest count value
in the curve

`norm = "first"`: Curve values are normalised to the first count value.

`norm = "last"`: Curve values are normalised to the last count value
(this can be useful in particular for radiofluorescence curves)

`norm = "huot"`: Curve values are normalised as suggested by Sébastien
Huot via GitHub: \$\$ y = (observed - median(background)) /
(max(observed) - median(background)) \$\$

The background of the curve is defined as the last 20% of the count
values of a curve.

`norm = "intensity"`: Curve values are normalised to the channel length.

`norm = 2.2`: Curve values are normalised to a positive number (e.g.,
2.2).

## Functions

- `normalise_RLum(list)`: Returns a list of
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects that had been passed to normalise_RLum

- `normalise_RLum(RLum.Analysis)`: Normalisation of `RLum.Data` records
  contained in the input object.

- `normalise_RLum(RLum.Data.Curve)`: Normalise
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  objects to value set via the argument `norm`

- `normalise_RLum(RLum.Data.Image)`: Normalise
  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  objects to value set via the argument `norm`.

- `normalise_RLum(RLum.Data.Spectrum)`: Normalise
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  objects to value set via the argument `norm`

## Function version

0.1.2

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2026. normalise_RLum(): Normalisation of RLum-class
objects. Function version 0.1.2. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A.,
2026. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.2.0. https://r-lum.github.io/Luminescence/

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

plot_RLum(normalise_RLum(curve))

```
