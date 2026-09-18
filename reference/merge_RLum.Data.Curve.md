# Merge RLum.Data objects

This function allows to merge
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
and
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
objects in different ways without modifying the original objects. For
`RLum.Data.Curve` objects, merging is always applied on the 2nd column
of the object's data matrix.

**Supported merge operations are:**

`"mean"` (default)

The mean over the count/cell values is calculated using
[rowMeans](https://rdrr.io/r/base/colSums.html).

`"median"`

The median over the count/cell values is calculated using
[matrixStats::rowMedians](https://rdrr.io/pkg/matrixStats/man/rowMedians.html).

`"sum"`

All count/cell values will be summed up using
[rowSums](https://rdrr.io/r/base/colSums.html).

`"sd"`

The standard deviation over the count/cell values is calculated using
[matrixStats::rowSds](https://rdrr.io/pkg/matrixStats/man/rowSds.html).

`"var"`

The variance over the count/cell values is calculated using
[matrixStats::rowVars](https://rdrr.io/pkg/matrixStats/man/rowVars.html).

`"min"`

The min values from the count/cell values is calculated using
[matrixStats::rowMins](https://rdrr.io/pkg/matrixStats/man/rowRanges.html).

`"max"`

The max values from the count/cell values is calculated using
[matrixStats::rowMins](https://rdrr.io/pkg/matrixStats/man/rowRanges.html).

`"append"`

Appends count/cell values of all objects to one combined data object.
The channel width is automatically re-calculated, but requires a
constant channel width of the original data. Note: for
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
objects, this method is only available when all objects have the same
number of columns.

`"-"`

The row sums of the last objects are subtracted from the first object.

`"*"`

The row sums of the last objects are multiplied with the first object.

`"/"`

Values of the first object are divided by row sums of the last objects.

## Usage

``` r
merge_RLum.Data.Curve(
  object,
  merge.method = c("mean", "median", "sum", "sd", "var", "max", "min", "append", "-",
    "*", "/"),
  method.info = NULL,
  ...
)

merge_RLum.Data.Spectrum(
  object,
  merge.method = c("mean", "median", "sum", "sd", "var", "min", "max", "append", "-",
    "*", "/"),
  method.info = NULL,
  max.temp.diff = 0.1,
  ...
)
```

## Arguments

- object:

  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  (**required**): list of objects to be merged.

- merge.method:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  method for combining of the objects, e.g. `'mean'` (default),
  `'median'`, `'sum'`, see details for further information and allowed
  methods.

- method.info:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): allows to
  specify how info elements of the input objects are combined, e.g. `1`
  means that just the elements from the first object are kept, `2` keeps
  only the info elements from the 2 object etc. If set to `NULL`
  (default), all elements are combined.

- ...:

  currently not used.

- max.temp.diff:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  maximum difference in the time/temperature values between the spectra
  to be merged: when differences exceed this threshold value, the
  merging occurs but a warning is raised. Only used for
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  objects.

## Value

Returns an
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
or
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
object, depending on the input.

## Note

The information from the slot `recordType` is taken from the first
object in the input list. The slot 'curveType' is filled with the name
`merged`.

## S3-generic support

These functions are fully operational via S3-generics: `+`, `-`, `/`,
`*`, `merge`

## Function version

0.2.3

## See also

[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany)  
, RLum Developer Team

## How to cite

Kreutzer, S., Colombo, M., 2026. merge_RLum.Data.Curve(): Merge
RLum.Data objects. Function version 0.2.3. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., de Boer, A.,
Bluszcz, A., 2026. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.3.1. https://r-lum.github.io/Luminescence/

## Examples

``` r

## load example data
data(ExampleData.XSYG, envir = environment())

## grep first and third TL curves
TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
TL.curve.1 <- TL.curves[[1]]
TL.curve.3 <- TL.curves[[3]]

## plot single curves
plot_RLum(TL.curve.1)

plot_RLum(TL.curve.3)


## subtract the 1st curve from the 3rd and plot
TL.curve.merged <- merge_RLum.Data.Curve(list(TL.curve.3, TL.curve.1),
                                         merge.method = "/")
#> Warning: [merge_RLum.Data.Curve()] 8 'Inf' values replaced by 0 in the matrix
plot_RLum(TL.curve.merged)

```
