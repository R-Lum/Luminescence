# Merge function for RLum.Data.Spectrum S4 class objects

This function allows to merge
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
objects in different ways without modifying the original objects.

## Usage

``` r
merge_RLum.Data.Spectrum(
  object,
  merge.method = "mean",
  method.info = NULL,
  max.temp.diff = 0.1
)
```

## Arguments

- object:

  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  (**required**): list of objects to be merged.

- merge.method:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  method for combining of the objects, e.g. `'mean'` (default),
  `'median'`, `'sum'`, see details for further information and allowed
  methods. Note: Elements in slot info will be taken from the first
  object in the list.

- method.info:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): allows to
  specify how info elements of the input objects are combined, e.g. `1`
  means that just the elements from the first object are kept, `2` keeps
  only the info elements from the 2 object etc. If set to `NULL`, all
  elements are combined.

- max.temp.diff:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  maximum difference in the time/temperature values between the spectra
  to be merged: when differences exceed this threshold value, the
  merging occurs but a warning is raised.

## Value

Returns an
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
object.

## Details

**Supported merge operations are:**

`"mean"` (default)

The mean over the cell values is calculated using the function
[rowMeans](https://rdrr.io/r/base/colSums.html).

`"median"`

The median over the cell values is calculated using the function
[matrixStats::rowMedians](https://rdrr.io/pkg/matrixStats/man/rowMedians.html).

`"sum"`

All cell values will be summed up using the function
[rowSums](https://rdrr.io/r/base/colSums.html).

`"sd"`

The standard deviation over the cell values is calculated using the
function
[matrixStats::rowSds](https://rdrr.io/pkg/matrixStats/man/rowSds.html).

`"var"`

The variance over the cell values is calculated using the function
[matrixStats::rowVars](https://rdrr.io/pkg/matrixStats/man/rowVars.html).

`"min"`

The min values from the cell values is chosen using the function
[matrixStats::rowMins](https://rdrr.io/pkg/matrixStats/man/rowRanges.html).

`"max"`

The max values from the cell values is chosen using the function
[matrixStats::rowMins](https://rdrr.io/pkg/matrixStats/man/rowRanges.html).

`"append"` (only for
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md))

Appends cell values of all curves to one combined data curve. The
channel width is automatically re-calculated, but requires a constant
channel width of the original data.

`"-"`

The cell sums of the last objects are subtracted from the first object.

`"*"`

The cell sums of the last objects are multiplied with the first object.

`"/"`

Values of the first object are divided by cell sums of the last objects.

## Note

The information from the slot `recordType` is taken from the first
object in the input list. The slot 'curveType' is filled with the name
`merged`.

## S3-generic support

This function is fully operational via S3-generics: `+`, `-`, `/`, `*`,
`merge`

## Function version

0.1.2

## See also

[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)

## Author

Marco Colombo, Institute of Geography, Heidelberg University (Germany)
Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Colombo, M., Kreutzer, S., 2025. merge_RLum.Data.Spectrum(): Merge
function for RLum.Data.Spectrum S4 class objects. Function version
0.1.2. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data
data(ExampleData.XSYG, envir = environment())

## plot single curve
plot_RLum(TL.Spectrum)


## sum two copies of the same curve
merged <- merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                   merge.method = "sum")
plot_RLum(merged)

```
