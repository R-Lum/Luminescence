# Merge function for RLum.Data.Curve S4 class objects

This function allows to merge
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
objects in different ways without modifying the original objects.
Merging is always applied on the 2nd column of the object's data matrix.

**Supported merge operations are:**

`"mean"` (default)

The mean over the count values is calculated using the function
[rowMeans](https://rdrr.io/r/base/colSums.html).

`"median"`

The median over the count values is calculated using the function
[matrixStats::rowMedians](https://rdrr.io/pkg/matrixStats/man/rowMedians.html).

`"sum"`

All count values will be summed up using the function
[rowSums](https://rdrr.io/r/base/colSums.html).

`"sd"`

The standard deviation over the count values is calculated using the
function
[matrixStats::rowSds](https://rdrr.io/pkg/matrixStats/man/rowSds.html).

`"var"`

The variance over the count values is calculated using the function
[matrixStats::rowVars](https://rdrr.io/pkg/matrixStats/man/rowVars.html).

`"min"`

The min values from the count values is chosen using the function
[matrixStats::rowMins](https://rdrr.io/pkg/matrixStats/man/rowRanges.html).

`"max"`

The max values from the count values is chosen using the function
[matrixStats::rowMins](https://rdrr.io/pkg/matrixStats/man/rowRanges.html).

`"append"`

Appends count values of all curves to one combined data curve. The
channel width is automatically re-calculated, but requires a constant
channel width of the original data.

`"-"`

The row sums of the last objects are subtracted from the first object.

`"*"`

The row sums of the last objects are multiplied with the first object.

`"/"`

Values of the first object are divided by row sums of the last objects.

## Usage

``` r
merge_RLum.Data.Curve(object, merge.method = "mean", method.info = NULL)
```

## Arguments

- object:

  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  (**required**): list of objects to be merged.

- merge.method:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  method for combining of the objects, e.g. `'mean'` (default),
  `'median'`, `'sum'`, see details for further information and allowed
  methods. Note: Elements in slot info will be taken from the first
  curve in the list.

- method.info:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): allows to
  specify how info elements of the input objects are combined, e.g. `1`
  means that just the elements from the first object are kept, `2` keeps
  only the info elements from the 2 object etc. If set to `NULL`, all
  elements are combined.

## Value

Returns an
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
object.

## Note

The information from the slot `recordType` is taken from the first
object in the input list. The slot 'curveType' is filled with the name
`merged`.

## S3-generic support

This function is fully operational via S3-generics: `+`, `-`, `/`, `*`,
`merge`

## Function version

0.2.2

## See also

[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. merge_RLum.Data.Curve(): Merge function for
RLum.Data.Curve S4 class objects. Function version 0.2.2. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##load example data
data(ExampleData.XSYG, envir = environment())

##grep first and 3d TL curves
TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
TL.curve.1 <- TL.curves[[1]]
TL.curve.3 <- TL.curves[[3]]

##plot single curves
plot_RLum(TL.curve.1)

plot_RLum(TL.curve.3)


##subtract the 1st curve from the 2nd and plot
TL.curve.merged <- merge_RLum.Data.Curve(list(TL.curve.3, TL.curve.1), merge.method = "/")
#> Warning: [merge_RLum.Data.Curve()] 8 'Inf' values replaced by 0 in the matrix
plot_RLum(TL.curve.merged)

```
