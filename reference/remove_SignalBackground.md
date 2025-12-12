# Remove Signal Background from `RLum.Data.Curve` Objects

Convenient (background) of luminescence curves using
[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md)

## Usage

``` r
remove_SignalBackground(
  object,
  object_bg = NULL,
  recordType = NULL,
  clean_up = TRUE
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): A non-empty
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object with, e.g., OSL/IRSL/TL curves or a
  [list](https://rdrr.io/r/base/list.html) of such object. If a list is
  provided non-conform list elements are silently removed from the
  [list](https://rdrr.io/r/base/list.html)

- object_bg:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
  a [list](https://rdrr.io/r/base/list.html) of such objects, a
  [matrix](https://rdrr.io/r/base/matrix.html) or
  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): Sets the
  background as a curve that is subtracted from the record types set
  with `recordType`. If you provide a
  [matrix](https://rdrr.io/r/base/matrix.html) or
  [numeric](https://rdrr.io/r/base/numeric.html) internally, everything
  is coerced to a
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  object. If you desire full freedom, you can construct a
  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  objects.

- recordType:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  provide the `recordType` subject to the background subtraction.
  Subsequent curve selection uses
  [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).
  If set to `NULL` the record type of highest occurrence will be used.
  Example: `recordType = "TL (UVVIS)"`

- clean_up:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable background curve removal after background subtraction.
  If `object_bg` is set, nothing is removed from the input object as the
  background is already stored separately.

## Value

Returns an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object or a [list](https://rdrr.io/r/base/list.html) of such objects.

## Details

The function aims to simplify a frequently encountered task: subtracting
curves or backgrounds from luminescence curves, such as OSL, TL, or RF.
The function presumes that if curves are presented in pairs, for
instance, TL - TL, the second curve represents a background signal that
needs to be removed from the first curve. Following the removal, the
background curve is discarded from the dataset. Alternatively, custom
background curves can be provided, which are then utilised for the
subtraction. In essence, the function utilises the
[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md)
function but simplifies the selection of pairs and curves.

## Function version

0.1.0

## See also

[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md),
[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. remove_SignalBackground(): Remove Signal Background
from RLum.Data.Curve Objects. Function version 0.1.0. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example dataset
xsyg <- read_XSYG2R(
system.file("extdata/XSYG_file.xsyg", package = "Luminescence"),
fastForward = TRUE,
verbose = FALSE)

## remove constant background from OSL curves
remove_SignalBackground(
object = xsyg,
object_bg = 100,
recordType = "OSL (UVVIS)")
#> [[1]]
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  1
#>   number of records: 12
#>   .. : RLum.Data.Curve : 12
#>   .. .. : #1 TL (UVVIS) <> #2 TL (NA) <> #3 TL (NA) 
#>   .. .. : #4 OSL (UVVIS) <> #5 OSL (NA) <> #6 OSL (NA) <> #7 OSL (NA) <> #8 OSL (NA)
#>   .. .. : #9 irradiation (NA) 
#>   .. .. : #10 TL (UVVIS) <> #11 TL (NA) <> #12 TL (NA)
#> 

## use a more elaborate examples
## with two TL curves (2nd is background)
xsyg_v1 <- set_RLum("RLum.Analysis", records = c(
 rep(xsyg[[1]]@records[[1]], 2),
 xsyg[[1]]@records[[4]],
 xsyg[[1]]@records[[4]],
 rep(xsyg[[1]]@records[[10]], 2),
 xsyg[[1]]@records[[4]],
 xsyg[[1]]@records[[4]]))

## remove background and strip background
## curves from the object
o <- remove_SignalBackground(
 object = xsyg_v1,
 recordType = "TL (UVVIS)")
```
