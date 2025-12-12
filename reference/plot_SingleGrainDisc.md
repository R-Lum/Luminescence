# Plot a disc with its values

Shows a schematic representation of the physical appearance of one disc
(one position in the reader) and illustrates the measured or calculated
values per grain location.

## Usage

``` r
plot_SingleGrainDisc(
  object,
  show_coordinates = FALSE,
  show_location_ids = FALSE,
  show_neighbours = FALSE,
  show_positioning_holes = TRUE,
  df_neighbours = NULL,
  ignore_borders = FALSE,
  str_transform = "sqrt",
  ...
)
```

## Arguments

- object:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [numeric](https://rdrr.io/r/base/numeric.html) (**required**): the
  values to show, should have length 100.

- show_coordinates:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): Show
  coordinates (1..10) in x and in y direction. Defaults to `FALSE`.

- show_location_ids:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): Show
  id with every grain location (1..100). Defaults to `FALSE`.

- show_neighbours:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): Show
  which neighbour connections are taken into account if calculating
  Moran's I. This makes sense when there are `NA` observations, or when
  a non-standard neighbour setting is defined.

- show_positioning_holes:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): Show
  the 3 positioning holes for orientation. Defaults to `TRUE`.

- df_neighbours:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (*with default*):
  only relevant if `show_neighbours` is `TRUE`. Data frame indicating
  which borders to consider, and their respective weights (see the
  description provided for
  [calc_MoransI](https://r-lum.github.io/Luminescence/reference/calc_MoransI.md)).
  If `NULL` (default), this is constructed automatically by the internal
  function `.get_Neighbours`.

- ignore_borders:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether only grain locations that do not lie on the border of the disc
  should be considered (`FALSE` by default). Thus if `TRUE`, only the
  inner 8x8 grain locations rather than the full 10x10 are considered.
  Ignored if `df_neighbours` is not `NULL` or if
  `show_neighbours = FALSE`.

- str_transform:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  The observed value of each individual grain is reflected in the size
  of a triangle (or other dot-like element). To account for large value
  differences, the transformation from value to triangle size can be
  `"lin"` (linear), `"log"` (logarithmic) and `"sqrt"` (square root).
  Defaults to `"sqrt"`, so that the surface is linear to the value. Note
  that the log and sqrt transformations can come with an addition to
  avoid negative values. When the legend is shown, the actual lower,
  middle and upper values are printed.

- ...:

  other arguments to be given to the base R plot function, such as
  `main`, `col` and `pch`. `legend` can be used to enable/disable the
  legend (`FALSE` by default).

## Details

Depending of the available plotting space, some optional elements might
have not enough room to be displayed. As this function is wrapped around
the base plot function, one can also choose to add elements manually.

## How to cite

Boer, A.d., Steinbuch, L., 2025. plot_SingleGrainDisc(): Plot a disc
with its values. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025. A
novel tool to assess crosstalk in single-grain luminescence detection.
Submitted.

## Author

Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research,
2025 , RLum Developer Team

## Examples

``` r
plot_SingleGrainDisc(1:100)

```
