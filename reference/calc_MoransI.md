# Calculate Moran's I

Calculate Moran's I

## Usage

``` r
calc_MoransI(
  object,
  df_neighbours = NULL,
  spatial_autocorrelation = TRUE,
  compute_pseudo_p = FALSE,
  tested_moransI = NULL,
  n_permutations = 999,
  ignore_borders = FALSE,
  return_intermediate_values = FALSE
)
```

## Arguments

- object:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [numeric](https://rdrr.io/r/base/numeric.html) (**required**)
  containing the values of the grains of one. Should have length 100;
  can contain `NA` values.

- df_neighbours:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (*with default*):
  a data frame with 3 columns (`location`, `neighbour` and `weight`,
  respectively), with each row indicating the index of one location and
  one of its neighbours (note that the concept of "location" versus
  "neighbour" is symmetric, so each neighbouring pair needs to be
  specified only once), alongside their relative weight (generally set
  to 1). If `NULL` (default), this is constructed automatically by the
  internal function `.get_Neighbours`.

- spatial_autocorrelation:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether spatial autocorrelation should be considered in the
  computation of Moran's I (`TRUE` by default). If `FALSE`, the function
  computes Moran's I expected value in case of no autocorrelation (H_0).
  See details for further information.

- compute_pseudo_p:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether a pseudo p-value should be computed (`FALSE` by default).

- tested_moransI:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): The
  value of Moran's I to be tested against when computing the pseudo
  p-value. If `NULL` (default), the value calculated by the function
  will be used. Ignored if `compute_pseudo_p` is `FALSE`.

- n_permutations:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of random permutations tested to calculate the fraction which
  is the pseudo p (defaults to 999). Influences the calculation speed,
  which will have impact in case of large scale simulation loops.
  Ignored if `compute_pseudo_p` is `FALSE`.

- ignore_borders:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether only grain locations that do not lie on the border of the disc
  should be considered (`FALSE` by default). Thus if `TRUE`, only the
  inner 8x8 grain locations rather than the full 10x10 are considered.
  Ignored if `df_neighbours` is not `NULL` or if
  `spatial_autocorrelation = FALSE`.

- return_intermediate_values:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether the function should return a list with several intermediate
  calculation results (defaults to `FALSE`). Ignored if
  `spatial_autocorrelation` is `FALSE`.

## Value

By default one numerical value, roughly between -1 and 1, where close to
zero means no spatial correlation, and value close to 1 a positive
spatial correlation given the pattern we interested in (by default all
rook neighbours). A value closer to -1 has no meaning within the context
of luminescence crosstalk. If `compute_pseudo_p = TRUE`, then the
computed pseudo p-value is returned. If `return_intermediate_values` is
set to `TRUE`, a list with several values used for calculation is
returned instead of a single outcome.

## Details

**Case of no spatial autocorrelation**

Perhaps a bit counter-intuitive, the expected value of Moran's I under
the null hypothesis of no spatial correlation is a value slightly
smaller than zero. When setting `spatial_autocorrelation = FALSE`, this
function calculates the expected value based on the number of
observations or the length of the observation vector (while taking out
`NA` values). Note that the expected value only depends on the number of
observed separate grain values. This can be useful for plotting.

The expected Moran's I for the null hypothesis of no spatial correlation
corresponds to `-1 / (n - 1)`, with `n` being the number of non-missing
observations.

## How to cite

Boer, A.d., Steinbuch, L., 2025. calc_MoransI(): Calculate Moran's I.
In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C.,
Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## References

de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025. A
novel tool to assess crosstalk in single-grain luminescence detection.
Submitted.

## Author

Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research,
2025 , RLum Developer Team

## Examples

``` r
## Test a fictional sample with spatial correlation
calc_MoransI(object = c(1:100))
#> [1] 0.8888889

## Test some fictional samples without spatial correlation;
## note the randomness with each repetition
calc_MoransI(object = rnorm(n = 100))
#> [1] -0.05292131
calc_MoransI(object = rnorm(n = 100))
#> [1] 0.003450826
calc_MoransI(object = rnorm(n = 100))
#> [1] 0.01774666
```
