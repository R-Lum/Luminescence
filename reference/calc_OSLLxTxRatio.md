# Calculate `Lx/Tx` ratio for CW-OSL curves

Calculate `Lx/Tx` ratios from a given set of CW-OSL curves assuming late
light background subtraction.

## Usage

``` r
calc_OSLLxTxRatio(
  Lx.data,
  Tx.data = NULL,
  signal.integral = NULL,
  signal.integral.Tx = NULL,
  background.integral = NULL,
  background.integral.Tx = NULL,
  background.count.distribution = "non-poisson",
  use_previousBG = FALSE,
  sigmab = NULL,
  sig0 = 0,
  digits = NULL
)
```

## Arguments

- Lx.data:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): requires a CW-OSL shine down curve (x = time, y =
  counts)

- Tx.data:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*):
  requires a CW-OSL shine down curve (x = time, y = counts). If no input
  is given the `Tx.data` will be treated as `NA` and no `Lx/Tx` ratio is
  calculated.

- signal.integral:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): vector
  with the limits for the signal integral. If set to `NA`, no integrals
  are considered and all other integrals are ignored.

- signal.integral.Tx:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): vector
  with the limits for the signal integral for the `Tx`-curve. If
  missing, the value from `signal.integral` is used.

- background.integral:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): vector
  with the limits for the background integral. If set to `NA`, no
  integrals are considered and all other integrals ignored.

- background.integral.Tx:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): vector
  with the limits for the background integral for the `Tx` curve. If
  missing, the value from `background.integral` is used.

- background.count.distribution:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  sets the count distribution assumed for the error calculation.
  Possible arguments are `"poisson"` or `"non-poisson"` (default). See
  details for further information.

- use_previousBG:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): If
  set to `TRUE` the background of the `Lx`-signal is subtracted also
  from the `Tx`-signal. Please note that in this case separate signal
  integral limits for the `Tx`-signal are not allowed and will be reset.

- sigmab:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): option to
  set a manual value for the overdispersion (for `LnTx` and `TnTx`),
  used for the `Lx/Tx` error calculation. The value should be provided
  as absolute squared count values, e.g. `sigmab = c(300,300)`.
  **Note:** If only one value is provided this value is taken for both
  (`LnTx` and `TnTx`) signals.

- sig0:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): allow
  adding an extra component of error to the final `Lx/Tx` error value
  (e.g., instrumental error, see details).

- digits:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*): round
  numbers to the specified digits. If set to `NULL` no rounding occurs.

## Value

Returns an S4 object of type
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md).

Slot `data` contains a [list](https://rdrr.io/r/base/list.html) with the
following structure:

**@data**

    $LxTx.table (data.frame)
    .. $ LnLx
    .. $ LnLx.BG
    .. $ TnTx
    .. $ TnTx.BG
    .. $ Net_LnLx
    .. $ Net_LnLx.Error
    .. $ Net_TnTx
    .. $ Net_TnTx.Error
    .. $ LxTx
    .. $ LxTx.Error
    $ calc.parameters (list)
    .. $ sigmab.LnTx
    .. $ sigmab.TnTx
    .. $ k

**@info**

    $ call (original function call)

## Details

The integrity of the chosen values for the signal and background
integral is checked by the function; the signal integral limits have to
be lower than the background integral limits. If a
[vector](https://rdrr.io/r/base/vector.html) is given as input instead
of a [data.frame](https://rdrr.io/r/base/data.frame.html), an artificial
[data.frame](https://rdrr.io/r/base/data.frame.html) is produced. The
error calculation is done according to Galbraith (2002).

**Please note:** In cases where the calculation results in `NaN` values
(for example due to zero-signal, and therefore a division of 0 by 0),
these `NaN` values are replaced by 0.

**`sigmab`**

The default value of `sigmab` is calculated assuming the background is
constant and **would not** applicable when the background varies as,
e.g., as observed for the early light subtraction method.

**`sig0`**

This argument allows to add an extra component of error to the final
`Lx/Tx` error value. The input will be treated as factor that is
multiplied with the already calculated `LxTx` and the result is add up
by:

\$\$se(LxTx) = \sqrt(se(LxTx)^2 + (LxTx \* sig0)^2)\$\$

**`background.count.distribution`**

This argument allows selecting the distribution assumption that is used
for the error calculation. According to Galbraith (2002, 2014) the
background counts may be overdispersed (i.e. do not follow a Poisson
distribution, which is assumed for the photomultiplier counts). In that
case (might be the normal case) it has to be accounted for the
overdispersion by estimating \\\sigma^2\\ (i.e. the overdispersion
value). Therefore the relative standard error is calculated as:

- `poisson` \$\$rse(\mu\_{S}) \approx \sqrt(Y\_{0} +
  Y\_{1}/k^2)/Y\_{0} - Y\_{1}/k\$\$

- `non-poisson` \$\$rse(\mu\_{S}) \approx \sqrt(Y\_{0} + Y\_{1}/k^2 +
  \sigma^2(1+1/k))/Y\_{0} - Y\_{1}/k\$\$

**Please note** that when using the early background subtraction method
in combination with the 'non-poisson' distribution argument, the
corresponding `Lx/Tx` error may considerably increase due to a high
`sigmab` value. Please check whether this is valid for your data set and
if necessary consider to provide an own `sigmab` value using the
corresponding argument `sigmab`.

## Note

The results of this function have been cross-checked with the Analyst
(version 3.24b). Access to the results object via
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

**Caution:** If you are using early light subtraction (EBG), please
either provide your own `sigmab` value or use
`background.count.distribution = "poisson"`.

## Function version

0.8.2

## How to cite

Kreutzer, S., 2025. calc_OSLLxTxRatio(): Calculate Lx/Tx ratio for
CW-OSL curves. Function version 0.8.2. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Duller, G., 2018. Analyst v4.57 - User Manual.
`https://users.aber.ac.uk/ggd`  

Galbraith, R.F., 2002. A note on the variance of a background-corrected
OSL count. Ancient TL, 20 (2), 49-51.
[doi:10.26034/la.atl.2002.348](https://doi.org/10.26034/la.atl.2002.348)

Galbraith, R.F., 2014. A further note on the variance of a
background-corrected OSL count. Ancient TL, 31 (2), 1-3.
[doi:10.26034/la.atl.2014.477](https://doi.org/10.26034/la.atl.2014.477)

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[fit_DoseResponseCurve](https://r-lum.github.io/Luminescence/reference/fit_DoseResponseCurve.md),
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##load data
data(ExampleData.LxTxOSLData, envir = environment())

##calculate Lx/Tx ratio
results <- calc_OSLLxTxRatio(
 Lx.data = Lx.data,
 Tx.data = Tx.data,
 signal.integral = c(1:2),
 background.integral = c(85:100))

##get results object
get_RLum(results)
#>    LnLx LnLx.BG TnTx TnTx.BG Net_LnLx Net_LnLx.Error Net_TnTx Net_TnTx.Error
#> 1 81709     530 7403     513    81179       286.5461     6890       88.53581
#>       LxTx LxTx.Error
#> 1 11.78215  0.1570077
```
