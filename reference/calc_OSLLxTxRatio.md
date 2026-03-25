# Calculate `Lx/Tx` ratio for CW-OSL curves

Calculate `Lx/Tx` ratios from a given set of CW-OSL curves assuming late
light background subtraction.

## Usage

``` r
calc_OSLLxTxRatio(
  Lx.data,
  Tx.data = NULL,
  signal_integral = NULL,
  background_integral = NULL,
  signal_integral_Tx = NULL,
  background_integral_Tx = NULL,
  integral_input = c("channel", "measurement"),
  background.count.distribution = "non-poisson",
  use_previousBG = FALSE,
  sigmab = NULL,
  sig0 = 0,
  digits = NULL,
  ...
)
```

## Arguments

- Lx.data:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
  [data.frame](https://rdrr.io/r/base/data.frame.html),
  [list](https://rdrr.io/r/base/list.html) (**required**): requires a
  CW-OSL shine down curve (x = time, y = counts). Data can also be
  provided as a list.

- Tx.data:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*):
  requires a CW-OSL shine down curve (x = time, y = counts). If no input
  is given the `Tx.data` will be treated as `NA` and no `Lx/Tx` ratio is
  calculated. When `Lx.data` is a list, it must be provided as a list of
  the same length.

- signal_integral:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): vector
  of channels for the signal integral. If set to `NA` (alternate mode),
  no integrals are taken into account and their settings are ignored.

- background_integral:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): vector
  of channels for the background integral. If set to `NA`, no background
  integral is subtracted; in this case, `sigmab.LnLx` (unless manually
  set) and the signal-to-noise ratio for Ln/Lx`will be`NA\`.

- signal_integral_Tx:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): vector of
  channels for the signal integral for the `Tx` curve. If `NULL`, the
  `signal_integral` vector is used.

- background_integral_Tx:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): vector of
  channels for the background integral for the `Tx` curve. If `NULL`,
  the `background_integral` vector is used. If set to `NA`, no
  background integral for the `Tx` curve is subtracted; in this case,
  `sigmab.TxTx` (unless manually set) and the signal-to-noise ratio for
  `Tn/Tx` will be `NA`.

- integral_input:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  input type for `signal_integral`, one of `"channel"` (default) or
  `"measurement"`. If set to `"measurement"`, the best matching channels
  corresponding to the given time/temperature range are selected.

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

- ...:

  currently not used.

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
    .. $ SN_RATIO_LnLx,
    .. $ SN_RATIO_TnTx,
    .. $ LxTx
    .. $ LxTx.Error
    $calc.parameters (list)
    .. $ sigmab.LnTx
    .. $ sigmab.TnTx
    .. $ k

**@info**

    $ call (original function call)

## Details

The function checks the integrity of the values chosen for the signal
and background integrals; the signal integral limits have to be lower
than the background integral limits. If a
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

\$\$se(LxTx) = \sqrt{se(LxTx)^2 + (LxTx \* sig0)^2}\$\$

**`SN_RATIO_LnLx` and `SN_RATIO_TnTx`**

For convenience, the function returns the signal-to-noise ratio
(`SN_RATIO`) for the `LnLx` and the `TnTx` curves. This is simply the
signal divided by the background signal counts normalised to the `k`
value (see below).

**`background.count.distribution`**

This argument allows selecting the distribution assumption that is used
for the error calculation. According to Galbraith (2002, 2014) the
background counts may be overdispersed (i.e. do not follow a Poisson
distribution, which is assumed for the photomultiplier counts). In that
case (might be the normal case) it has to be accounted for the
overdispersion by estimating \\\sigma^2\\ (i.e. the overdispersion
value). Therefore the relative standard error is calculated as:

- `poisson` \$\$rse(\mu\_{S}) \approx \sqrt{Y\_{0} + Y\_{1}/k^2) /
  (Y\_{0} - Y\_{1}/k)} \$\$

- `non-poisson` \$\$rse(\mu\_{S}) \approx \sqrt{Y\_{0} + Y\_{1}/k^2 +
  \sigma^2(1+1/k)) / (Y\_{0} - Y\_{1}/k)} \$\$

If `background_integral = NA`, then in both cases the relative standard
error simplifies to:

\$\$rse(\mu\_{S}) \approx \sqrt{Y\_{0}} / Y\_{0}\$\$

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

0.9.8

## How to cite

Kreutzer, S., Colombo, M., 2026. calc_OSLLxTxRatio(): Calculate Lx/Tx
ratio for CW-OSL curves. Function version 0.9.8. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
Bluszcz, A., 2026. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.2.1. https://r-lum.github.io/Luminescence/

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

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## Examples

``` r
##load data
data(ExampleData.LxTxOSLData, envir = environment())

##calculate Lx/Tx ratio
results <- calc_OSLLxTxRatio(
 Lx.data = Lx.data,
 Tx.data = Tx.data,
 signal_integral = 1:2,
 background_integral = 85:100)

##get results object
get_RLum(results)
#>    LnLx LnLx.BG TnTx TnTx.BG Net_LnLx Net_LnLx.Error Net_TnTx Net_TnTx.Error
#> 1 81709     530 7403     513    81179       285.9637     6890       86.91786
#>   SN_RATIO_LnLx SN_RATIO_TnTx     LxTx LxTx.Error
#> 1      154.1679       14.4308 11.78215  0.1543187
```
