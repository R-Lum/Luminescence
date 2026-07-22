# Fit a dose-response curve for luminescence data (Lx/Tx against dose)

A dose-response curve is produced for luminescence measurements using a
regenerative or additive protocol. The function supports interpolation
and extrapolation to calculate the equivalent dose.

## Usage

``` r
fit_DoseResponseCurve(
  object,
  mode = c("interpolation", "extrapolation", "alternate"),
  fit.method = c("SSE", "LIN", "QDR", "SSE OR LIN", "SSE+LIN", "DSE", "GOK", "OTOR",
    "OTORX"),
  fit.force_through_origin = FALSE,
  fit.weights = c("inverse_var", "inverse_std", "norm_inverse_std"),
  fit.includingRepeatedRegPoints = TRUE,
  fit.NumberRegPoints = NULL,
  fit.NumberRegPointsReal = NULL,
  fit.bounds = TRUE,
  n.MC = 100,
  txtProgressBar = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  [data.frame](https://rdrr.io/r/base/data.frame.html) or a
  [list](https://rdrr.io/r/base/list.html) of such objects
  (**required**): data frame with columns for `Dose`, `LxTx`,
  `LxTx.Error` and `TnTx`.

  The column for the test dose response is optional, but requires
  `'TnTx'` as column name if used. For exponential fits at least three
  dose points (including the natural) should be provided. If `object` is
  a list, the function is called on each of its elements.

  If `fit.method = "OTORX"` you have to provide the test dose in the
  same unit as the dose in a column called `Test_Dose`. The function
  searches explicitly for this column name. Only the first value will be
  used assuming a constant test dose over the measurement cycle.

- mode:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  selects calculation mode of the function.

  - `"interpolation"` (default) calculates the De by interpolation,

  - `"extrapolation"` calculates the equivalent dose by extrapolation
    (useful for MAAD measurements) and

  - `"alternate"` calculates no equivalent dose and just fits the data
    points.

  Please note that for option `"interpolation"` the first point is
  considered as natural dose.

- fit.method:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  function used for fitting. Possible options are: `LIN`, `QDR`, `SSE`,
  `SSE OR LIN`, `SSE+LIN`, `DSE` (not defined for extrapolation), `GOK`,
  `OTOR` and `OTORX`. See details.

- fit.force_through_origin:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*) allow
  to force the fitted function through the origin. For `method = "DSE"`
  the function will be fixed through the origin in either case, so this
  option will have no effect.

- fit.weights:

  [character](https://rdrr.io/r/base/character.html)
  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  weighting approach to be used for the fitting. Options are
  `inverse_var` (default), `inverse_std`, `norm_inverse_std`, a
  [numeric](https://rdrr.io/r/base/numeric.html) vector, or `NULL` (no
  weighting). If the input is a numeric vector, it must have length
  equal to the number of data points to fit (usually the `LxTx` values).
  See details.

- fit.includingRepeatedRegPoints:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  includes repeated points for fitting (`TRUE`/`FALSE`).

- fit.NumberRegPoints:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): set
  number of regeneration points manually. By default the number of all
  (!) regeneration points is used automatically.

- fit.NumberRegPointsReal:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): if the
  number of regeneration points is provided manually, the value of the
  real, regeneration points = all points (repeated points) including reg
  0, has to be inserted.

- fit.bounds:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): set
  lower fit bounds for all fitting parameters to 0. Limited to use with
  the fit methods `SSE`, `SSE+LIN`, `SSE OR LIN`, `GOK`, `OTOR`, `OTORX`
  Argument to be inserted for experimental application only!

- n.MC:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of Monte Carlo simulations for error estimation.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar. If `verbose = FALSE` also no
  `txtProgressBar` is shown.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  Further arguments to be passed (currently ignored).

## Value

An
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the slot `data` with the following
elements:

**Overview elements**

|  |  |  |
|----|----|----|
| **DATA.OBJECT** | **TYPE** | **DESCRIPTION** |
| `..$De` : | `data.frame` | Table with De values |
| `..$De.MC` : | `numeric` | Table with De values from MC runs |
| `..$Fit` : | [nls](https://rdrr.io/r/stats/nls.html) or [lm](https://rdrr.io/r/stats/lm.html) | object from the fitting for `SSE`, `SSE+LIN` and `DSE`. In case of a resulting linear fit when using `LIN`, `QDR` or `SSE OR LIN` |
| `..Fit.Args` : | `list` | Arguments to the function |
| `..$Formula` : | [expression](https://rdrr.io/r/base/expression.html) | Fitting formula as R expression |

The `@info` slot contains the following elements:

|                   |             |                            |
|-------------------|-------------|----------------------------|
| **DATA.OBJECT**   | **TYPE**    | **DESCRIPTION**            |
| `..$fit_message`: | `character` | The fit message reported   |
| `..$call` :       | `call`      | The original function call |

If `object` is a list, then the function returns a list of
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
objects as defined above.

**Details - `DATA.OBJECT$De`** This object is a
[data.frame](https://rdrr.io/r/base/data.frame.html) with the following
columns

|  |  |  |
|----|----|----|
| `De` | [numeric](https://rdrr.io/r/base/numeric.html) | equivalent dose |
| `De.Error` | [numeric](https://rdrr.io/r/base/numeric.html) | standard error the equivalent dose |
| `D01` | [numeric](https://rdrr.io/r/base/numeric.html) | \\D_0\\ value, curvature parameter of the exponential |
| `D01.ERROR` | [numeric](https://rdrr.io/r/base/numeric.html) | standard error of the \\D_0\\ value |
| `D02` | [numeric](https://rdrr.io/r/base/numeric.html) | 2nd \\D_0\\ value, only for `DSE` |
| `D02.ERROR` | [numeric](https://rdrr.io/r/base/numeric.html) | standard error for 2nd \\D_0\\; only for `DSE` |
| `R` | [numeric](https://rdrr.io/r/base/numeric.html) | the material specific parameter \\R\\ (only `OTOR` and `OTORX`) |
| `R.LOWER` | [numeric](https://rdrr.io/r/base/numeric.html) | lower 25% quantile of \\R\\ |
| `R.UPPER` | [numeric](https://rdrr.io/r/base/numeric.html) | upper 75% quantile of \\R\\ |
| `Dc` | [numeric](https://rdrr.io/r/base/numeric.html) | value indicating saturation level; only for `OTOR` |
| `Dc.LOWER` | [numeric](https://rdrr.io/r/base/numeric.html) | lower 25% quantile for `Dc`; only for `OTOR` |
| `Dc.UPPER` | [numeric](https://rdrr.io/r/base/numeric.html) | upper 75% quantile for `Dc`; only for `OTOR` |
| `D63` | [numeric](https://rdrr.io/r/base/numeric.html) | the specific saturation level; only for `OTOR`, `OTORX` |
| `D63.LOWER` \\ tab [numeric](https://rdrr.io/r/base/numeric.html) | lower 25% quantile of `D63`; only for `OTOR`, `OTORX` | `D63.UPPER` \\ tab [numeric](https://rdrr.io/r/base/numeric.html) |
| upper 75% quantile of `D63`; only for `OTOR`, `OTORX` | `D80` | [numeric](https://rdrr.io/r/base/numeric.html) |
| the specific saturation level; only for `SSE`, `OTOR`, `OTORX` | `D80.LOWER` \\ tab [numeric](https://rdrr.io/r/base/numeric.html) | lower 25% quantile of `D80`; only for `OTOR`, `OTORX` |
| `D80.UPPER` \\ tab [numeric](https://rdrr.io/r/base/numeric.html) | upper 75% quantile of `D80`; only for `OTOR`, `OTORX` | `n_N` |
| [numeric](https://rdrr.io/r/base/numeric.html) | saturation level of dose-response curve derived via integration from the used function; it compares the full integral of the curves (`N`) to the integral until `De` (`n`) (e.g., Guralnik et al., 2015) | `De.MC` |
| [numeric](https://rdrr.io/r/base/numeric.html) | equivalent dose derived by Monte-Carlo simulation; ideally identical to `De` | `Fit` |
| [character](https://rdrr.io/r/base/character.html) | applied fit function | `Mode` |
| [character](https://rdrr.io/r/base/character.html) | mode used in fitting | `HPDI68_L` |
| [numeric](https://rdrr.io/r/base/numeric.html) | highest probability density of the approximated equivalent dose probability curve representing the lower boundary of 68% probability | `HPDI68_U` |
| [numeric](https://rdrr.io/r/base/numeric.html) | same as `HPDI68_L` for the upper bound | `HPDI95_L` |
| [numeric](https://rdrr.io/r/base/numeric.html) | same as `HPDI68_L` but for 95% probability | `HPDI95_U` |
| [numeric](https://rdrr.io/r/base/numeric.html) | same as `HPDI95_L` but for the upper bound | `.De.plot` |
| [numeric](https://rdrr.io/r/base/numeric.html) | equivalent dose used internally for plotting | `.De.raw` |
| [numeric](https://rdrr.io/r/base/numeric.html) | equivalent dose reported 'as is', that is, containing infinities and negative values if they could be calculated. Bear in mind that negative values are meaningless and may be arbitrary. |  |

## Details

### Implemented fitting methods

For all options (except for the `LIN`, `QDR` and the `SSE OR LIN`), the
[minpack.lm::nlsLM](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)
function with the `LM` (Levenberg-Marquardt algorithm) algorithm is
used. Note: For historical reasons for the Monte Carlo simulations
partly the function [nls](https://rdrr.io/r/stats/nls.html) using the
`port` algorithm.

The solution is found by transforming the function or using
[stats::uniroot](https://rdrr.io/r/stats/uniroot.html).

**Keyword: `LIN`**

Fits a linear function to the data using
[lm](https://rdrr.io/r/stats/lm.html): \$\$y = mx + D_i\$\$

**Keyword: `QDR`**

Fits a linear function with a quadratic term to the data using
[lm](https://rdrr.io/r/stats/lm.html): \$\$y = a + bx + cx^2\$\$

**Keyword: `SSE` (formerly `EXP`)**

Fits a single saturating exponential function of the form \$\$y = N (1 -
\exp(-\frac{x + D_i}{D_0}))\$\$

Parameters \\D_0\\ and \\D_i\\ are approximated by a linear fit using
[lm](https://rdrr.io/r/stats/lm.html).

**Keyword: `SSE OR LIN` (formerly `EXP OR LIN`)**

Works for some cases where an `SSE` fit fails. If the `SSE` fit fails, a
`LIN` fit is done instead, which always works.

**Keyword: `SSE+LIN` (formerly `EXP+LIN`)**

Tries to fit an exponential plus linear function of the form:

\$\$y = N(1 - \exp(-\frac{x + D_i}{D_0}) + gx)\$\$ The \\D_e\\ is
calculated by iteration.

**Note:** In the context of luminescence dating, this function has no
physical meaning. Therefore, no \\D_0\\ value is returned.

**Keyword: `DSE` (formerly `EXP+EXP`)**

Tries to fit a double exponential function of the form

\$\$y = N_1 (1 - \exp(-\frac{x + D_i}{D0_1})) + N_2 (1 - \exp(-\frac{x +
D_i}{D0_2}))\$\$

*This fitting procedure is not really robust against wrong start
parameters.*

**Keyword: `GOK`**

Tries to fit the general-order kinetics function following Guralnik et
al. (2015) of the form

\$\$y = a (d - (1 + \frac{1}{D_0} x c)^{-1 / c})\$\$

where \\c \> 0\\ is a kinetic order modifier.

**Keyword: `OTOR`** (formerly `LambertW`)

This tries to fit a dose-response curve based on the Lambert W function
and the one trap one recombination centre (OTOR) model according to
Pagonis et al. (2020). The function has the form:

\$\$y = (1 + (\mathcal{W}((R - 1) \* \exp(R - 1 - (x + D_i) / D_c)) /
(1 - R))) \* N\$\$

with \\W\\ the Lambert-W function (calculated using
[lamW::lambertW0](https://rdrr.io/pkg/lamW/man/lamW.html)), \\R\\ the
dimensionless retrapping ratio, \\N\\ the total concentration of
trappings states in cm\\^{-3}\\, \\D\_{c} = N/R\\ a constant, and
\\D\_{i}\\ is the offset on the x-axis (not part of the original formula
in Pagonis et al. 2020). Note that \\R\\ and \\D\_{c}\\ have a valid
physical interpretation only when saturation is reached. Please note
that finding the root in `mode = "extrapolation"` is a non-easy task due
to the shape of the function and the results might be unexpected.

**Keyword: `OTORX`**

This adapts extended OTOR (therefore: OTORX) model proposed by Lawless
and Timar-Gabor (2024) accounting for retrapping (the equation
implemented here is written slightly differently than in the original
manuscript):

\$\$F\_{OTORX} = 1 + \left\[\mathcal{W}\left(-Q \*
\exp\left(-Q-(1-Q(1-\frac{1}{\exp(1)})) \frac{D +
D_i}{D\_{63}}\right)\right)\right\] / Q\$\$

with

\$\$Q = \frac{A_m - A_n}{A_m}\frac{N}{N+N_D}\$\$

where \\A_m\\ and \\A_n\\ are rate constants for the recombination and
the trapping of electrons (\\N\\), respectively. \\D\_{63}\\ corresponds
to the value at which the trap occupation corresponds to 63% of the
saturation value. \\D_i\\ is an offset: if set to zero, the curve will
be forced through the origin as in the original publication.

For the implementation the calculation reads further

\$\$y = \frac{F\_{OTORX}(((D + D_i)/D\_{63}),
Q)}{F\_{OTORX}((D\_{test} + D_i)/D\_{63}, Q)}\$\$

with \\D\_{test}\\ being the test dose in the same unit (usually s or
Gy) as the regeneration dose points. This value is essential and needs
to provided along with the usual dose and \\\frac{L_x}{T_x}\\ values
(see `object` parameter input and the example section). For more details
see Lawless and Timar-Gabor (2024).

The fit also returns the parameter \\R\\ know from `OTOR`, which is
derived as \\R = 1 - Q\\.

*Note: The offset adder \\D_i\\ is not part of the formula in
Timar-Gabor (2024) and can be set to zero with the option
`fit.force_through_origin = TRUE`*

**Fit weighting**

- `"inverse_var"` (inverse variance weighting - current default) \$\$w_i
  = \frac{1}{\sigma_i^2}\$\$

- `"inverse_std"` (inverse standard error) \$\$w_i =
  \frac{1}{\sigma_i}\$\$

- `"norm_inverse_std"` (normalised inverse standard error weighting -
  default up to v1.2.1) \$\$w_i =
  \frac{\frac{1}{\sigma_i}}{\Sigma{\frac{1}{\sigma_i}}}\$\$ *Although
  used until Luminescence v1.2.1, this method is no longer recommended,
  as it does not align with the mathematical approach used in common nls
  fitting methods.*

If the option `fit.weights = NULL` all weights are set to 1, which
disables weighting altogether. If `fit.weights` is a
[numeric](https://rdrr.io/r/base/numeric.html) vector of correct length
(same number of rows as the input `LxTx`), then those fit weights are
used. This may be helpful to compare different fitting algorithms that
have implemented fit weights differently.

**Error estimation using Monte Carlo simulation**

Error estimation is done using a parametric bootstrap. A set of
\\\frac{L_x}{T_x}\\ values is constructed by randomly drawing curve data
from normal distributions defined by the input values (`mean = value`,
`sd = value.error`). A dose-response curve is then fitted for each
sampled dataset using the chosen fitting method, producing a
distribution of single `De` values. The standard deviation of this
distribution is taken as the error of the `De`. With more iterations
(`n.MC`) the error estimate stabilizes. However, naturally the error
will not decrease with more MC runs.

Alternatively, the function returns highest probability density interval
estimates as output, users may find more useful under certain
circumstances.

**Note:** It may take some calculation time with increasing MC runs,
especially for the composed functions (`SSE+LIN` and `DSE`).

## Function version

1.7

## How to cite

Kreutzer, S., Dietze, M., Colombo, M., 2026. fit_DoseResponseCurve():
Fit a dose-response curve for luminescence data (Lx/Tx against dose).
Function version 1.7. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A., 2026.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.3.0. https://r-lum.github.io/Luminescence/

## References

Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits.
Ancient TL 7, 43-46.
[doi:10.26034/la.atl.1989.150](https://doi.org/10.26034/la.atl.1989.150)

Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li,
S.-H., Pagonis, P., Herman, F., 2015. Radiation-induced growth and
isothermal decay of infrared-stimulated luminescence from feldspar.
Radiation Measurements 81, 224-231.
[doi:10.1016/j.radmeas.2015.02.011](https://doi.org/10.1016/j.radmeas.2015.02.011)

Lawless, J.L., Timar-Gabor, A., 2024. A new analytical model to fit both
fine and coarse grained quartz luminescence dose response curves.
Radiation Measurements 170, 107045.
[doi:10.1016/j.radmeas.2023.107045](https://doi.org/10.1016/j.radmeas.2023.107045)

Pagonis, V., Kitis, G., Chen, R., 2020. A new analytical equation for
the dose response of dosimetric materials, based on the Lambert W
function. Journal of Luminescence 225, 117333.
[doi:10.1016/j.jlumin.2020.117333](https://doi.org/10.1016/j.jlumin.2020.117333)

## See also

[plot_DoseResponseCurve](https://r-lum.github.io/Luminescence/reference/plot_DoseResponseCurve.md),
[nls](https://rdrr.io/r/stats/nls.html),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md),
[minpack.lm::nlsLM](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html),
[lm](https://rdrr.io/r/stats/lm.html),
[uniroot](https://rdrr.io/r/stats/uniroot.html),
[lamW::lambertW0](https://rdrr.io/pkg/lamW/man/lamW.html)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  
Michael Dietze, RWTH Aachen (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## Examples

``` r

##(1) fit growth curve for a dummy data.set and show De value
data(ExampleData.LxTxData, envir = environment())
temp <- fit_DoseResponseCurve(LxTxData)
#> [fit_DoseResponseCurve()] Fit:    SSE (interpolation) | De = 1737.71 | D01 = 1721.83
get_RLum(temp)
#>         De De.Error      D01 D01.ERROR D02 D02.ERROR  R R.LOWER R.UPPER Dc
#> 1 1737.707 62.49229 1721.831  79.09111  NA        NA NA      NA      NA NA
#>   Dc.LOWER Dc.UPPER D63 D63.LOWER D63.UPPER      D80 D80.LOWER D80.UPPER
#> 1       NA       NA  NA        NA        NA 2770.426        NA        NA
#>         n_N    De.MC Fit          Mode HPDI68_L HPDI68_U HPDI95_L HPDI95_U
#> 1 0.5284827 1742.485 SSE interpolation 1671.255 1803.028  1615.91 1864.523
#>   .De.plot  .De.raw
#> 1 1737.707 1737.707

##(1b) to access the fitting value try
get_RLum(temp, data.object = "Fit")
#> Nonlinear regression model
#>   model: y ~ N * (1 - exp(-(x + Di)/D0))
#>    data: data
#>        N       D0       Di 
#>    6.707 1721.831    4.641 
#>  weighted residual sum-of-squares: 6.374
#> 
#> Number of iterations to convergence: 8 
#> Achieved convergence tolerance: 1.49e-08

##(2) fit using the 'extrapolation' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(fit_DoseResponseCurve(LxTxData, mode = "extrapolation"))
#> [fit_DoseResponseCurve()] Fit:    SSE (extrapolation) | De = 139.25 | D01 = 3059.14
#> 
#>  [RLum.Results-class]
#>   originator: fit_DoseResponseCurve()
#>   data: 5
#>       .. $De : data.frame
#>   .. $De.MC : numeric
#>   .. $Fit : nls
#>   .. $Fit.Args : list
#>   .. $Formula : expression
#>   additional info elements:  2 

##(3) fit using the 'alternate' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(fit_DoseResponseCurve(LxTxData, mode = "alternate"))
#> [fit_DoseResponseCurve()] Fit:    SSE (alternate) | De = NA | D01 = 3059.14
#> 
#>  [RLum.Results-class]
#>   originator: fit_DoseResponseCurve()
#>   data: 5
#>       .. $De : data.frame
#>   .. $De.MC : numeric
#>   .. $Fit : nls
#>   .. $Fit.Args : list
#>   .. $Formula : expression
#>   additional info elements:  2 

##(4) import and fit test data set by Berger & Huntley 1989
QNL84_2_unbleached <-
read.table(system.file("extdata/QNL84_2_unbleached.txt", package = "Luminescence"))

results <- fit_DoseResponseCurve(
 QNL84_2_unbleached,
 mode = "extrapolation",
 verbose = FALSE)
#> Warning: [fit_DoseResponseCurve()] Error column invalid, infinite, or contains 0, 'fit.weights' reset to NULL

#calculate confidence interval for the parameters
#as alternative error estimation
confint(results$Fit, level = 0.68)
#> Waiting for profiling to be done...
#>            16%         84%
#> N  140543.3023 146731.8470
#> D0    374.0861    425.5679
#> Di    116.3499    133.3474

if (FALSE) { # \dontrun{
##(5) special case the OTORX model with test dose column
df <- cbind(LxTxData, Test_Dose = 15)
fit_DoseResponseCurve(object = df, fit.method = "OTORX", n.MC = 10) |>
 plot_DoseResponseCurve()

QNL84_2_bleached <-
read.table(system.file("extdata/QNL84_2_bleached.txt", package = "Luminescence"))
STRB87_1_unbleached <-
read.table(system.file("extdata/STRB87_1_unbleached.txt", package = "Luminescence"))
STRB87_1_bleached <-
read.table(system.file("extdata/STRB87_1_bleached.txt", package = "Luminescence"))

print(
 fit_DoseResponseCurve(
 QNL84_2_bleached,
 mode = "alternate",
 verbose = FALSE)$Fit)

print(
 fit_DoseResponseCurve(
 STRB87_1_unbleached,
 mode = "alternate",
 verbose = FALSE)$Fit)

print(
 fit_DoseResponseCurve(
 STRB87_1_bleached,
 mode = "alternate",
 verbose = FALSE)$Fit)
 } # }
```
