# Fit a dose-response curve for luminescence data (Lx/Tx against dose)

A dose-response curve is produced for luminescence measurements using a
regenerative or additive protocol. The function supports interpolation
and extrapolation to calculate the equivalent dose.

## Usage

``` r
fit_DoseResponseCurve(
  object,
  mode = "interpolation",
  fit.method = "EXP",
  fit.force_through_origin = FALSE,
  fit.weights = TRUE,
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
  `LxTx.Error` and `TnTx`. The column for the test dose response is
  optional, but requires `'TnTx'` as column name if used. For
  exponential fits at least three dose points (including the natural)
  should be provided. If `object` is a list, the function is called on
  each of its elements. If `fit.method = "OTORX"` you have to provide
  the test dose in the same unit as the dose in a column called
  `Test_Dose`. The function searches explicitly for this column name.
  Only the first value will be used assuming a constant test dose over
  the measurement cycle.

- mode:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  selects calculation mode of the function.

  - `"interpolation"` (default) calculates the De by interpolation,

  - `"extrapolation"` calculates the equivalent dose by extrapolation
    (useful for MAAD measurements) and

  - `"alternate"` calculates no equivalent dose and just fits the data
    points.

  Please note that for option `"interpolation"` the first point is
  considered as natural dose

- fit.method:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  function used for fitting. Possible options are:

  - `LIN`,

  - `QDR`,

  - `EXP`,

  - `EXP OR LIN`,

  - `EXP+LIN`,

  - `EXP+EXP` (not defined for extrapolation),

  - `GOK`,

  - `OTOR`,

  - `OTORX`

  See details.

- fit.force_through_origin:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*) allow
  to force the fitted function through the origin. For
  `method = "EXP+EXP"` the function will be fixed through the origin in
  either case, so this option will have no effect.

- fit.weights:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  option whether the fitting is done with or without weights. See
  details.

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
  lower fit bounds for all fitting parameters to 0. Limited for the use
  with the fit methods `EXP`, `EXP+LIN`, `EXP OR LIN`, `GOK`, `OTOR`,
  `OTORX` Argument to be inserted for experimental application only!

- n.MC:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of Monte Carlo simulations for error estimation, see details.

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

An `RLum.Results` object is returned containing the slot `data` with the
following elements:

**Overview elements**

|                 |                                                                                  |                                                                                                                                       |
|-----------------|----------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| **DATA.OBJECT** | **TYPE**                                                                         | **DESCRIPTION**                                                                                                                       |
| `..$De` :       | `data.frame`                                                                     | Table with De values                                                                                                                  |
| `..$De.MC` :    | `numeric`                                                                        | Table with De values from MC runs                                                                                                     |
| `..$Fit` :      | [nls](https://rdrr.io/r/stats/nls.html) or [lm](https://rdrr.io/r/stats/lm.html) | object from the fitting for `EXP`, `EXP+LIN` and `EXP+EXP`. In case of a resulting linear fit when using `LIN`, `QDR` or `EXP OR LIN` |
| `..Fit.Args` :  | `list`                                                                           | Arguments to the function                                                                                                             |
| `..$Formula` :  | [expression](https://rdrr.io/r/base/expression.html)                             | Fitting formula as R expression                                                                                                       |
| `..$call` :     | `call`                                                                           | The original function call                                                                                                            |

If `object` is a list, then the function returns a list of
`RLum.Results` objects as defined above.

**Details - `DATA.OBJECT$De`** This object is a
[data.frame](https://rdrr.io/r/base/data.frame.html) with the following
columns

|             |                                                    |                                                                                                                                                                                                          |
|-------------|----------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `De`        | [numeric](https://rdrr.io/r/base/numeric.html)     | equivalent dose                                                                                                                                                                                          |
| `De.Error`  | [numeric](https://rdrr.io/r/base/numeric.html)     | standard error the equivalent dose                                                                                                                                                                       |
| `D01`       | [numeric](https://rdrr.io/r/base/numeric.html)     | D-nought value, curvature parameter of the exponential                                                                                                                                                   |
| `D01.ERROR` | [numeric](https://rdrr.io/r/base/numeric.html)     | standard error of the D-nought value                                                                                                                                                                     |
| `D02`       | [numeric](https://rdrr.io/r/base/numeric.html)     | 2nd D-nought value, only for `EXP+EXP`                                                                                                                                                                   |
| `D02.ERROR` | [numeric](https://rdrr.io/r/base/numeric.html)     | standard error for 2nd D-nought; only for `EXP+EXP`                                                                                                                                                      |
| `Dc`        | [numeric](https://rdrr.io/r/base/numeric.html)     | value indicating saturation level; only for `OTOR`                                                                                                                                                       |
| `D63`       | [numeric](https://rdrr.io/r/base/numeric.html)     | the specific saturation level; only for `OTORX`                                                                                                                                                          |
| `n_N`       | [numeric](https://rdrr.io/r/base/numeric.html)     | saturation level of dose-response curve derived via integration from the used function; it compares the full integral of the curves (`N`) to the integral until `De` (`n`) (e.g., Guralnik et al., 2015) |
| `De.MC`     | [numeric](https://rdrr.io/r/base/numeric.html)     | equivalent dose derived by Monte-Carlo simulation; ideally identical to `De`                                                                                                                             |
| `Fit`       | [character](https://rdrr.io/r/base/character.html) | applied fit function                                                                                                                                                                                     |
| `Mode`      | [character](https://rdrr.io/r/base/character.html) | mode used in fitting                                                                                                                                                                                     |
| `HPDI68_L`  | [numeric](https://rdrr.io/r/base/numeric.html)     | highest probability density of approximated equivalent dose probability curve representing the lower boundary of 68% probability                                                                         |
| `HPDI68_U`  | [numeric](https://rdrr.io/r/base/numeric.html)     | same as `HPDI68_L` for the upper bound                                                                                                                                                                   |
| `HPDI95_L`  | [numeric](https://rdrr.io/r/base/numeric.html)     | same as `HPDI68_L` but for 95% probability                                                                                                                                                               |
| `HPDI95_U`  | [numeric](https://rdrr.io/r/base/numeric.html)     | same as `HPDI95_L` but for the upper bound                                                                                                                                                               |
| `.De.plot`  | [numeric](https://rdrr.io/r/base/numeric.html)     | equivalent dose used internally for plotting                                                                                                                                                             |
| `.De.raw`   | [numeric](https://rdrr.io/r/base/numeric.html)     | equivalent dose reported 'as is', that is containing infinities and negative values if they could be calculated. Bear in mind that negative values are meaningless and may be arbitrary.                 |

## Details

### Implemented fitting methods

For all options (except for the `LIN`, `QDR` and the `EXP OR LIN`), the
[minpack.lm::nlsLM](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)
function with the `LM` (Levenberg-Marquardt algorithm) algorithm is
used. Note: For historical reasons for the Monte Carlo simulations
partly the function [nls](https://rdrr.io/r/stats/nls.html) using the
`port` algorithm.

The solution is found by transforming the function or using
[stats::uniroot](https://rdrr.io/r/stats/uniroot.html).

**Keyword: `LIN`**

Fits a linear function to the data using
[lm](https://rdrr.io/r/stats/lm.html): \$\$y = mx + n\$\$

**Keyword: `QDR`**

Fits a linear function with a quadratic term to the data using
[lm](https://rdrr.io/r/stats/lm.html): \$\$y = a + bx + cx^2\$\$

**Keyword: `EXP`**

Adapts a function of the form \$\$y = a(1 - exp(-\frac{(x+c)}{b}))\$\$

Parameters b and c are approximated by a linear fit using
[lm](https://rdrr.io/r/stats/lm.html). Note: \\b = D0\\

**Keyword: `EXP OR LIN`**

Works for some cases where an `EXP` fit fails. If the `EXP` fit fails, a
`LIN` fit is done instead, which always works.

**Keyword: `EXP+LIN`**

Tries to fit an exponential plus linear function of the form:

\$\$y = a(1-exp(-\frac{x+c}{b}) + (gx))\$\$ The \\D_e\\ is calculated by
iteration.

**Note:** In the context of luminescence dating, this function has no
physical meaning. Therefore, no \\D_0\\ value is returned.

**Keyword: `EXP+EXP`**

Tries to fit a double exponential function of the form

\$\$y = (a_1 (1-exp(-\frac{x}{b_1}))) + (a_2 (1 -
exp(-\frac{x}{b_2})))\$\$

*This fitting procedure is not really robust against wrong start
parameters.*

**Keyword: `GOK`**

Tries to fit the general-order kinetics function following Guralnik et
al. (2015) of the form

\$\$y = a (d - (1 + (\frac{1}{b}) x c)^{(-1/c)})\$\$

where \\c \> 0\\ is a kinetic order modifier (not to be confused with
**c** in `EXP` or `EXP+LIN`!).

**Keyword: `OTOR`** (former `LambertW`)

This tries to fit a dose-response curve based on the Lambert W function
and the one trap one recombination centre (OTOR) model according to
Pagonis et al. (2020). The function has the form

\$\$y = (1 + (\mathcal{W}((R - 1) \* exp(R - 1 - ((x + D\_{int}) /
D\_{c}))) / (1 - R))) \* N\$\$

with \\W\\ the Lambert W function, calculated using the package
[lamW::lambertW0](https://rdrr.io/pkg/lamW/man/lamW-package.html), \\R\\
the dimensionless retrapping ratio, \\N\\ the total concentration of
trappings states in cm\\^{-3}\\ and \\D\_{c} = N/R\\ a constant.
\\D\_{int}\\ is the offset on the x-axis. Please note that finding the
root in `mode = "extrapolation"` is a non-easy task due to the shape of
the function and the results might be unexpected.

**Keyword: `OTORX`**

This adapts extended OTOR (therefore: OTORX) model proposed by Lawless
and Timar-Gabor (2024) accounting for retrapping. Mathematically, the
implementation reads (the equation here as implemented, it is slightly
differently written than in the original manuscript):

\$\$F\_{OTORX} = 1 + \left\[\mathcal{W}(-Q \*
exp(-Q-(1-Q\*(1-\frac{1}{exp(1)})) \* \frac{(D + a)}{D\_{63}}))\right\]
/ Q\$\$

with

\$\$Q = \frac{A_m - A_n}{A_m}\frac{N}{N+N_D}\$\$

where \\A_m\\ and \\A_n\\ are rate constants for the recombination and
the trapping of electrons (\\N\\), respectively. \\D\_{63}\\ corresponds
to the value at which the trap occupation corresponds to the 63% of the
saturation value. \\a\\ is in an offset. If set to zero, the curve will
be forced through the origin as in the original publication.

For the implementation the calculation reads further

\$\$y = \frac{F\_{OTORX}(((D + a)/D\_{63}), Q)}{F\_{OTORX}((D\_{test} +
a)/D\_{63}, Q)}\$\$

with \\D\_{test}\\ being the test dose in the same unit (usually s or
Gy) as the regeneration dose points. This value is essential and needs
to provided along with the usual dose and \\\frac{L_x}{T_x}\\ values
(see `object` parameter input and the example section). For more details
see Lawless and Timar-Gabor (2024).

*Note: The offset adder \\a\\ is not part of the formula in Timar-Gabor
(2024) and can be set to zero with the option
`fit.force_through_origin = TRUE`*

**Fit weighting**

If the option `fit.weights = TRUE` is chosen, weights are calculated
using provided signal errors (\\\frac{L_x}{T_x}\\ error):
\$\$fit.weights = \frac{\frac{1}{error}}{\Sigma{\frac{1}{error}}}\$\$

**Error estimation using Monte Carlo simulation**

Error estimation is done using a parametric bootstrapping approach. A
set of \\\frac{L_x}{T_x}\\ values is constructed by randomly drawing
curve data sampled from normal distributions. The normal distribution is
defined by the input values (`mean = value`, `sd = value.error`). Then,
a dose-response curve fit is attempted for each dataset resulting in a
new distribution of single `De` values. The standard deviation of this
distribution becomes then the error of the `De`. With increasing
iterations, the error value becomes more stable. However, naturally the
error will not decrease with more MC runs.

Alternatively, the function returns highest probability density interval
estimates as output, users may find more useful under certain
circumstances.

**Note:** It may take some calculation time with increasing MC runs,
especially for the composed functions (`EXP+LIN` and `EXP+EXP`).  
Each error estimation is done with the function of the chosen fitting
method.

## Function version

1.4.3

## How to cite

Kreutzer, S., Dietze, M., Colombo, M., 2025. fit_DoseResponseCurve():
Fit a dose-response curve for luminescence data (Lx/Tx against dose).
Function version 1.4.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

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

[plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md),
[nls](https://rdrr.io/r/stats/nls.html),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md),
[minpack.lm::nlsLM](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html),
[lm](https://rdrr.io/r/stats/lm.html),
[uniroot](https://rdrr.io/r/stats/uniroot.html),
[lamW::lambertW0](https://rdrr.io/pkg/lamW/man/lamW-package.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Michael Dietze, GFZ Potsdam (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## Examples

``` r
##(1) fit growth curve for a dummy data.set and show De value
data(ExampleData.LxTxData, envir = environment())
temp <- fit_DoseResponseCurve(LxTxData)
#> [fit_DoseResponseCurve()] Fit: EXP (interpolation) | De = 1737.88 | D01 = 1766.07
get_RLum(temp)
#>         De De.Error      D01 D01.ERROR D02 D02.ERROR Dc D63       n_N    De.MC
#> 1 1737.881 62.66268 1766.074  92.70931  NA        NA NA  NA 0.5271352 1738.381
#>   Fit          Mode HPDI68_L HPDI68_U HPDI95_L HPDI95_U .De.plot  .De.raw
#> 1 EXP interpolation 1670.985 1792.255 1604.373 1858.867 1737.881 1737.881

##(1b) to access the fitting value try
get_RLum(temp, data.object = "Fit")
#> Nonlinear regression model
#>   model: y ~ a * (1 - exp(-(x + c)/b))
#>    data: data
#>        a        b        c 
#>    6.806 1766.074    5.051 
#>  weighted residual sum-of-squares: 0.0004268
#> 
#> Number of iterations to convergence: 4 
#> Achieved convergence tolerance: 1.49e-08

##(2) fit using the 'extrapolation' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(fit_DoseResponseCurve(LxTxData, mode = "extrapolation"))
#> [fit_DoseResponseCurve()] Fit: EXP (extrapolation) | De = 109.74 | D01 = 2624.06
#> 
#>  [RLum.Results-class]
#>   originator: fit_DoseResponseCurve()
#>   data: 5
#>       .. $De : data.frame
#>   .. $De.MC : numeric
#>   .. $Fit : nls
#>   .. $Fit.Args : list
#>   .. $Formula : expression
#>   additional info elements:  1 

##(3) fit using the 'alternate' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(fit_DoseResponseCurve(LxTxData, mode = "alternate"))
#> 
#>  [RLum.Results-class]
#>   originator: fit_DoseResponseCurve()
#>   data: 5
#>       .. $De : data.frame
#>   .. $De.MC : numeric
#>   .. $Fit : nls
#>   .. $Fit.Args : list
#>   .. $Formula : expression
#>   additional info elements:  1 

##(4) import and fit test data set by Berger & Huntley 1989
QNL84_2_unbleached <-
read.table(system.file("extdata/QNL84_2_unbleached.txt", package = "Luminescence"))

results <- fit_DoseResponseCurve(
 QNL84_2_unbleached,
 mode = "extrapolation",
 verbose = FALSE)
#> Warning: [fit_DoseResponseCurve()] Error column invalid or 0, 'fit.weights' ignored

#calculate confidence interval for the parameters
#as alternative error estimation
confint(results$Fit, level = 0.68)
#> Waiting for profiling to be done...
#>           16%         84%
#> a 140543.3024 146731.8471
#> b    374.0861    425.5679
#> c    116.3499    133.3474

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
