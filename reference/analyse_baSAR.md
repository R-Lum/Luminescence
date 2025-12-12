# Bayesian models (baSAR) applied on luminescence data

This function allows the application of Bayesian models on luminescence
data measured with the single-aliquot regenerative-dose (SAR, Murray and
Wintle, 2000) protocol. In particular, it follows the idea proposed by
Combès et al., 2015 of using an hierarchical model for estimating a
central equivalent dose from a set of luminescence measurements. This
function (I) implements this approach for the R environment and (II)
provides an extension and a technical refinement of the published code.

## Usage

``` r
analyse_baSAR(
  object,
  CSV_file = NULL,
  aliquot_range = NULL,
  source_doserate = NULL,
  signal.integral,
  signal.integral.Tx = NULL,
  background.integral,
  background.integral.Tx = NULL,
  irradiation_times = NULL,
  sigmab = 0,
  sig0 = 0.025,
  distribution = "cauchy",
  baSAR_model = NULL,
  n.MCMC = 1e+05,
  fit.method = "EXP",
  fit.force_through_origin = TRUE,
  fit.includingRepeatedRegPoints = TRUE,
  method_control = list(),
  digits = 3L,
  distribution_plot = "kde",
  plot = TRUE,
  plot_reduced = TRUE,
  plot_singlePanels = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
  [character](https://rdrr.io/r/base/character.html) or
  [list](https://rdrr.io/r/base/list.html) (**required**): input object
  used for the Bayesian analysis. If a `character` is provided the
  function assumes a file connection and tries to import a BIN/BINX-file
  using the provided path. If a `list` is provided the list can only
  contain either `Risoe.BINfileData` objects or `character`s providing a
  file connection. Mixing of both types is not allowed. If an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  is provided the function directly starts with the Bayesian Analysis
  (see details)

- CSV_file:

  [character](https://rdrr.io/r/base/character.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*): if
  a `character`, it must be the path to a CSV file with data for the
  analysis. Either way, data should contain 3 columns: the name of the
  file, the disc position and the grain position (the last being 0 for
  multi-grain measurements).

- aliquot_range:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): allows to
  limit the range of the aliquots used for the analysis. This argument
  has only an effect if the argument `CSV_file` is used or `object` is a
  previous output (i.e. is
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)).
  In this case, the new selection will add the aliquots to the removed
  aliquots table.

- source_doserate:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): source
  dose rate of beta-source used for the measurement and its uncertainty
  in Gy/s, e.g., `source_doserate = c(0.12, 0.04)`. Parameter can be
  provided as `list`, for the case that more than one BIN-file is
  provided, e.g.,
  `source_doserate = list(c(0.04, 0.004), c(0.05, 0.004))`.

- signal.integral:

  [vector](https://rdrr.io/r/base/vector.html) (**required**): vector
  with the limits for the signal integral used for the calculation,
  e.g., `signal.integral = c(1:5)`. Ignored if `object` is an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object. The parameter can be provided as `list`, see
  `source_doserate`.

- signal.integral.Tx:

  [vector](https://rdrr.io/r/base/vector.html) (*optional*): vector with
  the limits for the signal integral for the Tx curve. I f nothing is
  provided the value from `signal.integral` is used and it is ignored if
  `object` is an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object. The parameter can be provided as `list`, see
  `source_doserate`.

- background.integral:

  [vector](https://rdrr.io/r/base/vector.html) (**required**): vector
  with the bounds for the background integral. Ignored if `object` is an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object. The parameter can be provided as `list`, see
  `source_doserate`.

- background.integral.Tx:

  [vector](https://rdrr.io/r/base/vector.html) (*optional*): vector with
  the limits for the background integral for the Tx curve. If nothing is
  provided the value from `background.integral` is used. Ignored if
  `object` is an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object. The parameter can be provided as `list`, see
  `source_doserate`.

- irradiation_times:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): if set
  this vector replaces all irradiation times for one aliquot and one
  cycle (Lx and Tx curves) and recycles it for all others cycles and
  aliquots. Please note that if this argument is used, for every(!)
  single curve in the dataset an irradiation time needs to be set.

- sigmab:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  option to set a manual value for the overdispersion (for `LnTx` and
  `TnTx`), used for the `Lx`/`Tx` error calculation. The value should be
  provided as absolute squared count values, cf.
  [calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md).
  The parameter can be provided as `list`, see `source_doserate`.

- sig0:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): allow
  adding an extra component of error to the final Lx/Tx error value
  (e.g., instrumental error, see details is
  [calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md)).
  The parameter can be provided as `list`, see `source_doserate`.

- distribution:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  type of distribution that is used during Bayesian calculations for
  determining the Central dose and overdispersion values. Allowed inputs
  are `"cauchy"`, `"normal"` and `"log_normal"`.

- baSAR_model:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  option to provide an own modified or new model for the Bayesian
  calculation (see details). If an own model is provided the argument
  `distribution` is ignored and set to `'user_defined'`

- n.MCMC:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of iterations for the Markov chain Monte Carlo (MCMC)
  simulations

- fit.method:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  equation used for the fitting of the dose-response curve using the
  function
  [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)
  and then for the Bayesian modelling. Here supported methods: `EXP`,
  `EXP+LIN` and `LIN`

- fit.force_through_origin:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): force
  fitting through origin

- fit.includingRepeatedRegPoints:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  includes the recycling point (assumed to be measured during the last
  cycle)

- method_control:

  [list](https://rdrr.io/r/base/list.html) (*optional*): named list of
  control parameters that can be directly passed to the Bayesian
  analysis, e.g., `method_control = list(n.chains = 4)`. See details for
  further information

- digits:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*): round
  output to the number of given digits

- distribution_plot:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  sets the final distribution plot that shows equivalent doses obtained
  using the frequentist approach and sets in the central dose as
  comparison obtained using baSAR. Allowed input is `'abanico'` or
  `'kde'`. If set to `NULL` nothing is plotted.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- plot_reduced:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the advanced plot output.

- plot_singlePanels:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable single plots or plots arranged by `analyse_baSAR`.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  parameters that can be passed to the function
  [calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md)
  (almost full support),
  [data.table::fread](https://rdatatable.gitlab.io/data.table/reference/fread.html)
  (`skip`),
  [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)
  (`n.records`, `position`, `duplicated.rm`), see details.

## Value

Function returns results numerically and graphically:

———————————–  
`[ NUMERICAL OUTPUT ]`  
———————————–  

**`RLum.Results`**-object

**slot:** **`@data`**

|                     |              |                                                                                                                 |
|---------------------|--------------|-----------------------------------------------------------------------------------------------------------------|
| **Element**         | **Type**     | **Description**                                                                                                 |
| `$summary`          | `data.frame` | statistical summary, including the central dose                                                                 |
| `$mcmc`             | `mcmc`       | [coda::mcmc.list](https://rdrr.io/pkg/coda/man/mcmc.list.html) object including raw output                      |
| `$models`           | `character`  | implemented models used in the baSAR-model core                                                                 |
| `$input_object`     | `data.frame` | summarising table (same format as the XLS-file) including, e.g., Lx/Tx values                                   |
| `$removed_aliquots` | `data.frame` | table with removed aliquots (e.g., `NaN`, or `Inf` `Lx`/`Tx` values). If nothing was removed `NULL` is returned |

**slot:** **`@info`**

The original function call

————————  
`[ PLOT OUTPUT ]`  
————————  

- \(A\) Ln/Tn curves with set integration limits,

- \(B\) trace plots are returned by the baSAR-model, showing the
  convergence of the parameters (trace) and the resulting kernel density
  plots. If `plot_reduced = FALSE` for every(!) dose a trace and a
  density plot is returned (this may take a long time),

- \(C\) dose plots showing the dose for every aliquot as boxplots and
  the marked HPD in within. If boxes are coloured 'orange' or 'red' the
  aliquot itself should be checked,

- \(D\) the dose response curve resulting from the monitoring of the
  Bayesian modelling are provided along with the Lx/Tx values and the
  HPD. Note: The amount for curves displayed is limited to 1000 (random
  choice) for performance reasons,

- \(E\) the final plot is the De distribution as calculated using the
  conventional (frequentist) approach and the central dose with the HPDs
  marked within. This figure is only provided for a comparison, no
  further statistical conclusion should be drawn from it.

**Please note: If distribution was set to `log_normal` the central dose
is given as geometric mean!**

## Details

Internally the function consists of two parts: (I) a Bayesian core that
performs the hierarchical modelling and calculations, and (II) a data
pre-processing stage. The Bayesian core can be run independently if
sufficient input data are provided (see below). The data pre-processing
stage exists to simplify the user workflow by preparing all required
inputs; in principle it is enough to provide a BIN/BINX-file with the
SAR measurement data. For the Bayesian analysis for each aliquot the
following information are needed from the SAR analysis: `LxTx`, the
`LxTx` error and the dose values for all regeneration points.

**How is the systematic error contribution calculated?**

Standard errors (so far) provided with the source dose rate are
considered as systematic uncertainties and added to final central dose
by:

\$\$systematic.error = 1/n \sum SE(source.doserate)\$\$

\$\$SE(central.dose.final) = \sqrt{SE(central.dose)^2 +
systematic.error^2}\$\$

Please note that this approach is rather rough and can only be valid if
the source dose rate errors, in case different readers had been used,
are similar. In cases where more than one source dose rate is provided a
warning is given.

**Input / output scenarios**

Various inputs are allowed for this function. Unfortunately this makes
the function handling rather complex, but at the same time very
powerful. Available scenarios:

**(1) - `object` is BIN-file or link to a BIN-file**

It does not matter how the information of the BIN/BINX file are
provided. The function supports **(a)** either a path to a file or
directory or a `list` of file names or paths or **(b)** a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object or a list of these objects. The latter one can be produced by
using function
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
but this function is called automatically if only a file name and/or a
path is provided. In both cases it will become the data that can be used
for the analysis.

`[CSV_file = NULL]`

If no CSV file (or data frame with the same format) is provided, the
function runs an automatic process that consists of the following steps:

1.  Select all valid aliquots using the function
    [verify_SingleGrainData](https://r-lum.github.io/Luminescence/reference/verify_SingleGrainData.md)

2.  Calculate `Lx/Tx` values using the function
    [calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md)

3.  Calculate De values using the function
    [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)

These proceeded data are subsequently used in for the Bayesian analysis

`[CSV_file != NULL]`

If a CSV file is provided (or a `data.frame` containing similar
information) the pre-processing phase consists of the following steps:

1.  Calculate `Lx/Tx` values using the function
    [calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md)

2.  Calculate De values using the function
    [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)

The CSV file should contain the BIN-file names and the aliquots selected
for the further analysis. This allows a manual selection of input data,
as the automatic selection by
[verify_SingleGrainData](https://r-lum.github.io/Luminescence/reference/verify_SingleGrainData.md)
might not be sufficient.

**(2) - `object` `RLum.Results object`**

If an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is provided as input and(!) this object was previously created by
the function `analyse_baSAR()` itself, the pre-processing part is
skipped and the function starts directly with the Bayesian analysis.
This option is very powerful as it allows to change parameters for the
Bayesian analysis without the need to repeat the data pre-processing. If
furthermore the argument `aliquot_range` is set, aliquots can be
manually excluded based on previous runs.

**`method_control`**

These are arguments that can be passed directly to the Bayesian
calculation core, supported arguments are:

|                  |                                                    |                                                                                                                                                                                                   |
|------------------|----------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Parameter**    | **Type**                                           | **Description**                                                                                                                                                                                   |
| `lower_centralD` | [numeric](https://rdrr.io/r/base/numeric.html)     | sets the lower bound for the expected De range. Change it only if you know what you are doing!                                                                                                    |
| `upper_centralD` | [numeric](https://rdrr.io/r/base/numeric.html)     | sets the upper bound for the expected De range. Change it only if you know what you are doing!                                                                                                    |
| `n.chains`       | [integer](https://rdrr.io/r/base/integer.html)     | sets number of parallel chains for the model (default = 3) (cf. [rjags::jags.model](https://rdrr.io/pkg/rjags/man/jags.model.html))                                                               |
| `inits`          | [list](https://rdrr.io/r/base/list.html)           | option to set initialisation values (cf. [rjags::jags.model](https://rdrr.io/pkg/rjags/man/jags.model.html))                                                                                      |
| `thin`           | [numeric](https://rdrr.io/r/base/numeric.html)     | thinning interval for monitoring the Bayesian process (cf. [rjags::jags.model](https://rdrr.io/pkg/rjags/man/jags.model.html))                                                                    |
| `variable.names` | [character](https://rdrr.io/r/base/character.html) | set the variables to be monitored during the MCMC run, default: `'central_D'`, `'sigma_D'`, `'D'`, `'Q'`, `'a'`, `'b'`, `'c'`, `'g'`. Note: only variables present in the model can be monitored. |

**User defined models**  

The function provides the option to modify and to define own models that
can be used for the Bayesian calculation. In the case the user wants to
modify a model, a new model can be piped into the function via the
argument `baSAR_model` as `character`. The model has to be provided in
the JAGS dialect of the BUGS language (cf.
[rjags::jags.model](https://rdrr.io/pkg/rjags/man/jags.model.html)) and
parameter names given with the pre-defined names have to be respected,
otherwise the function will break.

**FAQ**

Q: How can I set the seed for the random number generator (RNG)?

A: Use the argument `method_control`, e.g., for three MCMC chains (as it
is the default):

    method_control = list(
    inits = list(
     list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1),
     list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2),
     list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 3)
    ))

This sets a reproducible set for every chain separately.  

Q: How can I modify the output plots?

A: You can't, but you can use the function output to create own,
modified plots.

Q: Can I change the boundaries for the central_D?

A: Yes, we made it possible, but we DO NOT recommend it, except you know
what you are doing!  
Example: `method_control = list(lower_centralD = 10))`

Q: The lines in the baSAR-model appear to be in a wrong logical order?  

A: This is correct and allowed (cf. JAGS manual)

**Additional arguments support via the `...` argument**

This list summarizes the additional arguments that can be passed to the
internally used functions.

|                                 |                                                                                                    |                                   |                                                  |
|---------------------------------|----------------------------------------------------------------------------------------------------|-----------------------------------|--------------------------------------------------|
| **Supported argument**          | **Corresponding function**                                                                         | **Default**                       | \*\*Short description \*\*                       |
| `threshold`                     | [verify_SingleGrainData](https://r-lum.github.io/Luminescence/reference/verify_SingleGrainData.md) | `30`                              | change rejection threshold for curve selection   |
| `skip`                          | [data.table::fread](https://rdatatable.gitlab.io/data.table/reference/fread.html)                  | `0`                               | number of rows to be skipped during import       |
| `n.records`                     | [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)                         | `NULL`                            | limit records during BIN-file import             |
| `duplicated.rm`                 | [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)                         | `TRUE`                            | remove duplicated records in the BIN-file        |
| `pattern`                       | [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)                         | `TRUE`                            | select BIN-file by name pattern                  |
| `position`                      | [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)                         | `NULL`                            | limit import to a specific position              |
| `background.count.distribution` | [calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md)           | `"non-poisson"`                   | set assumed count distribution                   |
| `fit.weights`                   | [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)             | `TRUE`                            | enable/disable fit weights                       |
| `fit.bounds`                    | [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)             | `TRUE`                            | enable/disable fit bounds                        |
| `n.MC`                          | [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)             | `100`                             | number of MC runs for error calculation          |
| `output.plot`                   | [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)             | `TRUE`                            | enable/disable dose response curve plot          |
| `output.plotExtended`           | [plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md)             | `TRUE`                            | enable/disable extended dose response curve plot |
| `recordType`                    | [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)                             | `c(OSL (UVVIS), irradiation (NA)` | helps for the curve selection                    |

## Note

**If you provide more than one BIN-file**, it is **strongly**
recommended to provide a `list` with the same number of elements for the
following parameters:

`source_doserate`, `signal.integral`, `signal.integral.Tx`,
`background.integral`, `background.integral.Tx`, `sigmab`, `sig0`.

Example for two BIN-files:
`source_doserate = list(c(0.04, 0.006), c(0.05, 0.006))`

**The function is currently limited to work with standard Risoe
BIN-files only!**

## Function version

0.1.37

## How to cite

Mercier, N., Kreutzer, S., 2025. analyse_baSAR(): Bayesian models
(baSAR) applied on luminescence data. Function version 0.1.37. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Combès, B., Philippe, A., Lanos, P., Mercier, N., Tribolo, C., Guérin,
G., Guibert, P., Lahaye, C., 2015. A Bayesian central equivalent dose
model for optically stimulated luminescence dating. Quaternary
Geochronology 28, 62-70. doi:10.1016/j.quageo.2015.04.001

Mercier, N., Kreutzer, S., Christophe, C., Guérin, G., Guibert, P.,
Lahaye, C., Lanos, P., Philippe, A., Tribolo, C., 2016. Bayesian
statistics in luminescence dating: The 'baSAR'-model and its
implementation in the R package 'Luminescence'. Ancient TL 34, 14-21.

**Further reading**

Gelman, A., Carlin, J.B., Stern, H.S., Dunson, D.B., Vehtari, A., Rubin,
D.B., 2013. Bayesian Data Analysis, Third Edition. CRC Press.

Murray, A.S., Wintle, A.G., 2000. Luminescence dating of quartz using an
improved single-aliquot regenerative-dose protocol. Radiation
Measurements 32, 57-73. doi:10.1016/S1350-4487(99)00253-X

Plummer, M., 2017. JAGS Version 4.3.0 user manual.
`https://sourceforge.net/projects/mcmc-jags/files/Manuals/4.x/jags_user_manual.pdf/download`

## See also

[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md),
[plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md),
[data.table::fread](https://rdatatable.gitlab.io/data.table/reference/fread.html),
[verify_SingleGrainData](https://r-lum.github.io/Luminescence/reference/verify_SingleGrainData.md),
[rjags::jags.model](https://rdrr.io/pkg/rjags/man/jags.model.html),
[rjags::coda.samples](https://rdrr.io/pkg/rjags/man/coda.samples.html),
[boxplot.default](https://rdrr.io/r/graphics/boxplot.html)

## Author

Norbert Mercier, Archéosciences Bordeaux, CNRS-Université Bordeaux
Montaigne (France)  
Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
The underlying Bayesian model based on a contribution by Combès et al.,
2015. , RLum Developer Team

## Examples

``` r
##(1) load package test data set
data(ExampleData.BINfileData, envir = environment())

##(2) selecting relevant curves, and limit dataset
CWOSL.SAR.Data <- subset(
  CWOSL.SAR.Data,
  subset = POSITION%in%c(1:3) & LTYPE == "OSL")

if (FALSE) { # \dontrun{
##(3) run analysis
##please not that the here selected parameters are
##choosen for performance, not for reliability
results <- analyse_baSAR(
  object = CWOSL.SAR.Data,
  source_doserate = c(0.04, 0.001),
  signal.integral = c(1:2),
  background.integral = c(80:100),
  fit.method = "LIN",
  plot = FALSE,
  n.MCMC = 200
)

print(results)


##CSV_file template
##copy and paste this the code below in the terminal
##you can further use the function write.csv() to export the example

CSV_file <-
structure(
list(
 BIN_FILE = NA_character_,
 DISC = NA_real_,
 GRAIN = NA_real_),
   .Names = c("BIN_FILE", "DISC", "GRAIN"),
   class = "data.frame",
   row.names = 1L
)

} # }
```
