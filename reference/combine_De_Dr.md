# Combine Dose Rate and Equivalent Dose Distribution

A Bayesian statistical analysis of OSL age requiring dose rate sample.
Estimation contains a preliminary step for detecting outliers in the
equivalent dose sample.

## Usage

``` r
combine_De_Dr(
  De,
  s,
  Dr,
  int_OD,
  Age_range = c(1, 300),
  outlier_threshold = 0.05,
  outlier_method = "default",
  outlier_analysis_plot = FALSE,
  method_control = list(),
  par_local = TRUE,
  verbose = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- De:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): a
  equivalent dose sample

- s:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): a
  vector of measurement errors on the equivalent dose

- Dr:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): a dose
  rate sample

- int_OD:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): the
  intrinsic overdispersion, typically the standard deviation
  characterizing a dose-recovery test distribution

- Age_range:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): the
  age range to be investigated by the algorithm, the larger the value
  the more iterations are needed and the longer it takes. Should not be
  set too narrow, cut the algorithm some slack.

- outlier_threshold:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): the
  required significance level used for the outlier detection. If set to
  `1`, no outliers are removed. If
  `outlier_method = "RousseeuwCroux1993"`, the median distance is used
  as outlier threshold. Please see details for further information.

- outlier_method:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  select the outlier detection method, either `"default"` or
  `"RousseeuwCroux1993"`. See details for further information.

- outlier_analysis_plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the outlier analysis plot. Note: the outlier analysis
  will happen independently of the plot output.

- method_control:

  [list](https://rdrr.io/r/base/list.html) (*with default*): named
  [list](https://rdrr.io/r/base/list.html) of parameters to control
  [rjags::jags.model](https://rdrr.io/pkg/rjags/man/jags.model.html).
  This can be used to set the random seed for the MCMC chains (four by
  default):

      method_control = list(inits = list(
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 3),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 4))
      )

- par_local:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  set to `TRUE` the function uses its own
  [graphics::par](https://rdrr.io/r/graphics/par.html) settings (which
  will end in two plots next to each other)

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  a few further arguments to fine-tune the plot output such as
  `cdf_ADr_quantiles` (`TRUE`/`FALSE`), `legend.pos`, `legend`
  (`TRUE`/`FALSE`)

## Value

The function returns a plot if `plot = TRUE` and an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object with the following slots:

`@data`  
`.. $Ages`: a [numeric](https://rdrr.io/r/base/numeric.html) vector with
the modelled ages to be further analysed or visualised  
`.. $Ages_stats`: a [data.frame](https://rdrr.io/r/base/data.frame.html)
with sum HPD, CI 68% and CI 95% for the ages  
`.. $outliers_index`: the index with the detected outliers  
`.. $cdf_ADr_mean` : empirical cumulative density distribution A \* Dr
(mean)  
`.. $cdf_ADr_quantiles` : empirical cumulative density distribution A \*
Dr (quantiles .025,.975)  
`.. $cdf_De_no_outlier` : empirical cumulative density distribution of
the De with no outliers  
`.. $cdf_De_initial` : empirical cumulative density distribution of the
initial De  
`.. $mcmc_IAM` : the MCMC list of the Individual Age Model, only of
`method_control = list(return_mcmc = TRUE)` otherwise `NULL`  
`.. $mcmc_BCAM` : the MCMC list of the Bayesian Central Age Model, only
of `method_control = list(return_mcmc = TRUE)` otherwise `NULL`  

`@info`  
`.. $call`: the original function call  
`.. $model_IAM`: the BUGS model used to derive the individual age  
`.. $model_BCAM`: the BUGS model used to calculate the Bayesian Central
Age  

## Details

**Outlier detection**

Two different outlier detection methods are implemented (full details
are given in the cited literature).

1.  The *default* and recommend method, uses quantiles to compare prior
    and posterior distributions of the individual variances of the
    equivalent doses. If the corresponding quantile in the corresponding
    posterior distribution is larger than the quantile in the prior
    distribution, the value is marked as outlier (cf. Galharret et al.,
    preprint)

2.  The alternative method employs the method suggested by Rousseeuw and
    Croux (1993) using the absolute median distance.

**Parameters available for `method_control`**

The parameters listed below are used to granular control Bayesian
modelling using
[rjags::rjags](https://rdrr.io/pkg/rjags/man/rjags-package.html).
Internally the functions `.calc_IndividualAgeModel()` and
`.calc_BayesianCentraAgelModel()`. The parameter settings affect both
models. Note: `method_control` expects a **named** list of parameters

|                       |                                                    |                        |                                                                                                                        |
|-----------------------|----------------------------------------------------|------------------------|------------------------------------------------------------------------------------------------------------------------|
| **PARAMETER**         | **TYPE**                                           | **DEFAULT**            | **REMARKS**                                                                                                            |
| `variable.names_IAM`  | [character](https://rdrr.io/r/base/character.html) | `c('A', 'a', 'sig_a')` | variables names to be monitored in the modelling process using the internal function `.calc_IndividualAgeModel()`      |
| `variable.names_BCAM` | [character](https://rdrr.io/r/base/character.html) | `c('A', 'D_e')`        | variables names to be monitored in the modelling process using the internal function `.calc_BayesianCentraAgelModel()` |
| `n.chains`            | [integer](https://rdrr.io/r/base/integer.html)     | `4`                    | number of MCMC chains                                                                                                  |
| `n.adapt`             | [integer](https://rdrr.io/r/base/integer.html)     | `1000`                 | number of iterations for the adaptation                                                                                |
| `n.iter`              | [integer](https://rdrr.io/r/base/integer.html)     | `5000`                 | number of iterations to monitor cf. [rjags::coda.samples](https://rdrr.io/pkg/rjags/man/coda.samples.html)             |
| `thin`                | [numeric](https://rdrr.io/r/base/numeric.html)     | `1`                    | thinning interval for the monitoring cf. [rjags::coda.samples](https://rdrr.io/pkg/rjags/man/coda.samples.html)        |
| `diag`                | [logical](https://rdrr.io/r/base/logical.html)     | `FALSE`                | additional terminal convergence diagnostic. `FALSE` if `verbose = FALSE`                                               |
| `progress.bar`        | [logical](https://rdrr.io/r/base/logical.html)     | `FALSE`                | enable/disable progress bar. `FALSE` if `verbose = FALSE`                                                              |
| `quiet`               | [logical](https://rdrr.io/r/base/logical.html)     | `TRUE`                 | silence terminal output. Set to `TRUE` if `verbose = FALSE`                                                            |
| `return_mcmc`         | [logical](https://rdrr.io/r/base/logical.html)     | `FALSE`                | return additional MCMC diagnostic information                                                                          |

## Function version

0.1.0

## How to cite

Philippe, A., Galharret, J., Mercier, N., Kreutzer, S., 2025.
combine_De_Dr(): Combine Dose Rate and Equivalent Dose Distribution.
Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Mercier, N., Galharret, J.-M., Tribolo, C., Kreutzer, S., Philippe, A.,
preprint. Luminescence age calculation through Bayesian convolution of
equivalent dose and dose-rate distributions: the De_Dr model.
Geochronology, 1-22.

Galharret, J-M., Philippe, A., Mercier, N., preprint. Detection of
outliers with a Bayesian hierarchical model: application to the
single-grain luminescence dating method. Electronic Journal of Applied
Statistics

**Further reading**

Rousseeuw, P.J., Croux, C., 1993. Alternatives to the median absolute
deviation. Journal of the American Statistical Association 88,
1273–1283. [doi:10.2307/2291267](https://doi.org/10.2307/2291267)

Rousseeuw, P.J., Debruyne, M., Engelen, S., Hubert, M., 2006. Robustness
and outlier detection in chemometrics. Critical Reviews in Analytical
Chemistry 36, 221–242.
[doi:10.1080/10408340600969403](https://doi.org/10.1080/10408340600969403)

## See also

[plot_OSLAgeSummary](https://r-lum.github.io/Luminescence/reference/plot_OSLAgeSummary.md),
[rjags::rjags](https://rdrr.io/pkg/rjags/man/rjags-package.html),
[mclust::mclust-package](https://mclust-org.github.io/mclust/reference/mclust-package.html)

## Author

Anne Philippe, Université de Nantes (France), Jean-Michel Galharret,
Université de Nantes (France), Norbert Mercier, IRAMAT-CRP2A, Université
Bordeaux Montaigne (France), Sebastian Kreutzer, Institute of Geography,
Heidelberg University (Germany) , RLum Developer Team

## Examples

``` r
## set parameters
Dr <- stats::rlnorm (1000, 0, 0.3)
De <- 50*sample(Dr, 50, replace = TRUE)
s <- stats::rnorm(50, 10, 2)

## run modelling
## note: modify parameters for more realistic results
if (FALSE) { # \dontrun{
results <- combine_De_Dr(
 Dr = Dr,
 int_OD = 0.1,
 De,
 s,
 Age_range = c(0,100),
 method_control = list(
  n.iter = 100,
  n.chains = 1))

## show models used
writeLines(results@info$model_IAM)
writeLines(results@info$model_BCAM)
} # }
```
