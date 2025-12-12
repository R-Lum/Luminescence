# Fading Correction after Huntley & Lamothe (2001)

Apply a fading correction according to Huntley & Lamothe (2001) for a
given \\g\\-value and a given \\t\_{c}\\

## Usage

``` r
calc_FadingCorr(
  age.faded,
  g_value,
  tc,
  tc.g_value = tc,
  n.MC = 10000,
  seed = NULL,
  interval = c(0.01, 500),
  txtProgressBar = TRUE,
  verbose = TRUE
)
```

## Arguments

- age.faded:

  [numeric](https://rdrr.io/r/base/numeric.html)
  [vector](https://rdrr.io/r/base/vector.html) (**required**): vector of
  length 2 containing the uncorrected age and the error in ka (see
  example).

- g_value:

  [vector](https://rdrr.io/r/base/vector.html) or
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  (**required**): either a vector of length 2 containing the g-value and
  error obtained from separate fading measurements (see example), or an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object produced by
  [analyse_FadingMeasurement](https://r-lum.github.io/Luminescence/reference/analyse_FadingMeasurement.md).
  If the latter, the `tc` argument is set automatically.

- tc:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): time in
  seconds between irradiation and the prompt measurement (cf. Huntley &
  Lamothe 2001). The argument is ignored when `g_value` is an
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object.

- tc.g_value:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): time
  in seconds between irradiation and the prompt measurement used in the
  estimation of the g-value. If the g-value was normalised, the
  normalisation time (in seconds) should be given, e.g., for a g-value
  normalised to 2 days, the value 172800 should be used. If nothing is
  provided the time is set to `tc`, which is usual case for g-values
  obtained using the SAR method and \\g\\-values that have been not
  normalised to 2 days.

- n.MC:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of Monte Carlo simulation runs for error estimation. If
  `n.MC = 'auto'` is used the function tries to find a 'stable' error
  for the age. See details for further information. **Note:** This may
  take a while!

- seed:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): sets the
  seed for the random number generator in R using
  [set.seed](https://rdrr.io/r/base/Random.html)

- interval:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): a
  vector containing the end-points (age interval) of the interval to be
  searched for the root in 'ka'. This argument is passed to the function
  [stats::uniroot](https://rdrr.io/r/stats/uniroot.html) used for
  solving the equation.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

## Value

Returns an S4 object of type
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md).  

Slot: **`@data`**  

|               |                                                      |                                                                   |
|---------------|------------------------------------------------------|-------------------------------------------------------------------|
| **Object**    | **Type**                                             | **Comment**                                                       |
| `age.corr`    | [data.frame](https://rdrr.io/r/base/data.frame.html) | Corrected age                                                     |
| `age.corr.MC` | [numeric](https://rdrr.io/r/base/numeric.html)       | MC simulation results with all possible ages from that simulation |

Slot: **`@info`**  

|            |                                                    |                            |
|------------|----------------------------------------------------|----------------------------|
| **Object** | **Type**                                           | **Comment**                |
| `info`     | [character](https://rdrr.io/r/base/character.html) | the original function call |

## Details

This function solves the equation used for correcting the fading
affected age including the error for a given \\g\\-value according to
Huntley & Lamothe (2001):

\$\$ \frac{A\_{f}}{A} = 1 - \kappa \* \Big\[ln(\frac{A}{t_c}) - 1\Big\]
\$\$

with \\\kappa\\ defined as

\$\$ \kappa = \frac{\frac{\mathrm{g\\value}}{ln(10)}}{100} \$\$

\\A\\ and \\A\_{f}\\ are given in ka. \\t_c\\ is given in s, however, it
is internally recalculated to ka.

As the \\g\\-value slightly depends on the time \\t\_{c}\\ between
irradiation and the prompt measurement, a value for `tc` must always be
provided. If the \\g\\-value was normalised to a distinct time or
evaluated with a different tc value (e.g., external irradiation), also
the \\t\_{c}\\ value for the \\g\\-value needs to be provided (argument
`tc.g_value` and then the \\g\\-value is recalculated to \\t\_{c}\\ of
the measurement used for estimating the age applying the following
equation:

\$\$\kappa\_{tc} = \kappa\_{tc.g} / (1 - \kappa\_{tc.g} \*
ln(tc/tc.g))\$\$

where

\$\$\kappa\_{tc.g} = g / 100 / ln(10)\$\$

The error of the fading-corrected age is determined using a Monte Carlo
simulation approach. Solving of the equation is performed using
[uniroot](https://rdrr.io/r/stats/uniroot.html). Large values for `n.MC`
will significantly increase the computation time.  

**`n.MC = 'auto'`**

The error estimation based on a stochastic process, i.e. for a small
number of MC runs the calculated error varies considerably every time
the function is called, even with the same input values. The argument
option `n.MC = 'auto'` tries to find a stable value for the standard
error, i.e. the standard deviation of values calculated during the MC
runs (`age.corr.MC`), within a given precision (2 digits) by increasing
the number of MC runs stepwise and calculating the corresponding error.

If the determined error does not differ from the 9 values calculated
previously within a precision of (here) 3 digits the calculation is
stopped as it is assumed that the error is stable. Please note that (a)
the duration depends on the input values as well as on the provided
computation resources and it may take a while, (b) the length (size) of
the output vector `age.corr.MC`, where all the single values produced
during the MC runs are stored, equals the number of MC runs (here termed
observations).

To avoid an endless loop the calculation is stopped if the number of
observations exceeds 10^7. This limitation can be overwritten by setting
the number of MC runs manually, e.g. `n.MC = 10000001`. Note: For this
case the function is not checking whether the calculated error is
stable.  

**`seed`**

This option allows to recreate previously calculated results by setting
the seed for the R random number generator (see
[set.seed](https://rdrr.io/r/base/Random.html) for details). This option
should not be mixed up with the option **`n.MC = 'auto'`**. The results
may appear similar, but they are not comparable!  

**FAQ**  

**Q**: Which \\t\_{c}\\ value is expected?  

**A**: \\t\_{c}\\ is the time in seconds between irradiation and the
prompt measurement applied during your \\D\_{e}\\ measurement. However,
this \\t\_{c}\\ might differ from the \\t\_{c}\\ used for estimating the
\\g\\-value. In the case of an SAR measurement \\t\_{c}\\ should be
similar, however, if it differs, you have to provide this \\t\_{c}\\
value (the one used for estimating the \\g\\-value) using the argument
`tc.g_value`.  

**Q**: The function could not find a solution, what should I do?  

**A**: This usually happens for model parameters exceeding the
boundaries of the fading correction model (e.g., very high \\g\\-value).
Please check whether another fading correction model might be more
appropriate.

## Note

Special thanks to Sébastien Huot for his support and clarification via
e-mail.

## Function version

0.4.4

## How to cite

Kreutzer, S., 2025. calc_FadingCorr(): Fading Correction after Huntley &
Lamothe (2001). Function version 0.4.4. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading in
K-feldspars and the measurement and correction for it in optical dating.
Canadian Journal of Earth Sciences, 38, 1093-1106.

## See also

[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[analyse_FadingMeasurement](https://r-lum.github.io/Luminescence/reference/analyse_FadingMeasurement.md),
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md),
[uniroot](https://rdrr.io/r/stats/uniroot.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##run the examples given in the appendix of Huntley and Lamothe, 2001

##(1) faded age: 100 a
results <- calc_FadingCorr(
   age.faded = c(0.1,0),
   g_value = c(5.0, 1.0),
   tc = 2592000,
   tc.g_value = 172800,
   n.MC = 100)
#> 
#> 
#> [calc_FadingCorr()]
#> 
#>  >> Fading correction according to Huntley & Lamothe (2001)
#>  >> g-value re-calculated for the given tc
#> 
#>  .. used g-value:    5.312 ± 1.012 %/decade
#>  .. used tc:     8.214e-05 ka
#>  .. used kappa:      0.0231 ± 0.0044
#>  ----------------------------------------------
#>  seed:            NA
#>  n.MC:           100
#>  observations:       100
#>  ----------------------------------------------
#>  Age (faded):        0.1 ka ± 0 ka
#>  Age (corr.):        0.1169 ka ± 0.0041 ka
#>  ---------------------------------------------- 

##(2) faded age: 1 ka
results <- calc_FadingCorr(
   age.faded = c(1,0),
   g_value = c(5.0, 1.0),
   tc = 2592000,
   tc.g_value = 172800,
   n.MC = 100)
#> 
#> 
#> [calc_FadingCorr()]
#> 
#>  >> Fading correction according to Huntley & Lamothe (2001)
#>  >> g-value re-calculated for the given tc
#> 
#>  .. used g-value:    5.312 ± 1.012 %/decade
#>  .. used tc:     8.214e-05 ka
#>  .. used kappa:      0.0231 ± 0.0044
#>  ----------------------------------------------
#>  seed:            NA
#>  n.MC:           100
#>  observations:       100
#>  ----------------------------------------------
#>  Age (faded):        1 ka ± 0 ka
#>  Age (corr.):        1.2486 ka ± 0.0682 ka
#>  ---------------------------------------------- 

##(3) faded age: 10.0 ka
results <- calc_FadingCorr(
   age.faded = c(10,0),
   g_value = c(5.0, 1.0),
   tc = 2592000,
   tc.g_value = 172800,
   n.MC = 100)
#> 
#> 
#> [calc_FadingCorr()]
#> 
#>  >> Fading correction according to Huntley & Lamothe (2001)
#>  >> g-value re-calculated for the given tc
#> 
#>  .. used g-value:    5.312 ± 1.012 %/decade
#>  .. used tc:     8.214e-05 ka
#>  .. used kappa:      0.0231 ± 0.0044
#>  ----------------------------------------------
#>  seed:            NA
#>  n.MC:           100
#>  observations:       100
#>  ----------------------------------------------
#>  Age (faded):        10 ka ± 0 ka
#>  Age (corr.):        13.402 ka ± 0.9297 ka
#>  ---------------------------------------------- 

##access the last output
get_RLum(results)
#>      AGE AGE.ERROR AGE_FADED AGE_FADED.ERROR  G_VALUE G_VALUE.ERROR      KAPPA
#> 1 13.402    0.9297        10               0 5.312393      1.011901 0.02307143
#>   KAPPA.ERROR           TC   TC.G_VALUE n.MC OBSERVATIONS SEED
#> 1  0.00439463 8.213721e-05 5.475814e-06  100          100   NA
```
