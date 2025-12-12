# Apply the maximum age model to a given De distribution

Function to fit the maximum age model to De data. This is a wrapper
function that calls
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md)
and applies a similar approach as described in Olley et al. (2006).

## Usage

``` r
calc_MaxDose(
  data,
  sigmab,
  log = TRUE,
  par = 3,
  bootstrap = FALSE,
  init.values = NULL,
  plot = TRUE,
  ...
)
```

## Arguments

- data:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): for
  [data.frame](https://rdrr.io/r/base/data.frame.html): two columns with
  De `(data[ ,1])` and De error `(data[ ,2])`.

- sigmab:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  additional spread in De values, representing the expected
  overdispersion in the data should the sample be well-bleached
  (Cunningham & Wallinga 2012, p. 100). **NOTE**: For the logged model
  (`log = TRUE`) this value must be a fraction, e.g. 0.2 (= 20 %). If
  the un-logged model is used (`log = FALSE`), sigmab must be provided
  in the same absolute units of the De values (seconds or Gray). See
  details
  ([calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md).

- log:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): fit
  the (un-)logged three parameter minimum dose model to De data

- par:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): apply
  the 3- or 4-parameter minimum age model (`par=3` or `par=4`).

- bootstrap:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): apply
  the recycled bootstrap approach of Cunningham & Wallinga (2012).

- init.values:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  starting values for gamma, sigma, p0 and mu. Custom values need to be
  provided in a vector of length three in the form of
  `c(gamma, sigma, p0)`.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further arguments for bootstrapping (`bs.M, bs.N, bs.h, sigmab.sd`).
  See details for their usage.

## Value

Please see
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md).

## Details

**Data transformation**

To estimate the maximum dose population and its standard error, the
three parameter minimum age model of Galbraith et al. (1999) is adapted.
The measured De values are transformed as follows:

1.  convert De values to natural logs

2.  multiply the logged data to create a mirror image of the De
    distribution

3.  shift De values along x-axis by the smallest x-value found to obtain
    only positive values

4.  combine in quadrature the measurement error associated with each De
    value with a relative error specified by `sigmab`

5.  apply the MAM to these data

When all calculations are done the results are then converted as follows

1.  subtract the x-offset

2.  multiply the natural logs by -1

3.  take the exponent to obtain the maximum dose estimate in Gy

**Further documentation**

Please see
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md).

## Function version

0.3.2

## How to cite

Burow, C., 2025. calc_MaxDose(): Apply the maximum age model to a given
De distribution. Function version 0.3.2. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Arnold, L.J., Roberts, R.G., Galbraith, R.F. & DeLong, S.B., 2009. A
revised burial dose estimation procedure for optical dating of young and
modern-age sediments. Quaternary Geochronology 4, 306-325.

Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed
fission track ages. Nuclear Tracks Radiation Measurements 4, 459-470.

Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley,
J.M., 1999. Optical dating of single grains of quartz from Jinmium rock
shelter, northern Australia. Part I: experimental design and statistical
models. Archaeometry 41, 339-364.

Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman &
Hall/CRC, Boca Raton.

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent
dose and error calculation and display in OSL dating: An overview and
some recommendations. Quaternary Geochronology 11, 1-27.

Olley, J.M., Roberts, R.G., Yoshida, H., Bowler, J.M., 2006.
Single-grain optical dating of grave-infill associated with human
burials at Lake Mungo, Australia. Quaternary Science Reviews 25,
2469-2474

**Further reading**

Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain
equivalent dose (De) distributions: Implications for OSL dating of
sediment mixtures. Quaternary Geochronology 4, 204-230.

Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain
quartz De distributions and an assessment of procedures for estimating
burial dose. Quaternary Science Reviews 25, 2475-2502.

Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of
fluvial archives using robust OSL chronologies. Quaternary Geochronology
12, 98-106.

Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing
the reproducibility and accuracy of optical dating of fluvial deposits.
Quaternary Geochronology 1, 109-120.

Rodnight, H., 2008. How many equivalent dose values are needed to obtain
a reproducible distribution?. Ancient TL 26, 3-10.

## See also

[calc_CentralDose](https://r-lum.github.io/Luminescence/reference/calc_CentralDose.md),
[calc_CommonDose](https://r-lum.github.io/Luminescence/reference/calc_CommonDose.md),
[calc_FiniteMixture](https://r-lum.github.io/Luminescence/reference/calc_FiniteMixture.md),
[calc_FuchsLang2001](https://r-lum.github.io/Luminescence/reference/calc_FuchsLang2001.md),
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md)

## Author

Christoph Burow, University of Cologne (Germany)  
Based on a rewritten S script of Rex Galbraith, 2010 , RLum Developer
Team

## Examples

``` r
## load example data
data(ExampleData.DeValues, envir = environment())

# apply the maximum dose model
calc_MaxDose(ExampleData.DeValues$CA1, sigmab = 0.2, par = 3)
#> 
#> ----------- meta data -----------
#>   n par sigmab logged      Lmax      BIC
#>  62   3    0.2   TRUE -19.79245 58.86603
#> 
#> --- final parameter estimates ---
#>  gamma sigma   p0 mu
#>  76.58  1.71 0.65  0
#> 
#> ------ confidence intervals -----
#>       2.5 % 97.5 %
#> gamma 69.65  99.33
#> sigma  1.43   2.61
#> p0       NA   0.87
#> 
#> ------ De (asymmetric error) -----
#>     De lower upper
#>  76.58 69.65 99.33
#> 
#> ------ De (symmetric error) -----
#>     De error
#>  76.58  7.57

```
