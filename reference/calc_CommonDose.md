# Apply the (un-)logged common age model after Galbraith et al. (1999) to a given De distribution

Function to calculate the common dose of a De distribution.

## Usage

``` r
calc_CommonDose(data, sigmab, log = TRUE, ...)
```

## Arguments

- data:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): for
  [data.frame](https://rdrr.io/r/base/data.frame.html): two columns with
  De `(data[,1])` and De error `(data[,2])`

- sigmab:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  additional spread in De values, representing the expected
  overdispersion in the data should the sample be well-bleached
  (Cunningham & Wallinga 2012, p. 100). **NOTE**: For the logged model
  (`log = TRUE`) this value must be a fraction, e.g. 0.2 (= 20%). If the
  un-logged model is used (`log = FALSE`), `sigmab` must be provided in
  the same absolute units of the De values (seconds or Gray).

- log:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): fit
  the (un-)logged central age model to De data

- ...:

  currently not used.

## Value

Returns a terminal output. In addition an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following element:

- \$summary:

  [data.frame](https://rdrr.io/r/base/data.frame.html) summary of all
  relevant model results.

- \$data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) original input
  data

- \$args:

  [list](https://rdrr.io/r/base/list.html) used arguments

- \$call:

  [call](https://rdrr.io/r/base/call.html) the function call

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Details

**(Un-)logged model**

When `log = TRUE` this function calculates the weighted mean of
logarithmic De values. Each of the estimates is weighted by the inverse
square of its relative standard error. The weighted mean is then
transformed back to the dose scale (Galbraith & Roberts 2012, p. 14).

The log transformation is not applicable if the De estimates are close
to zero or negative. In this case the un-logged model can be applied
instead (`log = FALSE`). The weighted mean is then calculated using the
un-logged estimates of De and their absolute standard error (Galbraith &
Roberts 2012, p. 14).

## Function version

0.1.1

## How to cite

Burow, C., 2025. calc_CommonDose(): Apply the (un-)logged common age
model after Galbraith et al. (1999) to a given De distribution. Function
version 0.1.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed
fission track ages. Nuclear Tracks Radiation Measurements 4, 459-470.

Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley,
J.M., 1999. Optical dating of single grains of quartz from Jinmium rock
shelter, northern Australia. Part I: experimental design and statistical
models. Archaeometry 41, 339-364.

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent
dose and error calculation and display in OSL dating: An overview and
some recommendations. Quaternary Geochronology 11, 1-27.

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
Quaternary Geochronology, 1 109-120.

Rodnight, H., 2008. How many equivalent dose values are needed to obtain
a reproducible distribution?. Ancient TL 26, 3-10.

## See also

[calc_CentralDose](https://r-lum.github.io/Luminescence/reference/calc_CentralDose.md),
[calc_FiniteMixture](https://r-lum.github.io/Luminescence/reference/calc_FiniteMixture.md),
[calc_FuchsLang2001](https://r-lum.github.io/Luminescence/reference/calc_FuchsLang2001.md),
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md)

## Author

Christoph Burow, University of Cologne (Germany) , RLum Developer Team

## Examples

``` r
## load example data
data(ExampleData.DeValues, envir = environment())

## apply the common dose model
calc_CommonDose(ExampleData.DeValues$CA1)
#> 
#>  [calc_CommonDose]
#> 
#> ----------- meta data --------------
#>  n:                       62
#>  log:                     TRUE
#> ----------- dose estimate ----------
#>  common dose:             62.16
#>  SE:                      0.78
#>  rel. SE [%]:             1.26
#> ------------------------------------
#> 
#> 
#>  [RLum.Results-class]
#>   originator: calc_CommonDose()
#>   data: 4
#>       .. $summary : data.frame
#>   .. $data : data.frame
#>   .. $args : list
#>   .. $call : call
#>   additional info elements:  0 
```
