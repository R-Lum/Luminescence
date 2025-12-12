# Plot function for an RLum.Results S4 class object

The function provides a standardised plot output for data of an
RLum.Results S4 class object

## Usage

``` r
plot_RLum.Results(object, single = TRUE, ...)
```

## Arguments

- object:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  (**required**): S4 object of class `RLum.Results`

- single:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  single plot output (`TRUE/FALSE`) to allow for plotting the results in
  as few plot windows as possible.

- ...:

  further arguments and graphical parameters will be passed to the
  `plot` function.

## Value

Returns multiple plots.

## Details

The function produces a multiple plot output. A file output is
recommended (e.g., [pdf](https://rdrr.io/r/grDevices/pdf.html)).

## Note

Not all arguments available for
[plot](https://rdrr.io/r/graphics/plot.default.html) will be passed!
Only plotting of `RLum.Results` objects are supported.

## Function version

0.2.1

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md)

## Author

Christoph Burow, University of Cologne (Germany)  
Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Burow, C., Kreutzer, S., 2025. plot_RLum.Results(): Plot function for an
RLum.Results S4 class object. Function version 0.2.1. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r

###load data
data(ExampleData.DeValues, envir = environment())

# apply the un-logged minimum age model
mam <- calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.2, log = TRUE, plot = FALSE)
#> 
#> ----------- meta data -----------
#>   n par sigmab logged      Lmax      BIC
#>  62   3    0.2   TRUE -32.43138 84.14389
#> 
#> --- final parameter estimates ---
#>  gamma sigma   p0 mu
#>  45.64  1.56 0.02  0
#> 
#> ------ confidence intervals -----
#>       2.5 % 97.5 %
#> gamma 38.61  53.65
#> sigma  1.33   1.89
#> p0       NA   0.28
#> 
#> ------ De (asymmetric error) -----
#>     De lower upper
#>  45.64 38.61 53.65
#> 
#> ------ De (symmetric error) -----
#>     De error
#>  45.64  3.84

##plot
plot_RLum.Results(mam)


# estimate the number of grains on an aliquot
grains<- calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, plot = FALSE, MC.iter = 100)
#> 
#>  [calc_AliquotSize]
#> 
#>  ---------------------------------------------------------
#>  mean grain size (microns)  : 125
#>  sample diameter (mm)       : 1
#>  packing density            : 0.65
#>  number of grains           : 42
#> 
#>  --------------- Monte Carlo Estimates -------------------
#>  number of iterations (n)     : 100
#>  median                       : 41
#>  mean                         : 48
#>  standard deviation (mean)    : 26
#>  standard error (mean)        : 2.6
#>  95% CI from t-test (mean)    : 43 - 53
#>  standard error from CI (mean): 2.6
#>  ---------------------------------------------------------

##plot
plot_RLum.Results(grains)


```
