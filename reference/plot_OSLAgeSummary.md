# Plot Posterior OSL-Age Summary

The function produces a graphical summary of the statistical inference
of an OSL age.

## Usage

``` r
plot_OSLAgeSummary(object, level = 0.95, digits = 1L, verbose = TRUE, ...)
```

## Arguments

- object:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): an
  object produced by
  [combine_De_Dr](https://r-lum.github.io/Luminescence/reference/combine_De_Dr.md).
  Alternatively, a [numeric](https://rdrr.io/r/base/numeric.html) vector
  of a parameter from an MCMC process.

- level:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  probability of shown credible interval.

- digits:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of digits considered for the calculation.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  further arguments to modify the plot, supported: `xlim`, `ylim`,
  `xlab`, `ylab`, `main`, `lwd`, `lty`, `col`, `rug`, `polygon_col`,
  `polygon_density`.

## Value

A posterior distribution plot and an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object with the credible interval.

## Details

The function is called automatically by
[combine_De_Dr](https://r-lum.github.io/Luminescence/reference/combine_De_Dr.md).

## Function version

0.1.0

## See also

[combine_De_Dr](https://r-lum.github.io/Luminescence/reference/combine_De_Dr.md),
[plot.default](https://rdrr.io/r/graphics/plot.default.html),
[rjags::rjags](https://rdrr.io/pkg/rjags/man/rjags-package.html)

## Author

Anne Philippe, Université de Nantes (France), Jean-Michel Galharret,
Université de Nantes (France), Norbert Mercier, IRAMAT-CRP2A, Université
Bordeaux Montaigne (France), Sebastian Kreutzer, Institute of Geography,
Heidelberg University (Germany) , RLum Developer Team

## How to cite

Philippe, A., Galharret, J., Mercier, N., Kreutzer, S., 2025.
plot_OSLAgeSummary(): Plot Posterior OSL-Age Summary. Function version
0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## Examples

``` r
##generate random data
set.seed(1234)
object <- rnorm(1000, 100, 10)
plot_OSLAgeSummary(object)
#> 
#> [plot_OSLAgeSummary()]
#>  Credible Interval (95 %):  80.5 : 120.6 
#>  Bayes estimate (posterior mean ± sd): 99.7 ± 10 

#> 
#>  [RLum.Results-class]
#>   originator: plot_OSLAgeSummary()
#>   data: 3
#>       .. $Estimate : numeric
#>   .. $Credible_Interval : matrix
#>   .. $level : numeric
#>   additional info elements:  1 
```
