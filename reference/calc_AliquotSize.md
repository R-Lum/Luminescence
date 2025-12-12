# Estimate the amount of grains on an aliquot

This function can be used to either estimate the number of grains on an
aliquot or to compute the packing density, depending on the arguments
provided.

The following formula is used to estimate the number of grains `n`:

\$\$n = (\pi\*x^2)/(\pi\*y^2)\*d\$\$

where `x` is the radius of the aliquot size (microns), `y` is the mean
radius of the mineral grains (mm), and `d` is the packing density (a
value between 0 and 1).

**Packing density**

The default value for `packing.density` is 0.65, which is the mean of
empirical values determined by Heer et al. (2012) and unpublished data
from the Cologne luminescence laboratory. If `packing.density = Inf`, a
maximum density of \\\pi / \sqrt{12} \approx 0.9069\\ is used. However,
note that this value is not appropriate as the standard preparation
procedure of aliquots resembles a PECC (*Packing Equal Circles in a
Circle*) problem, where the maximum packing density is asymptotic to
about 0.87.

**Monte Carlo simulation**

The number of grains on an aliquot can be estimated by Monte Carlo
simulation when setting `MC = TRUE`. All parameters necessary to
calculate `n` (`x`, `y`, `d`) are assumed to be normally distributed
with means \\\mu_x, \mu_y, \mu_d\\ and standard deviations \\\sigma_x,
\sigma_y, \sigma_d\\.

For the mean grain size, random samples are taken first from \\N(\mu_y,
\sigma_y)\\, where \\\mu_y = mean.grain.size\\ and \\\sigma_y =
(max.grain.size-min.grain.size)/4\\, so that 95% of all grains are
within the provided the grain size range. This effectively takes into
account that after sieving the sample there is still a small chance of
having grains smaller or larger than the used mesh sizes. For each
random sample the mean grain size is calculated, from which random
subsamples are drawn for the Monte Carlo simulation.

The packing density is assumed to be normally distributed with an
empirically determined \\\mu = 0.65\\ (or provided value) and \\\sigma =
0.18\\. The normal distribution is truncated at `d = 0.87` as this is
approximately the maximum packing density that can be achieved in a PECC
problem.

The sample diameter has \\\mu = sample.diameter\\ and \\\sigma = 0.2\\
to take into account variations in sample disc preparation (i.e.
applying silicon spray to the disc). A lower truncation point at
`x = 0.5` is used, which assumes that aliquots with sample diameter
smaller than 0.5 mm are discarded. Likewise, the normal distribution is
truncated at the diameter of the sample carrier (9.8 mm by default, but
controllable via the `sample_carrier.diameter` argument).

For each random sample drawn from the normal distribution, the amount of
grains on the aliquot is calculated. By default, `10^4` iterations are
used, but the can be controlled with option `MC.iter` (see `...`).
Results are visualised in a bar- and boxplot together with a statistical
summary.

## Usage

``` r
calc_AliquotSize(
  grain.size,
  sample.diameter,
  packing.density = 0.65,
  MC = TRUE,
  grains.counted = NULL,
  sample_carrier.diameter = 9.8,
  plot = TRUE,
  ...
)
```

## Arguments

- grain.size:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): mean
  grain size (microns) or a range of grain sizes from which the mean
  grain size is computed (e.g. `c(100,200)`).

- sample.diameter:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  diameter (mm) of the targeted area on the sample carrier.

- packing.density:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  empirical value for the mean packing density.  
  If `packing.density = Inf`, a hexagonal structure on an infinite plane
  with a packing density of \\\pi / \sqrt{12} \approx 0.9069\\ is
  assumed.

- MC:

  [logical](https://rdrr.io/r/base/logical.html) (*optional*): if `TRUE`
  the function performs a Monte Carlo simulation for estimating the
  amount of grains on the sample carrier and assumes random errors in
  grain size distribution and packing density. Requires `grain.size` to
  be specified as a 2-element vector with the range (min and max) of
  grain sizes. For more information see details.

- grains.counted:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): grains
  counted on a sample carrier. If a non-zero positive integer is
  provided, this function will calculate the packing density of the
  aliquot. If more than one value is provided, the mean packing density
  and its standard deviation are calculated. Note that this overrides
  `packing.density`.

- sample_carrier.diameter:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  diameter (mm) of the sample carrier.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further arguments and graphical parameters to be passed. In
  particular, `MC.iter` to control the number of Monte Carlo iterations,
  `main`, `xlab`, `col`, `line_col`, `line_lwd`, `cex`, `rug`
  (`TRUE/FALSE`), `boxplot` (`TRUE/FALSE`), `summary` (`TRUE/FALSE`),
  `legend` (`TRUE/FALSE`).

## Value

Returns a terminal output. In addition an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following element:

- .\$summary:

  [data.frame](https://rdrr.io/r/base/data.frame.html) summary of all
  relevant calculation results.

- .\$args:

  [list](https://rdrr.io/r/base/list.html) used arguments

- .\$call:

  [call](https://rdrr.io/r/base/call.html) the function call

- .\$MC:

  [list](https://rdrr.io/r/base/list.html) results of the Monte Carlo
  simulation

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Function version

0.33

## How to cite

Burow, C., Colombo, M., 2025. calc_AliquotSize(): Estimate the amount of
grains on an aliquot. Function version 0.33. In: Kreutzer, S., Burow,
C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Duller, G.A.T., 2008. Single-grain optical dating of Quaternary
sediments: why aliquot size matters in luminescence dating. Boreas 37,
589-612.

Heer, A.J., Adamiec, G., Moska, P., 2012. How many grains are there on a
single aliquot?. Ancient TL 30, 9-16.

**Further reading**

Chang, H.-C., Wang, L.-C., 2010. A simple proof of Thue's Theorem on
Circle Packing. <https://arxiv.org/pdf/1009.4322v1>, 2013-09-13.

Graham, R.L., Lubachevsky, B.D., Nurmela, K.J., Oestergard, P.R.J.,
1998. Dense packings of congruent circles in a circle. Discrete
Mathematics 181, 139-154.

Huang, W., Ye, T., 2011. Global optimization method for finding dense
packings of equal circles in a circle. European Journal of Operational
Research 210, 474-481.

## Author

Christoph Burow, University of Cologne (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## Examples

``` r
## Estimate the amount of grains on a small aliquot
calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, MC.iter = 100)
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
#>  median                       : 38
#>  mean                         : 42
#>  standard deviation (mean)    : 20
#>  standard error (mean)        : 2
#>  95% CI from t-test (mean)    : 38 - 46
#>  standard error from CI (mean): 2
#>  ---------------------------------------------------------


## Calculate the mean packing density of large aliquots
calc_AliquotSize(grain.size = c(100,200), sample.diameter = 8,
                 grains.counted = c(2525,2312,2880), MC.iter = 100)
#> [calc_AliquotSize()] Monte Carlo simulation is only available for estimating the amount of grains on the sample disc, 'MC' reset to FALSE
#> 
#>  [calc_AliquotSize]
#> 
#>  ---------------------------------------------------------
#>  mean grain size (microns)  : 150
#>  sample diameter (mm)       : 8
#>  mean counted grains        : 2572
#>  mean packing density       : 0.904
#>  standard deviation         : 0.101
#>  ---------------------------------------------------------
```
