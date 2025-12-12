# Apply the internal-external-uncertainty (IEU) model after Thomsen et al. (2007) to a given De distribution

Function to calculate the IEU De for a De data set.

## Usage

``` r
calc_IEU(data, a, b, interval, decimal.point = 2, plot = TRUE, ...)
```

## Arguments

- data:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): for
  [data.frame](https://rdrr.io/r/base/data.frame.html): two columns with
  De `(data[,1])` and De error `(values[,2])`

- a:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): slope

- b:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  intercept

- interval:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): fixed
  interval (e.g. 5 Gy) used for iteration of `Dbar`, from the mean to
  Lowest.De used to create Graph.IEU `[Dbar.Fixed vs Z]`

- decimal.point:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  number of decimal points for rounding calculations (e.g. 2)

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further arguments (`trace, verbose`).

## Value

Returns a plot (*optional*) and terminal output. In addition an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following elements:

- .\$summary:

  [data.frame](https://rdrr.io/r/base/data.frame.html) summary of all
  relevant model results.

- .\$data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) original input
  data

- .\$args:

  [list](https://rdrr.io/r/base/list.html) used arguments

- .\$call:

  [call](https://rdrr.io/r/base/call.html) the function call

- .\$tables:

  [list](https://rdrr.io/r/base/list.html) a list of data frames
  containing all calculation tables

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Details

This function uses the equations of Thomsen et al. (2007). The
parameters a and b are estimated from dose-recovery experiments.

## Function version

0.1.1

## How to cite

Smedley, R.K., Colombo, M., 2025. calc_IEU(): Apply the
internal-external-uncertainty (IEU) model after Thomsen et al. (2007) to
a given De distribution. Function version 0.1.1. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Smedley, R.K., 2015. A new R function for the Internal External
Uncertainty (IEU) model. Ancient TL 33, 16-21.

Thomsen, K.J., Murray, A.S., Boetter-Jensen, L. & Kinahan, J., 2007.
Determination of burial dose in incompletely bleached fluvial samples
using single grains of quartz. Radiation Measurements 42, 370-379.

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html),
[calc_CommonDose](https://r-lum.github.io/Luminescence/reference/calc_CommonDose.md),
[calc_CentralDose](https://r-lum.github.io/Luminescence/reference/calc_CentralDose.md),
[calc_FiniteMixture](https://r-lum.github.io/Luminescence/reference/calc_FiniteMixture.md),
[calc_FuchsLang2001](https://r-lum.github.io/Luminescence/reference/calc_FuchsLang2001.md),
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md)

## Author

Rachel Smedley, Geography & Earth Sciences, Aberystwyth University
(United Kingdom)  
Based on an excel spreadsheet and accompanying macro written by Kristina
Thomsen.  
Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## Examples

``` r
## load data
data(ExampleData.DeValues, envir = environment())

## apply the IEU model
ieu <- calc_IEU(ExampleData.DeValues$CA1, a = 0.2, b = 1.9, interval = 1)


#> 
#>  [calc_IEU] 
#> 
#>  Dbar: 46.67 
#>  IEU.De (Gy): 46.67 
#>  IEU.Error (Gy): 2.55 Number of De: 24 
#>  a: 0.2000 
#>  b: 1.9000
```
