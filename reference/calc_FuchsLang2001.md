# Apply the model after Fuchs & Lang (2001) to a given De distribution

This function applies the method according to Fuchs & Lang (2001) for
heterogeneously bleached samples with a given coefficient of variation
threshold.

## Usage

``` r
calc_FuchsLang2001(data, cvThreshold = 5, startDeValue = 1, plot = TRUE, ...)
```

## Arguments

- data:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): for
  [data.frame](https://rdrr.io/r/base/data.frame.html): two columns with
  De `(data[,1])` and De error `(values[,2])`

- cvThreshold:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  coefficient of variation in percent, as threshold for the method, e.g.
  `cvThreshold = 3`. See details .

- startDeValue:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  number of the first aliquot that is used for the calculations

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further arguments and graphical parameters passed to
  [plot](https://rdrr.io/r/graphics/plot.default.html)

## Value

Returns a plot (*optional*) and terminal output. In addition an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following elements:

- summary:

  [data.frame](https://rdrr.io/r/base/data.frame.html) summary of all
  relevant model results.

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) original input
  data

- args:

  [list](https://rdrr.io/r/base/list.html) used arguments

- call:

  [call](https://rdrr.io/r/base/call.html) the function call

- usedDeValues:

  [data.frame](https://rdrr.io/r/base/data.frame.html) containing the
  used values for the calculation

## Details

**Used values**

If the coefficient of variation (`c[v]`) of the first two values is
larger than the threshold `c[v_threshold]`, the first value is skipped.
Use the `startDeValue` argument to define a start value for calculation
(e.g. 2nd or 3rd value).

**Basic steps of the approach**

1.  Estimate natural relative variation of the sample using a dose
    recovery test

2.  Sort the input values in ascending order

3.  Calculate a running mean, starting with the lowermost two values and
    add values iteratively.

4.  Stop if the calculated `c[v]` exceeds the specified `cvThreshold`

## Note

Please consider the requirements and the constraints of this method (see
Fuchs & Lang, 2001)

## Function version

0.4.1

## How to cite

Kreutzer, S., Burow, C., 2025. calc_FuchsLang2001(): Apply the model
after Fuchs & Lang (2001) to a given De distribution. Function version
0.4.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## References

Fuchs, M. & Lang, A., 2001. OSL dating of coarse-grain fluvial quartz
using single-aliquot protocols on sediments from NE Peloponnese, Greece.
In: Quaternary Science Reviews 20, 783-787.

Fuchs, M. & Wagner, G.A., 2003. Recognition of insufficient bleaching by
small aliquots of quartz for reconstructing soil erosion in Greece.
Quaternary Science Reviews 22, 1161-1167.

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html),
[calc_MinDose](https://r-lum.github.io/Luminescence/reference/calc_MinDose.md),
[calc_FiniteMixture](https://r-lum.github.io/Luminescence/reference/calc_FiniteMixture.md),
[calc_CentralDose](https://r-lum.github.io/Luminescence/reference/calc_CentralDose.md),
[calc_CommonDose](https://r-lum.github.io/Luminescence/reference/calc_CommonDose.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Christoph Burow, University of Cologne (Germany) , RLum Developer Team

## Examples

``` r
## load example data
data(ExampleData.DeValues, envir = environment())

## calculate De according to Fuchs & Lang (2001)
temp<- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5)
#> 
#> [calc_FuchsLang2001]
#> 
#> ----------- meta data --------------
#>  cvThreshold:             5 %
#>  used values:             22
#> ----------- dose estimate ----------
#>  mean:                    2866.11
#>  sd:                      157.35
#>  weighted mean:           2846.66
#>  weighted sd:             20.58
#>  se:                      33.55
#> ------------------------------------
#> 

```
