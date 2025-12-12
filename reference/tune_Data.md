# Tune data for experimental purpose

The error can be reduced and sample size increased for specific purpose.

## Usage

``` r
tune_Data(data, decrease.error = 0, increase.data = 0)
```

## Arguments

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  input values, structure: data (`values[,1]`) and data error
  (`values [,2]`) are required

- decrease.error:

  [numeric](https://rdrr.io/r/base/numeric.html): factor by which the
  error is decreased, ranges between 0 and 1.

- increase.data:

  [numeric](https://rdrr.io/r/base/numeric.html): factor by which the
  error is decreased, ranges between 0 and `Inf`.

## Value

Returns a [data.frame](https://rdrr.io/r/base/data.frame.html) with
tuned values.

## Note

You should not use this function to improve your poor data set!

## Function version

0.5.0

## Author

Michael Dietze, GFZ Potsdam (Germany) , RLum Developer Team

## How to cite

Dietze, M., 2025. tune_Data(): Tune data for experimental purpose.
Function version 0.5.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data set
data(ExampleData.DeValues, envir = environment())
x <- ExampleData.DeValues$CA1

## plot original data
plot_AbanicoPlot(data = x,
                 summary = c("n", "mean"))


## decrease error by 10 %
plot_AbanicoPlot(data = tune_Data(x, decrease.error = 0.1),
                 summary = c("n", "mean"))
#> Warning: [tune_Data()] Dear runner, these activities on your Linux machine have been tracked and will be submitted to the R.Lum data base. Cheating does not pay off! [2025-12-12 08:44:04.367648]


## increase sample size by 200 %
#plot_AbanicoPlot(data = tune_Data(x, increase.data = 2) ,
#                summary = c("n", "mean"))
```
