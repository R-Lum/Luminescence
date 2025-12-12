# Obtain the equivalent dose using the approach by Woda and Fuchs 2008

The function generates a histogram-like reorganisation of the data, to
assess counts per bin. The log-transformed counts per bin are used to
calculate the second derivative of the data (i.e., the curvature of the
curve) and to find the central value of the bin hosting the distribution
maximum. A normal distribution model is fitted to the counts per bin
data to estimate the dose distribution parameters. The uncertainty of
the model is estimated based on all input equivalent doses smaller that
of the modelled central value.

## Usage

``` r
calc_WodaFuchs2008(data, breaks = NULL, plot = TRUE, ...)
```

## Arguments

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html)
  [vector](https://rdrr.io/r/base/vector.html), or
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object (**required**): for
  [data.frame](https://rdrr.io/r/base/data.frame.html): either two
  columns: De (`values[,1]`) and De error (`values[,2]`), or one: De
  (`values[,1]`). If a numeric vector or a single-column data frame is
  provided, De error is set to `NA`. For plotting multiple data sets,
  these must be provided as `list` (e.g. `list(dataset1, dataset2)`).

- breaks:

  [numeric](https://rdrr.io/r/base/numeric.html): Either number or
  locations of breaks. See `[hist]` for details. If missing, the number
  of breaks will be estimated based on the bin width (as function of
  median error).

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  Further plot arguments passed to the function.

## Function version

0.2.0

## How to cite

Kreutzer, S., Dietze, M., 2025. calc_WodaFuchs2008(): Obtain the
equivalent dose using the approach by Woda and Fuchs 2008. Function
version 0.2.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Woda, C., Fuchs, M., 2008. On the applicability of the leading edge
method to obtain equivalent doses in OSL dating and dosimetry. Radiation
Measurements 43, 26-37.

## See also

[calc_FuchsLang2001](https://r-lum.github.io/Luminescence/reference/calc_FuchsLang2001.md),
[calc_CentralDose](https://r-lum.github.io/Luminescence/reference/calc_CentralDose.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany),  
Michael Dietze, GFZ Potsdam (Germany) , RLum Developer Team

## Examples

``` r
## read example data set
data(ExampleData.DeValues, envir = environment())

results <- calc_WodaFuchs2008(
  data = ExampleData.DeValues$CA1,
   xlab = expression(paste(D[e], " [Gy]"))
 )

```
