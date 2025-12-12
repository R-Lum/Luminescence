# Converting equivalent dose values from seconds (s) to Gray (Gy)

Conversion of absorbed radiation dose in seconds (s) to the SI unit Gray
(Gy) including error propagation. Normally used for equivalent dose
data.

Calculation of De values from seconds (s) to Gray (Gy)

\$\$De \[Gy\] = De \[s\] \* Dose Rate \[Gy/s\])\$\$

Provided calculation error propagation methods for error calculation
(with `'se'` as the standard error and `'DR'` of the dose rate of the
beta-source):

**(1) `omit`** (default)

\$\$se(De) \[Gy\] = se(De) \[s\] \* DR \[Gy/s\]\$\$

In this case the standard error of the dose rate of the beta-source is
treated as systematic (i.e. non-random), and error propagation is
omitted. However, the error must be considered during calculation of the
final age (cf. Aitken, 1985, pp. 242). This approach can be seen as
method (2) (gaussian) for the case the (random) standard error of the
beta-source calibration is 0. Which particular method is requested
depends on the situation and cannot be prescriptive.

**(2) `gaussian`** error propagation

\$\$se(De) \[Gy\] = \sqrt((DR \[Gy/s\] \* se(De) \[s\])^2 + (De \[s\] \*
se(DR) \[Gy/s\])^2)\$\$

Applicable under the assumption that errors of `De` and `se` are
uncorrelated.

**(3) `absolute`** error propagation

\$\$se(De) \[Gy\]= abs(DR \[Gy/s\] \* se(De) \[s\]) + abs(De \[s\] \*
se(DR) \[Gy/s\])\$\$

Applicable under the assumption that errors of `De` and `se` are
correlated.

## Usage

``` r
convert_Second2Gray(data, dose.rate, error.propagation = "omit")
```

## Arguments

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  input values, structure: data (`values[,1]`) and data error
  (`values [,2]`) are required.

- dose.rate:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
  [data.frame](https://rdrr.io/r/base/data.frame.html) or
  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  `RLum.Results` needs to be originated from the function
  [calc_SourceDoseRate](https://r-lum.github.io/Luminescence/reference/calc_SourceDoseRate.md),
  for `vector` dose rate in Gy/s and dose rate error in Gy/s.

- error.propagation:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  error propagation method used for error calculation (`omit`,
  `gaussian` or `absolute`), see details for further information.

## Value

Returns a [data.frame](https://rdrr.io/r/base/data.frame.html) with
converted values.

## Note

If no or a wrong error propagation method is given, the execution of the
function is stopped. Furthermore, if a `data.frame` is provided for the
dose rate values is has to be of the same length as the data frame
provided with the argument `data`

## Function version

0.6.0

## How to cite

Kreutzer, S., Dietze, M., Fuchs, M.C., 2025. convert_Second2Gray():
Converting equivalent dose values from seconds (s) to Gray (Gy).
Function version 0.6.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Aitken, M.J., 1985. Thermoluminescence dating. Academic Press.

## See also

[calc_SourceDoseRate](https://r-lum.github.io/Luminescence/reference/calc_SourceDoseRate.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Michael Dietze, GFZ Potsdam (Germany)  
Margret C. Fuchs, HZDR, Helmholtz-Institute Freiberg for Resource
Technology (Germany) , RLum Developer Team

## Examples

``` r
##(A) for known source dose rate at date of measurement
## - load De data from the example data help file
data(ExampleData.DeValues, envir = environment())
## - convert De(s) to De(Gy)
convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))
#>        De De.error
#> 1  151.48    5.334
#> 2  152.08    5.144
#> 3  165.80    6.805
#> 4  136.15    4.608
#> 5  144.42    4.642
#> 6  123.44    4.471
#> 7  123.64    4.227
#> 8  127.07    4.396
#> 9  125.06    4.630
#> 10 124.45    4.256
#> 11 118.60    4.049
#> 12 128.08    4.408
#> 13 110.78    3.701
#> 14 121.02    4.187
#> 15 124.09    4.129
#> 16 124.70    4.043
#> 17 123.68    4.262
#> 18 126.34    4.228
#> 19 128.59    4.254
#> 20 131.46    4.448
#> 21 127.77    4.330
#> 22 131.05    5.023
#> 23 126.34    4.317
#> 24 115.49    3.479
#> 25 119.58    3.815


##(B) for source dose rate calibration data
## - calculate source dose rate first
dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
                                  calib.date = "2014-12-19",
                                  calib.dose.rate = 0.0438,
                                  calib.error = 0.0019)
# read example data
data(ExampleData.DeValues, envir = environment())

# apply dose.rate to convert De(s) to De(Gy)
convert_Second2Gray(ExampleData.DeValues$BT998, dose.rate)
#>        De De.error
#> 1  162.37    5.717
#> 2  163.01    5.514
#> 3  177.72    7.294
#> 4  145.93    4.939
#> 5  154.80    4.976
#> 6  132.31    4.792
#> 7  132.53    4.531
#> 8  136.21    4.712
#> 9  134.04    4.962
#> 10 133.39    4.561
#> 11 127.12    4.340
#> 12 137.29    4.724
#> 13 118.74    3.967
#> 14 129.71    4.488
#> 15 133.01    4.426
#> 16 133.66    4.334
#> 17 132.57    4.568
#> 18 135.42    4.531
#> 19 137.83    4.560
#> 20 140.91    4.768
#> 21 136.96    4.641
#> 22 140.47    5.384
#> 23 135.42    4.628
#> 24 123.79    3.729
#> 25 128.18    4.090
```
