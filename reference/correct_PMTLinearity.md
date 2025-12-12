# Dead-time (linearity) Correction for Photomultiplier tubes (PMT)

Correct dead-time (also linearity) of PMT counts to avoid saturation
effects, depending on pulse-pair-resolution of individual PMTs.

## Usage

``` r
correct_PMTLinearity(object, PMT_pulse_pair_resolution = NULL)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  (**required**): object with records to correct; can be a
  [list](https://rdrr.io/r/base/list.html) of such objects

- PMT_pulse_pair_resolution:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  pulse-pair resolution in ns. Values can be found on the PMT
  datasheets. If `NULL` nothing is done.

## Value

Returns the same type of object type as `object`.

## Details

We correct for count linearity using a well-known formula that can be
found for example in the Hamamatsu Photomultiplier handbook (Hamamatsu
Photonics K.K., 2017):

\$\$N = \frac{M}{1 - M\*t}\$\$

where \\N\\ (in s\\^{-1}\\) is the true count rate, \\M\\ (in
s\\^{-1}\\) the measured count rate, and \\t\\ (in s) the pulse pair
resolution.

## Note

This function is an adaptation of core from the R package
'OSLdecomposition'.

## How to cite

Kreutzer, S., Mittelstrass, D., 2025. correct_PMTLinearity(): Dead-time
(linearity) Correction for Photomultiplier tubes (PMT). In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Hamamatsu Photonics K.K., 2017. Photomultiplier Tubes: Basics and
Applications, 4th edition. ed. Hamamatsu.

Mittelstraß, D., Kreutzer, S., Schmidt, C., 2022. OSLdecomposition:
Signal component analysis for optically stimulated luminescence.
[doi:10.32614/CRAN.package.OSLdecomposition](https://doi.org/10.32614/CRAN.package.OSLdecomposition)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Dirk Mittelstrass, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
o <- set_RLum("RLum.Data.Curve")
correct_PMTLinearity(o, PMT_pulse_pair_resolution = 10)
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: NA
#>   curveType: NA
#>   measured values: 1
#>   .. range of x-values: 0 0
#>   .. range of y-values: 0 0 
#>   additional info elements: 0 
```
