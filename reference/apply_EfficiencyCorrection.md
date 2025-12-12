# Function to apply spectral efficiency correction to RLum.Data.Spectrum S4 class objects

The function allows spectral efficiency corrections for
RLum.Data.Spectrum S4 class objects

## Usage

``` r
apply_EfficiencyCorrection(object, spectral.efficiency)
```

## Arguments

- object:

  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  or
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): S4 object of class `RLum.Data.Spectrum`,
  `RLum.Analysis`or a [list](https://rdrr.io/r/base/list.html) of such
  objects. Other objects in the list are skipped.

- spectral.efficiency:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  Data set containing wavelengths (x-column) and relative spectral
  response values (y-column) (values between 0 and 1). The provided data
  will be used to correct all spectra if `object` is a
  [list](https://rdrr.io/r/base/list.html)

## Value

Returns same object as provided as input

## Details

The efficiency correction is based on a spectral response dataset
provided by the user. Usually the data set for the quantum efficiency is
of lower resolution and values are interpolated for the required
spectral resolution using the function
[stats::approx](https://rdrr.io/r/stats/approxfun.html)

If the energy calibration differs for both data set `NA` values are
produces that will be removed from the matrix.

## Note

Please note that the spectral efficiency data from the camera alone may
not sufficiently correct for spectral efficiency of the entire optical
system (e.g., spectrometer, camera ...).

## Function version

0.2.0

## See also

[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)

## Author

Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux
Montaigne (France)  
Johannes Friedrich, University of Bayreuth (Germany) , RLum Developer
Team

## How to cite

Kreutzer, S., Friedrich, J., 2025. apply_EfficiencyCorrection():
Function to apply spectral efficiency correction to RLum.Data.Spectrum
S4 class objects. Function version 0.2.0. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##(1) - use with your own data (uncomment for usage)
## spectral.efficiency <- read.csv("your data")
##
## your.spectrum <- apply_EfficiencyCorrection(your.spectrum, )
```
