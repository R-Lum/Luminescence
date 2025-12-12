# Emission Spectra Conversion from Wavelength to Energy Scales (Jacobian Conversion)

The function provides a convenient and fast way to convert emission
spectra wavelength to energy scales. The function works on
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[data.frame](https://rdrr.io/r/base/data.frame.html) and
[matrix](https://rdrr.io/r/base/matrix.html) and a
[list](https://rdrr.io/r/base/list.html) of such objects. The function
was written to smooth the workflow while analysing emission spectra
data. This is in particular useful if you want to further treat your
data and apply, e.g., a signal deconvolution.

## Usage

``` r
convert_Wavelength2Energy(object, digits = 3L, order = FALSE)
```

## Arguments

- object:

  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
  [data.frame](https://rdrr.io/r/base/data.frame.html),
  [matrix](https://rdrr.io/r/base/matrix.html) (**required**): input
  object to be converted. If the input is not an `RLum.Data.Spectrum`
  object, the first column is always treated as the wavelength column.
  The function supports a list of allowed input objects.

- digits:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of digits on the returned energy axis.

- order:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable sorting of the values in ascending energy order. After
  the conversion, the longest wavelength has the lowest energy value and
  the shortest wavelength the highest. While this is correct, some R
  functions expect increasing x-values.

## Value

The same object class as provided as input is returned.

## Details

The intensity of the spectrum is re-calculated using the following
approach to recalculate wavelength and corresponding intensity values
(e.g., Appendix 4 in Blasse and Grabmaier, 1994; Mooney and Kambhampati,
2013):

\$\$\phi\_{E} = \phi\_{\lambda} \* \lambda^2 / (hc)\$\$

with \\\phi\_{E}\\ the intensity per interval of energy \\E\\ (1/eV),
\\\phi\_{\lambda}\\ the intensity per interval of wavelength \\\lambda\\
(1/nm) and \\h\\ (eV \* s) the Planck constant and \\c\\ (nm/s) the
velocity of light.

For transforming the wavelength axis (x-values) the equation as follow
is used

\$\$E = hc/\lambda\$\$

## Note

This conversion works solely for emission spectra. In case of absorption
spectra only the x-axis has to be converted.

## Function version

0.1.1

## How to cite

Kreutzer, S., 2025. convert_Wavelength2Energy(): Emission Spectra
Conversion from Wavelength to Energy Scales (Jacobian Conversion).
Function version 0.1.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Blasse, G., Grabmaier, B.C., 1994. Luminescent Materials. Springer.

Mooney, J., Kambhampati, P., 2013. Get the Basics Right: Jacobian
Conversion of Wavelength and Energy Scales for Quantitative Analysis of
Emission Spectra. J. Phys. Chem. Lett. 4, 3316–3318.
[doi:10.1021/jz401508t](https://doi.org/10.1021/jz401508t)

Mooney, J., Kambhampati, P., 2013. Correction to “Get the Basics Right:
Jacobian Conversion of Wavelength and Energy Scales for Quantitative
Analysis of Emission Spectra.” J. Phys. Chem. Lett. 4, 3316–3318.
[doi:10.1021/jz401508t](https://doi.org/10.1021/jz401508t)

**Further reading**

Angulo, G., Grampp, G., Rosspeintner, A., 2006. Recalling the
appropriate representation of electronic spectra. Spectrochimica Acta
Part A: Molecular and Biomolecular Spectroscopy 65, 727–731.
[doi:10.1016/j.saa.2006.01.007](https://doi.org/10.1016/j.saa.2006.01.007)

Wang, Y., Townsend, P.D., 2013. Potential problems in collection and
data processing of luminescence signals. Journal of Luminescence 142,
202–211.
[doi:10.1016/j.jlumin.2013.03.052](https://doi.org/10.1016/j.jlumin.2013.03.052)

## See also

[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##=====================##
##(1) Literature example after Mooney et al. (2013)
##(1.1) create matrix
m <- matrix(
  data = c(seq(400, 800, 50), rep(1, 9)), ncol = 2)

##(1.2) set plot function to reproduce the literature figure
p <- function(m) {
 plot(x = m[, 1], y = m[, 2])
 polygon(
 x = c(m[, 1], rev(m[, 1])),
 y = c(m[, 2], rep(0, nrow(m))))
 for (i in 1:nrow(m)) {
  lines(x = rep(m[i, 1], 2), y = c(0, m[i, 2]))
 }
}

##(1.3) plot curves
par(mfrow = c(1,2))
p(m)
p(convert_Wavelength2Energy(m))


##=====================##
##(2) Another example using density curves
##create dataset
xy <- density(
 c(rnorm(n = 100, mean = 500, sd = 20),
 rnorm(n = 100, mean = 800, sd = 20)))
xy <- data.frame(xy$x, xy$y)

##plot
par(mfrow = c(1,2))
plot(
 xy,
 type = "l",
 xlim = c(150, 1000),
 xlab = "Wavelength [nm]",
 ylab = "Luminescence [a.u.]"
)
plot(
 convert_Wavelength2Energy(xy),
 xy$y,
 type = "l",
 xlim = c(1.23, 8.3),
 xlab = "Energy [eV]",
 ylab = "Luminescence [a.u.]"
)

```
