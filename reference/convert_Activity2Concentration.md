# Convert Nuclide Activities to Abundance and Vice Versa

The function performs the conversion of the specific activities into
mass abundance and vice versa for the radioelements U, Th, and K to
harmonise the measurement unit with the required data input unit of
potential analytical tools for, e.g. dose rate calculation or related
functions such as
[use_DRAC](https://r-lum.github.io/Luminescence/reference/use_DRAC.md).

## Usage

``` r
convert_Activity2Concentration(data, input_unit = "activity", verbose = TRUE)
```

## Arguments

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  provide dose rate data (activity or concentration) in three columns.
  The first column indicates the nuclide, the 2nd column measured value
  and in the 3rd column its error value. Allowed nuclide data are
  `'U-238'`, `'Th-232'` and `'K-40'`. See examples for an example.

- input_unit:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  specify unit of input data given in the dose rate data frame, choose
  between `"activity"` (considered as given Bq/kg) and `"abundance"`
  (considered as given in mug/g or mass. %). The default value is
  `"activity"`

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

## Value

Returns an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object with a [data.frame](https://rdrr.io/r/base/data.frame.html)
containing input and newly calculated values. Please not that in the
column header µg/g is written as `mug/g` due to the R requirement to
maintain packages portable using ASCII characters only.

## Details

The conversion from nuclide activity of a sample to nuclide
concentration is performed using conversion factors that are based on
the mass-related specific activity of the respective nuclide.

Constants used in this function were obtained from
`https://physics.nist.gov/cuu/Constants/` all atomic weights and
composition values from
`https://www.nist.gov/pml/atomic-weights-and-isotopic-compositions-relative-atomic-masses`
and the nuclide data from
`https://www.iaea.org/resources/databases/livechart-of-nuclides-advanced-version`

The factors can be calculated using the equation:

\$\$ A = N_A \frac{N\_{abund}}{N\_{mol.mass}} ln(2) / N.half.life \$\$

to convert in µg/g we further use:

\$\$ f = A / 10^6 \$\$

where:

- `N_A` - Avogadro constant in 1/mol

- `A` - specific activity of the nuclide in Bq/kg

- `N.abund` - relative natural abundance of the isotope

- `N.mol.mass` molar mass in kg/mol

- `N.half.life` half-life of the nuclide in s

example for calculating the activity of the radionuclide U-238:

- `N_A` = 6.02214076e+23 (1/mol)

- `T_0.5` = 1.41e+17 (s)

- `m_U_238` = 0.23802891 (kg/mol)

- `U_abund` = 0.992745 (unitless)

\$\$A\_{U} = N\_{A} \* U\_{abund} / m\_{U_238} \* ln(2) / T\_{1/2} =
2347046\$\$ (Bq/kg)

\$\$f.U = A\_{U} / 10^6\$\$

## Note

Although written otherwise for historical reasons. Input values must be
element values. For instance, if a value is provided for U-238 the
function assumes that this value represents the sum (activity or
abundance) of U-238, U-235 and U-234. In other words, 1 µg/g of U means
that this is the composition of 0.992 parts of U-238, 0.000054 parts of
U-234, and 0.00072 parts of U-235.

## Function version

0.1.2

## How to cite

Fuchs, M.C., 2025. convert_Activity2Concentration(): Convert Nuclide
Activities to Abundance and Vice Versa. Function version 0.1.2. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Debertin, K., Helmer, R.G., 1988. Gamma- and X-ray Spectrometry with
Semiconductor Detectors, Elsevier Science Publishers, p.283

Wiechen, A., Ruehle, H., Vogl, K., 2013. Bestimmung der massebezogenen
Aktivitaet von Radionukliden. AEQUIVAL/MASSAKT, ISSN 1865-8725,
<https://www.bmuv.de/fileadmin/Daten_BMU/Download_PDF/Strahlenschutz/aequival-massakt_v2013-07_bf.pdf>

## Author

Margret C. Fuchs, Helmholtz-Institute Freiberg for Resource Technology
(Germany) , RLum Developer Team

## Examples

``` r
##construct data.frame
data <- data.frame(
 NUCLIDES = c("U-238", "Th-232", "K-40"),
 VALUE = c(40,80,100),
 VALUE_ERROR = c(4,8,10),
 stringsAsFactors = FALSE)

##perform analysis
convert_Activity2Concentration(data)
#>   NUCLIDE ACTIVIY (Bq/kg) ACTIVIY ERROR (Bq/kg) ABUND. (mug/g or mass. %)
#> 1   U-238              40                     4                 3.2398475
#> 2  Th-232              80                     8                19.6469710
#> 3    K-40             100                    10                 0.3222668
#>   ABUND. ERROR (mug/g or mass. %)
#> 1                      0.32398475
#> 2                      1.96469710
#> 3                      0.03222668
```
