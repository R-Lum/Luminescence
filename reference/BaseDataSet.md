# Base datasets

Collection of datasets with published and unpublished data used within
the package.

- BaseData.ConversionFactors:

  Collection of published dose-rate conversion factors to convert
  concentrations of radioactive isotopes to dose rate values

- BaseData.GrainSizeAttenuation:

  Grain size attenuation data by Guérin et al. (2012)

- BaseData.FractionalGammaDose:

  Collection of (un-)published fractional gamma dose-rate values to
  scale the gamma-dose rate considering layer-to-layer variations in
  soil radioactivity

## Format

**Dose-rate conversion factors**

A [`list`](https://rdrr.io/r/base/list.html) with three elements with
dose-rate conversion factors sorted by article and radiation type
(alpha, beta, gamma):

|                      |                                           |
|----------------------|-------------------------------------------|
| `AdamiecAitken1998`: | Conversion factors from Tables 5 and 6    |
| `Cresswelletal2018`: | Conversion factors from Tables 5 and 6    |
| `Guerinetal2011`:    | Conversion factors from Tables 1, 2 and 3 |
| `Liritzisetal2013`:  | Conversion factors from Tables 1, 2 and 3 |

**Grain size attenuation data**

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) seven columns
and sixteen rows. Column headers are `GrainSize`, `Q_K`, `FS_K`, `Q_Th`,
`FS_Th`, `Q_U`, `FS_U`. Grain sizes are quoted in µm (e.g., 20, 40, 60
etc.)

**Fractional gamma dose-rate values**

A [`list`](https://rdrr.io/r/base/list.html) with fractional gamma
dose-rate values sorted by article:

|               |                                             |
|---------------|---------------------------------------------|
| `Aitken1985`: | Fractional gamma-dose values from table H.1 |

## Source

**Dose-rate conversion factors**

All gamma conversion factors were carefully read from the tables given
in:

Adamiec, G., Aitken, M.J., 1998. Dose-rate conversion factors: update.
Ancient TL 16, 37-46.

Cresswell., A.J., Carter, J., Sanderson, D.C.W., 2018. Dose rate
conversion parameters: Assessment of nuclear data. Radiation
Measurements 120, 195-201.

Guérin, G., Mercier, N., Adamiec, G., 2011. Dose-rate conversion
factors: update. Ancient TL, 29, 5-8.

Liritzis, I., Stamoulis, K., Papachristodoulou, C., Ioannides, K., 2013.
A re-evaluation of radiation dose-rate conversion factors. Mediterranean
Archaeology and Archaeometry 13, 1-15.

**Grain size attenuation data**

Guérin, G., Mercier, N., Nathan, R., Adamiec, G., Lefrais, Y., 2012. On
the use of the infinite matrix assumption and associated concepts: A
critical review. Radiation Measurements, 47, 778-785.

**Fractional gamma dose-rate values**

Fractional gamma dose values were carefully read from the tables given
in:

Aitken, M.J., 1985. Thermoluminescence Dating. Academic Press, London.

## Version

0.2.0

## Examples

``` r
## conversion factors
data("BaseDataSet.ConversionFactors", envir = environment())

## grain size attenuation
data("BaseDataSet.GrainSizeAttenuation", envir = environment())

## fractional gamma dose
data("BaseDataSet.FractionalGammaDose", envir = environment())
```
