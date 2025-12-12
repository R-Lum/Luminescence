# Dose-rate conversion function

This function converts radionuclide concentrations (K in %, Th and U in
ppm) into dose rates (Gy/ka). Beta-dose rates are also attenuated for
the grain size. Beta and gamma-dose rates are corrected for the water
content. This function converts concentrations into dose rates (Gy/ka)
and corrects for grain size attenuation and water content

Dose rate conversion factors can be chosen from Adamiec and Aitken
(1998), Guérin et al. (2011), Liritzis et al. (201) and Cresswell et al.
(2018). Default is Guérin et al. (2011).

Grain size correction for beta dose rates is achieved using the
correction factors published by Guérin et al. (2012).

Water content correction is based on factors provided by Aitken (1985),
with the factor for beta dose rate being 1.25 and for gamma 1.14.

## Usage

``` r
convert_Concentration2DoseRate(input, conversion = "Guerinetal2011")
```

## Arguments

- input:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*): a
  table containing all relevant information for each individual layer.
  If nothing is provided, the function returns a template data frame,
  the values of which need to be filled in by the user. Please note that
  only one dataset per input is supported.

- conversion:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  dose rate conversion factors to use, by default those by Guérin et al.
  (2011). For accepted values see
  [BaseDataSet.ConversionFactors](https://r-lum.github.io/Luminescence/reference/BaseDataSet.md).

## Value

The function returns an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object for which the first element is
[matrix](https://rdrr.io/r/base/matrix.html) with the converted values.
If no input is provided, the function returns a template
[data.frame](https://rdrr.io/r/base/data.frame.html) that can be used as
input.

## Details

**The input data**

|                   |             |                                       |
|-------------------|-------------|---------------------------------------|
| COLUMN            | DATA TYPE   | DESCRIPTION                           |
| `Mineral`         | `character` | `'FS'` for feldspar, `'Q'` for quartz |
| `K`               | `numeric`   | K nuclide content in %                |
| `K_SE`            | `numeric`   | error on K nuclide content in %       |
| `Th`              | `numeric`   | Th nuclide content in ppm             |
| `Th_SE`           | `numeric`   | error on Th nuclide content in ppm    |
| `U`               | `numeric`   | U nuclide content in ppm              |
| `U_SE`            | `numeric`   | error on U nuclide content in ppm     |
| `GrainSize`       | `numeric`   | average grain size in µm              |
| `WaterContent`    | `numeric`   | mean water content in %               |
| `WaterContent_SE` | `numeric`   | relative error on water content       |

**Water content** The water content provided by the user should be
calculated according to:

\$\$(Wet_weight - Dry_weight) / Dry_weight \* 100\$\$

The unit for the weight is gram (g).

## Function version

0.1.0

## How to cite

Riedesel, S., Autzen, M., 2025. convert_Concentration2DoseRate():
Dose-rate conversion function. Function version 0.1.0. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Adamiec, G., Aitken, M.J., 1998. Dose-rate conversion factors: update.
Ancient TL 16, 37-46.

Cresswell., A.J., Carter, J., Sanderson, D.C.W., 2018. Dose rate
conversion parameters: Assessment of nuclear data. Radiation
Measurements 120, 195-201.

Guérin, G., Mercier, N., Adamiec, G., 2011. Dose-rate conversion
factors: update. Ancient TL, 29, 5-8.

Guérin, G., Mercier, N., Nathan, R., Adamiec, G., Lefrais, Y., 2012. On
the use of the infinite matrix assumption and associated concepts: A
critical review. Radiation Measurements, 47, 778-785.

Liritzis, I., Stamoulis, K., Papachristodoulou, C., Ioannides, K., 2013.
A re-evaluation of radiation dose-rate conversion factors. Mediterranean
Archaeology and Archaeometry 13, 1-15.

## Author

Svenja Riedesel, Aberystwyth University (United Kingdom)  
Martin Autzen, DTU NUTECH Center for Nuclear Technologies (Denmark) ,
RLum Developer Team

## Examples

``` r
## create input template
input <- convert_Concentration2DoseRate()
#> [convert_Concentration2DoseRate()] Input template returned, please fill this data frame and use it as input to the function

## fill input
input$Mineral <- "FS"
input$K <- 2.13
input$K_SE <- 0.07
input$Th <- 9.76
input$Th_SE <- 0.32
input$U <- 2.24
input$U_SE <- 0.12
input$GrainSize <- 200
input$WaterContent <- 30
input$WaterContent_SE <- 5

## convert
convert_Concentration2DoseRate(input)
#> 
#>  [RLum.Results-class]
#>   originator: convert_Concentration2DoseRate()
#>   data: 2
#>       .. $InfDRG : matrix
#>   .. $input_data : data.frame
#>   additional info elements:  1 
```
