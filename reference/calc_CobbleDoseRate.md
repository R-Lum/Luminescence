# Calculate dose rate of slices in a spherical cobble

Calculates the dose rate profile through the cobble based on Riedesel
and Autzen (2020).

Corrects the beta dose rate in the cobble for the grain size following
results of Guérin et al. (2012). Sediment beta and gamma dose rates are
corrected for the water content of the sediment using the correction
factors of Aitken (1985). Water content in the cobble is assumed to be
0.

## Usage

``` r
calc_CobbleDoseRate(input, conversion = "Guerinetal2011")
```

## Arguments

- input:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**): A
  table containing all relevant information for each individual layer.
  For the table layout see details.

- conversion:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  dose rate conversion factors to use, see
  [BaseDataSet.ConversionFactors](https://r-lum.github.io/Luminescence/reference/BaseDataSet.md)
  for the accepted values.

## Value

The function returns an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object for which the first element is a
[matrix](https://rdrr.io/r/base/matrix.html) (`DataIndividual`) that
gives the dose rate results for each slice for each decay chain
individually, for both, the cobble dose rate and the sediment dose rate.
The second element is also a
[matrix](https://rdrr.io/r/base/matrix.html) (`DataComponent`) that
gives the total beta and gamma-dose rates for the cobble and the
adjacent sediment for each slice of the cobble.

## Details

**The input table layout**

|                   |             |                                                                                                                                        |
|-------------------|-------------|----------------------------------------------------------------------------------------------------------------------------------------|
| COLUMN            | DATA TYPE   | DESCRIPTION                                                                                                                            |
| `Distance`        | `numeric`   | distance from the surface of the cobble to the top of each rock slice in mm. The distance for each slice will be listed in this column |
| `DistanceError`   | `numeric`   | Error on the distance in mm                                                                                                            |
| `Thickness`       | `numeric`   | Thickness of each slice in mm                                                                                                          |
| `ThicknessError`  | `numeric`   | uncertainty of the thickness in mm.                                                                                                    |
| `Mineral`         | `character` | `'FS'` for feldspar, `'Q'` for quartz, depending which mineral in the cobble is used for dating                                        |
| `Cobble_K`        | `numeric`   | K nuclide content in % of the bulk cobble                                                                                              |
| `Cobble_K_SE`     | `numeric`   | error on K nuclide content in % of the bulk cobble                                                                                     |
| `Cobble_Th`       | `numeric`   | Th nuclide content in ppm of the bulk cobble                                                                                           |
| `Cobble_Th_SE`    | `numeric`   | error on Th nuclide content in ppm of the bulk cobble                                                                                  |
| `Cobble_U`        | `numeric`   | U nuclide content in ppm of the bulk cobble                                                                                            |
| `Cobble_U_SE`     | `numeric`   | error on U nuclide content in ppm of the bulk cobble                                                                                   |
| `GrainSize`       | `numeric`   | average grain size in µm of the grains used for dating                                                                                 |
| `Density`         | `numeric`   | Density of the cobble. Default is 2.7 g cm^-3                                                                                          |
| `CobbleDiameter`  | `numeric`   | Diameter of the cobble in cm.                                                                                                          |
| `Sed_K`           | `numeric`   | K nuclide content in % of the sediment matrix                                                                                          |
| `Sed_K_SE`        | `numeric`   | error on K nuclide content in % of the sediment matrix                                                                                 |
| `Sed_Th`          | `numeric`   | Th nuclide content in ppm of the sediment matrix                                                                                       |
| `Sed_Th_SE`       | `numeric`   | error on Th nuclide content in ppm of the sediment matrix                                                                              |
| `Sed_U`           | `numeric`   | U nuclide content in ppm of the sediment matrix                                                                                        |
| `Sed_U_SE`        | `numeric`   | error on U nuclide content in ppm of the sediment matrix                                                                               |
| `GrainSize_Sed`   | `numeric`   | average grain size of the sediment matrix                                                                                              |
| `Density_Sed`     | `numeric`   | average density of the sediment matrix                                                                                                 |
| `WaterContent`    | `numeric`   | mean water content of the sediment matrix in %                                                                                         |
| `WaterContent_SE` | `numeric`   | relative error on water content                                                                                                        |

**Water content** The water content provided by the user should be
calculated according to:

\$\$(Wet\\weight - Dry\\weight) / Dry\\weight \* 100\$\$

## Function version

0.1.0

## How to cite

Riedesel, S., Autzen, M., 2025. calc_CobbleDoseRate(): Calculate dose
rate of slices in a spherical cobble. Function version 0.1.0. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Riedesel, S., Autzen, M., 2020. Beta and gamma dose rate attenuation in
rocks and sediment. Radiation Measurements 133, 106295.

## See also

[convert_Concentration2DoseRate](https://r-lum.github.io/Luminescence/reference/convert_Concentration2DoseRate.md)

## Author

Svenja Riedesel, Aberystwyth University (United Kingdom)  
Martin Autzen, DTU NUTECH Center for Nuclear Technologies (Denmark) ,
RLum Developer Team

## Examples

``` r
## load example data
data("ExampleData.CobbleData", envir = environment())

## run function
calc_CobbleDoseRate(ExampleData.CobbleData)
#> 
#>  [RLum.Results-class]
#>   originator: calc_CobbleDoseRate()
#>   data: 3
#>       .. $DataIndividual : matrix
#>   .. $DataComponent : matrix
#>   .. $input : data.frame
#>   additional info elements:  1 
```
