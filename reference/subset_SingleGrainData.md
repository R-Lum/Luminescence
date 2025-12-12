# Simple Subsetting of Single Grain Data from Risø BIN/BINX files

Most measured single grains do not exhibit light and it makes usually
sense to subset single grain datasets using a table of position and
grain pairs

## Usage

``` r
subset_SingleGrainData(object, selection)
```

## Arguments

- object:

  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  (**required**): input object with the data to subset

- selection:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  selection table with two columns for position (1st column) and grain
  (2nd column) (columns names do not matter)

## Value

A subset
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object

## Function version

0.1.0

## See also

[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[verify_SingleGrainData](https://r-lum.github.io/Luminescence/reference/verify_SingleGrainData.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. subset_SingleGrainData(): Simple Subsetting of
Single Grain Data from Risø BIN/BINX files. Function version 0.1.0. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data
data(ExampleData.BINfileData, envir = environment())

## set POSITION/GRAIN pair dataset
selection <- data.frame(POSITION = c(1,5,7), GRAIN = c(0,0,0))

##subset
subset_SingleGrainData(object = CWOSL.SAR.Data, selection = selection)
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     03
#>  Object date:          060920, 070920, 080920, 090920, 100920
#>  User:                 Default
#>  System ID:            0 (unknown)
#>  Overall records:      90
#>  Records type:         IRSL  (n = 3)
#>                        OSL   (n = 42)
#>                        TL    (n = 45)
#>  Position range:       1 : 7
#>  Run range:            1 : 8
#>  Set range:            2 : 6
```
