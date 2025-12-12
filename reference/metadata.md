# Safe manipulation of object metadata

Generic functions for manipulation of metadata in
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
and
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
objects.

## Usage

``` r
add_metadata(object, ...) <- value

rename_metadata(object, ...) <- value

replace_metadata(object, ...) <- value

# S4 method for class 'RLum.Analysis'
add_metadata(object, info_element) <- value

# S4 method for class 'RLum.Analysis'
rename_metadata(object, info_element) <- value

# S4 method for class 'RLum.Analysis'
replace_metadata(object, info_element, subset = NULL) <- value

# S4 method for class 'RLum.Data'
add_metadata(object, info_element) <- value

# S4 method for class 'RLum.Data'
rename_metadata(object, info_element) <- value

# S4 method for class 'RLum.Data'
replace_metadata(object, info_element, subset = NULL, verbose = TRUE) <- value

# S4 method for class 'Risoe.BINfileData'
add_metadata(object, info_element) <- value

# S4 method for class 'Risoe.BINfileData'
rename_metadata(object, info_element) <- value

# S4 method for class 'Risoe.BINfileData'
replace_metadata(object, info_element, subset = NULL) <- value
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  (**required**): object of class `RLum.Analysis` or `Risoe.BINfileData`
  to manipulate.

- ...:

  further arguments passed to the specific class method

- value:

  (**required**): value to be assigned to the selected metadata entry. A
  `NULL` value is acceptable only for `replace_metadata`, in which case
  the elements named in `info_element` will be removed.

- info_element:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of the metadata entry to manipulate.

- subset:

  [expression](https://rdrr.io/r/base/expression.html) (*optional*):
  logical expression to limit the substitution only to the selected
  subset of elements.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

## Functions

- `add_metadata(RLum.Analysis) <- value`: Adds metadata to
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects.

- `rename_metadata(RLum.Analysis) <- value`: Renames a metadata entry of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects.

- `replace_metadata(RLum.Analysis) <- value`: Replaces or removes
  metadata of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects.

- `add_metadata(RLum.Data) <- value`: Add metadata entries to
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects.

- `rename_metadata(RLum.Data) <- value`: Rename a metadata entry of
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects.

- `replace_metadata(RLum.Data) <- value`: Replaces or removes metadata
  of
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects.

- `add_metadata(Risoe.BINfileData) <- value`: Adds metadata to
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects.

- `rename_metadata(Risoe.BINfileData) <- value`: Renames a metadata
  entry of
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects.

- `replace_metadata(Risoe.BINfileData) <- value`: Replaces or removes
  metadata of
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects.

## See also

[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)

## Author

Marco Colombo, Institute of Geography, Heidelberg University (Germany) ,
RLum Developer Team

## How to cite

Colombo, M., 2025. add_metadata\<-(): Safe manipulation of object
metadata. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data
data(ExampleData.BINfileData, envir = environment())

## show data
CWOSL.SAR.Data
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     03
#>  Object date:          060920, 070920, 080920, 090920, 100920
#>  User:                 Default
#>  System ID:            0 (unknown)
#>  Overall records:      720
#>  Records type:         IRSL  (n = 24)
#>                        OSL   (n = 336)
#>                        TL    (n = 360)
#>  Position range:       1 : 24
#>  Run range:            1 : 8
#>  Set range:            2 : 6

## add a new field
add_metadata(CWOSL.SAR.Data,
             info_element = "INSTITUTE") <- "Heidelberg University"

## rename a field
rename_metadata(CWOSL.SAR.Data,
                info_element = "INSTITUTE") <- "INSTITUTION"

## replace all LTYPE to RSL
## but only for the first position
replace_metadata(
 object = CWOSL.SAR.Data,
 info_element = "LTYPE",
 subset = (POSITION == 1)) <- "RSL"

## replacing a field with NULL allows to remove that field
replace_metadata(CWOSL.SAR.Data,
                 info_element = "PREVIOUS") <- NULL

## show the modified data
CWOSL.SAR.Data
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     03
#>  Object date:          060920, 070920, 080920, 090920, 100920
#>  User:                 Default
#>  System ID:            0 (unknown)
#>  Overall records:      720
#>  Records type:         IRSL  (n = 23)
#>                        OSL   (n = 322)
#>                        RSL   (n = 30)
#>                        TL    (n = 345)
#>  Position range:       1 : 24
#>  Run range:            1 : 8
#>  Set range:            2 : 6
```
