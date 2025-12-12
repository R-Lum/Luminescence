# General merge function for RLum-class objects

The function provides a generalised access point for merging specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding merge function
will be selected. Allowed arguments can be found in the documentation of
each merge function. Empty list elements (`NULL`) are automatically
removed from the input list.

|                                                                                                  |     |                                                                                                        |
|--------------------------------------------------------------------------------------------------|-----|--------------------------------------------------------------------------------------------------------|
| **object**                                                                                       |     | **corresponding merge function**                                                                       |
| [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)       | -\> | [merge_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/merge_RLum.Data.Curve.md)       |
| [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md) | -\> | [merge_RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/merge_RLum.Data.Spectrum.md) |
| [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)           | -\> | [merge_RLum.Analysis](https://r-lum.github.io/Luminescence/reference/merge_RLum.Analysis.md)           |
| [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)             | -\> | [merge_RLum.Results](https://r-lum.github.io/Luminescence/reference/merge_RLum.Results.md)             |

## Usage

``` r
merge_RLum(objects, ...)
```

## Arguments

- objects:

  [list](https://rdrr.io/r/base/list.html) of
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): list of S4 object of class `RLum`.

- ...:

  further arguments that one might want to pass to the specific merge
  function

## Value

Returns an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object of class if any of the inputs is of that class. Otherwise, it
returns an object of the same type as the input.

## Note

So far merging of
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
objects is not supported.

## Function version

0.1.3

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. merge_RLum(): General merge function for RLum-class
objects. Function version 0.1.3. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##Example based using data and from the calc_CentralDose() function

##load example data
data(ExampleData.DeValues, envir = environment())

##apply the central dose model 1st time
temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)
#> 
#>  [calc_CentralDose]
#> 
#> ----------- meta data ----------------
#>  n:                       62
#>  log:                     TRUE
#> ----------- dose estimate ------------
#>  abs. central dose:       65.71
#>  abs. SE:                 3.05
#>  rel. SE [%]:             4.65
#> ----------- overdispersion -----------
#>  abs. OD:                 22.79
#>  abs. SE:                 2.27
#>  OD [%]:                  34.69
#>  SE [%]:                  3.46
#> -------------------------------------
#> 


##apply the central dose model 2nd time
temp2 <- calc_CentralDose(ExampleData.DeValues$CA1)
#> 
#>  [calc_CentralDose]
#> 
#> ----------- meta data ----------------
#>  n:                       62
#>  log:                     TRUE
#> ----------- dose estimate ------------
#>  abs. central dose:       65.71
#>  abs. SE:                 3.05
#>  rel. SE [%]:             4.65
#> ----------- overdispersion -----------
#>  abs. OD:                 22.79
#>  abs. SE:                 2.27
#>  OD [%]:                  34.69
#>  SE [%]:                  3.46
#> -------------------------------------
#> 

##merge the results and store them in a new object
temp.merged <- get_RLum(merge_RLum(objects = list(temp1, temp2)))
```
