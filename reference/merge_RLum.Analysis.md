# Merge function for RLum.Analysis S4 class objects

This function simply allows to merge
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects. Moreover, other
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
objects can be added to an existing
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object. Supported objects to be added are:
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
and
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md).

## Usage

``` r
merge_RLum.Analysis(objects)
```

## Arguments

- objects:

  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): list of S4 objects of class `RLum.Analysis`.
  Furthermore other objects of class
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  can be added, see details.

## Value

Returns an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object ordered according to the order provided with the input list.

## Note

The information for the slot 'protocol' is taken from the first
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object in the input list. Therefore at least one object of type
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
has to be provided.

## Function version

0.2.1

## See also

[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. merge_RLum.Analysis(): Merge function for
RLum.Analysis S4 class objects. Function version 0.2.1. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##merge different RLum objects from the example data
data(ExampleData.RLum.Analysis, envir = environment())
data(ExampleData.BINfileData, envir = environment())

object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
curve <- get_RLum(object)[[2]]

temp.merged <- merge_RLum.Analysis(list(curve, IRSAR.RF.Data, IRSAR.RF.Data))
```
