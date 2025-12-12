# General replication function for RLum-class objects

The function replicates
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects and returns a list of such objects.

## Usage

``` r
replicate_RLum(object, times = 1)

# S4 method for class 'RLum'
replicate_RLum(object, times = 1)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): an
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  object

- times:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): number
  for times each element should be repeated.

## Value

A [list](https://rdrr.io/r/base/list.html) with the object repeated.

## Functions

- `replicate_RLum(RLum)`: Replication method for
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects.

## Function version

0.1.0

## See also

[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. replicate_RLum(): General replication function for
RLum-class objects. Function version 0.1.0. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/
