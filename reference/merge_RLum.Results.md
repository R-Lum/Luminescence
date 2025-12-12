# Merge function for RLum.Results S4-class objects

Function merges objects of class
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md).
The slots in the objects are combined depending on the object type,
e.g., for [data.frame](https://rdrr.io/r/base/data.frame.html) and
[matrix](https://rdrr.io/r/base/matrix.html) rows are appended.

## Usage

``` r
merge_RLum.Results(objects)
```

## Arguments

- objects:

  [list](https://rdrr.io/r/base/list.html) (**required**): a list of
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  objects

## Details

Elements are appended where possible and attributes are preserved if not
of similar name as the default attributes of, e.g., a
[data.frame](https://rdrr.io/r/base/data.frame.html)

## Note

The `originator` is taken from the first element and not reset to
`merge_RLum`

## Function version

0.2.1

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. merge_RLum.Results(): Merge function for
RLum.Results S4-class objects. Function version 0.2.1. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/
