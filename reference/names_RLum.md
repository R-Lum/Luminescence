# Name retrieval function for RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding function will
be selected.

## Usage

``` r
names_RLum(object)

# S4 method for class 'list'
names_RLum(object)

# S4 method for class 'RLum.Analysis'
names_RLum(object)

# S4 method for class 'RLum.Data.Curve'
names_RLum(object)

# S4 method for class 'RLum.Data.Image'
names_RLum(object)

# S4 method for class 'RLum.Data.Spectrum'
names_RLum(object)

# S4 method for class 'RLum.Results'
names_RLum(object)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum`

## Value

A [character](https://rdrr.io/r/base/character.html) vector.

## Functions

- `names_RLum(list)`: Returns a list of names of the
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects that had been passed to it.

- `names_RLum(RLum.Analysis)`: Returns the names of the
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects stored in the object.

- `names_RLum(RLum.Data.Curve)`: Returns the names info elements stored
  in the object.

- `names_RLum(RLum.Data.Image)`: Returns the names of the info elements
  stored in the object.

- `names_RLum(RLum.Data.Spectrum)`: Returns the names of the info
  elements stored in the object.

- `names_RLum(RLum.Results)`: Returns the names of the `data` field
  stored in the object.

## Function version

0.1.0

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. names_RLum(): Name retrieval function for RLum-class
objects. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/
