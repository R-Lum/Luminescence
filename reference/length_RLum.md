# Length retrieval function for RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding function will
be selected.

## Usage

``` r
length_RLum(object)

# S4 method for class 'RLum.Analysis'
length_RLum(object)

# S4 method for class 'RLum.Data.Curve'
length_RLum(object)

# S4 method for class 'RLum.Results'
length_RLum(object)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum`

## Value

An [integer](https://rdrr.io/r/base/integer.html) indicating the length
of the object.

## Functions

- `length_RLum(RLum.Analysis)`: Returns the number of records stored in
  the object.

- `length_RLum(RLum.Data.Curve)`: Returns the number of channels in the
  curve, which is the maximum of the value time/temperature of the curve
  (corresponding to the stimulation length).

- `length_RLum(RLum.Results)`: Returns the number of stored data
  elements.

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

Kreutzer, S., 2025. length_RLum(): Length retrieval function for
RLum-class objects. Function version 0.1.0. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/
