# Convenience data visualisation function

Invokes the [utils::View](https://rdrr.io/r/utils/View.html) function
tailored to objects in the package. If started from RStudio, it uses the
RStudio viewer.

## Usage

``` r
view(object, ...)

# S4 method for class 'RLum.Analysis'
view(object, ...)

# S4 method for class 'RLum.Data'
view(object, ...)

# S4 method for class 'RLum.Results'
view(object, element = 1, ...)

# S4 method for class 'Risoe.BINfileData'
view(object, ...)
```

## Arguments

- object:

  (**required**) object to view

- ...:

  further arguments passed to the specific class method

- element:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*): index
  of the element to display.

## Value

`NULL` and opens the data viewer.

## Functions

- `view(RLum.Analysis)`: View method for
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects.

- `view(RLum.Data)`: View method for
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects.

- `view(RLum.Results)`: View method for
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  objects.

- `view(Risoe.BINfileData)`: View method for
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects.

## See also

[`utils::View()`](https://rdrr.io/r/utils/View.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. view(): Convenience data visualisation function. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/
