# General setter function for RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the given class, the corresponding method to
create an object from this class will be selected.

## Usage

``` r
set_RLum(class, originator, .uid = create_UID(), .pid = NA_character_, ...)

# S4 method for class 'RLum.Analysis'
set_RLum(
  class,
  originator,
  .uid,
  .pid,
  protocol = NA_character_,
  records = list(),
  info = list()
)

# S4 method for class 'RLum.Data.Curve'
set_RLum(
  class,
  originator,
  .uid,
  .pid,
  recordType = NA_character_,
  curveType = NA_character_,
  data = matrix(0, ncol = 2),
  info = list()
)

# S4 method for class 'RLum.Data.Image'
set_RLum(
  class,
  originator,
  .uid,
  .pid,
  recordType = "Image",
  curveType = NA_character_,
  data = array(),
  info = list()
)

# S4 method for class 'RLum.Data.Spectrum'
set_RLum(
  class,
  originator,
  .uid,
  .pid,
  recordType = "Spectrum",
  curveType = NA_character_,
  data = matrix(),
  info = list()
)

# S4 method for class 'RLum.Results'
set_RLum(class, originator, .uid, .pid, data = list(), info = list())
```

## Arguments

- class:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of the S4 class to create, must correspond to one of the
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  classes.

- originator:

  [character](https://rdrr.io/r/base/character.html) (*automatic*):
  contains the name of the calling function (the function that produces
  this object); can be set manually.

- .uid:

  [character](https://rdrr.io/r/base/character.html) (*automatic*):
  unique ID for this object, by default set using the internal C++
  function `create_UID`.

- .pid:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  option to provide a parent id for nesting at will.

- ...:

  further arguments passed to the specific class method

- protocol:

  [character](https://rdrr.io/r/base/character.html) (*optional*): sets
  protocol type for analysis object. Value may be used by subsequent
  analysis functions.

- records:

  [list](https://rdrr.io/r/base/list.html) (*optional*): list of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects

- info:

  [list](https://rdrr.io/r/base/list.html) (*optional*): a list
  containing additional info data for the object.

- recordType:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  record type (e.g., "OSL")

- curveType:

  [character](https://rdrr.io/r/base/character.html) (*optional*): curve
  type (e.g., "predefined" or "measured")

- data:

  [matrix](https://rdrr.io/r/base/matrix.html) or
  [list](https://rdrr.io/r/base/list.html) (*with default*): a matrix
  containing raw curve data or a list containing the data to be stored
  in the object (for
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  objects) . If `data` itself is a `RLum.Data.Curve`-object this can be
  used to re-construct the object, i.e. modified parameters except
  `.uid`, `.pid` and `originator`. The rest will be subject to copy and
  paste unless provided.

## Value

An object of the specified
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
class.

## Functions

- `set_RLum(RLum.Analysis)`: Construction method for
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects.

- `set_RLum(RLum.Data.Curve)`: Construction method for
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  objects.

- `set_RLum(RLum.Data.Image)`: Construction method for
  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  objects.

- `set_RLum(RLum.Data.Spectrum)`: Construction method for
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  objects.

- `set_RLum(RLum.Results)`: Construction method for
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  objects.

## Function version

0.3.0

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

Kreutzer, S., 2025. set_RLum(): General setter function for RLum-class
objects. Function version 0.3.0. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## produce empty objects from each class
set_RLum(class = "RLum.Data.Curve")
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: NA
#>   curveType: NA
#>   measured values: 1
#>   .. range of x-values: 0 0
#>   .. range of y-values: 0 0 
#>   additional info elements: 0 
set_RLum(class = "RLum.Data.Spectrum")
#> 
#>  [RLum.Data.Spectrum-class]
#>   recordType: Spectrum
#>   curveType: NA
#>   .. recorded frames: 1
#>   .. .. measured values per frame: 1
#>   .. .. range wavelength/pixel: Inf -Inf
#>   .. .. range time/temp.: Inf -Inf
#>   .. .. range count values: NA NA
#>   additional info elements: 0
set_RLum(class = "RLum.Data.Spectrum")
#> 
#>  [RLum.Data.Spectrum-class]
#>   recordType: Spectrum
#>   curveType: NA
#>   .. recorded frames: 1
#>   .. .. measured values per frame: 1
#>   .. .. range wavelength/pixel: Inf -Inf
#>   .. .. range time/temp.: Inf -Inf
#>   .. .. range count values: NA NA
#>   additional info elements: 0
set_RLum(class = "RLum.Analysis")
#> 
#>  [RLum.Analysis-class]
#>   originator: eval()
#>   protocol: NA
#>   additional info elements:  0
#>   number of records: 0
#>   >> This is an empty object, which cannot be used for further analysis! <<
set_RLum(class = "RLum.Results")
#> 
#>  [RLum.Results-class]
#>   originator: eval()
#>   data: 0
#>       .. $ : list
#>   additional info elements:  0 

## produce a curve object with arbitrary curve values
object <- set_RLum(
class = "RLum.Data.Curve",
curveType = "arbitrary",
recordType = "OSL",
data = matrix(c(1:100,exp(-c(1:100))),ncol = 2))

## plot this curve object
plot_RLum(object)

```
