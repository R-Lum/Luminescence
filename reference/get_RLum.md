# General accessor function for RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding function will
be selected.

## Usage

``` r
get_RLum(object, ...)

# S4 method for class 'list'
get_RLum(object, class = NULL, null.rm = FALSE, ...)

# S4 method for class 'NULL'
get_RLum(object, ...)

# S4 method for class 'RLum.Analysis'
get_RLum(
  object,
  record.id = NULL,
  recordType = NULL,
  curveType = NULL,
  RLum.type = NULL,
  protocol = "UNKNOWN",
  get.index = FALSE,
  drop = TRUE,
  recursive = TRUE,
  info.object = NULL,
  subset = NULL,
  env = parent.frame(2)
)

# S4 method for class 'RLum.Data.Curve'
get_RLum(object, info.object = NULL)

# S4 method for class 'RLum.Data.Image'
get_RLum(object, info.object = NULL)

# S4 method for class 'RLum.Data.Spectrum'
get_RLum(object, info.object = NULL)

# S4 method for class 'RLum.Results'
get_RLum(object, data.object, info.object = NULL, drop = TRUE)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum` or an object of type
  [list](https://rdrr.io/r/base/list.html) containing only objects of
  type
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)

- ...:

  further arguments passed to the specific class method.

- class:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  define which class gets selected if applied to a list, e.g., if a list
  consists of different type of
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects, this arguments allows to make a selection. If nothing is
  provided, all
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects are treated.

- null.rm:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether empty and `NULL` objects should be removed.

- record.id:

  [numeric](https://rdrr.io/r/base/numeric.html) or
  [logical](https://rdrr.io/r/base/logical.html) (*optional*): IDs of
  specific records. If of type `logical` the entire id range is assumed
  and `TRUE` and `FALSE` indicates the selection.

- recordType:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  record type (e.g., "OSL"). Can be also a vector, for multiple
  matching, e.g., `recordType = c("OSL", "IRSL")`

- curveType:

  [character](https://rdrr.io/r/base/character.html) (*optional*): curve
  type (e.g. "predefined" or "measured")

- RLum.type:

  [character](https://rdrr.io/r/base/character.html) (*optional*): RLum
  object type. Defaults to "RLum.Data.Curve" and "RLum.Data.Spectrum".

- protocol:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  currently ignored.

- get.index:

  [logical](https://rdrr.io/r/base/logical.html) (*optional*): return a
  numeric vector with the index of each element in the RLum.Analysis
  object (`FALSE` by default).

- drop:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  coerce to the next possible layer (which are
  [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
  objects if `object` is an
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object). If `drop = FALSE`, an object of the same type as the input is
  returned.

- recursive:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  `TRUE` (default) when the result of the `get_RLum()` request is a
  single object, the object itself will be returned directly, rather
  than being wrapped in a list. Mostly this makes things easier, but
  this might be undesired if this method is used within a loop.

- info.object:

  [character](https://rdrr.io/r/base/character.html) (*optional*): name
  of the wanted info element.

- subset:

  [expression](https://rdrr.io/r/base/expression.html) (*optional*):
  logical or character masking a logical expression indicating elements
  or rows to keep: missing values are taken as false. This argument
  takes precedence over all other arguments, meaning they are not
  considered when subsetting the object. `subset` works slots and info
  elements.

- env:

  [environment](https://rdrr.io/r/base/environment.html) (*with
  default*): An environment passed to
  [eval](https://rdrr.io/r/base/eval.html) as the enclosure. This
  argument is only relevant when subsetting the object and should not be
  used manually.

- data.object:

  [character](https://rdrr.io/r/base/character.html) or
  [numeric](https://rdrr.io/r/base/numeric.html): name or index of the
  data slot to be returned.

## Value

An object of the same type as the input object provided.

## Functions

- `get_RLum(list)`: Returns a list of
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  objects that had been passed to get_RLum

- `` get_RLum(`NULL`) ``: Returns `NULL`.

- `get_RLum(RLum.Analysis)`: Accessor method for RLum.Analysis objects.
  The optional arguments `record.id`, `recordType`, `curveType` and
  `RLum.type` allow to limit records by their id (list index number),
  their record type (e.g. `recordType = "OSL"`), their curve type (e.g.
  `curveType = "predefined"` or `curveType ="measured"`), or object
  type.

  The selection of a specific RLum.type object superimposes the default
  selection. Currently supported objects are: RLum.Data.Curve and
  RLum.Data.Spectrum

  Returns:

  1.  [list](https://rdrr.io/r/base/list.html) of
      [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
      objects or

  2.  Single
      [RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)
      object, if only one object is contained and `recursive = FALSE` or

  3.  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
      objects for `drop = FALSE`

- `get_RLum(RLum.Data.Curve)`: Accessor method for
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  object. The argument `info.object` is optional to directly access the
  info elements. If no info element name is provided, the raw curve data
  (matrix) will be returned.

- `get_RLum(RLum.Data.Image)`: Accessor method for
  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  objects. The argument `info.object` is optional to directly access the
  info elements. If no info element name is provided, the raw image data
  (`array`) will be returned.

- `get_RLum(RLum.Data.Spectrum)`: Accessor method for
  [RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
  objects. The argument `info.object` is optional to directly access the
  info elements. If no info element name is provided, the raw curve data
  (matrix) will be returned.

- `get_RLum(RLum.Results)`: Accessor method for
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  object. The argument `data.object` allows to access directly objects
  stored within the slot data. The default return object depends on the
  object originator (e.g., `fit_LMCurve`). If nothing is specified
  always the first `data.object` will be returned.

  Note: Detailed specification should be made in combination with the
  originator slot in the receiving function if results are piped.

  Returns:

  1.  Data object from the specified slot

  2.  [list](https://rdrr.io/r/base/list.html) of data objects from the
      slots if 'data.object' is vector or

  3.  an
      [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
      for `drop = FALSE`.

## Function version

0.3.3

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

Kreutzer, S., 2025. get_RLum(): General accessor function for RLum-class
objects. Function version 0.3.3. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## Example based using data and from the calc_CentralDose() function

## load example data
data(ExampleData.DeValues, envir = environment())

## apply the central dose model 1st time
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


## get results and store them in a new object
temp.get <- get_RLum(object = temp1)
```
