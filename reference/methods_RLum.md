# methods_RLum

Methods for S3-generics implemented for the package 'Luminescence'. This
document summarises all implemented S3-generics. The name of the
function is given before the first dot, after the dot the name of the
object that is supported by this method is given, e.g.
`plot.RLum.Data.Curve` can be called by `plot(object, ...)`, where
`object` is the `RLum.Data.Curve` object.

## Usage

``` r
# S3 method for class 'list'
plot(x, y, ...)

# S3 method for class 'RLum.Results'
plot(x, y, ...)

# S3 method for class 'RLum.Analysis'
plot(x, y, ...)

# S3 method for class 'RLum.Data.Curve'
plot(x, y, ...)

# S3 method for class 'RLum.Data.Spectrum'
plot(x, y, ...)

# S3 method for class 'RLum.Data.Image'
plot(x, y, ...)

# S3 method for class 'Risoe.BINfileData'
plot(x, y, ...)

# S3 method for class 'RLum.Results'
hist(x, ...)

# S3 method for class 'RLum.Data.Image'
hist(x, ...)

# S3 method for class 'RLum.Data.Curve'
hist(x, ...)

# S3 method for class 'RLum.Analysis'
hist(x, ...)

# S3 method for class 'RLum.Results'
summary(object, ...)

# S3 method for class 'RLum.Analysis'
summary(object, ...)

# S3 method for class 'RLum.Data.Image'
summary(object, ...)

# S3 method for class 'RLum.Data.Curve'
summary(object, ...)

# S3 method for class 'Risoe.BINfileData'
subset(x, subset, records.rm = TRUE, ...)

# S3 method for class 'RLum.Analysis'
subset(x, subset = NULL, ...)

bin(x, ...)

# S3 method for class 'RLum.Data.Curve'
bin(x, bin_size = 2, ...)

# S3 method for class 'RLum.Data.Spectrum'
bin(x, bin_size.row = 1, bin_size.col = 1, ...)

# S3 method for class 'RLum.Results'
length(x, ...)

# S3 method for class 'RLum.Analysis'
length(x, ...)

# S3 method for class 'RLum.Data.Curve'
length(x, ...)

# S3 method for class 'Risoe.BINfileData'
length(x, ...)

# S3 method for class 'RLum.Data.Curve'
dim(x)

# S3 method for class 'RLum.Data.Spectrum'
dim(x)

# S3 method for class 'RLum'
rep(x, ...)

# S3 method for class 'RLum.Data.Curve'
names(x, ...)

# S3 method for class 'RLum.Data.Spectrum'
names(x, ...)

# S3 method for class 'RLum.Data.Image'
names(x, ...)

# S3 method for class 'RLum.Analysis'
names(x, ...)

# S3 method for class 'RLum.Results'
names(x, ...)

# S3 method for class 'Risoe.BINfileData'
names(x)

# S3 method for class 'RLum.Data.Spectrum'
row.names(x, ...)

# S3 method for class 'RLum.Data.Curve'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'RLum.Data.Spectrum'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'Risoe.BINfileData'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'RLum.Results'
as.list(x, ...)

# S3 method for class 'RLum.Data.Curve'
as.list(x, ...)

# S3 method for class 'RLum.Data.Image'
as.list(x, ...)

# S3 method for class 'RLum.Data.Spectrum'
as.list(x, ...)

# S3 method for class 'RLum.Analysis'
as.list(x, ...)

# S3 method for class 'RLum.Data.Curve'
as.matrix(x, ...)

# S3 method for class 'RLum.Data.Spectrum'
as.matrix(x, ...)

# S3 method for class 'RLum.Data.Image'
as.matrix(x, ...)

is.RLum(x, ...)

is.RLum.Data(x, ...)

is.RLum.Data.Curve(x, ...)

is.RLum.Data.Spectrum(x, ...)

is.RLum.Data.Image(x, ...)

is.RLum.Analysis(x, ...)

is.RLum.Results(x, ...)

# S3 method for class 'RLum'
merge(x, y, ...)

# S3 method for class 'RLum.Analysis'
unlist(x, recursive = TRUE, ...)

# S3 method for class 'RLum.Data.Curve'
x + y

# S3 method for class 'RLum.Data.Spectrum'
x + y

# S3 method for class 'RLum.Data.Curve'
x - y

# S3 method for class 'RLum.Data.Spectrum'
x - y

# S3 method for class 'RLum.Data.Curve'
x * y

# S3 method for class 'RLum.Data.Spectrum'
x * y

# S3 method for class 'RLum.Data.Curve'
x/y

# S3 method for class 'RLum.Data.Spectrum'
x/y

# S3 method for class 'RLum.Data.Curve'
x[y, z, drop = TRUE]

# S3 method for class 'RLum.Data.Spectrum'
x[y, z, drop = TRUE]

# S3 method for class 'RLum.Data.Image'
x[y, z, drop = TRUE]

# S3 method for class 'RLum.Analysis'
x[i, drop = FALSE]

# S3 method for class 'RLum.Results'
x[i, drop = TRUE]

# S3 method for class 'RLum.Data.Curve'
x[i, j] <- value

# S3 method for class 'RLum.Analysis'
x[[i]]

# S3 method for class 'RLum.Results'
x[[i]]

# S3 method for class 'RLum.Data.Curve'
x$i

# S3 method for class 'RLum.Analysis'
x$i

# S3 method for class 'RLum.Results'
x$i
```

## Arguments

- x:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  or
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  (**required**): input object

- y:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): the row
  index of the matrix, data.frame

- ...:

  further arguments that can be passed to the method

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): input object

- subset:

  `[subset]` [expression](https://rdrr.io/r/base/expression.html)
  (**required**): logical expression indicating elements or rows to
  keep, this function works in
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects like [subset.data.frame](https://rdrr.io/r/base/subset.html),
  but takes care of the object structure. Works also on
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects.

- records.rm:

  [subset](https://rdrr.io/r/base/subset.html)
  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether records should be removed from the data set. If `FALSE`, the
  `SEL` column marks the selected records.

- row.names:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable row names (`as.data.frame`).

- optional:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  logical. If TRUE, setting row names and converting column names (to
  syntactic names: see make.names) is optional (see
  [base::as.data.frame](https://rdrr.io/r/base/as.data.frame.html))

- recursive:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable further sub-setting (`unlist`).

- z:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): the
  column index of the matrix, data.frame

- drop:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): keep
  object structure or drop it

- i:

  [character](https://rdrr.io/r/base/character.html) (*optional*): name
  of the wanted record type or data object or row in the
  `RLum.Data.Curve` object

- j:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): column of
  the data matrix in the `RLum.Data.Curve` object

- value:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): numeric
  value which replace the value in the `RLum.Data.Curve` object

## Details

The term S3-generics sounds complicated, however, it just means that
something has been implemented in the package to increase the usability
for users new in R and who are not familiar with the underlying
`RLum`-object structure of the package. The practical outcome is that
operations and functions presented in standard books on R can be used
without knowing the specifics of the R package `'Luminescence'`. For
examples see the example section.

## Note

`methods_RLum` are not really new functions, everything given here are
mostly just surrogates for existing functions in the package.

## Examples

``` r
##load example data
data(ExampleData.RLum.Analysis, envir = environment())


##combine curve is various ways
curve1 <- IRSAR.RF.Data[[1]]
curve2 <-  IRSAR.RF.Data[[1]]
curve1 + curve2
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
#>   curveType: merged
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 2846.3 2875.6 
#>   additional info elements: 0 
curve1 - curve2
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
#>   curveType: merged
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 0 0 
#>   additional info elements: 0 
curve1 / curve2
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
#>   curveType: merged
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 1 1 
#>   additional info elements: 0 
curve1 * curve2
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
#>   curveType: merged
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 2025356 2067269 
#>   additional info elements: 0 


##`$` access curves
IRSAR.RF.Data$RF
#> [[1]]
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
#>   curveType: NA
#>   measured values: 5
#>   .. range of x-values: 0.1747448 6.311132
#>   .. range of y-values: 1423.15 1437.8 
#>   additional info elements: 0 
#> 
#> [[2]]
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: RF
#>   curveType: NA
#>   measured values: 524
#>   .. range of x-values: 0.3768403 715.4821
#>   .. range of y-values: 1379.947 2103.4 
#>   additional info elements: 0 
#> 
```
