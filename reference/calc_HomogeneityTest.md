# Apply a simple homogeneity test after Galbraith (2003)

A simple homogeneity test for De estimates. For details see Galbraith
(2003).

## Usage

``` r
calc_HomogeneityTest(data, log = TRUE, ...)
```

## Arguments

- data:

  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): for
  [data.frame](https://rdrr.io/r/base/data.frame.html): two columns with
  De `(data[,1])` and De error `(values[,2])`

- log:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  perform the homogeneity test with (un-)logged data

- ...:

  further arguments (`verbose`).

## Value

Returns a terminal output. In addition an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)-object
is returned containing the following elements:

- summary:

  [data.frame](https://rdrr.io/r/base/data.frame.html) summary of all
  relevant model results.

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) original input
  data

- args:

  [list](https://rdrr.io/r/base/list.html) used arguments

- call:

  [call](https://rdrr.io/r/base/call.html) the function call

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Function version

0.3.0

## How to cite

Burow, C., Kreutzer, S., 2025. calc_HomogeneityTest(): Apply a simple
homogeneity test after Galbraith (2003). Function version 0.3.0. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Galbraith, R.F., 2003. A simple homogeneity test for estimates of dose
obtained using OSL. Ancient TL 21, 75-77.

## See also

[stats::pchisq](https://rdrr.io/r/stats/Chisquare.html)

## Author

Christoph Burow, University of Cologne (Germany), Sebastian Kreutzer,
IRAMAT-CRP2A, Université Bordeaux Montaigne (France) , RLum Developer
Team

## Examples

``` r
## load example data
data(ExampleData.DeValues, envir = environment())

## apply the homogeneity test
calc_HomogeneityTest(ExampleData.DeValues$BT998)
#> 
#>  [calc_HomogeneityTest()]
#> 
#>  ---------------------------------
#>  n:                  25
#>  ---------------------------------
#>  mu:                 7.9812
#>  G-value:            155.5127
#>  Degrees of freedom: 24
#>  P-value:            0
#>  ---------------------------------
#> 
#> 
#>  [RLum.Results-class]
#>   originator: calc_HomogeneityTest()
#>   data: 3
#>       .. $summary : data.frame
#>   .. $data : data.frame
#>   .. $args : list
#>   additional info elements:  1 

## using the data presented by Galbraith (2003)
df <-
 data.frame(
   x = c(30.1, 53.8, 54.3, 29.0, 47.6, 44.2, 43.1),
   y = c(4.8, 7.1, 6.8, 4.3, 5.2, 5.9, 3.0))

calc_HomogeneityTest(df)
#> 
#>  [calc_HomogeneityTest()]
#> 
#>  ---------------------------------
#>  n:                  7
#>  ---------------------------------
#>  mu:                 3.7727
#>  G-value:            19.2505
#>  Degrees of freedom: 6
#>  P-value:            0.0038
#>  ---------------------------------
#> 
#> 
#>  [RLum.Results-class]
#>   originator: calc_HomogeneityTest()
#>   data: 3
#>       .. $summary : data.frame
#>   .. $data : data.frame
#>   .. $args : list
#>   additional info elements:  1 

```
