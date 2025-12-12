# Example equivalent dose data from mortar samples

Arbitrary data to test the function `calc_EED_Model`

## Format

Two [`data.frame`](https://rdrr.io/r/base/data.frame.html)s containing
De and De error

## Source

Arbitrary measurements.

## References

unpublished data

## Examples

``` r
##load data
data(ExampleData.MortarData, envir = environment())

##plot data
plot(MortarData)

```
