# Example Lx and Tx curve data from an artificial OSL measurement

`Lx` and `Tx` data of continuous wave (CW-) OSL signal curves.

## Format

Two [`data.frame`](https://rdrr.io/r/base/data.frame.html)s containing
time and count values.

## Source

Arbitrary OSL measurement.

## References

unpublished data

## Examples

``` r
##load data
data(ExampleData.LxTxOSLData, envir = environment())

##plot data
plot(Lx.data)

plot(Tx.data)

```
