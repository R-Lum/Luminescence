# Example Lx/Tx data from CW-OSL SAR measurement

LxTx data from a SAR measurement for the package Luminescence.

## Format

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) with 4 columns
(Dose, LxTx, LxTx.Error, TnTx).

## Source

|           |                                                                               |
|-----------|-------------------------------------------------------------------------------|
| Lab:      | Luminescence Laboratory Bayreuth                                              |
| Lab-Code: | BT607                                                                         |
| Location: | Ostrau (Saxony-Anhalt/Germany)                                                |
| Material: | Middle grain (38-63 \\\mu\\m) quartz measured on a Risoe TL/OSL DA-15 reader. |

## References

unpublished data

## Examples

``` r
## plot Lx/Tx data vs dose [s]
data(ExampleData.LxTxData, envir = environment())
plot(LxTxData$Dose,LxTxData$LxTx)

```
