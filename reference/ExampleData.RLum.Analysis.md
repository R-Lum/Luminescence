# Example data as [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md) objects

Collection of different
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects for protocol analysis.

## Format

`IRSAR.RF.Data`: IRSAR.RF.Data on coarse grain feldspar

Each object contains data needed for the given protocol analysis.

## Source

**IRSAR.RF.Data**

These data were kindly provided by Tobias Lauer and Matthias Krbetschek.

|            |                                                     |
|------------|-----------------------------------------------------|
| Lab:       | Luminescence Laboratory TU Bergakademie Freiberg    |
| Lab-Code:  | ZEU/SA1                                             |
| Location:  | Zeuchfeld (Zeuchfeld Sandur; Saxony-Anhalt/Germany) |
| Material:  | K-feldspar (130-200 \\\mu\\m)                       |
| Reference: | Kreutzer et al. (2014)                              |

## Version

0.1

## References

**IRSAR.RF.Data**

Kreutzer, S., Lauer, T., Meszner, S., Krbetschek, M.R., Faust, D.,
Fuchs, M., 2014. Chronology of the Quaternary profile Zeuchfeld in
Saxony-Anhalt / Germany - a preliminary luminescence dating study.
Zeitschrift fuer Geomorphologie 58, 5-26. doi:
10.1127/0372-8854/2012/S-00112

## Examples

``` r
##load data
data(ExampleData.RLum.Analysis, envir = environment())

##plot data
plot_RLum(IRSAR.RF.Data)

```
