# Example portable OSL curve data for the package Luminescence

A `list` of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects, each containing the same number of
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
objects representing individual OSL, IRSL and dark count measurements of
a sample.

## Source

**ExampleData.portableOSL**

|            |                                 |
|------------|---------------------------------|
| Lab:       | Cologne Luminescence Laboratory |
| Lab-Code:  | `none`                          |
| Location:  | Nievenheim/Germany              |
| Material:  | Fine grain quartz               |
| Reference: | unpublished data                |

## Examples

``` r
data(ExampleData.portableOSL, envir = environment())
plot_RLum(ExampleData.portableOSL)














```
