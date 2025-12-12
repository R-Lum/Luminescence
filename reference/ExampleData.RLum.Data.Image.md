# Example data as [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md) objects

Measurement of Princton Instruments camera imported with the function
[read_SPE2R](https://r-lum.github.io/Luminescence/reference/read_SPE2R.md)
to R to produce an
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
object.

## Format

Object of class
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)

## Source

**ExampleData.RLum.Data.Image**

These data were kindly provided by Regina DeWitt.

|            |                                                          |
|------------|----------------------------------------------------------|
| Lab.:      | Department of Physics, East-Carolina University, NC, USA |
| Lab-Code:  | \-                                                       |
| Location:  | \-                                                       |
| Material:  | \-                                                       |
| Reference: | \-                                                       |

Image data is a measurement of fluorescent ceiling lights with a cooled
Princeton Instruments (TM) camera fitted on Risø DA-20 TL/OSL reader.

## Version

0.1

## Examples

``` r
##load data
data(ExampleData.RLum.Data.Image, envir = environment())

##plot data
plot_RLum(ExampleData.RLum.Data.Image)

```
