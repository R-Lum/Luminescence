# Example CW-OSL curve data for the package Luminescence

`data.frame` containing CW-OSL curve data (time, counts)

## Format

Data frame with 1000 observations on the following 2 variables:

- `list("x")`: a numeric vector, time

- `list("y")`: a numeric vector, counts

## Source

**ExampleData.CW_OSL_Curve**

|            |                                                                               |
|------------|-------------------------------------------------------------------------------|
| Lab:       | Luminescence Laboratory Bayreuth                                              |
| Lab-Code:  | BT607                                                                         |
| Location:  | Saxony/Germany                                                                |
| Material:  | Middle grain quartz measured on aluminium cups on a Risø TL/OSL DA-15 reader. |
| Reference: | unpublished data                                                              |

**CW_Curve.BosWallinga2012**

|            |                                                  |
|------------|--------------------------------------------------|
| Lab:       | Netherlands Centre for Luminescence Dating (NCL) |
| Lab-Code:  | NCL-2108077                                      |
| Location:  | Guadalentin Basin, Spain                         |
| Material:  | Coarse grain quartz                              |
| Reference: | Bos & Wallinga (2012) and Baartman et al. (2011) |

## References

Baartman, J.E.M., Veldkamp, A., Schoorl, J.M., Wallinga, J., Cammeraat,
L.H., 2011. Unravelling Late Pleistocene and Holocene landscape
dynamics: The Upper Guadalentin Basin, SE Spain. Geomorphology, 125,
172-185.

Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL signal
components. Radiation Measurements, 47, 752-758.

## Examples

``` r
data(ExampleData.CW_OSL_Curve, envir = environment())
plot(ExampleData.CW_OSL_Curve)

```
