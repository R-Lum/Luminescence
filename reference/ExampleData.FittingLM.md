# Example data for fit_LMCurve() in the package Luminescence

Linearly modulated (LM) measurement data from a quartz sample from
Norway including background measurement. Measurements carried out in the
luminescence laboratory at the University of Bayreuth.

## Format

Two objects (data.frames) with two columns (time and counts).

## Source

|           |                                                                                              |
|-----------|----------------------------------------------------------------------------------------------|
| Lab:      | Luminescence Laboratory Bayreuth                                                             |
| Lab-Code: | BT900                                                                                        |
| Location: | Norway                                                                                       |
| Material: | Beach deposit, coarse grain quartz measured on aluminium discs on a Risø TL/OSL DA-15 reader |

## References

Fuchs, M., Kreutzer, S., Fischer, M., Sauer, D., Soerensen, R., 2012.
OSL and IRSL dating of raised beach sand deposits along the
south-eastern coast of Norway. Quaternary Geochronology, 10, 195-200.

## Examples

``` r
##show LM data
data(ExampleData.FittingLM, envir = environment())
plot(values.curve,log="x")

```
