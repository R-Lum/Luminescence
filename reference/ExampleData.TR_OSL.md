# Example TR-OSL data

Single TR-OSL curve obtained by Schmidt et al. (2019) for quartz sample
BT729 (origin: Trebgast Valley, Germany, quartz, 90-200 µm, unpublished
data).

## Format

One
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
dataset imported using the function
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md)

## References

Schmidt, C., Simmank, O., Kreutzer, S., 2019. Time-Resolved Optically
Stimulated Luminescence of Quartz in the Nanosecond Time Domain. Journal
of Luminescence 213, 376-387.

## See also

[fit_OSLLifeTimes](https://r-lum.github.io/Luminescence/reference/fit_OSLLifeTimes.md)

## Examples

``` r
##(1) curves
data(ExampleData.TR_OSL, envir = environment())
plot_RLum(ExampleData.TR_OSL)

```
