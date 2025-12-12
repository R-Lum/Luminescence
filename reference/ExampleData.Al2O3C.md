# Example Al2O3:C Measurement Data

Measurement data obtained from measuring Al2O3:C chips at the
IRAMAT-CRP2A, Université Bordeaux Montaigne in 2017 on a Freiberg
Instruments lexsyg SMART reader. The example data used in particular to
allow test of the functions developed in framework of the work by
Kreutzer et al., 2018.

## Format

Two datasets comprising
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
data imported using the function
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md)

## Note

From both datasets unneeded curves have been removed and the number of
aliquots have been reduced to a required minimum to keep the file size
small, but still being able to run the corresponding functions.

## References

Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier,
N., 2018. Environmental Dose Rate Determination Using a Passive
Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
Geochronometria 45, 56–67.

## See also

[analyse_Al2O3C_ITC](https://r-lum.github.io/Luminescence/reference/analyse_Al2O3C_ITC.md),
[analyse_Al2O3C_CrossTalk](https://r-lum.github.io/Luminescence/reference/analyse_Al2O3C_CrossTalk.md),
[analyse_Al2O3C_Measurement](https://r-lum.github.io/Luminescence/reference/analyse_Al2O3C_Measurement.md)

## Examples

``` r
##(1) curves
data(ExampleData.Al2O3C, envir = environment())
plot_RLum(data_ITC[1:2])

```
