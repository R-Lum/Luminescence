# Transform a CW-OSL curve into a pLM-OSL curve

Transforms a conventionally measured continuous-wave (CW) curve into a
pseudo linearly modulated (pLM) curve using the equations given in Bulur
(2000).

## Usage

``` r
convert_CW2pLM(values)
```

## Arguments

- values:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): `RLum.Data.Curve` data object. Alternatively, a
  `data.frame` of the measured curve data of type stimulation time (t)
  (`values[,1]`) and measured counts (cts) (`values[,2]`) can be
  provided.

## Value

The function returns the same data type as the input data type with the
transformed curve values
([data.frame](https://rdrr.io/r/base/data.frame.html) or
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)).

## Details

According to Bulur (2000) the curve data are transformed by introducing
two new parameters `P` (stimulation period) and `u` (transformed time):

\$\$P=2\*max(t)\$\$ \$\$u=\sqrt{(2\*t\*P)}\$\$

The new count values are then calculated by \$\$ctsNEW = cts(u/P)\$\$

and the returned `data.frame` is produced by: `data.frame(u,ctsNEW)`

The output of the function can be further used for LM-OSL fitting.

## Note

The transformation is recommended for curves recorded with a channel
resolution of at least 0.05 s/channel.

## Function version

0.4.2

## How to cite

Kreutzer, S., 2025. convert_CW2pLM(): Transform a CW-OSL curve into a
pLM-OSL curve. Function version 0.4.2. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Bulur, E., 2000. A simple transformation for converting CW-OSL curves to
LM-OSL curves. Radiation Measurements, 32, 141-145.

**Further Reading**

Bulur, E., 1996. An Alternative Technique For Optically Stimulated
Luminescence (OSL) Experiment. Radiation Measurements, 26, 701-709.

## See also

[convert_CW2pHMi](https://r-lum.github.io/Luminescence/reference/convert_CW2pHMi.md),
[convert_CW2pLMi](https://r-lum.github.io/Luminescence/reference/convert_CW2pLMi.md),
[convert_CW2pPMi](https://r-lum.github.io/Luminescence/reference/convert_CW2pPMi.md),
[fit_LMCurve](https://r-lum.github.io/Luminescence/reference/fit_LMCurve.md),
[lm](https://rdrr.io/r/stats/lm.html),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##read curve from CWOSL.SAR.Data transform curve and plot values
data(ExampleData.BINfileData, envir = environment())

##read id for the 1st OSL curve
id.OSL <- CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"LTYPE"] == "OSL","ID"]

##produce x and y (time and count data for the data set)
x<-seq(CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@METADATA[id.OSL[1],"NPOINTS"],
       CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"],
       by = CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@METADATA[id.OSL[1],"NPOINTS"])
y <- unlist(CWOSL.SAR.Data@DATA[id.OSL[1]])
values <- data.frame(x,y)

##transform values
values.transformed <- convert_CW2pLM(values)

##plot
plot(values.transformed)

```
