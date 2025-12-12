# Transform a CW-OSL curve into a pPM-OSL curve via interpolation under parabolic modulation conditions

Transforms a conventionally measured continuous-wave (CW) OSL-curve into
a pseudo parabolic modulated (pPM) curve under parabolic modulation
conditions using the interpolation procedure described by Bos & Wallinga
(2012).

## Usage

``` r
convert_CW2pPMi(values, P)
```

## Arguments

- values:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**):
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or `data.frame` with measured curve data of type stimulation time (t)
  (`values[,1]`) and measured counts (cts) (`values[,2]`)

- P:

  [vector](https://rdrr.io/r/base/vector.html) (*optional*): stimulation
  period in seconds. If no value is given, the optimal value is
  estimated automatically (see details). Greater values of P produce
  more points in the rising tail of the curve.

## Value

The function returns the same data type as the input data type with the
transformed curve values.

`RLum.Data.Curve`

|                   |                                                         |
|-------------------|---------------------------------------------------------|
| `$CW2pPMi.x.t`    | : transformed time values                               |
| `$CW2pPMi.method` | : used method for the production of the new data points |

`data.frame`

|           |                                                         |
|-----------|---------------------------------------------------------|
| `$x`      | : time                                                  |
| `$y.t`    | : transformed count values                              |
| `$x.t`    | : transformed time values                               |
| `$method` | : used method for the production of the new data points |

## Details

The complete procedure of the transformation is given in Bos & Wallinga
(2012). The input `data.frame` consists of two columns: time (t) and
count values (CW(t))

**Nomenclature**

- P = stimulation time (s)

- 1/P = stimulation rate (1/s)

**Internal transformation steps**

\(1\) log(CW-OSL) values

\(2\) Calculate t' which is the transformed time: \$\$t' =
(1/3)\*(1/P^2)t^3\$\$

\(3\) Interpolate CW(t'), i.e. use the log(CW(t)) to obtain the count
values for the transformed time (t'). Values beyond `min(t)` and
`max(t)` produce `NA` values.

\(4\) Select all values for t' \< `min(t)`, i.e. values beyond the time
resolution of t. Select the first two values of the transformed data set
which contain no `NA` values and use these values for a linear fit using
[lm](https://rdrr.io/r/stats/lm.html).

\(5\) Extrapolate values for t' \< `min(t)` based on the previously
obtained fit parameters. The extrapolation is limited to two values.
Other values at the beginning of the transformed curve are set to 0.

\(6\) Transform values using \$\$pLM(t) = t^2/P^2\*CW(t')\$\$

\(7\) Combine all values and truncate all values for t' \> `max(t)`

**NOTE:** The number of values for t' \< `min(t)` depends on the
stimulation period `P`. To avoid the production of too many artificial
data at the raising tail of the determined pPM curve, it is recommended
to use the automatic estimation routine for `P`, i.e. provide no value
for `P`.

## Note

According to Bos & Wallinga (2012), the number of extrapolated points
should be limited to avoid artificial intensity data. If `P` is provided
manually, not more than two points are extrapolated.

## Function version

0.2.2

## How to cite

Kreutzer, S., 2025. convert_CW2pPMi(): Transform a CW-OSL curve into a
pPM-OSL curve via interpolation under parabolic modulation conditions.
Function version 0.2.2. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL signal
components. Radiation Measurements, 47, 752-758.

**Further Reading**

Bulur, E., 1996. An Alternative Technique For Optically Stimulated
Luminescence (OSL) Experiment. Radiation Measurements, 26, 701-709.

Bulur, E., 2000. A simple transformation for converting CW-OSL curves to
LM-OSL curves. Radiation Measurements, 32, 141-145.

## See also

[convert_CW2pLM](https://r-lum.github.io/Luminescence/reference/convert_CW2pLM.md),
[convert_CW2pLMi](https://r-lum.github.io/Luminescence/reference/convert_CW2pLMi.md),
[convert_CW2pHMi](https://r-lum.github.io/Luminescence/reference/convert_CW2pHMi.md),
[fit_LMCurve](https://r-lum.github.io/Luminescence/reference/fit_LMCurve.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)

Based on comments and suggestions from:  
Adrie J.J. Bos, Delft University of Technology, The Netherlands , RLum
Developer Team

## Examples

``` r

##(1)
##load CW-OSL curve data
data(ExampleData.CW_OSL_Curve, envir = environment())

##transform values
values.transformed <- convert_CW2pPMi(ExampleData.CW_OSL_Curve)
#> Warning: [convert_CW2pPMi()] t' is beyond the time resolution: only two data points have been extrapolated, the first 0 points were set to 0

##plot
plot(values.transformed$x,values.transformed$y.t, log = "x")


##(2) - produce Fig. 4 from Bos & Wallinga (2012)

##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

##open plot area
plot(NA, NA,
     xlim = c(0.001,10),
     ylim = c(0,8000),
     ylab = "pseudo OSL (cts/0.01 s)",
     xlab = "t [s]",
     log = "x",
     main = "Fig. 4 - Bos & Wallinga (2012)")

values.t <- convert_CW2pLMi(values, P = 1/20)
lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
      col = "red",lwd = 1.3)
text(0.03,4500,"LM", col = "red", cex = .8)

values.t <- convert_CW2pHMi(values, delta = 40)
#> Warning: [convert_CW2pHMi()] 56 invalid values have been found and replaced by the mean of the nearest values
lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
      col = "black", lwd = 1.3)
text(0.005,3000,"HM", cex = .8)

values.t <- convert_CW2pPMi(values, P = 1/10)
#> Warning: [convert_CW2pPMi()] t' is beyond the time resolution: only two data points have been extrapolated, the first 3 points were set to 0
lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
      col = "blue", lwd = 1.3)
text(0.5,6500,"PM", col = "blue", cex = .8)

```
