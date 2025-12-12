# Calculate the cosmic dose rate

This function calculates the cosmic dose rate taking into account the
soft- and hard-component of the cosmic ray flux and allows corrections
for geomagnetic latitude, altitude above sea-level and geomagnetic field
changes.

This function calculates the total cosmic dose rate considering both the
soft- and hard-component of the cosmic ray flux.

**Internal calculation steps**

\(1\) Calculate total depth of all absorber in hg/cm² (1 hg/cm² = 100
g/cm²)

\$\$absorber = depth_1\*density_1 + depth_2\*density_2 + ... +
depth_n\*density_n\$\$

\(2\) If `half.depth = TRUE`

\$\$absorber = absorber/2\$\$

\(3\) Calculate cosmic dose rate at sea-level and 55 deg. latitude

a\) If absorber is \> 167 g/cm² (only hard-component; Allkofer et al.
1975): apply equation given by Prescott & Hutton (1994) (c.f. Barbouti &
Rastin 1983)

\$\$D0 = C/(((absorber+d)^\alpha+a)\*(absober+H))\*exp(-B\*absorber)\$\$

b\) If absorber is \< 167 g/cm² (soft- and hard-component): derive D0
from Fig. 1 in Prescott & Hutton (1988).

\(4\) Calculate geomagnetic latitude (Prescott & Stephan 1982, Prescott
& Hutton 1994)

\$\$\lambda = arcsin(0.203\*cos(latitude)\*cos(longitude-291)+0.979\*
sin(latitude))\$\$

\(5\) Apply correction for geomagnetic latitude and altitude above
sea-level. Values for F, J and H were read from Fig. 3 shown in Prescott
& Stephan (1982) and fitted with 3-degree polynomials for lambda \< 35
degree and a linear fit for lambda \> 35 degree.

\$\$Dc = D0\*(F+J\*exp((altitude/1000)/H))\$\$

\(6\) Optional: Apply correction for geomagnetic field changes in the
last 0-80 ka (Prescott & Hutton 1994). Correction and altitude factors
are given in Table 1 and Fig. 1 in Prescott & Hutton (1994). Values for
altitude factor were fitted with a 2-degree polynomial. The altitude
factor is operated on the decimal part of the correction factor.

\$\$Dc' = Dc\*correctionFactor\$\$

**Usage of `depth` and `density`**

\(1\) If only one value for depth and density is provided, the cosmic
dose rate is calculated for exactly one sample and one absorber as
overburden (i.e. `depth*density`).

\(2\) In some cases it might be useful to calculate the cosmic dose rate
for a sample that is overlain by more than one absorber, e.g. in a
profile with soil layers of different thickness and a distinct
difference in density. This can be calculated by providing a matching
number of values for `depth` and `density` (e.g.
`depth = c(1, 2), density = c(1.7, 2.4)`)

\(3\) Another possibility is to calculate the cosmic dose rate for more
than one sample of the same profile. This is done by providing more than
one values for `depth` and only one for `density`. For example,
`depth = c(1, 2, 3)` and `density = 1.7` will calculate the cosmic dose
rate for three samples in 1, 2 and 3 m depth in a sediment of density
1.7 g/cm³.

## Usage

``` r
calc_CosmicDoseRate(
  depth,
  density,
  latitude,
  longitude,
  altitude,
  corr.fieldChanges = FALSE,
  est.age = NA,
  half.depth = FALSE,
  error = 10,
  ...
)
```

## Arguments

- depth:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): depth
  of overburden (m). For more than one absorber use  
  `c(depth_1, depth_2, ..., depth_n)`

- density:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): average
  overburden density (g/cm³). For more than one absorber use  
  `c(density_1, density_2, ..., density_n)`

- latitude:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  latitude (decimal degree), N positive

- longitude:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  longitude (decimal degree), E positive

- altitude:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**):
  altitude (m above sea-level)

- corr.fieldChanges:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  correct for geomagnetic field changes after Prescott & Hutton (1994).
  Apply only when justified by the data.

- est.age:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  estimated age range (ka) for geomagnetic field change correction (0-80
  ka allowed)

- half.depth:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): How
  to overcome with varying overburden thickness. If `TRUE` only half the
  depth is used for calculation. Apply only when justified, i.e. when a
  constant sedimentation rate can safely be assumed.

- error:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  general error (percentage) to be implemented on corrected cosmic dose
  rate estimate

- ...:

  further arguments (`verbose` to disable/enable console output).

## Value

Returns a terminal output. In addition an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following element:

- summary:

  [data.frame](https://rdrr.io/r/base/data.frame.html) summary of all
  relevant calculation results.

- args:

  [list](https://rdrr.io/r/base/list.html) used arguments

- call:

  [call](https://rdrr.io/r/base/call.html) the function call

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Note

Despite its universal use, the equation to calculate the cosmic dose
rate provided by Prescott & Hutton (1994) is falsely stated to be valid
from the surface to 10^4 hg/cm² of standard rock. The original
expression by Barbouti & Rastin (1983) only considers the muon flux
(i.e. hard-component) and is, by their own definition, only valid for
depths between 10-10^4 hg/cm².

Thus, for near-surface samples (i.e. for depths \< 167 g/cm²) the
equation of Prescott & Hutton (1994) underestimates the total cosmic
dose rate, as it neglects the influence of the soft-component of the
cosmic ray flux. For samples at zero depth and at sea-level the
underestimation can be as large as ~0.1 Gy/ka. In a previous article,
Prescott & Hutton (1988) give another approximation of Barbouti &
Rastin's equation in the form of

\$\$D = 0.21\*exp(-0.070\*absorber+0.0005\*absorber^2)\$\$

which is valid for depths between 150-5000 g/cm². For shallower depths
(\< 150 g/cm²) they provided a graph (Fig. 1) from which the dose rate
can be read.

As a result, this function employs the equation of Prescott & Hutton
(1994) only for depths \> 167 g/cm², i.e. only for the hard-component of
the cosmic ray flux. Cosmic dose rate values for depths \< 167 g/cm²
were obtained from the "AGE" program (Gruen 2009) and fitted with a
6-degree polynomial curve (and hence reproduces the graph shown in
Prescott & Hutton 1988). However, these values assume an average
overburden density of 2 g/cm³.

It is currently not possible to obtain more precise cosmic dose rate
values for near-surface samples as there is no equation known to the
author of this function at the time of writing.

## Function version

0.5.3

## How to cite

Burow, C., 2025. calc_CosmicDoseRate(): Calculate the cosmic dose rate.
Function version 0.5.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Allkofer, O.C., Carstensen, K., Dau, W.D., Jokisch, H., 1975. Letter to
the editor. The absolute cosmic ray flux at sea level. Journal of
Physics G: Nuclear and Particle Physics 1, L51-L52.

Barbouti, A.I., Rastin, B.C., 1983. A study of the absolute intensity of
muons at sea level and under various thicknesses of absorber. Journal of
Physics G: Nuclear and Particle Physics 9, 1577-1595.

Crookes, J.N., Rastin, B.C., 1972. An investigation of the absolute
intensity of muons at sea-level. Nuclear Physics B 39, 493-508.

Gruen, R., 2009. The "AGE" program for the calculation of luminescence
age estimates. Ancient TL 27, 45-46.

Prescott, J.R., Hutton, J.T., 1988. Cosmic ray and gamma ray dosimetry
for TL and ESR. Nuclear Tracks and Radiation Measurements 14, 223-227.

Prescott, J.R., Hutton, J.T., 1994. Cosmic ray contributions to dose
rates for luminescence and ESR dating: large depths and long-term time
variations. Radiation Measurements 23, 497-500.

Prescott, J.R., Stephan, L.G., 1982. The contribution of cosmic
radiation to the environmental dose for thermoluminescence dating.
Latitude, altitude and depth dependences. PACT 6, 17-25.

## See also

[BaseDataSet.CosmicDoseRate](https://r-lum.github.io/Luminescence/reference/BaseDataSet.CosmicDoseRate.md)

## Author

Christoph Burow, University of Cologne (Germany) , RLum Developer Team

## Examples

``` r
##(1) calculate cosmic dose rate (one absorber)
calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                    latitude = 38.06451, longitude = 1.49646,
                    altitude = 364, error = 10)
#> 
#> 
#>  [calc_CosmicDoseRate]
#> 
#>  ---------------------------------------------------------
#>  depth (m)              : 2.78
#>  density (g cm^-3)      : 1.7
#>  latitude (N deg.)      : 38.06451
#>  longitude (E deg.)     : 1.49646
#>  altitude (m)           : 364
#>  ---------------------------------------------------------
#>  total absorber (g cm^-2)       : 472.6
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.1518
#>   [@sea-level & 55 deg. N G.lat]
#> 
#>  geomagnetic latitude (deg.)    : 41.1
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.161 +- 0.0161
#>   [corrected]                 
#>  ---------------------------------------------------------
#> 
#> 
#>  [RLum.Results-class]
#>   originator: calc_CosmicDoseRate()
#>   data: 3
#>       .. $summary : data.frame
#>   .. $args : list
#>   .. $call : call
#>   additional info elements:  0 

##(2a) calculate cosmic dose rate (two absorber)
calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
                    latitude = 38.06451, longitude = 1.49646,
                    altitude = 364, error = 10)
#> 
#> 
#>  [calc_CosmicDoseRate]
#> 
#>  ---------------------------------------------------------
#>  depth (m)              : 5 2.78
#>  density (g cm^-3)      : 2.65 1.7
#>  latitude (N deg.)      : 38.06451
#>  longitude (E deg.)     : 1.49646
#>  altitude (m)           : 364
#>  ---------------------------------------------------------
#>  total absorber (g cm^-2)       : 1797.6
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.0705
#>   [@sea-level & 55 deg. N G.lat]
#> 
#>  geomagnetic latitude (deg.)    : 41.1
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.0747 +- 0.0075
#>   [corrected]                 
#>  ---------------------------------------------------------
#> 
#> 
#>  [RLum.Results-class]
#>   originator: calc_CosmicDoseRate()
#>   data: 3
#>       .. $summary : data.frame
#>   .. $args : list
#>   .. $call : call
#>   additional info elements:  0 

##(2b) calculate cosmic dose rate (two absorber) and
##correct for geomagnetic field changes
calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
                    latitude = 12.04332, longitude = 4.43243,
                    altitude = 364, corr.fieldChanges = TRUE,
                    est.age = 67, error = 15)
#> corr.fac: 1.045626  diff.one: 0.04562629  alt.fac: 1.281314 
#> 
#> 
#>  [calc_CosmicDoseRate]
#> 
#>  ---------------------------------------------------------
#>  depth (m)              : 5 2.78
#>  density (g cm^-3)      : 2.65 1.7
#>  latitude (N deg.)      : 12.04332
#>  longitude (E deg.)     : 4.43243
#>  altitude (m)           : 364
#>  ---------------------------------------------------------
#>  total absorber (g cm^-2)       : 1797.6
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.0705
#>   [@sea-level & 55 deg. N G.lat]
#> 
#>  geomagnetic latitude (deg.)    : 15.1
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.072 +- 0.0108
#>   [corrected]                 
#>  ---------------------------------------------------------
#> 
#> 
#>  [RLum.Results-class]
#>   originator: calc_CosmicDoseRate()
#>   data: 3
#>       .. $summary : data.frame
#>   .. $args : list
#>   .. $call : call
#>   additional info elements:  0 


##(3) calculate cosmic dose rate and export results to .csv file
#calculate cosmic dose rate and save to variable
results<- calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                              latitude = 38.06451, longitude = 1.49646,
                              altitude = 364, error = 10)
#> 
#> 
#>  [calc_CosmicDoseRate]
#> 
#>  ---------------------------------------------------------
#>  depth (m)              : 2.78
#>  density (g cm^-3)      : 1.7
#>  latitude (N deg.)      : 38.06451
#>  longitude (E deg.)     : 1.49646
#>  altitude (m)           : 364
#>  ---------------------------------------------------------
#>  total absorber (g cm^-2)       : 472.6
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.1518
#>   [@sea-level & 55 deg. N G.lat]
#> 
#>  geomagnetic latitude (deg.)    : 41.1
#> 
#>  cosmic dose rate (Gy ka^-1)    : 0.161 +- 0.0161
#>   [corrected]                 
#>  ---------------------------------------------------------
#> 

# the results can be accessed by
get_RLum(results, "summary")
#>   depth density latitude longitude altitude total_absorber.gcm2        d0
#> 1  2.78     1.7 38.06451   1.49646      364               472.6 0.1518269
#>   geom_lat       dc
#> 1  41.0685 0.161002

#export results to .csv file - uncomment for usage
#write.csv(results, file = "c:/users/public/results.csv")

##(4) calculate cosmic dose rate for 6 samples from the same profile
##    and save to .csv file
#calculate cosmic dose rate and save to variable
results<- calc_CosmicDoseRate(depth = c(0.1, 0.5 , 2.1, 2.7, 4.2, 6.3),
                              density = 1.7, latitude = 38.06451,
                              longitude = 1.49646, altitude = 364,
                              error = 10)
#> 
#> 
#>  [calc_CosmicDoseRate]
#> 
#>  Calculating cosmic dose rate for 6 samples.
#> 
#>   depth (m) d0 (Gy/ka) dc (Gy/ka) dc_error (Gy/ka)
#> 1       0.1     0.2599     0.2756           0.0276
#> 2       0.5     0.1999     0.2120           0.0212
#> 3       2.1     0.1640     0.1739           0.0174
#> 4       2.7     0.1532     0.1625           0.0162
#> 5       4.2     0.1299     0.1377           0.0138
#> 6       6.3     0.1045     0.1109           0.0111

#export results to .csv file - uncomment for usage
#write.csv(results, file = "c:/users/public/results_profile.csv")
```
