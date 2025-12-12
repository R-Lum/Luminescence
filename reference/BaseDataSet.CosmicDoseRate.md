# Base data set for cosmic dose rate calculation

Collection of data from various sources needed for cosmic dose rate
calculation

## Format

|                           |                                                                                                                                                                                                                                        |
|---------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `values.cosmic.Softcomp`: | data frame containing cosmic dose rates for shallow depths (\< 167 g cm^-2) obtained using the "AGE" program by Rainer Gruen (cf. Gruen 2009). These data essentially reproduce the graph shown in Fig. 1 of Prescott & Hutton (1988). |
| `values.factor.Altitude`: | data frame containing altitude factors for adjusting geomagnetic field-change factors. Values were read from Fig. 1 in Prescott & Hutton (1994).                                                                                       |
| `values.par.FJH`:         | data frame containing values for parameters F, J and H (read from Fig. 2 in Prescott & Hutton 1994) used in the expression                                                                                                             |

\$\$Dc = D0\*(F+J\*exp((altitude/1000)/H))\$\$

## Source

The following data were carefully read from figures in mentioned sources
and used for fitting procedures. The derived expressions are used in the
function `calc_CosmicDoseRate`.

**values.cosmic.Softcomp**

|            |                            |
|------------|----------------------------|
| Program:   | "AGE"                      |
| Reference: | Gruen (2009)               |
| Fit:       | Polynomials in the form of |

For depths between 40-167 g cm^-2:

\$\$y = 2\*10^-6\*x^2-0.0008\*x+0.2535\$\$

(For depths \<40 g cm^-2)

\$\$y = -6\*10^-8\*x^3+2\*10^-5\*x^2-0.0025\*x+0.2969\$\$

**`values.factor.Altitude`**

|            |                                    |
|------------|------------------------------------|
| Reference: | Prescott & Hutton (1994)           |
| Page:      | 499                                |
| Figure:    | 1                                  |
| Fit:       | 2-degree polynomial in the form of |

\$\$y = -0.026\*x^2 + 0.6628\*x + 1.0435\$\$

**`values.par.FJH`**

|            |                                      |
|------------|--------------------------------------|
| Reference: | Prescott & Hutton (1994)             |
| Page:      | 500                                  |
| Figure:    | 2                                    |
| Fits:      | 3-degree polynomials and linear fits |

F (non-linear part, \\\lambda\\ \< 36.5 deg.):

\$\$y = -7\*10^-7\*x^3-8\*10^-5\*x^2-0.0009\*x+0.3988\$\$

F (linear part, \\\lambda\\ \> 36.5 deg.):

\$\$y = -0.0001\*x + 0.2347\$\$

J (non-linear part, \\\lambda\\ \< 34 deg.):

\$\$y = 5\*10^-6\*x^3-5\*10^-5\*x^2+0.0026\*x+0.5177\$\$

J (linear part, \\\lambda\\ \> 34 deg.):

\$\$y = 0.0005\*x + 0.7388\$\$

H (non-linear part, \\\lambda\\ \< 36 deg.):

\$\$y = -3\*10^-6\*x^3-5\*10^-5\*x^2-0.0031\*x+4.398\$\$

H (linear part, \\\lambda\\ \> 36 deg.):

\$\$y = 0.0002\*x + 4.0914\$\$

## Version

0.1

## References

Gruen, R., 2009. The "AGE" program for the calculation of luminescence
age estimates. Ancient TL, 27, pp. 45-46.

Prescott, J.R., Hutton, J.T., 1988. Cosmic ray and gamma ray dosimetry
for TL and ESR. Nuclear Tracks and Radiation Measurements, 14, pp.
223-227.

Prescott, J.R., Hutton, J.T., 1994. Cosmic ray contributions to dose
rates for luminescence and ESR dating: large depths and long-term time
variations. Radiation Measurements, 23, pp. 497-500.

## Examples

``` r
##load data
data(BaseDataSet.CosmicDoseRate)
```
