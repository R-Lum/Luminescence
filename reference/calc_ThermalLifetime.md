# Calculates the Thermal Lifetime using the Arrhenius equation

The function calculates the thermal lifetime of charges for given E (in
eV), s (in 1/s) and T (in deg. C.) parameters. The function can be used
in two operational modes:

## Usage

``` r
calc_ThermalLifetime(
  E,
  s,
  T = 20,
  output_unit = "Ma",
  profiling = FALSE,
  profiling_config = list(),
  verbose = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- E:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): vector
  of trap depths in eV, if `profiling = TRUE` only the first two
  elements are considered

- s:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): vector
  of frequency factor in 1/s, if `profiling = TRUE` only the first two
  elements are considered

- T:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  temperature in deg. C for which the lifetime(s) will be calculated. A
  vector can be provided.

- output_unit:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  output unit of the calculated lifetimes, accepted entries are: `"Ma"`,
  `"ka"`, `"a"`, `"d"`, `"h"`, `"min"`, `"s"`

- profiling:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): this
  option allows to estimate uncertainties based on given E and s
  parameters and their corresponding standard error (cf. details and
  examples section)

- profiling_config:

  [list](https://rdrr.io/r/base/list.html) (*optional*): allows to set
  configuration parameters used for the profiling (and only have an
  effect here). Supported parameters are:

  - `n` (number of MC runs),

  - `E.distribution` (distribution used for the re-sampling for E) and

  - `s.distribution` (distribution used for the re-sampling for s).

  Currently only the normal distribution is supported (e.g.,
  `profiling_config = list(E.distribution = "norm")`

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output, currently only in combination with
  `profiling = TRUE`.

- ...:

  further arguments that can be passed in combination with the plot
  output. Standard plot parameters are supported
  ([plot.default](https://rdrr.io/r/graphics/plot.default.html))

## Value

A
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned a along with a plot (for `profiling = TRUE`). The
output object contain the following slots:

**`@data`**

|                    |                                                                                              |                                       |
|--------------------|----------------------------------------------------------------------------------------------|---------------------------------------|
| **Object**         | **Type**                                                                                     | **Description**                       |
| `lifetimes`        | [array](https://rdrr.io/r/base/array.html) or [numeric](https://rdrr.io/r/base/numeric.html) | calculated lifetimes                  |
| `profiling_matrix` | [matrix](https://rdrr.io/r/base/matrix.html)                                                 | profiling matrix used for the MC runs |

**`@info`**

|            |          |                            |
|------------|----------|----------------------------|
| **Object** | **Type** | **Description**            |
| `call`     | `call`   | the original function call |

## Details

**Mode 1 `(profiling = FALSE)`**

An arbitrary set of input parameters (E, s, T) can be provided and the
function calculates the thermal lifetimes using the Arrhenius equation
for all possible combinations of these input parameters. An array with
3-dimensions is returned that can be used for further analyses or
graphical output (see example 1)

**Mode 2 `(profiling = TRUE)`**

This mode tries to profile the variation of the thermal lifetime for a
chosen temperature by accounting for the provided E and s parameters and
their corresponding standard errors, e.g., `E = c(1.600, 0.001)` The
calculation based on a Monte Carlo simulation, where values are sampled
from a normal distribution (for E and s).

**Used equation (Arrhenius equation)**

\$\$\tau = 1/s exp(E/kT)\$\$ where: \\\tau\\ in s as the mean time an
electron spends in the trap for a given \\T\\, \\E\\ trap depth in eV,
\\s\\ the frequency factor in 1/s, \\T\\ the temperature in K and \\k\\
the Boltzmann constant in eV/K (cf. Furetta, 2010).

## Note

The profiling is currently based on re-sampling from a normal
distribution, this distribution assumption might be, however, not valid
for given E and s parameters.

## Function version

0.1.0

## How to cite

Kreutzer, S., 2025. calc_ThermalLifetime(): Calculates the Thermal
Lifetime using the Arrhenius equation. Function version 0.1.0. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Furetta, C., 2010. Handbook of Thermoluminescence, Second Edition. World
Scientific.

## See also

[graphics::matplot](https://rdrr.io/r/graphics/matplot.html),
[stats::rnorm](https://rdrr.io/r/stats/Normal.html),
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##EXAMPLE 1
##calculation for two trap-depths with similar frequency factor for different temperatures
E <- c(1.66, 1.70)
s <- 1e+13
T <- 10:20
temp <- calc_ThermalLifetime(
  E = E,
  s = s,
  T = T,
  output_unit = "Ma"
)
#> 
#> [calc_ThermalLifetime()]
#> 
#>  mean:    1.354649e+03 Ma
#>  sd:  1.515822e+03 Ma
#>  min:     1.094723e+02 Ma (@ 20 °C)
#>  max:     5.743187e+03 Ma (@ 10 °C)
#>  --------------------------
#>  (22 lifetimes calculated in total)
graphics::contour(x = E, y = T, z = temp$lifetimes[1,,],
        ylab = "Temperature [\u00B0C]",
        xlab = "Trap depth [eV]",
        main = "Thermal Lifetime Contour Plot"
)
mtext(side = 3, "(values quoted in Ma)")


##EXAMPLE 2
##profiling of thermal life time for E and s and their standard error
E <- c(1.600, 0.003)
s <- c(1e+13,1e+011)
T <- 20
calc_ThermalLifetime(
  E = E,
  s = s,
  T = T,
  profiling = TRUE,
  output_unit = "Ma"
)
#> 
#> [calc_ThermalLifetime()]
#> 
#>  profiling = TRUE
#>  --------------------------
#>  mean:    1.020848e+01 Ma
#>  sd:  1.215245e+00 Ma
#>  min:     6.882699e+00 Ma
#>  max:     1.511764e+01 Ma
#>  --------------------------
#>  (1000 lifetimes calculated in total)

#> 
#>  [RLum.Results-class]
#>   originator: calc_ThermalLifetime()
#>   data: 2
#>       .. $lifetimes : numeric
#>   .. $profiling_matrix : matrix
#>   additional info elements:  1 
```
