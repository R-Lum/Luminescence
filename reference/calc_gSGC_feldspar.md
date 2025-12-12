# Calculate Global Standardised Growth Curve (gSGC) for Feldspar MET-pIRIR

Implementation of the gSGC approach for feldspar MET-pIRIR by Li et al.
(2015)

## Usage

``` r
calc_gSGC_feldspar(
  data,
  gSGC.type = "50LxTx",
  gSGC.parameters = NULL,
  n.MC = 100,
  plot = FALSE
)
```

## Arguments

- data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  data frame with five columns per sample
  `c("LnTn", "LnTn.error", "Lr1Tr1", "Lr1Tr1.error","Dr1")`

- gSGC.type:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  growth curve type to be selected according to Table 3 in Li et al.
  (2015). Allowed options are `"50LxTx"`, `"50Lx"`, `"50Tx"`,
  `"100LxTx"`, `"100Lx"`, `"100Tx"`, `"150LxTx"`, `"150Lx"`, `"150Tx"`,
  `"200LxTx"`, `"200Lx"`, `"200Tx"`, `"250LxTx"`, `"250Lx"`, `"250Tx"`

- gSGC.parameters:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*): an
  own parameter set for the gSGC with the following columns `y1`,
  `y1_err`, `D1` `D1_err`, `y2`, `y2_err`, `y0`, `y0_err`.

- n.MC:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  number of Monte-Carlo runs for the error calculation

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

## Value

Returns an S4 object of type
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md).

**`@data`**  
`$ df` ([data.frame](https://rdrr.io/r/base/data.frame.html))  
`.. $DE` the calculated equivalent dose  
`.. $DE.ERROR` error on the equivalent dose, which is the standard
deviation of the MC runs  
`.. $HPD95_LOWER` lower boundary of the highest probability density
(95%)  
`.. $HPD95_UPPER` upper boundary of the highest probability density
(95%)  
`$ m.MC` ([list](https://rdrr.io/r/base/list.html)) numeric vector with
results from the MC runs.  

**`@info`**  
\`\$ call“ ([call](https://rdrr.io/r/base/call.html)) the original
function call

## Details

\##TODO

## Function version

0.1.1

## How to cite

Gray, H.J., Kreutzer, S., 2025. calc_gSGC_feldspar(): Calculate Global
Standardised Growth Curve (gSGC) for Feldspar MET-pIRIR. Function
version 0.1.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Li, B., Roberts, R.G., Jacobs, Z., Li, S.-H., Guo, Y.-J., 2015.
Construction of a “global standardised growth curve” (gSGC) for infrared
stimulated luminescence dating of K-feldspar 27, 119–130.
[doi:10.1016/j.quageo.2015.02.010](https://doi.org/10.1016/j.quageo.2015.02.010)

## See also

[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md),
[uniroot](https://rdrr.io/r/stats/uniroot.html),
[calc_gSGC](https://r-lum.github.io/Luminescence/reference/calc_gSGC.md)

## Author

Harrison Gray, USGS (United States), Sebastian Kreutzer, Institute of
Geography, Heidelberg University (Germany) , RLum Developer Team

## Examples

``` r
##test on a generated random sample
n_samples <- 10
data <- data.frame(
  LnTn = rnorm(n=n_samples, mean=1.0, sd=0.02),
  LnTn.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
  Lr1Tr1 = rnorm(n=n_samples, mean=1.0, sd=0.02),
  Lr1Tr1.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
  Dr1 = rep(100,n_samples))

results <- calc_gSGC_feldspar(
  data = data, gSGC.type = "50LxTx",
  plot = FALSE)

plot_AbanicoPlot(results)

```
