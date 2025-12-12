# Al2O3:C Reader Cross-Talk Analysis

The function provides the analysis of cross-talk measurements on a FI
lexsyg SMART reader using Al2O3:C chips.

## Usage

``` r
analyse_Al2O3C_CrossTalk(
  object,
  signal_integral = NULL,
  dose_points = c(0, 4),
  recordType = "OSL (UVVIS)",
  irradiation_time_correction = NULL,
  method_control = NULL,
  plot = TRUE,
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  or [list](https://rdrr.io/r/base/list.html) (**required**):
  measurement input

- signal_integral:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): signal
  integral, used for the signal and the background. If nothing is
  provided, the full range is used.

- dose_points:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  vector with dose points, if dose points are repeated, only the general
  pattern needs to be provided. Default values follow the suggestions
  made by Kreutzer et al., 2018.

- recordType:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  input curve selection, which is passed to
  [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).
  To deactivate the automatic selection set the argument to `NULL`.

- irradiation_time_correction:

  [numeric](https://rdrr.io/r/base/numeric.html) or
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  (*optional*): information on the used irradiation time correction
  obtained by another experiment.

- method_control:

  [list](https://rdrr.io/r/base/list.html) (*optional*): optional
  parameters to control the calculation. See details for further
  explanations.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further arguments that can be passed to the plot output

## Value

Function returns results numerically and graphically:

———————————–  
`[ NUMERICAL OUTPUT ]`  
———————————–  

**`RLum.Results`**-object

**slot:** **`@data`**

|              |              |                                        |
|--------------|--------------|----------------------------------------|
| **Element**  | **Type**     | **Description**                        |
| `$data`      | `data.frame` | summed apparent dose table             |
| `$data_full` | `data.frame` | full apparent dose table               |
| `$fit`       | `lm`         | the linear model obtained from fitting |
| `$col.seq`   | `numeric`    | the used colour vector                 |

**slot:** **`@info`**

The original function call

————————  
`[ PLOT OUTPUT ]`  
————————  

- An overview of the obtained apparent dose values

## Function version

0.1.3

## How to cite

Kreutzer, S., 2025. analyse_Al2O3C_CrossTalk(): Al2O3:C Reader
Cross-Talk Analysis. Function version 0.1.3. In: Kreutzer, S., Burow,
C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier,
N., 2018. Environmental Dose Rate Determination Using a Passive
Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
Geochronometria 45, 56-67. doi: 10.1515/geochr-2015-0086

## See also

[analyse_Al2O3C_ITC](https://r-lum.github.io/Luminescence/reference/analyse_Al2O3C_ITC.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##load data
data(ExampleData.Al2O3C, envir = environment())

##run analysis
analyse_Al2O3C_CrossTalk(data_CrossTalk)

#> 
#>  [RLum.Results-class]
#>   originator: analyse_Al2O3C_CrossTalk()
#>   data: 4
#>       .. $data : data.frame
#>   .. $data_full : data.frame
#>   .. $fit : lm
#>   .. $col.seq : character
#>   additional info elements:  1 
```
