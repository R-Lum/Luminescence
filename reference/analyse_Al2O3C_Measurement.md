# Al2O3:C Passive Dosimeter Measurement Analysis

The function provides the analysis routines for measurements on a FI
lexsyg SMART reader using Al2O3:C chips according to Kreutzer et al.,
2018

## Usage

``` r
analyse_Al2O3C_Measurement(
  object,
  signal_integral = NULL,
  dose_points = c(0, 4),
  recordType = c("OSL (UVVIS)", "TL (UVVIS)"),
  calculate_TL_dose = FALSE,
  irradiation_time_correction = NULL,
  cross_talk_correction = NULL,
  travel_dosimeter = NULL,
  test_parameters = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): measurement input

- signal_integral:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): signal
  integral, used for the signal and the background. Example: `c(1:10)`
  for the first 10 channels. If nothing is provided the full range is
  used

- dose_points:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  vector with dose points, if dose points are repeated, only the general
  pattern needs to be provided. Default values follow the suggestions
  made by Kreutzer et al., 2018

- recordType:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  input curve selection, which is passed to function
  [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).
  To deactivate the automatic selection set the argument to `NULL`

- calculate_TL_dose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enables/disable experimental dose estimation based on the TL curves.
  It is computed as the ratio of the peak sums of each curves +/- 5
  channels.

- irradiation_time_correction:

  [numeric](https://rdrr.io/r/base/numeric.html) or
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  (*optional*): information on the used irradiation time correction
  obtained by another experiments. If a `numeric` is provided it has to
  be of length two: mean, standard error

- cross_talk_correction:

  [numeric](https://rdrr.io/r/base/numeric.html) or
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  (*optional*): information on the used irradiation time correction
  obtained by another experiments. If a `numeric` vector is provided it
  has to be of length three: mean, 2.5 % quantile, 97.5 % quantile.

- travel_dosimeter:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): specify
  the position of the travel dosimeter (so far measured at the same
  time). The dose of travel dosimeter will be subtracted from all other
  values.

- test_parameters:

  [list](https://rdrr.io/r/base/list.html) (*with default*): set test
  parameters. Supported parameters are: `TL_peak_shift` All input:
  [numeric](https://rdrr.io/r/base/numeric.html) values, `NA` and `NULL`
  (see details).

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output. If `object` is of type
  [list](https://rdrr.io/r/base/list.html), a numeric vector can be
  provided to limit the plot output to certain aliquots.

- ...:

  further arguments that can be passed to the plot output, supported are
  `norm`, `main`, `mtext`, `title` (for self-call mode to specify, e.g.,
  sample names)

## Value

Function returns results numerically and graphically:

———————————–  
`[ NUMERICAL OUTPUT ]`  
———————————–  

**`RLum.Results`**-object

**slot:** **`@data`**

|                    |              |                                                              |
|--------------------|--------------|--------------------------------------------------------------|
| **Element**        | **Type**     | **Description**                                              |
| `$data`            | `data.frame` | the estimated equivalent dose                                |
| `$data_table`      | `data.frame` | full dose and signal table                                   |
| `test_parameters`  | `data.frame` | results with test parameters                                 |
| `data_TDcorrected` | `data.frame` | travel dosimeter corrected results (only if TD was provided) |

*Note: If correction the irradiation time and the cross-talk correction
method is used, the De values in the table `data` table are already
corrected, i.e. if you want to get an uncorrected value, you can use the
column `CT_CORRECTION` remove the correction*

**slot:** **`@info`**

The original function call

————————  
`[ PLOT OUTPUT ]`  
————————  

- OSL and TL curves, combined on two plots.

## Details

**Working with a travel dosimeter**

The function allows to define particular position numbers as travel
dosimeters. For example: `travel_dosimeter = c(1,3,5)` sets positions 1,
3 and 5 as travel dosimeters. These dose values \#' of this dosimeters
are combined and automatically subtracted from the obtained dose values
of the other dosimeters.

**Calculate TL dose**

The argument `calculate_TL_dose` provides the possibility to
experimentally calculate a TL-dose, i.e. an apparent dose value derived
from the TL curve ratio. However, it should be noted that this value is
only a fall back in case something went wrong during the measurement of
the optical stimulation. The TL derived dose value is corrected for
cross-talk and for the irradiation time, but not considered if a travel
dosimeter is defined.

Calculating the palaeodose is possible without **any TL** curve in the
sequence!

**Test parameters**

`TL_peak_shift` [numeric](https://rdrr.io/r/base/numeric.html) (default:
`15`):

Checks whether the TL peak shift is bigger \> 15 K, indicating a problem
with the thermal contact of the chip.

`stimulation_power` [numeric](https://rdrr.io/r/base/numeric.html)
(default: `0.05`):

So far available, information on the delivered optical stimulation are
compared. Compared are the information from the first curves with all
others. If the ratio differs more from unity than the defined by the
threshold, a warning is returned.

## Function version

0.2.6

## How to cite

Kreutzer, S., 2025. analyse_Al2O3C_Measurement(): Al2O3:C Passive
Dosimeter Measurement Analysis. Function version 0.2.6. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier,
N., 2018. Environmental Dose Rate Determination Using a Passive
Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
Geochronometria 45, 56-67.

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
analyse_Al2O3C_Measurement(data_CrossTalk)
#> Warning: [analyse_Al2O3C_Measurement()] TL peak shift detected for aliquot position 1, check the curves
#>  [analyse_Al2O3_Measurement()] #1 DE: 0 ± 0

#>  ... (#1 | ALQ POS: 1)
#>  [analyse_Al2O3_Measurement()] #2 DE: 0 ± 0

#>  ... (#2 | ALQ POS: 2)
#> 
#>  [RLum.Results-class]
#>   originator: analyse_Al2O3C_Measurement()
#>   data: 3
#>       .. $data : data.frame
#>   .. $data_table : data.frame
#>   .. $test_parameters : data.frame
#>   additional info elements:  1 
```
