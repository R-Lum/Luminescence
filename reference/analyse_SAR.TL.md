# Analyse SAR TL measurements

The function performs a SAR TL analysis on a
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object including growth curve fitting.

## Usage

``` r
analyse_SAR.TL(
  object,
  object.background,
  signal_integral,
  integral_input = "channel",
  sequence.structure = c("PREHEAT", "SIGNAL", "BACKGROUND"),
  rejection.criteria = list(recycling.ratio = 10, recuperation.rate = 10),
  dose.points = NULL,
  log = "",
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  or a [list](https://rdrr.io/r/base/list.html) of such objects
  (**required**) : input object containing data for analysis

- object.background:

  currently not used

- signal_integral:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): vector
  of inputs (as defined by `integral_input`) for the signal integral.

- integral_input:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  input type for `signal_integral`, one of `"channel"` (default) or
  `"measurement"`. If set to `"measurement"`, the best matching channels
  corresponding to the given temperature range are selected.

- sequence.structure:

  [vector](https://rdrr.io/r/base/vector.html)
  [character](https://rdrr.io/r/base/character.html) (*with default*):
  specifies the general sequence structure. Three steps are allowed
  (`"PREHEAT"`, `"SIGNAL"`, `"BACKGROUND"`), in addition a parameter
  `"EXCLUDE"`. This allows excluding TL curves which are not relevant
  for the protocol analysis. (**Note:** No TL are removed by default)

- rejection.criteria:

  [list](https://rdrr.io/r/base/list.html) (*with default*): list
  containing rejection criteria in percentage for the calculation.

- dose.points:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): option
  set dose points manually

- log:

  [character](https://rdrr.io/r/base/character.html) (*with default*): a
  character string which contains `"x"` if the x-axis is to be
  logarithmic, `"y"` if the y axis is to be logarithmic and `"xy"` or
  `"yx"` if both axes are to be logarithmic. See
  [plot.default](https://rdrr.io/r/graphics/plot.default.html)).

- ...:

  further arguments that will be passed to the function
  [fit_DoseResponseCurve](https://r-lum.github.io/Luminescence/reference/fit_DoseResponseCurve.md)

## Value

A plot (*optional*) and an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following elements:

- De.values:

  [data.frame](https://rdrr.io/r/base/data.frame.html) containing
  De-values and further parameters

- LnLxTnTx.values:

  [data.frame](https://rdrr.io/r/base/data.frame.html) of all calculated
  `Lx/Tx` values including signal, background counts and the dose
  points.

- rejection.criteria:

  [data.frame](https://rdrr.io/r/base/data.frame.html) with values that
  might by used as rejection criteria. `NA` is produced if no R0 dose
  point exists.

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Details

This function performs a SAR TL analysis on a set of curves. The SAR
procedure in general is given by Murray and Wintle (2000). For the
calculation of the `Lx/Tx` value the function
[calc_TLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_TLLxTxRatio.md)
is used.

**Provided rejection criteria**

`[recycling.ratio]`: calculated for every repeated regeneration dose
point.

`[recuperation.rate]`: recuperation rate calculated by comparing the
`Lx/Tx` values of the zero regeneration point with the `Ln/Tn` value
(the `Lx/Tx` ratio of the natural signal). For methodological background
see Aitken and Smith (1988)

## Note

**THIS IS A BETA VERSION**

No TL curves will be removed from the input object without further
warning.

## Function version

0.3.4

## How to cite

Kreutzer, S., 2026. analyse_SAR.TL(): Analyse SAR TL measurements.
Function version 0.3.4. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A., 2026.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.2.1. https://r-lum.github.io/Luminescence/

## References

Aitken, M.J. and Smith, B.W., 1988. Optical dating: recuperation after
bleaching. Quaternary Science Reviews 7, 387-393.

Murray, A.S. and Wintle, A.G., 2000. Luminescence dating of quartz using
an improved single-aliquot regenerative-dose protocol. Radiation
Measurements 32, 57-73.

## See also

[calc_TLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_TLLxTxRatio.md),
[fit_DoseResponseCurve](https://r-lum.github.io/Luminescence/reference/fit_DoseResponseCurve.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## Examples

``` r
##load data
data(ExampleData.BINfileData, envir = environment())

##transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos=3)

##perform analysis
analyse_SAR.TL(
 object = object,
 signal_integral = 210:220,
 fit.method = "EXP OR LIN",
 sequence.structure = c("SIGNAL", "BACKGROUND"))

#> [fit_DoseResponseCurve()] Fit: EXP OR LIN (interpolation) | De = 415.65 | D01 = 11207248.85
#> 
#>  [RLum.Results-class]
#>   originator: analyse_SAR.TL()
#>   data: 3
#>       .. $data : data.frame
#>   .. $LnLxTnTx.table : data.frame
#>   .. $rejection.criteria : data.frame
#>   additional info elements:  1 
```
