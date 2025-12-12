# Analyse post-IR IRSL measurement sequences

The function performs an analysis of post-IR IRSL sequences including
curve fitting on
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects.

## Usage

``` r
analyse_pIRIRSequence(
  object,
  signal.integral.min,
  signal.integral.max,
  background.integral.min,
  background.integral.max,
  dose.points = NULL,
  sequence.structure = c("TL", "IR50", "pIRIR225"),
  plot = TRUE,
  plot_singlePanels = FALSE,
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  or [list](https://rdrr.io/r/base/list.html) of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects (**required**): input object containing data for analysis. If
  a [list](https://rdrr.io/r/base/list.html) is provided the functions
  tries to iterate over each element in the list.

- signal.integral.min:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): lower
  bound of the signal integral. Provide this value as vector for
  different integration limits for the different IRSL curves.

- signal.integral.max:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): upper
  bound of the signal integral. Provide this value as vector for
  different integration limits for the different IRSL curves.

- background.integral.min:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): lower
  bound of the background integral. Provide this value as vector for
  different integration limits for the different IRSL curves.

- background.integral.max:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): upper
  bound of the background integral. Provide this value as vector for
  different integration limits for the different IRSL curves.

- dose.points:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): a numeric
  vector containing the dose points values. Using this argument
  overwrites dose point values in the signal curves.

- sequence.structure:

  [vector](https://rdrr.io/r/base/vector.html)
  [character](https://rdrr.io/r/base/character.html) (*with default*):
  specifies the general sequence structure. Allowed values are `"TL"`
  and any `"IR"` combination (e.g., `"IR50"`,`"pIRIR225"`). Additionally
  a parameter `"EXCLUDE"` is allowed to exclude curves from the analysis
  (Note: If a preheat without PMT measurement is used, i.e. preheat as
  none TL, remove the TL step.)

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- plot_singlePanels:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable plotting of the results in a single windows for each
  plot. Ignored if `plot = FALSE`.

- ...:

  further arguments that will be passed to
  [analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)
  and
  [plot_DoseResponseCurve](https://r-lum.github.io/Luminescence/reference/plot_DoseResponseCurve.md).
  Furthermore, the arguments `main` (headers), `log` (IRSL curves),
  `cex` (control the size) and `mtext.outer` (additional text on the
  plot area) can be passed to influence the plotting. If the input is a
  list, `main` can be passed as
  [vector](https://rdrr.io/r/base/vector.html) or
  [list](https://rdrr.io/r/base/list.html).

## Value

Plots (*optional*) and an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned containing the following elements:

|                           |                                                      |                                                      |
|---------------------------|------------------------------------------------------|------------------------------------------------------|
| **DATA.OBJECT**           | **TYPE**                                             | **DESCRIPTION**                                      |
| `..$data` :               | `data.frame`                                         | Table with De values                                 |
| `..$LnLxTnTx.table` :     | `data.frame`                                         | with the `LnLxTnTx` values                           |
| `..$rejection.criteria` : | [data.frame](https://rdrr.io/r/base/data.frame.html) | rejection criteria                                   |
| `..$Formula` :            | [list](https://rdrr.io/r/base/list.html)             | Function used for fitting of the dose response curve |
| `..$call` :               | [call](https://rdrr.io/r/base/call.html)             | the original function call                           |

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Details

To allow post-IR IRSL protocol (Thomsen et al., 2008) measurement
analyses, this function has been written as extended wrapper for
function
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md),
thus facilitating an entire sequence analysis in one run. With this, its
functionality is strictly limited by the functionality provided by
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md).

**Defining the sequence structure**

The argument `sequence.structure` expects a shortened pattern of your
sequence structure and was mainly introduced to ease the use of the
function. For example: If your measurement data contains the following
curves: `TL`, `IRSL`, `IRSL`, `TL`, `IRSL`, `IRSL`, the pattern in
`sequence.structure` becomes `c('TL', 'IRSL', 'IRSL')`. The second part
of your sequence for one cycle should be similar and can be discarded.
If this is not the case (e.g., additional hotbleach) such curves must be
removed before using the function.

**If the input is a `list`**

If the input is a list of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects, every argument can be provided as list to allow for different
sets of parameters for every single input element. For further
information see
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md).

## Note

Best graphical output can be achieved by using the function `pdf` with
the following options:

`pdf(file = "<YOUR FILENAME>", height = 18, width = 18)`

## Function version

0.2.6

## How to cite

Kreutzer, S., 2025. analyse_pIRIRSequence(): Analyse post-IR IRSL
measurement sequences. Function version 0.2.6. In: Kreutzer, S., Burow,
C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Murray, A.S., Wintle, A.G., 2000. Luminescence dating of quartz using an
improved single-aliquot regenerative-dose protocol. Radiation
Measurements 32, 57-73.
[doi:10.1016/S1350-4487(99)00253-X](https://doi.org/10.1016/S1350-4487%2899%2900253-X)

Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008.
Laboratory fading rates of various luminescence signals from
feldspar-rich sediment extracts. Radiation Measurements 43, 1474-1486.
[doi:10.1016/j.radmeas.2008.06.002](https://doi.org/10.1016/j.radmeas.2008.06.002)

## See also

[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md),
[calc_OSLLxTxRatio](https://r-lum.github.io/Luminescence/reference/calc_OSLLxTxRatio.md),
[plot_GrowthCurve](https://r-lum.github.io/Luminescence/reference/plot_GrowthCurve.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
### NOTE: For this example existing example data are used. These data are non pIRIR data.
###
##(1) Compile example data set based on existing example data (SAR quartz measurement)
##(a) Load example data
data(ExampleData.BINfileData, envir = environment())

##(b) Transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##(c) Grep curves and exclude the last two (one TL and one IRSL)
object <- get_RLum(object, record.id = c(-29,-30))

##(d) Define new sequence structure and set new RLum.Analysis object
sequence.structure  <- c(1,2,2,3,4,4)
sequence.structure <- as.vector(sapply(seq(0,length(object)-1,by = 4),
                                       function(x){sequence.structure + x}))

object <-  sapply(1:length(sequence.structure), function(x){
  object[[sequence.structure[x]]]
})

object <- set_RLum(class = "RLum.Analysis", records = object, protocol = "pIRIR")

##(2) Perform pIRIR analysis (for this example with quartz OSL data!)
## Note: output as single plots to avoid problems with this example
results <- analyse_pIRIRSequence(object,
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 900,
     background.integral.max = 1000,
     fit.method = "EXP",
     sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
     main = "Pseudo pIRIR data set based on quartz OSL",
     plot_singlePanels = TRUE)





#> [fit_DoseResponseCurve()] Fit: EXP (interpolation) | De = 1668.25 | D01 = 1982.76






#> [fit_DoseResponseCurve()] Fit: EXP (interpolation) | De = 1668.25 | D01 = 1982.76








##(3) Perform pIRIR analysis (for this example with quartz OSL data!)
## Alternative for PDF output, uncomment and complete for usage
if (FALSE) { # \dontrun{
tempfile <- tempfile(fileext = ".pdf")
pdf(file = tempfile, height = 18, width = 18)
  results <- analyse_pIRIRSequence(object,
         signal.integral.min = 1,
         signal.integral.max = 2,
         background.integral.min = 900,
         background.integral.max = 1000,
         fit.method = "EXP",
         main = "Pseudo pIRIR data set based on quartz OSL")

  dev.off()
} # }
```
