# Create De(t) plot

Plots the equivalent dose (\\D_e\\) in dependency of the chosen signal
integral (cf. Bailey et al., 2003). The function is simply passing
several arguments to the function
[plot](https://rdrr.io/r/graphics/plot.default.html) and the used
analysis functions and runs it in a loop. Example: `legend.pos` for
legend position, `legend` for legend text.

## Usage

``` r
plot_DetPlot(
  object,
  signal_integral,
  background_integral,
  method = "shift",
  signal_integral.seq = NULL,
  integral_input = c("channel", "measurement"),
  analyse_function = c("analyse_SAR.CWOSL", "analyse_pIRIRSequence"),
  analyse_function.control = list(),
  n.channels = NULL,
  show_ShineDownCurve = TRUE,
  respect_RC.Status = FALSE,
  multicore = TRUE,
  verbose = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): input object containing data for analysis Can be
  provided as a [list](https://rdrr.io/r/base/list.html) of such
  objects.

- signal_integral:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): vector
  of channels for the signal integral.

- background_integral:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): vector
  of channels for the background integral.

- method:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  method applied for constructing the De(t) plot.

  - `shift` (*the default*): the chosen signal integral is shifted the
    shine down curve,

  - `expansion`: the chosen signal integral is expanded each time by its
    length

- signal_integral.seq:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): argument
  to provide an own signal integral sequence for constructing the De(t)
  plot

- integral_input:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  input type for `signal_integral`, one of `"channel"` (default) or
  `"measurement"`. If set to `"measurement"`, the best matching channels
  corresponding to the given time range (in seconds) are selected.

- analyse_function:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  name of the analyse function to be called. Supported functions are:
  [analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md),
  [analyse_pIRIRSequence](https://r-lum.github.io/Luminescence/reference/analyse_pIRIRSequence.md)

- analyse_function.control:

  [list](https://rdrr.io/r/base/list.html) (*optional*): selected
  arguments to be passed to the supported analyse functions
  ([analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md),
  [analyse_pIRIRSequence](https://r-lum.github.io/Luminescence/reference/analyse_pIRIRSequence.md)).
  The arguments must be provided as named
  [list](https://rdrr.io/r/base/list.html), e.g.,
  `list(dose.points = c(0,10,20,30,0,10)` will set the regeneration dose
  points.

- n.channels:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): number of
  channels used for the De(t) plot. If nothing is provided all De-values
  are calculated and plotted until the start of the background integral.

- show_ShineDownCurve:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable shine down curve in the plot output.

- respect_RC.Status:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  remove De values with 'FAILED' RC.Status from the plot (cf.
  [analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)
  and
  [analyse_pIRIRSequence](https://r-lum.github.io/Luminescence/reference/analyse_pIRIRSequence.md)).

- multicore:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*) :
  enable/disable multi core calculation if `object` is a
  [list](https://rdrr.io/r/base/list.html) of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  objects. Can be an [integer](https://rdrr.io/r/base/integer.html)
  specifying the number of cores

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output. Disabling the plot is useful in cases
  where the output need to be processed differently.

- ...:

  further arguments and graphical parameters passed to
  [plot.default](https://rdrr.io/r/graphics/plot.default.html),
  [analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)
  and
  [analyse_pIRIRSequence](https://r-lum.github.io/Luminescence/reference/analyse_pIRIRSequence.md)
  (see details for further information). Plot control parameters are:
  `ylim`, `xlim`, `ylab`, `xlab`, `main`, `pch`, `mtext`, `cex`,
  `legend`, `legend.text`, `legend.pos`

## Value

A plot and an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object with the produced \\D_e\\ values

`@data`:

|                       |              |                                            |
|-----------------------|--------------|--------------------------------------------|
| **Object**            | **Type**     | **Description**                            |
| `De.values`           | `data.frame` | table with De values                       |
| `signal_integral.seq` | `numeric`    | integral sequence used for the calculation |

`@info`:

|            |          |                            |
|------------|----------|----------------------------|
| **Object** | **Type** | **Description**            |
| call       | `call`   | the original function call |

## Details

**method**

The original method presented by Bailey et al., 2003 shifted the signal
integrals and slightly extended them accounting for changes in the
counting statistics. Example: `c(1:3, 3:5, 5:7)`. However, here also
another method is provided allowing to expand the signal integral by
consecutively expanding the integral by its chosen length. Example:
`c(1:3, 1:5, 1:7)`

Note that in both cases the integral limits are overlap. The finally
applied limits are part of the function output.

**analyse_function.control**

The argument `analyse_function.control` currently supports the following
arguments: `sequence.structure`, `dose.points`, `mtext.outer`,
`fit.method`, `fit.force_through_origin`, `plot`, `plot_singlePanels`

## Note

The entire analysis is based on the used analysis functions, namely
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)
and
[analyse_pIRIRSequence](https://r-lum.github.io/Luminescence/reference/analyse_pIRIRSequence.md).
However, the integrity checks of this function are not that thoughtful
as in these functions itself. It means, that every sequence should be
checked carefully before running long calculations using several
hundreds of channels.

## Function version

0.1.10

## How to cite

Kreutzer, S., 2026. plot_DetPlot(): Create De(t) plot. Function version
0.1.10. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., Bluszcz, A., 2026. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.2.1. https://r-lum.github.io/Luminescence/

## References

Bailey, R.M., Singarayer, J.S., Ward, S., Stokes, S., 2003.
Identification of partial resetting using De as a function of
illumination time. Radiation Measurements 37, 511-518.
doi:10.1016/S1350-4487(03)00063-5

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html),
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md),
[analyse_pIRIRSequence](https://r-lum.github.io/Luminescence/reference/analyse_pIRIRSequence.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## Examples

``` r
if (FALSE) { # \dontrun{
##load data
##ExampleData.BINfileData contains two BINfileData objects
##CWOSL.SAR.Data and TL.SAR.Data
data(ExampleData.BINfileData, envir = environment())

##transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

plot_DetPlot(
  object,
  signal_integral = 1:3,
  background_integral = 900:1000,
  n.channels = 5)
} # }
```
