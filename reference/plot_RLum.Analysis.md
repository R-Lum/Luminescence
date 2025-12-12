# Plot function for an `RLum.Analysis` S4 class object

The function provides a standardised plot output for curve data of an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object.

The function produces a multiple plot output. A file output is
recommended (e.g., [pdf](https://rdrr.io/r/grDevices/pdf.html)).

**curve.transformation**

This argument allows transforming continuous wave (CW) curves to pseudo
(linear) modulated curves. For the transformation, the functions of the
package are used. Currently, it is not possible to pass further
arguments to the transformation functions. The argument works only for
`ltype` `OSL` and `IRSL`.

Please note: The curve transformation within this functions works
roughly, i.e. every IRSL or OSL curve is transformed, without
considering whether it is measured with the PMT or not! However, for a
fast look it might be helpful.

## Usage

``` r
plot_RLum.Analysis(
  object,
  subset = NULL,
  nrows = NULL,
  ncols = NULL,
  abline = NULL,
  combine = FALSE,
  records_max = NULL,
  curve.transformation = "None",
  plot_singlePanels = FALSE,
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): S4 object of class `RLum.Analysis`

- subset:

  named [list](https://rdrr.io/r/base/list.html) (*optional*): subsets
  elements for plotting. The arguments in the named
  [list](https://rdrr.io/r/base/list.html) will be directly passed to
  the function
  [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)
  (e.g., `subset = list(curveType = "measured")`)

- nrows:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): number of
  rows in the plot output. If set to `NULL` the function tries to find a
  reasonable value.

- ncols:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): number of
  columns in the plot output. If set to `NULL` the function tries to
  find a reasonable value.

- abline:

  [list](https://rdrr.io/r/base/list.html) (*optional*): allows to add
  ab-lines to the plot. Argument are provided in a list and will be
  forward to the function
  [abline](https://rdrr.io/r/graphics/abline.html), e.g.,
  `list(v = c(10, 100))` adds two vertical lines add 10 and 100 to all
  plots. In contrast `list(v = c(10), v = c(100)` adds a vertical at 10
  to the first and a vertical line at 100 to the 2nd plot.

- combine:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  allows to combine all
  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  objects in one single plot.

- records_max:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): limits
  number of records shown when `combine = TRUE` is used. The first and
  last curves are always shown, the other curves shown are distributed
  evenly; this may result in fewer curves plotted than specified. This
  argument must be at least 2 to have an effect.

- curve.transformation:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  allows transforming CW-OSL and CW-IRSL curves to pseudo-LM curves via
  transformation functions. Allowed values are: `CW2pLM`, `CW2pLMi`,
  `CW2pHMi` and `CW2pPMi`, see details. If set to `None` (default), no
  transformation is applied.

- plot_singlePanels:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  global par settings are considered, normally this should end in one
  plot per page

- ...:

  further arguments and graphical parameters will be passed to the
  `plot` function.

  Supported arguments: `main`, `mtext`, `log`, `lwd`, `lty` `type`,
  `pch`, `col`, `norm` (see
  [plot_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Curve.md)),
  `xlim`,`ylim`, `xlab`, `ylab`, ...

  and for `combine = TRUE` also: `sub_title`, `legend`, `legend.text`,
  `legend.pos` (typical plus 'outside'), `legend.col`, `smooth`.

  All arguments can be provided as `vector` or `list` to gain in full
  control of all plot settings.

## Value

Returns multiple plots.

## Note

Not all arguments available for
[plot](https://rdrr.io/r/graphics/plot.default.html) will be passed and
they partly do not behave in the way you might expect them to work. This
function was designed to serve as an overview plot, if you want to have
more control, extract the objects and plot them individually.

## Function version

0.3.16

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md),
[plot_RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Curve.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. plot_RLum.Analysis(): Plot function for an
RLum.Analysis S4 class object. Function version 0.3.16. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##(1) plot (combine) TL curves in one plot
plot_RLum.Analysis(
temp,
subset = list(recordType = "TL"),
combine = TRUE,
norm = TRUE,
abline = list(v = c(110))
)


##(2) same as example (1) but using
## the argument smooth = TRUE
plot_RLum.Analysis(
temp,
subset = list(recordType = "TL"),
combine = TRUE,
norm = TRUE,
smooth = TRUE,
abline = list(v = c(110))
)

```
