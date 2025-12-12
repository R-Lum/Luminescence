# Create a violin plot

Draws a kernel density plot in combination with a boxplot in its middle.
The shape of the violin is constructed using a mirrored density curve.
This plot is especially designed for cases where the individual errors
are zero or too small to be visualised. The idea for this plot is based
on the 'violin plot' in the `ggplot2` package by Hadley Wickham and
Winston Chang. The general idea for the violin plot seems to have been
introduced by Hintze and Nelson (1998).

## Usage

``` r
plot_ViolinPlot(
  data,
  boxplot = TRUE,
  rug = TRUE,
  summary = c("n", "median"),
  summary.pos = "sub",
  na.rm = TRUE,
  ...
)
```

## Arguments

- data:

  [numeric](https://rdrr.io/r/base/numeric.html) or
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  (**required**): input data for plotting. Alternatively a
  [data.frame](https://rdrr.io/r/base/data.frame.html) or a
  [matrix](https://rdrr.io/r/base/matrix.html) can be provided, but only
  the first column will be considered by the function

- boxplot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable boxplot

- rug:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable rug

- summary:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  add statistic measures of centrality and dispersion to the plot. Can
  be one or more of several keywords. See details for available
  keywords.

- summary.pos:

  [numeric](https://rdrr.io/r/base/numeric.html) or
  [character](https://rdrr.io/r/base/character.html) (*with default*):
  optional position keywords (cf.
  [legend](https://rdrr.io/r/graphics/legend.html)) for the statistical
  summary. Alternatively, the keyword `"sub"` may be specified to place
  the summary below the plot header. However, this latter option in only
  possible if `mtext` is not used.

- na.rm:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  exclude NA values from the data set prior to any further operations.

- ...:

  further arguments and graphical parameters passed to
  [plot.default](https://rdrr.io/r/graphics/plot.default.html),
  [stats::density](https://rdrr.io/r/stats/density.html) and
  [boxplot](https://rdrr.io/r/graphics/boxplot.html). See details for
  further information.

## Details

The function is passing several arguments to the functions
[plot](https://rdrr.io/r/graphics/plot.default.html),
[stats::density](https://rdrr.io/r/stats/density.html),
[graphics::boxplot](https://rdrr.io/r/graphics/boxplot.html):

Supported arguments are: `xlim`, `main`, `xlab`, `ylab`, `col.violin`,
`col.boxplot`, `mtext`, `cex`, `mtext`

**`Valid summary keywords`**

`'n'`, `'mean'`, `'median'`, `'sd.abs'`, `'sd.rel'`, `'se.abs'`,
`'se.rel'`. `'skewness'`, `'kurtosis'`

## Note

Although the code for this function was developed independently and just
the idea for the plot was based on the 'ggplot2' plot type 'violin', it
should be mentioned that, beyond this, at least another R package
produces this kind of plot, namely `'vioplot'` (see references for
details).

## Function version

0.1.4

## How to cite

Kreutzer, S., 2025. plot_ViolinPlot(): Create a violin plot. Function
version 0.1.4. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## References

Daniel Adler (2025). vioplot: violin plot. R package version 0.5.1
http://CRAN.R-project.org/package=vioplot

Hintze, J.L., Nelson, R.D. (1998). A Box Plot-Density Trace Synergism.
The American Statistician 52, 181-184.

Wickham. H (2009). ggplot2: elegant graphics for data analysis. Springer
New York.

## See also

[stats::density](https://rdrr.io/r/stats/density.html),
[plot](https://rdrr.io/r/graphics/plot.default.html),
[boxplot](https://rdrr.io/r/graphics/boxplot.html),
[rug](https://rdrr.io/r/graphics/rug.html),
[calc_Statistics](https://r-lum.github.io/Luminescence/reference/calc_Statistics.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
## read example data set
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- convert_Second2Gray(ExampleData.DeValues$BT998,
                                            c(0.0438,0.0019))

## create plot straightforward
plot_ViolinPlot(data = ExampleData.DeValues)

```
