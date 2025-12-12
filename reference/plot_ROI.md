# Create Regions of Interest (ROI) Graphic

The function creates ROI graphic with data extracted from the data
imported via
[read_RF2R](https://r-lum.github.io/Luminescence/reference/read_RF2R.md).
This function might be of use to work with reduced data from spatially
resolved measurements. The plot dimensions mimic the original image
dimensions.

## Usage

``` r
plot_ROI(
  object,
  exclude_ROI = 1,
  dist_thre = -Inf,
  dim.CCD = NULL,
  bg_image = NULL,
  plot = TRUE,
  ...
)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
  [RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
  or a [list](https://rdrr.io/r/base/list.html) of such objects
  (**required**): input data created either by
  [read_RF2R](https://r-lum.github.io/Luminescence/reference/read_RF2R.md)
  or
  [extract_ROI](https://r-lum.github.io/Luminescence/reference/extract_ROI.md).

- exclude_ROI:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  option to remove particular ROIs from the analysis. Those ROIs are
  plotted but not coloured and not taken into account in distance
  analysis. `NULL` excludes nothing.

- dist_thre:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): euclidean
  distance threshold in pixel distance. All ROI for which the euclidean
  distance is smaller are marked. This helps to identify ROIs that might
  be affected by signal cross-talk. Note: the distance is calculated
  from the centre of an ROI, e.g., the threshold should include consider
  the ROIs or grain radius.

- dim.CCD:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): metric x
  and y for the recorded (chip) surface in µm. For instance
  `c(8192,8192)`, if set additional x and y-axes are shown

- bg_image:

  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
  (*optional*): background image object (please note that dimensions are
  not checked).

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further parameters to manipulate the plot. On top of all arguments of
  [graphics::plot.default](https://rdrr.io/r/graphics/plot.default.html)
  the following arguments are supported: `lwd.ROI`, `lty.ROI`,
  `col.ROI`, `col.pixel`, `text.labels`, `text.offset`, `grid`
  (`TRUE/FALSE`), `legend` (`TRUE/FALSE`), `legend.text`, `legend.pos`

## Value

An ROI plot and an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object with a matrix containing the extracted ROI data and a object
produced by [stats::dist](https://rdrr.io/r/stats/dist.html) containing
the euclidean distance between the ROIs.

## Function version

0.2.0

## See also

[read_RF2R](https://r-lum.github.io/Luminescence/reference/read_RF2R.md),
[extract_ROI](https://r-lum.github.io/Luminescence/reference/extract_ROI.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. plot_ROI(): Create Regions of Interest (ROI)
Graphic. Function version 0.2.0. In: Kreutzer, S., Burow, C., Dietze,
M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N.,
Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J.,
Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## simple example
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
temp <- read_RF2R(file)
#> 
#> [read_RF2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  RF_file.rf
plot_ROI(temp)


## in combination with extract_ROI()
m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
t <- extract_ROI(object = m, roi = roi)
plot_ROI(t, bg_image = m)

```
