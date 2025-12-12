# Extract Pixel Values through Circular Region-of-Interests (ROI) from an Image

Light-weighted function to extract pixel values from pre-defined
regions-of-interest (ROI) from
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[array](https://rdrr.io/r/base/array.html) or
[matrix](https://rdrr.io/r/base/matrix.html) objects and provide simple
image processing capacity. The function is limited to circular ROIs.

## Usage

``` r
extract_ROI(object, roi, roi_summary = "mean", plot = FALSE)
```

## Arguments

- object:

  [RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
  [array](https://rdrr.io/r/base/array.html) or
  [matrix](https://rdrr.io/r/base/matrix.html) (**required**): input
  image data

- roi:

  [matrix](https://rdrr.io/r/base/matrix.html) (**required**): matrix
  with three columns containing the centre coordinates of the ROI (first
  two columns) and the diameter of the circular ROI. All numbers must by
  of type [integer](https://rdrr.io/r/base/integer.html) and will
  forcefully coerced into such numbers using
  [`as.integer()`](https://rdrr.io/r/base/integer.html) regardless.

- roi_summary:

  (*with default*): defines what is returned in the `roi_summary`
  element; it can be `"mean"` (default), `"median"`, `"sd"` or `"sum"`.
  Pixel values are conveniently summarised using the above defined
  keyword.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*optional*):
  enable/disable the plot output. Only the first image frame is shown.

## Value

[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object with the following elements: `..$roi_signals`: a named
[list](https://rdrr.io/r/base/list.html) with all ROI values and their
coordinates `..$roi_summary`: a
[matrix](https://rdrr.io/r/base/matrix.html) where rows are frames from
the image, and columns are different ROI The element has two attributes:
`summary` (the method used to summarise pixels) and `area` (the pixel
area) `..$roi_coord`: a [matrix](https://rdrr.io/r/base/matrix.html)
that can be passed to
[plot_ROI](https://r-lum.github.io/Luminescence/reference/plot_ROI.md)

If `plot = TRUE` a control plot is returned.

## Details

The function uses a cheap approach to decide whether a pixel lies within
a circle or not. It assumes that pixel coordinates are integer values
and that a pixel centring within the circle is satisfied by:

\$\$x^2 + y^2 \<= (d/2)^2\$\$

where \\x\\ and \\y\\ are integer pixel coordinates and \\d\\ is the
integer diameter of the circle in pixel.

## Function version

0.1.0

## See also

[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. extract_ROI(): Extract Pixel Values through Circular
Region-of-Interests (ROI) from an Image. Function version 0.1.0. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
extract_ROI(object = m, roi = roi, plot = TRUE)

#> 
#>  [RLum.Results-class]
#>   originator: extract_ROI()
#>   data: 3
#>       .. $roi_signals : list
#>   .. $roi_summary : matrix
#>   .. $roi_coord : matrix
#>   additional info elements:  1 
```
