# Class `"RLum.Data.Image"`

Class for representing luminescence image data (TL/OSL/RF). Such data
are for example produced by function
[read_SPE2R](https://r-lum.github.io/Luminescence/reference/read_SPE2R.md).

## Slots

- `recordType`:

  Object of class [character](https://rdrr.io/r/base/character.html)
  containing the type of the curve (e.g. "OSL image", "TL image").

- `curveType`:

  Object of class [character](https://rdrr.io/r/base/character.html)
  containing curve type, allowed values are measured or predefined

- `data`:

  Object of class [array](https://rdrr.io/r/base/array.html) containing
  image data.

- `info`:

  Object of class [list](https://rdrr.io/r/base/list.html) containing
  further meta information objects

## Note

The class should only contain data for a set of images. For additional
elements the slot `info` can be used.

## Objects from the class

Objects can be created by calls of the form
`set_RLum("RLum.Data.Image", ...)`.

## Class version

0.5.1

## See also

[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md),
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md),
[read_SPE2R](https://r-lum.github.io/Luminescence/reference/read_SPE2R.md),
[read_TIFF2R](https://r-lum.github.io/Luminescence/reference/read_TIFF2R.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
showClass("RLum.Data.Image")
#> Class "RLum.Data.Image" [package "Luminescence"]
#> 
#> Slots:
#>                                                                         
#> Name:  recordType  curveType       data       info originator       .uid
#> Class:  character  character      array       list  character  character
#>                  
#> Name:        .pid
#> Class:  character
#> 
#> Extends: 
#> Class "RLum.Data", directly
#> Class "RLum", by class "RLum.Data", distance 2

##create empty RLum.Data.Image object
set_RLum(class = "RLum.Data.Image")
#> 
#>  [RLum.Data.Image-class]
#>   recordType: Image
#>   curveType: NA
#>   .. recorded frames: 1
#>   .. .. pixel per frame: NA
#>   .. .. x dimension [px]: 1
#>   .. .. y dimension [px]: NA
#>   .. .. full pixel value range: NA : NA
#>   additional info elements: 0
```
