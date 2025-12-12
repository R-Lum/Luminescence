# Class `"RLum.Data.Curve"`

Class for representing luminescence curve data.

## Slots

- `recordType`:

  Object of class "character" containing the type of the curve (e.g.
  "TL" or "OSL").

- `curveType`:

  Object of class "character" containing curve type, allowed values are
  "measured" or "predefined".

- `data`:

  Object of class [matrix](https://rdrr.io/r/base/matrix.html)
  containing curve x and y data. 'data' can also be of type
  `RLum.Data.Curve` to change object values without de-constructing the
  object. For example:

      set_RLum(class = 'RLum.Data.Curve',
               data = Your.RLum.Data.Curve,
               recordType = 'never seen before')

  would just change the `recordType`. Missing arguments the value is
  taken from the input object in 'data' (which is already an
  `RLum.Data.Curve` object in this example).

## Note

The class should only contain data for a single curve. For additional
elements the slot `info` can be used (e.g. providing additional heating
ramp curve). Objects from the class `RLum.Data.Curve` are produced by
other functions (partly within
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects), namely:
[Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md),
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md)

## Create objects from this Class

Objects can be created by calls of the form
`set_RLum(class = "RLum.Data.Curve", ...)`.

## Class version

0.5.1

## See also

[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md),
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md),
[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
showClass("RLum.Data.Curve")
#> Class "RLum.Data.Curve" [package "Luminescence"]
#> 
#> Slots:
#>                                                                         
#> Name:  recordType  curveType       data originator       info       .uid
#> Class:  character  character     matrix  character       list  character
#>                  
#> Name:        .pid
#> Class:  character
#> 
#> Extends: 
#> Class "RLum.Data", directly
#> Class "RLum", by class "RLum.Data", distance 2

##set empty curve object
set_RLum(class = "RLum.Data.Curve")
#> 
#>  [RLum.Data.Curve-class]
#>   recordType: NA
#>   curveType: NA
#>   measured values: 1
#>   .. range of x-values: 0 0
#>   .. range of y-values: 0 0 
#>   additional info elements: 0 
```
