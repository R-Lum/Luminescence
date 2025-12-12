# Class `"RLum"`

Abstract class for data in the package Luminescence Subclasses are:

## Details

**RLum-class**  
\|  
\|—-[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md)  
\|—-\|–
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)  
\|—-\|–
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)  
\|—-\|–
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)  
\|—-[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)  
\|—-[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)

## Slots

- `originator`:

  Object of class [character](https://rdrr.io/r/base/character.html)
  containing the name of the producing function for the object. Set
  automatically by using the function
  [set_RLum](https://r-lum.github.io/Luminescence/reference/set_RLum.md).

- `info`:

  Object of class [list](https://rdrr.io/r/base/list.html) for
  additional information on the object itself

- `.uid`:

  Object of class [character](https://rdrr.io/r/base/character.html) for
  a unique object identifier. This id is usually calculated using the
  internal function `create_UID()` if the function
  [set_RLum](https://r-lum.github.io/Luminescence/reference/set_RLum.md)
  is called.

- `.pid`:

  Object of class [character](https://rdrr.io/r/base/character.html) for
  a parent id. This allows nesting RLum-objects at will. The parent id
  can be the uid of another object.

## Note

`RLum` is a virtual class.

## Objects from the Class

A virtual Class: No objects can be created from it.

## Class version

0.4.0

## See also

[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[methods_RLum](https://r-lum.github.io/Luminescence/reference/methods_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
showClass("RLum")
#> Virtual Class "RLum" [package "Luminescence"]
#> 
#> Slots:
#>                                                   
#> Name:  originator       info       .uid       .pid
#> Class:  character       list  character  character
#> 
#> Known Subclasses: 
#> Class "RLum.Analysis", directly
#> Class "RLum.Data", directly
#> Class "RLum.Results", directly
#> Class "RLum.Data.Curve", by class "RLum.Data", distance 2
#> Class "RLum.Data.Image", by class "RLum.Data", distance 2
#> Class "RLum.Data.Spectrum", by class "RLum.Data", distance 2
```
