# Class `"RLum.Results"`

Object class contains results data from functions (e.g.,
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)).

## Slots

- `data`:

  Object of class [list](https://rdrr.io/r/base/list.html) containing
  output data

## Note

The class is intended to store results from functions to be used by
other functions. The data in the object should always be accessed by the
method `get_RLum`.

## Objects from the Class

Objects can be created by calls of the form `new("RLum.Results", ...)`.

## Class version

0.5.2

## See also

[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md),
[merge_RLum](https://r-lum.github.io/Luminescence/reference/merge_RLum.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
showClass("RLum.Results")
#> Class "RLum.Results" [package "Luminescence"]
#> 
#> Slots:
#>                                                              
#> Name:        data originator       info       .uid       .pid
#> Class:       list  character       list  character  character
#> 
#> Extends: "RLum"

##create an empty object from this class
set_RLum(class = "RLum.Results")
#> 
#>  [RLum.Results-class]
#>   originator: eval()
#>   data: 0
#>       .. $ : list
#>   additional info elements:  0 

##use another function to show how it works

##Basic calculation of the dose rate for a specific date
 dose.rate <-  calc_SourceDoseRate(
   measurement.date = "2012-01-27",
   calib.date = "2014-12-19",
   calib.dose.rate = 0.0438,
   calib.error = 0.0019)

##show object
dose.rate
#> 
#>  [RLum.Results-class]
#>   originator: calc_SourceDoseRate()
#>   data: 3
#>       .. $dose.rate : data.frame
#>   .. $parameters : list
#>   .. $call : call
#>   additional info elements:  0 

##get results
get_RLum(dose.rate)
#>    dose.rate dose.rate.error       date
#> 1 0.04694815     0.002036563 2012-01-27

##get parameters used for the calcualtion from the same object
get_RLum(dose.rate, data.object = "parameters")
#> $source.type
#> [1] "Sr-90"
#> 
#> $halflife
#> [1] 28.9
#> 
#> $dose.rate.unit
#> [1] "Gy/s"
#> 

##alternatively objects can be accessed using S3 generics, such as
dose.rate$parameters
#> $source.type
#> [1] "Sr-90"
#> 
#> $halflife
#> [1] 28.9
#> 
#> $dose.rate.unit
#> [1] "Gy/s"
#> 
```
