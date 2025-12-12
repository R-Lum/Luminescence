# Convert portable OSL data to a Risoe.BINfileData object

Converts an `RLum.Analysis` object produced by the function
[`read_PSL2R()`](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)
to a `Risoe.BINfileData` object **(BETA)**.

This function converts an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object that was produced by the
[read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)
function to a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md).
The `Risoe.BINfileData` can be used to write a Risoe BIN file via
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md).

## Usage

``` r
convert_PSL2Risoe.BINfileData(object, ...)
```

## Arguments

- object:

  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): `RLum.Analysis` object produced by
  [read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)

- ...:

  currently not used.

## Value

Returns an S4
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object that can be used to write a BIN file using
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md).

## Function version

0.0.1

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)

## Author

Christoph Burow, University of Cologne (Germany) , RLum Developer Team

## How to cite

Burow, C., 2025. convert_PSL2Risoe.BINfileData(): Convert portable OSL
data to a Risoe.BINfileData object. Function version 0.0.1. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
# (1) load and plot example data set
data("ExampleData.portableOSL", envir = environment())
plot_RLum(ExampleData.portableOSL)















# (2) merge all RLum.Analysis objects into one
merged <- merge_RLum(ExampleData.portableOSL)
merged
#> 
#>  [RLum.Analysis-class]
#>   originator: merge_RLum.Analysis()
#>   protocol: portable OSL
#>   additional info elements:  196
#>   number of records: 70
#>   .. : RLum.Data.Curve : 70
#>   .. .. : #1 USER | #2 IRSL | #3 USER | #4 OSL | #5 USER | #6 USER | #7 IRSL
#>   .. .. : #8 USER | #9 OSL | #10 USER | #11 USER | #12 IRSL | #13 USER | #14 OSL
#>   .. .. : #15 USER | #16 USER | #17 IRSL | #18 USER | #19 OSL | #20 USER | #21 USER
#>   .. .. : #22 IRSL | #23 USER | #24 OSL | #25 USER | #26 USER | #27 IRSL | #28 USER
#>   .. .. : #29 OSL | #30 USER | #31 USER | #32 IRSL | #33 USER | #34 OSL | #35 USER
#>   .. .. : #36 USER | #37 IRSL | #38 USER | #39 OSL | #40 USER | #41 USER | #42 IRSL
#>   .. .. : #43 USER | #44 OSL | #45 USER | #46 USER | #47 IRSL | #48 USER | #49 OSL
#>   .. .. : #50 USER | #51 USER | #52 IRSL | #53 USER | #54 OSL | #55 USER | #56 USER
#>   .. .. : #57 IRSL | #58 USER | #59 OSL | #60 USER | #61 USER | #62 IRSL | #63 USER
#>   .. .. : #64 OSL | #65 USER | #66 USER | #67 IRSL | #68 USER | #69 OSL | #70 USER

# (3) convert to RisoeBINfile object
bin <- convert_PSL2Risoe.BINfileData(merged)
bin
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     7
#>  File name:            Praktikum2016
#>  Object date:          190516
#>  User:                 RLum
#>  System ID:            0 (unknown)
#>  Overall records:      70
#>  Records type:         IRSL  (n = 14)
#>                        OSL   (n = 14)
#>                        USER  (n = 42)
#>  Position range:       1 : 1
#>  Run range:            1 : 70
#>  Set range:            1 : 1

# (4) write Risoe BIN file
if (FALSE) { # \dontrun{
write_R2BIN(bin, "~/portableOSL.binx")
} # }
```
