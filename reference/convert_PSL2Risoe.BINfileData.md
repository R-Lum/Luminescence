# Convert portable OSL data to a Risoe.BINfileData object

Converts an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object produced by the function
[read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)
to a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object **(BETA)**.

This function converts an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object that was produced by the
[read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)
function to a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md).
The
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
can be used to write a Risø BIN file via
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

Burow, C., 2026. convert_PSL2Risoe.BINfileData(): Convert portable OSL
data to a Risoe.BINfileData object. Function version 0.0.1. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., Bluszcz, A., 2026. Luminescence: Comprehensive Luminescence
Dating Data Analysis. R package version 1.2.0.
https://r-lum.github.io/Luminescence/

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
#>   .. .. : #1 USER (PMT) | #2 IRSL (PMT) | #3 USER (PMT) | #4 OSL (PMT) | #5 USER (PMT) | #6 USER (PMT) | #7 IRSL (PMT)
#>   .. .. : #8 USER (PMT) | #9 OSL (PMT) | #10 USER (PMT) | #11 USER (PMT) | #12 IRSL (PMT) | #13 USER (PMT) | #14 OSL (PMT)
#>   .. .. : #15 USER (PMT) | #16 USER (PMT) | #17 IRSL (PMT) | #18 USER (PMT) | #19 OSL (PMT) | #20 USER (PMT) | #21 USER (PMT)
#>   .. .. : #22 IRSL (PMT) | #23 USER (PMT) | #24 OSL (PMT) | #25 USER (PMT) | #26 USER (PMT) | #27 IRSL (PMT) | #28 USER (PMT)
#>   .. .. : #29 OSL (PMT) | #30 USER (PMT) | #31 USER (PMT) | #32 IRSL (PMT) | #33 USER (PMT) | #34 OSL (PMT) | #35 USER (PMT)
#>   .. .. : #36 USER (PMT) | #37 IRSL (PMT) | #38 USER (PMT) | #39 OSL (PMT) | #40 USER (PMT) | #41 USER (PMT) | #42 IRSL (PMT)
#>   .. .. : #43 USER (PMT) | #44 OSL (PMT) | #45 USER (PMT) | #46 USER (PMT) | #47 IRSL (PMT) | #48 USER (PMT) | #49 OSL (PMT)
#>   .. .. : #50 USER (PMT) | #51 USER (PMT) | #52 IRSL (PMT) | #53 USER (PMT) | #54 OSL (PMT) | #55 USER (PMT) | #56 USER (PMT)
#>   .. .. : #57 IRSL (PMT) | #58 USER (PMT) | #59 OSL (PMT) | #60 USER (PMT) | #61 USER (PMT) | #62 IRSL (PMT) | #63 USER (PMT)
#>   .. .. : #64 OSL (PMT) | #65 USER (PMT) | #66 USER (PMT) | #67 IRSL (PMT) | #68 USER (PMT) | #69 OSL (PMT) | #70 USER (PMT)

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
