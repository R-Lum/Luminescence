# Verify single grain data sets and check for invalid grains, i.e. zero-light level grains

This function tries to identify automatically zero-light level curves
(grains) from single grain data measurements.

## Usage

``` r
verify_SingleGrainData(
  object,
  threshold = 10,
  use_fft = FALSE,
  cleanup = FALSE,
  cleanup_level = "aliquot",
  verbose = TRUE,
  plot = FALSE,
  ...
)
```

## Arguments

- object:

  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  or
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  (**required**): input object. The function also accepts a list with
  objects of allowed type.

- threshold:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  numeric threshold value for the allowed difference between the `mean`
  and the `var` of the count values (see details)

- use_fft:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  applies an additional approach based on
  [stats::fft](https://rdrr.io/r/stats/fft.html). The threshold is fixed
  and cannot be changed.

- cleanup:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  set to `TRUE`, curves/aliquots identified as zero light level
  curves/aliquots are automatically removed. Output is an object as same
  type as the input, i.e. either
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  or
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)

- cleanup_level:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  selects the level for the clean-up of the input data sets. Two options
  are allowed: `"curve"` or `"aliquot"`:

  - If `"curve"` is selected, every single curve marked as `invalid` is
    removed.

  - If `"aliquot"` is selected, curves of one aliquot (grain or disc)
    can be marked as invalid, but will not be removed. An aliquot will
    be only removed if all curves of this aliquot are marked as invalid.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disables output to the terminal.

- plot:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the plot output.

- ...:

  further parameters to control the plot output; if selected. Supported
  arguments `main`, `ylim`

## Value

The function returns

———————————–  
`[ NUMERICAL OUTPUT ]`  
———————————–  

**`RLum.Results`**-object

**slot:\*\*\*\*`@data`**

|                   |              |                                                 |
|-------------------|--------------|-------------------------------------------------|
| **Element**       | **Type**     | **Description**                                 |
| `$unique_pairs`   | `data.frame` | the unique position and grain pairs             |
| `$selection_id`   | `numeric`    | the selection as record ID                      |
| `$selection_full` | `data.frame` | implemented models used in the baSAR-model core |

**slot:\*\*\*\*`@info`**

The original function call

**Output variation**

For `cleanup = TRUE` the same object as the input is returned, but
cleaned up (invalid curves were removed). This means: Either a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
or an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object is returned in such cases. A
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object can be exported to a BINX-file by using the function
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md).

## Details

**How does the method work?**

The function compares the expected values (\\E(X)\\) and the variance
(\\Var(X)\\) of the count values for each curve. Assuming that the
background roughly follows a Poisson distribution, the absolute
difference of both values should be zero or at least around zero as

\$\$E(x) = Var(x) = \lambda\$\$

Thus the function checks for:

\$\$abs(E(x) - Var(x)) \>= \Theta\$\$

With \\\Theta\\ an arbitrary, user defined, threshold. Values above the
threshold indicate curves comprising a signal.

Note: the absolute difference of \\E(X)\\ and \\Var(x)\\ instead of the
ratio was chosen as both terms can become 0 which would result in 0 or
`Inf`, if the ratio is calculated.

## Note

This function can work with
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
objects or
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects (or a list of it). However, the function is highly optimised for
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
objects as it make sense to remove identify invalid grains before the
conversion to an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object.

The function checking for invalid curves works rather robust and it is
likely that Reg0 curves within a SAR cycle are removed as well.
Therefore it is strongly recommended to use the argument
`cleanup = TRUE` carefully if the cleanup works only on curves.

## Function version

0.2.6

## See also

[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. verify_SingleGrainData(): Verify single grain data
sets and check for invalid grains, i.e. zero-light level grains.
Function version 0.2.6. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
##01 - basic example I
##just show how to apply the function
data(ExampleData.XSYG, envir = environment())

##verify and get data.frame out of it
verify_SingleGrainData(OSL.SARMeasurement$Sequence.Object)$selection_full
#> Warning: [verify_SingleGrainData()] 'selection_id' is NA, everything tagged for removal
#>     POSITION GRAIN      MEAN          VAR       RATIO THRESHOLD VALID
#> 1         NA    NA  23.66271 2.572619e+02   10.872037        10  TRUE
#> 2         NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 3         NA    NA 146.85632 4.982419e+03   33.927165        10  TRUE
#> 4         NA    NA  23.20000 4.901230e+03  211.259934        10  TRUE
#> 5         NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 6         NA    NA 107.87043 7.125950e+02    6.606027        10 FALSE
#> 7         NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 8         NA    NA  27.06067 3.495743e+02   12.918172        10  TRUE
#> 9         NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 10        NA    NA 111.65882 2.120132e+04  189.875911        10  TRUE
#> 11        NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 12        NA    NA  94.00808 1.421120e+03   15.117001        10  TRUE
#> 13        NA    NA  14.78800 1.018989e+02    6.890645        10 FALSE
#> 14        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 15        NA    NA 107.90087 7.100111e+02    6.580216        10 FALSE
#> 16        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 17        NA    NA  26.88278 3.521022e+02   13.097686        10  TRUE
#> 18        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 19        NA    NA 203.04576 1.440995e+05  709.689721        10  TRUE
#> 20        NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 21        NA    NA 147.35434 4.941044e+03   33.531721        10  TRUE
#> 22        NA    NA  18.48800 1.309212e+03   70.814165        10  TRUE
#> 23        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 24        NA    NA 107.37241 7.180268e+02    6.687256        10 FALSE
#> 25        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 26        NA    NA  26.53464 3.569946e+02   13.453909        10  TRUE
#> 27        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 28        NA    NA 149.22647 4.077393e+04  273.235223        10  TRUE
#> 29        NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 30        NA    NA  93.65100 1.425739e+03   15.223959        10  TRUE
#> 31        NA    NA  15.43400 1.468754e+02    9.516353        10 FALSE
#> 32        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 33        NA    NA 107.74087 7.151408e+02    6.637600        10 FALSE
#> 34        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 35        NA    NA  26.88278 3.521553e+02   13.099660        10  TRUE
#> 36        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 37        NA    NA 437.18390 7.404312e+05 1693.637907        10  TRUE
#> 38        NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 39        NA    NA 146.74713 4.976541e+03   33.912357        10  TRUE
#> 40        NA    NA  26.99200 8.015916e+03  296.973761        10  TRUE
#> 41        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 42        NA    NA 107.87652 7.046340e+02    6.531857        10 FALSE
#> 43        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 44        NA    NA  26.88079 3.520762e+02   13.097687        10  TRUE
#> 45        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 46        NA    NA 183.18088 6.418055e+04  350.367102        10  TRUE
#> 47        NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 48        NA    NA  94.05556 1.416457e+03   15.059791        10  TRUE
#> 49        NA    NA  16.43000 2.741815e+02   16.687855        10  TRUE
#> 50        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 51        NA    NA 108.64071 6.910736e+02    6.361093        10 FALSE
#> 52        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 53        NA    NA  27.62109 3.416066e+02   12.367601        10  TRUE
#> 54        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 55        NA    NA 693.53644 1.868981e+06 2694.856789        10  TRUE
#> 56        NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 57        NA    NA 146.77471 4.985033e+03   33.963838        10  TRUE
#> 58        NA    NA  41.61400 3.277204e+04  787.524417        10  TRUE
#> 59        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 60        NA    NA 107.84087 7.062001e+02    6.548538        10 FALSE
#> 61        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 62        NA    NA  26.89205 3.523737e+02   13.103264        10  TRUE
#> 63        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 64        NA    NA 229.96912 1.033205e+05  449.279678        10  TRUE
#> 65        NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 66        NA    NA  94.09293 1.419207e+03   15.083033        10  TRUE
#> 67        NA    NA  17.54000 4.446938e+02   25.353124        10  TRUE
#> 68        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 69        NA    NA 107.82348 7.152073e+02    6.633131        10 FALSE
#> 70        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 71        NA    NA  26.88212 3.521107e+02   13.098323        10  TRUE
#> 72        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 73        NA    NA 919.37203 3.298185e+06 3587.432532        10  TRUE
#> 74        NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 75        NA    NA 146.25829 4.992561e+03   34.135236        10  TRUE
#> 76        NA    NA  55.33200 7.126496e+04 1287.951921        10  TRUE
#> 77        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 78        NA    NA 107.44397 7.226060e+02    6.725422        10 FALSE
#> 79        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 80        NA    NA  26.70197 3.544455e+02   13.274131        10  TRUE
#> 81        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 82        NA    NA 278.14412 1.553639e+05  558.573294        10  TRUE
#> 83        NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 84        NA    NA  94.03131 1.419523e+03   15.096277        10  TRUE
#> 85        NA    NA  20.75400 7.620376e+02   36.717624        10  TRUE
#> 86        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 87        NA    NA 108.72566 6.921674e+02    6.366182        10 FALSE
#> 88        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 89        NA    NA  27.60612 3.412339e+02   12.360804        10  TRUE
#> 90        NA    NA  15.83136 4.363311e+01    2.756120        10 FALSE
#> 91        NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 92        NA    NA 146.50230 4.921485e+03   33.593229        10  TRUE
#> 93        NA    NA  13.75400 2.191331e+01    1.593232        10 FALSE
#> 94        NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 95        NA    NA 108.25351 6.988567e+02    6.455741        10 FALSE
#> 96        NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 97        NA    NA  27.23758 3.468351e+02   12.733694        10  TRUE
#> 98        NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 99        NA    NA 296.30735 1.763985e+05  595.322619        10  TRUE
#> 100       NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 101       NA    NA  93.99192 1.420900e+03   15.117255        10  TRUE
#> 102       NA    NA  19.43600 8.624027e+02   44.371409        10  TRUE
#> 103       NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 104       NA    NA 107.82174 7.110169e+02    6.594375        10 FALSE
#> 105       NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 106       NA    NA  26.88344 3.521723e+02   13.099971        10  TRUE
#> 107       NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 108       NA    NA 532.60085 1.130401e+06 2122.416765        10  TRUE
#> 109       NA    NA 133.00000 1.364500e+04  102.593985        10  TRUE
#> 110       NA    NA 146.72471 4.962003e+03   33.818454        10  TRUE
#> 111       NA    NA  27.52000 7.629970e+03  277.251800        10  TRUE
#> 112       NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 113       NA    NA 107.92696 7.062523e+02    6.543799        10 FALSE
#> 114       NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 115       NA    NA  26.87815 3.520057e+02   13.096354        10  TRUE
#> 116       NA    NA   1.00000 0.000000e+00    0.000000        10 FALSE
#> 117       NA    NA 302.94412 1.862969e+05  614.954596        10  TRUE
#> 118       NA    NA  76.25000 3.389583e+03   44.453552        10  TRUE
#> 119       NA    NA  94.41327 1.419383e+03   15.033721        10  TRUE
#> 120       NA    NA  20.19800 8.579387e+02   42.476417        10  TRUE
#> 121       NA    NA  79.00000 1.967500e+03   24.905063        10  TRUE
#> 122       NA    NA 108.15702 6.957128e+02    6.432433        10 FALSE
#> 123       NA    NA  40.00000 0.000000e+00    0.000000        10 FALSE
#> 124       NA    NA  27.24765 3.470933e+02   12.738468        10  TRUE

##02 - basic example II
data(ExampleData.BINfileData, envir = environment())
id <- verify_SingleGrainData(object = CWOSL.SAR.Data,
cleanup_level = "aliquot")$selection_id

if (FALSE) { # \dontrun{
##03 - advanced example I
##importing and exporting a BIN-file

##select and import file
file <- file.choose()
object <- read_BIN2R(file)

##remove invalid aliquots(!)
object <- verify_SingleGrainData(object, cleanup = TRUE)

##export to new BIN-file
write_R2BIN(object, paste0(dirname(file),"/", basename(file), "_CLEANED.BIN"))
} # }
```
