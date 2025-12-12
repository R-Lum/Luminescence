# Convert Risoe.BINfileData object to an RLum.Analysis object

Converts values from one specific position of a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object to an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object.

The
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object requires a set of curves for specific further protocol analyses.
However, the
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
usually contains a set of curves for different aliquots and different
protocol types that may be mixed up. Therefore, a conversion is needed.

## Usage

``` r
Risoe.BINfileData2RLum.Analysis(
  object,
  pos = NULL,
  grain = NULL,
  run = NULL,
  set = NULL,
  ltype = NULL,
  dtype = NULL,
  protocol = "unknown",
  keep.empty = TRUE,
  txtProgressBar = FALSE
)
```

## Arguments

- object:

  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  (**required**): object to convert.

- pos:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): position
  number of the `Risoe.BINfileData` object for which the curves are
  stored in the `RLum.Analysis` object. If `length(pos) > 1`, a list of
  `RLum.Analysis` objects is returned. If nothing is provided every
  position will be converted. If the position is not valid `NULL` is
  returned.

- grain:

  [vector](https://rdrr.io/r/base/vector.html),
  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): grain
  number from the measurement to limit the converted data set (e.g.,
  `grain = c(1:48)`). Please be aware that this option may lead to
  unwanted effects, as the output is strictly limited to the chosen
  grain number for all position numbers.

- run:

  [vector](https://rdrr.io/r/base/vector.html),
  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): run
  number from the measurement to limit the converted data set (e.g.,
  `run = c(1:48)`).

- set:

  [vector](https://rdrr.io/r/base/vector.html),
  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): set
  number from the measurement to limit the converted data set (e.g.,
  `set = c(1:48)`).

- ltype:

  [vector](https://rdrr.io/r/base/vector.html),
  [character](https://rdrr.io/r/base/character.html) (*optional*): curve
  type to limit the converted data. Commonly allowed values are: `IRSL`,
  `OSL`, `TL`, `RIR`, `RBR` and `USER` (see also
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)).

- dtype:

  [vector](https://rdrr.io/r/base/vector.html),
  [character](https://rdrr.io/r/base/character.html) (*optional*): data
  type to limit the converted data. Commonly allowed values are listed
  in
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md).

- protocol:

  [character](https://rdrr.io/r/base/character.html) (*optional*): sets
  protocol type for analysis object. Value may be used by subsequent
  analysis functions.

- keep.empty:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): If
  `TRUE` (default) an `RLum.Analysis` object is returned even if it does
  not contain any records. Set to `FALSE` to discard all empty objects.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar.

## Value

Returns an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object.

## Note

The `protocol` argument of the
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object is set to 'unknown' if not stated otherwise.

## Function version

0.4.3

## See also

[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. Risoe.BINfileData2RLum.Analysis(): Convert
Risoe.BINfileData object to an RLum.Analysis object. Function version
0.4.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## Examples

``` r
##load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
```
