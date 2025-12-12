# Merge Risoe.BINfileData objects or Risoe BIN-files

The function allows merging Risoe BIN/BINX files or
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
objects.

## Usage

``` r
merge_Risoe.BINfileData(
  input.objects,
  output.file,
  keep.position.number = FALSE,
  position.number.append.gap = 0,
  verbose = TRUE
)
```

## Arguments

- input.objects:

  [character](https://rdrr.io/r/base/character.html) or
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects (**required**): Character vector with path and files names
  with ".bin" or ".binx" extension (e.g.
  `input.objects = c("path/file1.bin", "path/file2.bin")` or a list of
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  objects (e.g. `input.objects = c(object1, object2)`).

- output.file:

  [character](https://rdrr.io/r/base/character.html) (*optional*): File
  output path and name. If no value is given, a
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  object returned instead of a file.

- keep.position.number:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  Allows keeping the original position numbers of the input objects.
  Otherwise the position numbers are recalculated.

- position.number.append.gap:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*): Set
  the position number gap between merged BIN-file sets, if the option
  `keep.position.number = FALSE` is used. See details for further
  information.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

## Value

Returns a
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object or writes to the BIN-file specified by `output.file`.

## Details

The function allows merging different measurements to one file or one
object. The record IDs are recalculated for the new object. Other values
are kept for each object. The number of input objects is not limited.

`position.number.append.gap` option

If the option `keep.position.number = FALSE` is used, the position
numbers of the new data set are recalculated by adding the highest
position number of the previous data set to the each position number of
the next data set. For example: The highest position number is 48, then
this number will be added to all other position numbers of the next data
set (e.g. 1 + 48 = 49)

However, there might be cases where an additional addend (summand) is
needed before the next position starts. Example:

- Position number set (A): `1,3,5,7`

- Position number set (B): `1,3,5,7`

With no additional summand the new position numbers would be:
`1,3,5,7,8,9,10,11`. That might be unwanted. Using the argument
`position.number.append.gap = 1` it will become:
`1,3,5,7,9,11,13,15,17`.

## Note

The validity of the output objects is not further checked.

## Function version

0.2.10

## How to cite

Kreutzer, S., 2025. merge_Risoe.BINfileData(): Merge Risoe.BINfileData
objects or Risoe BIN-files. Function version 0.2.10. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Duller, G.A.T., 2007. Analyst (Version 3.24) (manual). Aberystwyth
University, Aberystwyth.

## See also

[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##merge two objects
data(ExampleData.BINfileData, envir = environment())

object1 <- CWOSL.SAR.Data
object2 <- CWOSL.SAR.Data

object.new <- merge_Risoe.BINfileData(c(object1, object2))
```
