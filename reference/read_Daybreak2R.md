# Import measurement data produced by a Daybreak TL/OSL reader into R

Import a TXT-file (ASCII file) or a DAT-file (binary file) produced by a
Daybreak reader into R. The import of the DAT-files is limited to the
file format described for the software TLAPLLIC v.3.2 used for Daybreak
models 1100 and 1150, and for FLConsole (any release) for Daybreak
models 11xx and 2200.

## Usage

``` r
read_Daybreak2R(file, raw = FALSE, verbose = TRUE, txtProgressBar = TRUE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) or
  [list](https://rdrr.io/r/base/list.html) (**required**): name of one
  or multiple `.DAT` or `.txt` files (URLs are supported); it can be the
  path to a directory, in which case the function tries to detect and
  import all files with `.txt` extension.

- raw:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  the input is a DAT-file (binary) a
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  instead of the
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object can be returned for debugging purposes.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable
  [txtProgressBar](https://rdrr.io/r/utils/txtProgressBar.html).

- ...:

  not in use, for compatibility reasons only

## Value

A list of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects (each per position) is returned.

## Note

**`[BETA VERSION]`** This function still needs to be tested properly. In
particular the function has underwent only very rough tests using a few
files.

## Function version

0.4.1

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  
Andrzej Bluszcz, Silesian University of Technology, Gliwice (Poland)  
Antoine Zink, C2RMF, Palais du Louvre, Paris (France)

The ASCII-file import is based on a suggestion by William Amidon and
Andrew Louis Gorin. , RLum Developer Team

## How to cite

Kreutzer, S., Bluszcz, A., Zink, A., 2026. read_Daybreak2R(): Import
measurement data produced by a Daybreak TL/OSL reader into R. Function
version 0.4.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A., 2026. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.2.0. https://r-lum.github.io/Luminescence/

## Examples

``` r
if (FALSE) { # \dontrun{
file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
temp <- read_Daybreak2R(file)
} # }
```
