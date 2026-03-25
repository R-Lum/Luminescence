# Import Risø BINX-data from the BINX Log File

The function attempts to extract meaningful measurement data from the
log file (`.txt`) stored along with a BINX file if this option was
chosen. This is particularly helpful if the BINX file is broken and the
data would be lost otherwise.

## Usage

``` r
read_BINXLOG2R(file, verbose = TRUE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of one or multiple ASCII log files to read; it can be a path to a
  directory, in which case the function tries to read all files it finds
  (there is no file extension check, so it may fail ungracefully on
  binary files).

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  further arguments that will be passed to the function (currently not
  used)

## Value

Returns an S4
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object. Results are returned as a list when multiple files are processed
or `file` is a list.

## Details

This function is basically a hack trying to make the most out of a
broken file in case at least the log file is salvageable. Please do not
expect black magic; the extract is very basic, and the function was
written to work for TL, OSL, IRSL curves only. If it breaks, it breaks.

**TL curve extraction** Since the BIN/BINX do not contain x-values, they
also cannot be created magically here. To avoid problems with the
conversion, we assume always BIN-file version `3` for the conversion.
This should be okay in most cases, however, it may become problematic if
you have data for dedicated curve analysis.

## Note

This function tries to extract data from a log file produced for
debugging purposes. There is no guarantee regarding the format
stability, and the function is meant as a last resort if your BIN/BINX
files are unusable.

## Function version

0.1.1

## See also

[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[base::readLines](https://rdrr.io/r/base/readLines.html),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[list.files](https://rdrr.io/r/base/list.files.html)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2026. read_BINXLOG2R(): Import Risø BINX-data from the
BINX Log File. Function version 0.1.1. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
Bluszcz, A., 2026. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.2.1. https://r-lum.github.io/Luminescence/

## Examples

``` r
## read log file (RLum.Analysis)
file <-  system.file("extdata/BINX_IRSL_LOG.TXT", package = "Luminescence")
object <-  read_BINXLOG2R(file = file)
#> 
#> [read_BINXLOG2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  BINX_IRSL_LOG.TXT
#>  size:  13.39 kB

## convert to RisoeBINFile-class object
object <- convert_RLum2Risoe.BINfileData(object)

## export as BINX
if (FALSE) { # \dontrun{
 write_R2BIN(t, file = tempfile(), version = "04")
} # }
```
