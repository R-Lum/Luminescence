# Import Risø BIN/BINX-files into R

Import a `*.bin` or a `*.binx` file produced by a Risø DA15 and DA20
TL/OSL reader into R.

## Usage

``` r
read_BIN2R(
  file,
  show.raw.values = FALSE,
  position = NULL,
  n.records = NULL,
  zero_data.rm = TRUE,
  duplicated.rm = FALSE,
  fastForward = FALSE,
  show.record.number = FALSE,
  txtProgressBar = TRUE,
  forced.VersionNumber = NULL,
  ignore.RECTYPE = FALSE,
  pattern = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) or
  [list](https://rdrr.io/r/base/list.html) (**required**): path and file
  name of the BIN/BINX file (URLs are supported). If input is a `list`
  it should comprise only `character`s representing valid path and
  BIN/BINX-file names. Alternatively, the input character can be just a
  directory (path), in which case the function tries to detect and
  import all BIN/BINX files found in the directory.

- show.raw.values:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): shows
  raw values from BIN-file for `LTYPE`, `DTYPE` and `LIGHTSOURCE`
  without translation in characters. Can be provided as `list` if `file`
  is a `list`.

- position:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): imports
  only the selected position. Note: the import performance will not
  benefit by any selection made here. Can be provided as `list` if
  `file` is a `list`.

- n.records:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): limits
  the number of imported records to the provided record id (e.g.,
  `n.records = 1:10` imports the first ten records, while
  `n.records = 3` imports only record number 3. Can be used in
  combination with `show.record.number` for debugging purposes, e.g.
  corrupt BIN-files. Can be provided as `list` if `file` is a `list`.

- zero_data.rm:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  remove erroneous data with no count values. As such data are usually
  not needed for the subsequent data analysis they will be removed by
  default. Can be provided as `list` if `file` is a `list`.

- duplicated.rm:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  remove duplicated entries if `TRUE`. This may happen due to an
  erroneous produced BIN/BINX-file. This option compares only
  predecessor and successor. Can be provided as `list` if `file` is a
  `list`.

- fastForward:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  `TRUE` for a more efficient data processing only a list of
  `RLum.Analysis` objects is returned instead of a
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  object. Can be provided as `list` if `file` is a `list`.

- show.record.number:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): shows
  record number of the imported record, for debugging usage only. Can be
  provided as `list` if `file` is a `list`. Ignored if
  `verbose = FALSE`.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar. Ignored if `verbose = FALSE`.

- forced.VersionNumber:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): allows to
  cheat the version number check in the function by own values for cases
  where the BIN-file version is not supported. Can be provided as `list`
  if `file` is a `list`.

  **Note:** The usage is at own risk, only supported BIN-file versions
  have been tested.

- ignore.RECTYPE:

  [logical](https://rdrr.io/r/base/logical.html) or
  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): this
  argument allows to ignore values in the byte 'RECTYPE' (BIN-file
  version 08), in case there are not documented or faulty set. In this
  case the corrupted records are skipped. If the setting is
  [numeric](https://rdrr.io/r/base/numeric.html) (e.g.,
  `ignore.RECTYPE = 128`), records of those type are ignored for import.

- pattern:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  regular expression pattern passed to
  [list.files](https://rdrr.io/r/base/list.files.html) to construct a
  list of files to read (used only when a path is provided).

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  further arguments that will be passed to the function
  [Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md).
  Please note that any matching argument automatically sets
  `fastForward = TRUE`

## Value

Returns an S4
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
object containing two slots:

- METADATA:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) containing all
  variables stored in the BIN-file.

- DATA:

  A [list](https://rdrr.io/r/base/list.html) containing a numeric
  [vector](https://rdrr.io/r/base/vector.html) of the measured data. The
  ID corresponds to the record ID in METADATA.

If `fastForward = TRUE` a list of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object is returned. The internal coercing is done using the function
[Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md)

## Details

The binary data file is parsed byte by byte following the data structure
published in the Appendices of the Analyst manual p. 42.

For the general BIN/BINX-file structure, the reader is referred to the
Risø website: <https://www.fysik.dtu.dk>

## Note

The function works for BIN/BINX-format versions 03, 04, 05, 06, 07 and
08. The version number depends on the used Sequence Editor.

## Function version

0.18

## How to cite

Kreutzer, S., Fuchs, M.C., Colombo, M., 2025. read_BIN2R(): Import Risø
BIN/BINX-files into R. Function version 0.18. In: Kreutzer, S., Burow,
C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

DTU Nutech, 2016. The Sequence Editor, Users Manual, February, 2016.
<https://www.fysik.dtu.dk>

## See also

[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[base::readBin](https://rdrr.io/r/base/readBin.html),
[merge_Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/merge_Risoe.BINfileData.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
[utils::txtProgressBar](https://rdrr.io/r/utils/txtProgressBar.html),
[list.files](https://rdrr.io/r/base/list.files.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Margret C. Fuchs, HZDR Freiberg, (Germany)  
Marco Colombo, Institute of Geography, Heidelberg University (Germany)  
based on information provided by Torben Lapp and Karsten Bracht Nielsen
(Risø DTU, Denmark) , RLum Developer Team

## Examples

``` r
file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- read_BIN2R(file)
#> 
#> [read_BIN2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  BINfile_V8.binx
#>  n_rec: 2
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#>   >> 2 records read successfully
temp
#> 
#> [Risoe.BINfileData object]
#> 
#>  BIN/BINX version:     8
#>  File name:            ExampleData.BINfileData
#>  Object date:          060920
#>  User:                 Default
#>  System ID:            0 (unknown)
#>  Overall records:      2
#>  Records type:         TL    (n = 2)
#>  Position range:       1 : 2
#>  Run range:            1 : 1
#>  Set range:            2 : 2
```
