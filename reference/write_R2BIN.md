# Export Risoe.BINfileData into Risø BIN/BINX-file

Exports a `Risoe.BINfileData` object in a `*.bin` or `*.binx` file that
can be opened by the Analyst software or other Risø software.

## Usage

``` r
write_R2BIN(
  object,
  file,
  version,
  compatibility.mode = FALSE,
  verbose = TRUE,
  txtProgressBar = TRUE
)
```

## Arguments

- object:

  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  (**required**): input object to be stored in a bin file.

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  file name and path of the output file

  - `[WIN]`: `write_R2BIN(object, "C:/Desktop/test.bin")`

  - `[MAC/LINUX]`: `write_R2BIN("/User/test/Desktop/test.bin")`

- version:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  version number for the output file. If no value is provided, the
  highest version number from the
  [Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)
  is taken automatically.

  **Note:** This argument can be used to convert BIN-file versions.

- compatibility.mode:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): this
  option recalculates the position values if necessary and set the max.
  value to 48. The old position number is appended as comment (e.g.,
  'OP: 70). This option accounts for potential compatibility problems
  with the Analyst software. It further limits the maximum number of
  points per curve to 9,999. If a curve contains more data the curve
  data get binned using the smallest possible bin width.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar. Ignored if `verbose = FALSE`.

## Value

Write a binary file and returns the name and path of the file as
[character](https://rdrr.io/r/base/character.html).

## Details

The structure of the exported binary data follows the data structure
published in the Appendices of the *Analyst* manual p. 42.

If `LTYPE`, `DTYPE` and `LIGHTSOURCE` are not of type
[character](https://rdrr.io/r/base/character.html), no transformation
into numeric values is done.

## Note

The function just roughly checks the data structures. The validity of
the output data depends on the user.

The validity of the file path is not further checked. BIN-file
conversions using the argument `version` may be a lossy conversion,
depending on the chosen input and output data (e.g., conversion from
version 08 to 07 to 06 to 05 to 04 or 03).

**Warning**

Although the coding was done carefully, it seems that the BIN/BINX-files
produced by Risø DA 15/20 TL/OSL readers slightly differ on the byte
level. No obvious differences are observed in the METADATA, however, the
BIN/BINX-file may not fully compatible, at least not similar to the ones
directly produced by the Risø readers!

ROI definitions (introduced in BIN-file version 8) are not supported!
There are furthermore ignored by the function
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md).

## Function version

0.5.4

## How to cite

Kreutzer, S., 2025. write_R2BIN(): Export Risoe.BINfileData into Risø
BIN/BINX-file. Function version 0.5.4. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

DTU Nutech, 2016. The Sequence Editor, Users Manual, February, 2016.
<https://www.fysik.dtu.dk>

## See also

[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[writeBin](https://rdrr.io/r/base/readBin.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
##load example dataset
file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- read_BIN2R(file)
#> 
#> [read_BIN2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  BINfile_V8.binx
#>  n_rec: 2
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#>   >> 2 records read successfully

##create temporary file path
##(for usage replace by own path)
temp_file <- tempfile(pattern = "output", fileext = ".binx")

##export to temporary file path
write_R2BIN(temp, file = temp_file)
#> 
#> [write_R2BIN()] Exporting ...
#>  path:  /tmp/RtmpJnfeVf
#>  file:  output19697d1d775f.binx
#>  n_rec: 2
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#>   >> 2 records written successfully
```
