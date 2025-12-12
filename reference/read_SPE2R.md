# Import Princeton Instruments (TM) SPE-file into R

The function imports Princeton Instruments (TM) SPE-files into R and
provides
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
objects as output. The import functionality is based on the file format
description provided by Princeton Instruments and a MatLab script
written by Carl Hall (see references).

## Usage

``` r
read_SPE2R(
  file,
  output.object = "RLum.Data.Image",
  frame.range = NULL,
  txtProgressBar = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  SPE-file name (including path), e.g.

  - `[WIN]`: `read_SPE2R("C:/Desktop/test.spe")`

  - `[MAC/LINUX]`: `read_SPE2R("/User/test/Desktop/test.spe")`.
    Additionally, it can be a URL starting with `http://` or `https://`.

- output.object:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  set the output object type. Allowed types are `"RLum.Data.Spectrum"`,
  `"RLum.Data.Image"` or `"matrix"`.

- frame.range:

  [vector](https://rdrr.io/r/base/vector.html),
  [integer](https://rdrr.io/r/base/integer.html) (*optional*): range of
  frames to read. For example, `frame.range = c(1, 10)` selects only the
  first 10 frames. If not specifie, all available frames (up to a
  maximum of 100 if `output.object = "RLum.Data.Image"`) are read.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar. Ignored if `verbose = FALSE`.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

- ...:

  not used, for compatibility reasons only.

## Value

Depending on the chosen option the functions returns three different
type of objects:

`output.object`

`RLum.Data.Spectrum`

An object of type
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
is returned. Row sums are used to integrate all counts over one channel.

`RLum.Data.Image`

An object of type
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)
is returned. Due to performance reasons the import is aborted for files
containing more than 100 frames. This limitation can be overwritten
manually by using the argument `frame.range`.

`matrix`

Returns a matrix of the form: Rows = Channels, columns = Frames. For the
transformation the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)
is used, meaning that the same results can be obtained by using the
function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md)
on an `RLum.Data.Spectrum` or `RLum.Data.Image` object.

## Note

**The function does not test whether the input data are spectra or
pictures for spatial resolved analysis!**

The function has been successfully tested for SPE format versions 2.x.

*Currently not all information provided by the SPE format are
supported.*

## Function version

0.1.5

## How to cite

Kreutzer, S., 2025. read_SPE2R(): Import Princeton Instruments (TM)
SPE-file into R. Function version 0.1.5. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Princeton Instruments, 2014. Princeton Instruments SPE 3.0 File Format
Specification, Version 1.A (for document URL please use an internet
search machine)

Hall, C., 2012: readSPE.m.
`https://www.mathworks.com/matlabcentral/fileexchange/35940-readspe`

## See also

[readBin](https://rdrr.io/r/base/readBin.html),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
## to run examples uncomment lines and run the code

##(1) Import data as RLum.Data.Spectrum object
#file <- file.choose()
#temp <- read_SPE2R(file)
#temp

##(2) Import data as RLum.Data.Image object
#file <- file.choose()
#temp <- read_SPE2R(file, output.object = "RLum.Data.Image")
#temp

##(3) Import data as matrix object
#file <- file.choose()
#temp <- read_SPE2R(file, output.object = "matrix")
#temp

##(4) Export raw data to csv, if temp is a RLum.Data.Spectrum object
# write.table(x = get_RLum(temp),
#             file = "[your path and filename]",
#             sep = ";", row.names = FALSE)
```
