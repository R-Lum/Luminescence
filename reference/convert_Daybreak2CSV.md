# Export measurement data produced by a Daybreak luminescence reader to CSV-files

This function is a wrapper function around the functions
[read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.md)
and
[write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md)
and it imports a Daybreak-file (TXT-file, DAT-file) and directly exports
its content to CSV-files. If nothing is set for the argument `path`
([write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md))
the input folder will become the output folder.

## Usage

``` r
convert_Daybreak2CSV(file, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of the Daybreak-file (TXT-file, DAT-file) to be converted to
  CSV-files

- ...:

  further arguments that will be passed to the function
  [read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.md)
  and
  [write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md)

## Value

The function returns either a CSV-file (or many of them) or for the
option `export = FALSE` a list comprising objects of type
[data.frame](https://rdrr.io/r/base/data.frame.html) and
[matrix](https://rdrr.io/r/base/matrix.html)

## Function version

0.1.0

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[utils::write.table](https://rdrr.io/r/utils/write.table.html),
[write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md),
[read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. convert_Daybreak2CSV(): Export measurement data
produced by a Daybreak luminescence reader to CSV-files. Function
version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C.,
Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A.,
Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J.,
Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
if (FALSE) { # \dontrun{
##select your BIN-file
file <- file.choose()

##convert
convert_Daybreak2CSV(file)

} # }
```
