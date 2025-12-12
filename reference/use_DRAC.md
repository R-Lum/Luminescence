# Use DRAC to calculate dose rate data

The function provides an interface from R to DRAC. An R-object or a CSV
file is passed to the DRAC website and results are re-imported into R.

## Usage

``` r
use_DRAC(
  file,
  name = NULL,
  print_references = TRUE,
  citation_style = "text",
  ...
)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of a CSV file (formatted according to the DRAC v1.2 CSV template)
  to be sent to the DRAC website for calculation. It can also be a DRAC
  template object obtained from
  [`template_DRAC()`](https://r-lum.github.io/Luminescence/reference/template_DRAC.md),
  which supports also import from CSV-files.

- name:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  Optional user name submitted to DRAC. If `NULL`, a random name will be
  generated.

- print_references:

  (*with default*): Print all references used in the input data table to
  the console.

- citation_style:

  (*with default*): If `print_references = TRUE` this argument
  determines the output style of the used references. Valid options are
  `"Bibtex"`, `"citation"`, `"html"`, `"latex"` or `"R"`. Default is
  `"text"`.

- ...:

  further arguments:

  - `url` [character](https://rdrr.io/r/base/character.html): provide an
    alternative URL to DRAC

  - `ignore_version` [logical](https://rdrr.io/r/base/logical.html):
    ignores the version check, this might come in handy if the version
    has changed, but not the column order

  - `user` [character](https://rdrr.io/r/base/character.html): option to
    provide username for secured site

  - `password` [character](https://rdrr.io/r/base/character.html):
    password for secured site, only works jointly with `user`

  - `verbose` [logical](https://rdrr.io/r/base/logical.html): show or
    hide console output

## Value

Returns an
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object containing the following elements:

- DRAC:

  [list](https://rdrr.io/r/base/list.html): a named list containing the
  following elements in slot `@data`:

  |               |                                                      |                                                  |
  |---------------|------------------------------------------------------|--------------------------------------------------|
  | `$highlights` | [data.frame](https://rdrr.io/r/base/data.frame.html) | summary of 25 most important input/output fields |
  | `$header`     | [character](https://rdrr.io/r/base/character.html)   | HTTP header from the DRAC server response        |
  | `$labels`     | [data.frame](https://rdrr.io/r/base/data.frame.html) | descriptive headers of all input/output fields   |
  | `$content`    | [data.frame](https://rdrr.io/r/base/data.frame.html) | complete DRAC input/output table                 |
  | `$input`      | [data.frame](https://rdrr.io/r/base/data.frame.html) | DRAC input table                                 |
  | `$output`     | [data.frame](https://rdrr.io/r/base/data.frame.html) | DRAC output table                                |
  | `references`  | [list](https://rdrr.io/r/base/list.html)             | A list of bib entries of used references         |

- data:

  [character](https://rdrr.io/r/base/character.html) or
  [list](https://rdrr.io/r/base/list.html) path to the input spreadsheet
  or a DRAC template

- call:

  [call](https://rdrr.io/r/base/call.html) the function call

- args:

  [list](https://rdrr.io/r/base/list.html) used arguments

The output should be accessed using the function
[get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).

## Function version

0.17

## How to cite

Kreutzer, S., Dietze, M., Burow, C., 2025. use_DRAC(): Use DRAC to
calculate dose rate data. Function version 0.17. In: Kreutzer, S.,
Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich,
J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass,
D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d.,
2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R
package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose Rate and Age
Calculator for trapped charge dating. Quaternary Geochronology 28,
54-61. doi:10.1016/j.quageo.2015.03.012

## See also

[template_DRAC](https://r-lum.github.io/Luminescence/reference/template_DRAC.md),
[.as.latex.table](https://r-lum.github.io/Luminescence/reference/dot-as.latex.table.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Michael Dietze, GFZ Potsdam (Germany)  
Christoph Burow, University of Cologne (Germany) , RLum Developer Team

## Examples

``` r
## (1) Method using the DRAC spreadsheet

file <-  "/PATH/TO/DRAC_Input_Template.csv"

# send the actual IO template spreadsheet to DRAC
if (FALSE) { # \dontrun{
use_DRAC(file = file)
} # }

## (2) Method using an R template object
# Create a template
input <- template_DRAC(preset = "DRAC-example_quartz")
#> 
#>  -------------------- IMPORTANT NOTE ------------------------
#>   This function returns a DRAC input template to be used in 
#>   conjunction with the use_DRAC() function.  
#>   The template was reproduced with great care, but we do not
#>   take any responsibility and we are not liable for any 
#>   mistakes or unforeseen misbehaviour.
#>   Note that this template is only compatible with DRAC
#>   version 1.1. Before using this template make sure that
#>   this is the correct version, otherwise expect unspecified
#>   errors.
#>   Please ensure you cite the use of DRAC in your work,
#>   published or otherwise. Please cite the website name and
#>   version (e.g. DRAC v1.1) and the accompanying journal
#>   article:
#>   Durcan, J.A., King, G.E., Duller, G.A.T., 2015.
#>   DRAC: Dose rate and age calculation for trapped charge
#>   dating. Quaternary Geochronology 28, 54-61. 
#>   Set 'notification = FALSE' to hide this message. 
#>  -------------------- IMPORTANT NOTE ------------------------
#> 

# Fill the template with values
input$`Project ID` <- "DRAC-Example"
input$`Sample ID` <- "Quartz"
input$`Conversion factors` <- "AdamiecAitken1998"
input$`External U (ppm)` <- 3.4
input$`errExternal U (ppm)` <- 0.51
input$`External Th (ppm)` <- 14.47
input$`errExternal Th (ppm)` <- 1.69
input$`External K (%)` <- 1.2
input$`errExternal K (%)` <- 0.14
input$`Calculate external Rb from K conc?` <- "N"
input$`Calculate internal Rb from K conc?` <- "N"
input$`Scale gammadoserate at shallow depths?` <- "N"
input$`Grain size min (microns)` <- 90
#> [[[<-.DRAC.list]]()] Error: Grain size min (microns): found numeric, expected integer -> coercing to integer
input$`Grain size max (microns)` <- 125
#> [[[<-.DRAC.list]]()] Error: Grain size max (microns): found numeric, expected integer -> coercing to integer
input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
input$`errWater content %` <- 2
input$`Depth (m)` <- 2.2
input$`errDepth (m)` <- 0.22
input$`Overburden density (g cm-3)` <- 1.8
input$`errOverburden density (g cm-3)` <- 0.1
input$`Latitude (decimal degrees)` <- 30.0000
input$`Longitude (decimal degrees)` <- 70.0000
input$`Altitude (m)` <- 150
input$`De (Gy)` <- 20
input$`errDe (Gy)` <- 0.2

# use DRAC
if (FALSE) { # \dontrun{
output <- use_DRAC(input)

## export as LaTeX table
.as.latex.table(output)
} # }
```
