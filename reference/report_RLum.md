# Create a HTML-report for (RLum) objects

Create a HTML-report for (RLum) objects

## Usage

``` r
report_RLum(
  object,
  file = tempfile(),
  title = "RLum.Report",
  compact = TRUE,
  timestamp = TRUE,
  show_report = TRUE,
  launch.browser = FALSE,
  css.file = NULL,
  quiet = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- object:

  (**required**): The object to be reported on, preferably of any
  `RLum`-class.

- file:

  [character](https://rdrr.io/r/base/character.html) (*with default*): A
  character string naming the output file. If no filename is provided a
  temporary file is created.

- title:

  [character](https://rdrr.io/r/base/character.html) (*with default*): A
  character string specifying the title of the document.

- compact:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): When
  `TRUE` the following report components are hidden: `@.pid`, `@.uid`,
  `'Object structure'`, `'Session Info'` and only the first and last 5
  rows of long matrices and data frames are shown. See details.

- timestamp:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  `TRUE` to add a timestamp to the filename (suffix).

- show_report:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): If
  set to `TRUE` the function tries to display the report output in the
  local viewer, e.g., within *RStudio* after rendering.

- launch.browser:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  `TRUE` to open the HTML file in the system's default web browser after
  it has been rendered.

- css.file:

  [character](https://rdrr.io/r/base/character.html) (*optional*): Path
  to a CSS file to change the default styling of the HTML document.

- quiet:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  `TRUE` to suppress printing of the pandoc command line.

- clean:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  `TRUE` to clean intermediate files created during rendering.

- ...:

  further arguments passed to or from other methods and to control the
  document's structure (see details).

## Value

Writes a HTML and .Rds file.

## Details

This function creates a HTML-report for a given object, listing its
complete structure and content. The object itself is saved as a
serialised .Rds file. The report file serves both as a convenient way of
browsing through objects with complex data structures as well as a mean
of properly documenting and saving objects.

The HTML report is created with
[rmarkdown::render](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
and has the following structure:

|                    |                                                                                    |
|--------------------|------------------------------------------------------------------------------------|
| **Section**        | **Description**                                                                    |
| `Header`           | A summary of general characteristics of the object                                 |
| `Object content`   | A comprehensive list of the complete structure and content of the provided object. |
| `Object structure` | Summary of the objects structure given as a table                                  |
| `File`             | Information on the saved RDS file                                                  |
| `Session Info`     | Captured output from [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)   |
| `Plots`            | (*optional*) For `RLum-class` objects a variable number of plots                   |

The structure of the report can be controlled individually by providing
one or more of the following arguments (all `logical`):

|              |                                                |
|--------------|------------------------------------------------|
| **Argument** | **Description**                                |
| `header`     | Hide or show general information on the object |
| `main`       | Hide or show the object's content              |
| `structure`  | Hide or show object's structure                |
| `rds`        | Hide or show information on the saved RDS file |
| `session`    | Hide or show the session info                  |
| `plot`       | Hide or show the plots (depending on object)   |

Note that these arguments have higher precedence than `compact`.

Further options that can be provided via the `...` argument:

|               |                                                                                                                                                                                                                                                  |
|---------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Argument**  | **Description**                                                                                                                                                                                                                                  |
| `short_table` | If `TRUE` only show the first and last 5 rows of long tables.                                                                                                                                                                                    |
| `theme`       | Specifies the Bootstrap theme to use for the report. Valid themes include `"default"`, `"cerulean"`, `"journal"`, `"flatly"`, `"readable"`, `"spacelab"`, `"united"`, `"cosmo"`, `"lumen"`, `"paper"`, `"sandstone"`, `"simplex"`, and `"yeti"`. |
| `highlight`   | Specifies the syntax highlighting style. Supported styles include `"default"`, `"tango"`, `"pygments"`, `"kate"`, `"monochrome"`, `"espresso"`, `"zenburn"`, `"haddock"`, and `"textmate"`.                                                      |
| `css`         | `TRUE` or `FALSE` to enable/disable custom CSS styling                                                                                                                                                                                           |

The following arguments can be used to customise the report via CSS
(Cascading Style Sheets):

|                 |                                                                                 |
|-----------------|---------------------------------------------------------------------------------|
| **Argument**    | **Description**                                                                 |
| `font_family`   | Define the font family of the HTML document (default: `"arial"`)                |
| `headings_size` | Size of the `<h1>` to `<h6>` tags used to define HTML headings (default: 166%). |
| `content_color` | Colour of the object's content (default: \#a72925).                             |

Note that these arguments must all be of class
[character](https://rdrr.io/r/base/character.html) and follow standard
CSS syntax. For exhaustive CSS styling you can provide a custom CSS file
for argument `css.file`. CSS styling can be turned of using
`css = FALSE`.

## Note

This function requires the R packages 'rmarkdown', 'pander' and
'rstudioapi'.

## Function version

0.1.5

## See also

[rmarkdown::render](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
[pander::pander_return](https://rdrr.io/pkg/pander/man/pander_return.html),
[pander::openFileInOS](https://rdrr.io/pkg/pander/man/openFileInOS.html),
[rstudioapi::viewer](https://rstudio.github.io/rstudioapi/reference/viewer.html),
[browseURL](https://rdrr.io/r/utils/browseURL.html)

## Author

Christoph Burow, University of Cologne (Germany), Sebastian Kreutzer,
Institute of Geography, Heidelberg University (Germany)  
, RLum Developer Team

## How to cite

Burow, C., Kreutzer, S., 2025. report_RLum(): Create a HTML-report for
(RLum) objects. Function version 0.1.5. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
if (FALSE) { # \dontrun{
## Example: RLum.Results ----

# load example data
data("ExampleData.DeValues")

# apply the MAM-3 age model and save results
mam <- calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.2)

# create the HTML report
report_RLum(object = mam, file = "~/CA1_MAM.Rmd",
            timestamp = FALSE,
            title = "MAM-3 for sample CA1")

# when creating a report the input file is automatically saved to a
# .Rds file (see saveRDS()).
mam_report <- readRDS("~/CA1_MAM.Rds")
all.equal(mam, mam_report)


## Example: Temporary file & Viewer/Browser ----

# (a)
# Specifying a filename is not necessarily required. If no filename is provided,
# the report is rendered in a temporary file. If you use the RStudio IDE, the
# temporary report is shown in the interactive Viewer pane.
report_RLum(object = mam)

# (b)
# Additionally, you can view the HTML report in your system's default web browser.
report_RLum(object = mam, launch.browser = TRUE)


## Example: RLum.Analysis ----

data("ExampleData.RLum.Analysis")

# create the HTML report (note that specifying a file
# extension is not necessary)
report_RLum(object = IRSAR.RF.Data, file = "~/IRSAR_RF")


## Example: RLum.Data.Curve ----

data.curve <- get_RLum(IRSAR.RF.Data)[[1]]

# create the HTML report
report_RLum(object = data.curve, file = "~/Data_Curve")

## Example: Any other object ----
x <- list(x = 1:10,
          y = runif(10, -5, 5),
          z = data.frame(a = LETTERS[1:20], b = dnorm(0:9)),
          NA)

report_RLum(object = x, file = "~/arbitray_list")
} # }
```
