# Attempts to install the development version of the 'Luminescence' package

This function provides a convenient method for installing the
development version of the R package 'Luminescence' directly from
GitHub.

## Usage

``` r
install_DevelopmentVersion(force_install = FALSE, branch = "master")
```

## Arguments

- force_install:

  [logical](https://rdrr.io/r/base/logical.html) (*optional*): If
  `FALSE` (the default) the function produces and prints the required
  code to the console for the user to run manually afterwards. When
  `TRUE` and all requirements are fulfilled (see details) this function
  attempts to install the package itself.

- branch:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  Name of the branch to install. The default value ("master")
  corresponds to the main development branch.

## Value

This function requires user input at the command prompt to choose the
desired development branch to be installed. The required R code to
install the package is then printed to the console.

## Details

This function checks whether the 'devtools' package is currently
installed on the system. If `force_install = TRUE` the functions
attempts to install the chosen development branch via
[devtools::install_github](https://devtools.r-lib.org/reference/remote-reexports.html).

## Examples

``` r
if (FALSE) { # \dontrun{
install_DevelopmentVersion()
} # }
```
