# Create LaTeX tables from data.frames and RLum objects

This function takes a
[data.frame](https://rdrr.io/r/base/data.frame.html) and returns a table
in LaTeX code that can be copied into any TeX document.

## Usage

``` r
.as.latex.table(
  x,
  row.names = NULL,
  col.names = NULL,
  comments = TRUE,
  pos = "c",
  digits = 3,
  rm.zero = TRUE,
  select = NULL,
  split = NULL,
  tabular_only = FALSE,
  ...
)
```

## Arguments

- x:

  [data.frame](https://rdrr.io/r/base/data.frame.html) or `RLum` object
  (**required**)

- row.names:

  currently unused

- col.names:

  currently unused

- comments:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  insert LaTeX comments

- pos:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  `character` of length one specifying the alignment of each column,
  e.g., `pos = 'clr'` for a three column data frame and center, left and
  right alignment

- digits:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  number of digits to be displayed (numeric fields)

- rm.zero:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  remove columns containing only zeros, however, this might not be
  wanted in all cases

- select:

  [character](https://rdrr.io/r/base/character.html) (*optional*): a
  [character](https://rdrr.io/r/base/character.html) vector passed to
  [subset](https://rdrr.io/r/base/subset.html)

- split:

  [integer](https://rdrr.io/r/base/integer.html) (*optional*): an
  [integer](https://rdrr.io/r/base/integer.html) specifying the number
  of individual tables the
  [data.frame](https://rdrr.io/r/base/data.frame.html) is split into.
  Useful for wide tables. Currently unused.

- tabular_only:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  `TRUE` only the tabular but not the table environment is returned.
  This gives a lot of additional flexibility at hand

- ...:

  options: `verbose`

## Value

Returns LaTeX code

## Function version

0.2.1

## Author

Christoph Burow, University of Cologne (Germany)  
Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Burow, C., Kreutzer, S., 2025. .as.latex.table(): Create LaTeX tables
from data.frames and RLum objects. Function version 0.2.1. In: Kreutzer,
S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M.,
Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
 df <- data.frame(x = 1:10, y = letters[1:10])
.as.latex.table(df)
#> % add usepackage{adjustbox} to latex preamble 
#> \begin{table}[ht] 
#>   \centering 
#>   \begin{adjustbox}{max width=\textwidth} 
#>   \begin{tabular}{cc}
#>      \hline 
#>  \multicolumn{1}{p{2cm}}{\centering x } & 
#>  \multicolumn{1}{p{2cm}}{\centering y }\\ 
#> \hline 
#>  1.000 & a \\ 
#>  2.000 & b \\ 
#>  3.000 & c \\ 
#>  4.000 & d \\ 
#>  5.000 & e \\ 
#>  6.000 & f \\ 
#>  7.000 & g \\ 
#>  8.000 & h \\ 
#>  9.000 & i \\ 
#> 10.000 & j \\ 
#>      \hline 
#>    \end{tabular} 
#>    \end{adjustbox} 
#> \end{table}
.as.latex.table(df, pos = "lr")
#> % add usepackage{adjustbox} to latex preamble 
#> \begin{table}[ht] 
#>   \centering 
#>   \begin{adjustbox}{max width=\textwidth} 
#>   \begin{tabular}{lr}
#>      \hline 
#>  \multicolumn{1}{p{2cm}}{\centering x } & 
#>  \multicolumn{1}{p{2cm}}{\centering y }\\ 
#> \hline 
#>  1.000 & a \\ 
#>  2.000 & b \\ 
#>  3.000 & c \\ 
#>  4.000 & d \\ 
#>  5.000 & e \\ 
#>  6.000 & f \\ 
#>  7.000 & g \\ 
#>  8.000 & h \\ 
#>  9.000 & i \\ 
#> 10.000 & j \\ 
#>      \hline 
#>    \end{tabular} 
#>    \end{adjustbox} 
#> \end{table}
.as.latex.table(df, select = "y", pos = "r")
#> % add usepackage{adjustbox} to latex preamble 
#> \begin{table}[ht] 
#>   \centering 
#>   \begin{adjustbox}{max width=\textwidth} 
#>   \begin{tabular}{r}
#>      \hline 
#>  \multicolumn{1}{p{2cm}}{\centering y }\\ 
#> \hline 
#> a \\ 
#> b \\ 
#> c \\ 
#> d \\ 
#> e \\ 
#> f \\ 
#> g \\ 
#> h \\ 
#> i \\ 
#> j \\ 
#>      \hline 
#>    \end{tabular} 
#>    \end{adjustbox} 
#> \end{table}
```
