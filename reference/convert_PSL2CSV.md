# Export PSL-file(s) to CSV-files

This function is a wrapper function around the functions
[read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)
and
[write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md)
and it imports an PSL-file (SUERC portable OSL reader file format) and
directly exports its content to CSV-files. If nothing is set for the
argument `path`
([write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md))
the input folder will become the output folder.

## Usage

``` r
convert_PSL2CSV(file, extract_raw_data = FALSE, single_table = FALSE, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  name of the PSL-file to be converted to CSV-files

- extract_raw_data:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable raw data extraction. The PSL files imported into R
  contain an element `$raw_data`, which provides a few more information
  (e.g., count errors), sometimes it makes sense to use this data of the
  more compact standard values created by
  [read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)

- single_table:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the creation of single table with n rows and n columns,
  instead of separate
  [data.frame](https://rdrr.io/r/base/data.frame.html) objects. Each
  curve will be represented by two columns for time and counts

- ...:

  further arguments that will be passed to the function
  [read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)
  and
  [write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md)

## Value

The function returns either a CSV-file (or many of them) or for the
option `export = FALSE` a list comprising objects of type
[data.frame](https://rdrr.io/r/base/data.frame.html) and
[matrix](https://rdrr.io/r/base/matrix.html)

## Function version

0.1.2

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[utils::write.table](https://rdrr.io/r/utils/write.table.html),
[write_RLum2CSV](https://r-lum.github.io/Luminescence/reference/write_RLum2CSV.md),
[read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. convert_PSL2CSV(): Export PSL-file(s) to CSV-files.
Function version 0.1.2. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence:
Comprehensive Luminescence Dating Data Analysis. R package version
1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## export into single data.frame
file <- system.file("extdata/DorNie_0016.psl", package="Luminescence")
convert_PSL2CSV(file, export = FALSE, single_table = TRUE)
#> 
#> [read_PSL2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  DorNie_0016.psl
#> $conv_ALU0016_psl
#>     1_USER_t 1_USER_cts 2_IRSL_t 2_IRSL_cts 3_USER_t 3_USER_cts 4_OSL_t
#> 1          1          0        1      16660        1          4       1
#> 2          2         -2        2      16129        2         10       2
#> 3          3         14        3      15860        3         -5       3
#> 4          4          7        4      15773        4         -1       4
#> 5          5          6        5      13509        5         -8       5
#> 6          6         -7        6      15171        6         -6       6
#> 7          7         -1        7      15025        7         13       7
#> 8          8          6        8      14753        8          5       8
#> 9          9         -8        9      14438        9        -24       9
#> 10        10          3       10      14205       10         11      10
#> 11        11         -2       11      14048       11          9      11
#> 12        12          6       12      13886       12          7      12
#> 13        13          0       13      13769       13         -8      13
#> 14        14          6       14      13619       14        -12      14
#> 15        15         -3       15      13365       15         -7      15
#> 16        NA         NA       16      12847       NA         NA      16
#> 17        NA         NA       17      12967       NA         NA      17
#> 18        NA         NA       18      12693       NA         NA      18
#> 19        NA         NA       19      12726       NA         NA      19
#> 20        NA         NA       20      12663       NA         NA      20
#> 21        NA         NA       21      12740       NA         NA      21
#> 22        NA         NA       22      12394       NA         NA      22
#> 23        NA         NA       23      12142       NA         NA      23
#> 24        NA         NA       24      12139       NA         NA      24
#> 25        NA         NA       25      11967       NA         NA      25
#> 26        NA         NA       26      11870       NA         NA      26
#> 27        NA         NA       27      11836       NA         NA      27
#> 28        NA         NA       28      11673       NA         NA      28
#> 29        NA         NA       29      11539       NA         NA      29
#> 30        NA         NA       30      11513       NA         NA      30
#> 31        NA         NA       31      11443       NA         NA      31
#> 32        NA         NA       32      11391       NA         NA      32
#> 33        NA         NA       33      11263       NA         NA      33
#> 34        NA         NA       34      11128       NA         NA      34
#> 35        NA         NA       35      11026       NA         NA      35
#> 36        NA         NA       36      10991       NA         NA      36
#> 37        NA         NA       37      10798       NA         NA      37
#> 38        NA         NA       38      10781       NA         NA      38
#> 39        NA         NA       39      10754       NA         NA      39
#> 40        NA         NA       40      10455       NA         NA      40
#> 41        NA         NA       41      10705       NA         NA      41
#> 42        NA         NA       42      10457       NA         NA      42
#> 43        NA         NA       43      10358       NA         NA      43
#> 44        NA         NA       44      10578       NA         NA      44
#> 45        NA         NA       45      10543       NA         NA      45
#> 46        NA         NA       46      10260       NA         NA      46
#> 47        NA         NA       47      10263       NA         NA      47
#> 48        NA         NA       48      10209       NA         NA      48
#> 49        NA         NA       49       9936       NA         NA      49
#> 50        NA         NA       50       9943       NA         NA      50
#> 51        NA         NA       51       9782       NA         NA      51
#> 52        NA         NA       52       9836       NA         NA      52
#> 53        NA         NA       53       9876       NA         NA      53
#> 54        NA         NA       54       8814       NA         NA      54
#> 55        NA         NA       55       9854       NA         NA      55
#> 56        NA         NA       56       9571       NA         NA      56
#> 57        NA         NA       57       9581       NA         NA      57
#> 58        NA         NA       58       9433       NA         NA      58
#> 59        NA         NA       59       9568       NA         NA      59
#> 60        NA         NA       60       9466       NA         NA      60
#> 61        NA         NA       61       9362       NA         NA      61
#> 62        NA         NA       62       9267       NA         NA      62
#> 63        NA         NA       63       9240       NA         NA      63
#> 64        NA         NA       64       9425       NA         NA      64
#> 65        NA         NA       65       9129       NA         NA      65
#> 66        NA         NA       66       9132       NA         NA      66
#> 67        NA         NA       67       9036       NA         NA      67
#> 68        NA         NA       68       9166       NA         NA      68
#> 69        NA         NA       69       8859       NA         NA      69
#> 70        NA         NA       70       8973       NA         NA      70
#> 71        NA         NA       71       8914       NA         NA      71
#> 72        NA         NA       72       8921       NA         NA      72
#> 73        NA         NA       73       8819       NA         NA      73
#> 74        NA         NA       74       8935       NA         NA      74
#> 75        NA         NA       75       8711       NA         NA      75
#> 76        NA         NA       76       8862       NA         NA      76
#> 77        NA         NA       77       8847       NA         NA      77
#> 78        NA         NA       78       8611       NA         NA      78
#> 79        NA         NA       79       8785       NA         NA      79
#> 80        NA         NA       80       8583       NA         NA      80
#> 81        NA         NA       81       8340       NA         NA      81
#> 82        NA         NA       82       8481       NA         NA      82
#> 83        NA         NA       83       8525       NA         NA      83
#> 84        NA         NA       84       8286       NA         NA      84
#> 85        NA         NA       85       8547       NA         NA      85
#> 86        NA         NA       86       8292       NA         NA      86
#> 87        NA         NA       87       8415       NA         NA      87
#> 88        NA         NA       88       8231       NA         NA      88
#> 89        NA         NA       89       8054       NA         NA      89
#> 90        NA         NA       90       8489       NA         NA      90
#> 91        NA         NA       91       8075       NA         NA      91
#> 92        NA         NA       92       8076       NA         NA      92
#> 93        NA         NA       93       8048       NA         NA      93
#> 94        NA         NA       94       7913       NA         NA      94
#> 95        NA         NA       95       7952       NA         NA      95
#> 96        NA         NA       96       7993       NA         NA      96
#> 97        NA         NA       97       7914       NA         NA      97
#> 98        NA         NA       98       7909       NA         NA      98
#> 99        NA         NA       99       7863       NA         NA      99
#> 100       NA         NA      100       7966       NA         NA     100
#>     4_OSL_cts 5_USER_t 5_USER_cts
#> 1       88023        1        -22
#> 2       85585        2         -1
#> 3       83394        3         25
#> 4       80840        4          0
#> 5       78826        5         12
#> 6       76523        6          4
#> 7       74442        7         12
#> 8       72370        8        -13
#> 9       70429        9         26
#> 10      69502       10        -16
#> 11      67389       11        -26
#> 12      65571       12        -12
#> 13      64550       13         28
#> 14      62602       14         -8
#> 15      61530       15        -10
#> 16      59973       NA         NA
#> 17      58776       NA         NA
#> 18      56702       NA         NA
#> 19      56463       NA         NA
#> 20      55544       NA         NA
#> 21      54560       NA         NA
#> 22      53655       NA         NA
#> 23      51510       NA         NA
#> 24      51533       NA         NA
#> 25      50612       NA         NA
#> 26      49634       NA         NA
#> 27      48865       NA         NA
#> 28      47477       NA         NA
#> 29      47611       NA         NA
#> 30      46493       NA         NA
#> 31      46228       NA         NA
#> 32      45406       NA         NA
#> 33      44412       NA         NA
#> 34      43986       NA         NA
#> 35      42660       NA         NA
#> 36      42668       NA         NA
#> 37      42134       NA         NA
#> 38      41714       NA         NA
#> 39      41122       NA         NA
#> 40      40480       NA         NA
#> 41      39864       NA         NA
#> 42      39509       NA         NA
#> 43      38667       NA         NA
#> 44      34400       NA         NA
#> 45      37490       NA         NA
#> 46      37932       NA         NA
#> 47      37080       NA         NA
#> 48      36770       NA         NA
#> 49      35939       NA         NA
#> 50      35905       NA         NA
#> 51      35136       NA         NA
#> 52      34799       NA         NA
#> 53      34099       NA         NA
#> 54      33965       NA         NA
#> 55      33632       NA         NA
#> 56      33346       NA         NA
#> 57      32733       NA         NA
#> 58      32818       NA         NA
#> 59      31939       NA         NA
#> 60      31811       NA         NA
#> 61      31846       NA         NA
#> 62      31014       NA         NA
#> 63      31358       NA         NA
#> 64      30640       NA         NA
#> 65      30371       NA         NA
#> 66      30182       NA         NA
#> 67      29531       NA         NA
#> 68      29751       NA         NA
#> 69      29212       NA         NA
#> 70      28599       NA         NA
#> 71      28182       NA         NA
#> 72      28644       NA         NA
#> 73      28154       NA         NA
#> 74      27587       NA         NA
#> 75      27537       NA         NA
#> 76      27275       NA         NA
#> 77      27064       NA         NA
#> 78      27142       NA         NA
#> 79      26655       NA         NA
#> 80      26503       NA         NA
#> 81      26296       NA         NA
#> 82      25213       NA         NA
#> 83      25774       NA         NA
#> 84      24724       NA         NA
#> 85      25337       NA         NA
#> 86      25361       NA         NA
#> 87      24962       NA         NA
#> 88      24411       NA         NA
#> 89      24427       NA         NA
#> 90      24599       NA         NA
#> 91      24128       NA         NA
#> 92      23943       NA         NA
#> 93      21914       NA         NA
#> 94      23812       NA         NA
#> 95      23516       NA         NA
#> 96      23346       NA         NA
#> 97      22965       NA         NA
#> 98      22955       NA         NA
#> 99      22828       NA         NA
#> 100     22552       NA         NA
#> 


if (FALSE) { # \dontrun{
##select your BIN-file
file <- file.choose()

##convert
convert_PSL2CSV(file)

} # }
```
