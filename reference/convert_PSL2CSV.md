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

The function returns either a CSV-file (or many of them) or, when
`export = FALSE`, a list of
[data.frame](https://rdrr.io/r/base/data.frame.html) and
[matrix](https://rdrr.io/r/base/matrix.html) objects.

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

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2026. convert_PSL2CSV(): Export PSL-file(s) to CSV-files.
Function version 0.1.2. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs,
M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe,
A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret,
J., Colombo, M., Steinbuch, L., Boer, A.d., Bluszcz, A., 2026.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.2.0. https://r-lum.github.io/Luminescence/

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
#>     1_USER (PMT)_t 1_USER (PMT)_cts 2_IRSL (PMT)_t 2_IRSL (PMT)_cts
#> 1                1                0              1            16660
#> 2                2               -2              2            16129
#> 3                3               14              3            15860
#> 4                4                7              4            15773
#> 5                5                6              5            13509
#> 6                6               -7              6            15171
#> 7                7               -1              7            15025
#> 8                8                6              8            14753
#> 9                9               -8              9            14438
#> 10              10                3             10            14205
#> 11              11               -2             11            14048
#> 12              12                6             12            13886
#> 13              13                0             13            13769
#> 14              14                6             14            13619
#> 15              15               -3             15            13365
#> 16              NA               NA             16            12847
#> 17              NA               NA             17            12967
#> 18              NA               NA             18            12693
#> 19              NA               NA             19            12726
#> 20              NA               NA             20            12663
#> 21              NA               NA             21            12740
#> 22              NA               NA             22            12394
#> 23              NA               NA             23            12142
#> 24              NA               NA             24            12139
#> 25              NA               NA             25            11967
#> 26              NA               NA             26            11870
#> 27              NA               NA             27            11836
#> 28              NA               NA             28            11673
#> 29              NA               NA             29            11539
#> 30              NA               NA             30            11513
#> 31              NA               NA             31            11443
#> 32              NA               NA             32            11391
#> 33              NA               NA             33            11263
#> 34              NA               NA             34            11128
#> 35              NA               NA             35            11026
#> 36              NA               NA             36            10991
#> 37              NA               NA             37            10798
#> 38              NA               NA             38            10781
#> 39              NA               NA             39            10754
#> 40              NA               NA             40            10455
#> 41              NA               NA             41            10705
#> 42              NA               NA             42            10457
#> 43              NA               NA             43            10358
#> 44              NA               NA             44            10578
#> 45              NA               NA             45            10543
#> 46              NA               NA             46            10260
#> 47              NA               NA             47            10263
#> 48              NA               NA             48            10209
#> 49              NA               NA             49             9936
#> 50              NA               NA             50             9943
#> 51              NA               NA             51             9782
#> 52              NA               NA             52             9836
#> 53              NA               NA             53             9876
#> 54              NA               NA             54             8814
#> 55              NA               NA             55             9854
#> 56              NA               NA             56             9571
#> 57              NA               NA             57             9581
#> 58              NA               NA             58             9433
#> 59              NA               NA             59             9568
#> 60              NA               NA             60             9466
#> 61              NA               NA             61             9362
#> 62              NA               NA             62             9267
#> 63              NA               NA             63             9240
#> 64              NA               NA             64             9425
#> 65              NA               NA             65             9129
#> 66              NA               NA             66             9132
#> 67              NA               NA             67             9036
#> 68              NA               NA             68             9166
#> 69              NA               NA             69             8859
#> 70              NA               NA             70             8973
#> 71              NA               NA             71             8914
#> 72              NA               NA             72             8921
#> 73              NA               NA             73             8819
#> 74              NA               NA             74             8935
#> 75              NA               NA             75             8711
#> 76              NA               NA             76             8862
#> 77              NA               NA             77             8847
#> 78              NA               NA             78             8611
#> 79              NA               NA             79             8785
#> 80              NA               NA             80             8583
#> 81              NA               NA             81             8340
#> 82              NA               NA             82             8481
#> 83              NA               NA             83             8525
#> 84              NA               NA             84             8286
#> 85              NA               NA             85             8547
#> 86              NA               NA             86             8292
#> 87              NA               NA             87             8415
#> 88              NA               NA             88             8231
#> 89              NA               NA             89             8054
#> 90              NA               NA             90             8489
#> 91              NA               NA             91             8075
#> 92              NA               NA             92             8076
#> 93              NA               NA             93             8048
#> 94              NA               NA             94             7913
#> 95              NA               NA             95             7952
#> 96              NA               NA             96             7993
#> 97              NA               NA             97             7914
#> 98              NA               NA             98             7909
#> 99              NA               NA             99             7863
#> 100             NA               NA            100             7966
#>     3_USER (PMT)_t 3_USER (PMT)_cts 4_OSL (PMT)_t 4_OSL (PMT)_cts
#> 1                1                4             1           88023
#> 2                2               10             2           85585
#> 3                3               -5             3           83394
#> 4                4               -1             4           80840
#> 5                5               -8             5           78826
#> 6                6               -6             6           76523
#> 7                7               13             7           74442
#> 8                8                5             8           72370
#> 9                9              -24             9           70429
#> 10              10               11            10           69502
#> 11              11                9            11           67389
#> 12              12                7            12           65571
#> 13              13               -8            13           64550
#> 14              14              -12            14           62602
#> 15              15               -7            15           61530
#> 16              NA               NA            16           59973
#> 17              NA               NA            17           58776
#> 18              NA               NA            18           56702
#> 19              NA               NA            19           56463
#> 20              NA               NA            20           55544
#> 21              NA               NA            21           54560
#> 22              NA               NA            22           53655
#> 23              NA               NA            23           51510
#> 24              NA               NA            24           51533
#> 25              NA               NA            25           50612
#> 26              NA               NA            26           49634
#> 27              NA               NA            27           48865
#> 28              NA               NA            28           47477
#> 29              NA               NA            29           47611
#> 30              NA               NA            30           46493
#> 31              NA               NA            31           46228
#> 32              NA               NA            32           45406
#> 33              NA               NA            33           44412
#> 34              NA               NA            34           43986
#> 35              NA               NA            35           42660
#> 36              NA               NA            36           42668
#> 37              NA               NA            37           42134
#> 38              NA               NA            38           41714
#> 39              NA               NA            39           41122
#> 40              NA               NA            40           40480
#> 41              NA               NA            41           39864
#> 42              NA               NA            42           39509
#> 43              NA               NA            43           38667
#> 44              NA               NA            44           34400
#> 45              NA               NA            45           37490
#> 46              NA               NA            46           37932
#> 47              NA               NA            47           37080
#> 48              NA               NA            48           36770
#> 49              NA               NA            49           35939
#> 50              NA               NA            50           35905
#> 51              NA               NA            51           35136
#> 52              NA               NA            52           34799
#> 53              NA               NA            53           34099
#> 54              NA               NA            54           33965
#> 55              NA               NA            55           33632
#> 56              NA               NA            56           33346
#> 57              NA               NA            57           32733
#> 58              NA               NA            58           32818
#> 59              NA               NA            59           31939
#> 60              NA               NA            60           31811
#> 61              NA               NA            61           31846
#> 62              NA               NA            62           31014
#> 63              NA               NA            63           31358
#> 64              NA               NA            64           30640
#> 65              NA               NA            65           30371
#> 66              NA               NA            66           30182
#> 67              NA               NA            67           29531
#> 68              NA               NA            68           29751
#> 69              NA               NA            69           29212
#> 70              NA               NA            70           28599
#> 71              NA               NA            71           28182
#> 72              NA               NA            72           28644
#> 73              NA               NA            73           28154
#> 74              NA               NA            74           27587
#> 75              NA               NA            75           27537
#> 76              NA               NA            76           27275
#> 77              NA               NA            77           27064
#> 78              NA               NA            78           27142
#> 79              NA               NA            79           26655
#> 80              NA               NA            80           26503
#> 81              NA               NA            81           26296
#> 82              NA               NA            82           25213
#> 83              NA               NA            83           25774
#> 84              NA               NA            84           24724
#> 85              NA               NA            85           25337
#> 86              NA               NA            86           25361
#> 87              NA               NA            87           24962
#> 88              NA               NA            88           24411
#> 89              NA               NA            89           24427
#> 90              NA               NA            90           24599
#> 91              NA               NA            91           24128
#> 92              NA               NA            92           23943
#> 93              NA               NA            93           21914
#> 94              NA               NA            94           23812
#> 95              NA               NA            95           23516
#> 96              NA               NA            96           23346
#> 97              NA               NA            97           22965
#> 98              NA               NA            98           22955
#> 99              NA               NA            99           22828
#> 100             NA               NA           100           22552
#>     5_USER (PMT)_t 5_USER (PMT)_cts
#> 1                1              -22
#> 2                2               -1
#> 3                3               25
#> 4                4                0
#> 5                5               12
#> 6                6                4
#> 7                7               12
#> 8                8              -13
#> 9                9               26
#> 10              10              -16
#> 11              11              -26
#> 12              12              -12
#> 13              13               28
#> 14              14               -8
#> 15              15              -10
#> 16              NA               NA
#> 17              NA               NA
#> 18              NA               NA
#> 19              NA               NA
#> 20              NA               NA
#> 21              NA               NA
#> 22              NA               NA
#> 23              NA               NA
#> 24              NA               NA
#> 25              NA               NA
#> 26              NA               NA
#> 27              NA               NA
#> 28              NA               NA
#> 29              NA               NA
#> 30              NA               NA
#> 31              NA               NA
#> 32              NA               NA
#> 33              NA               NA
#> 34              NA               NA
#> 35              NA               NA
#> 36              NA               NA
#> 37              NA               NA
#> 38              NA               NA
#> 39              NA               NA
#> 40              NA               NA
#> 41              NA               NA
#> 42              NA               NA
#> 43              NA               NA
#> 44              NA               NA
#> 45              NA               NA
#> 46              NA               NA
#> 47              NA               NA
#> 48              NA               NA
#> 49              NA               NA
#> 50              NA               NA
#> 51              NA               NA
#> 52              NA               NA
#> 53              NA               NA
#> 54              NA               NA
#> 55              NA               NA
#> 56              NA               NA
#> 57              NA               NA
#> 58              NA               NA
#> 59              NA               NA
#> 60              NA               NA
#> 61              NA               NA
#> 62              NA               NA
#> 63              NA               NA
#> 64              NA               NA
#> 65              NA               NA
#> 66              NA               NA
#> 67              NA               NA
#> 68              NA               NA
#> 69              NA               NA
#> 70              NA               NA
#> 71              NA               NA
#> 72              NA               NA
#> 73              NA               NA
#> 74              NA               NA
#> 75              NA               NA
#> 76              NA               NA
#> 77              NA               NA
#> 78              NA               NA
#> 79              NA               NA
#> 80              NA               NA
#> 81              NA               NA
#> 82              NA               NA
#> 83              NA               NA
#> 84              NA               NA
#> 85              NA               NA
#> 86              NA               NA
#> 87              NA               NA
#> 88              NA               NA
#> 89              NA               NA
#> 90              NA               NA
#> 91              NA               NA
#> 92              NA               NA
#> 93              NA               NA
#> 94              NA               NA
#> 95              NA               NA
#> 96              NA               NA
#> 97              NA               NA
#> 98              NA               NA
#> 99              NA               NA
#> 100             NA               NA
#> 

if (FALSE) { # \dontrun{
##select your BIN-file
file <- file.choose()

##convert
convert_PSL2CSV(file)
} # }
```
