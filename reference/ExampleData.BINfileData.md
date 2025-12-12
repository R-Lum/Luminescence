# Example data from a SAR OSL and SAR TL measurement for the package Luminescence

Example data from a SAR OSL and TL measurement for package Luminescence
directly extracted from a Risoe BIN-file and provided in an object of
type
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md)

## Format

`CWOSL.SAR.Data`: SAR OSL measurement data

`TL.SAR.Data`: SAR TL measurement data

Each class object contains two slots: (a) `METADATA` is a
[data.frame](https://rdrr.io/r/base/data.frame.html) with all metadata
stored in the BIN file of the measurements and (b) `DATA` contains a
list of vectors of the measured data (usually count values).

## Source

**CWOSL.SAR.Data**

|            |                                                                              |
|------------|------------------------------------------------------------------------------|
| Lab:       | Luminescence Laboratory Bayreuth                                             |
| Lab-Code:  | BT607                                                                        |
| Location:  | Saxony/Germany                                                               |
| Material:  | Middle grain quartz measured on aluminium cups on a Risø TL/OSL DA-15 reader |
| Reference: | unpublished                                                                  |

**TL.SAR.Data**

|            |                                                                                              |
|------------|----------------------------------------------------------------------------------------------|
| Lab:       | Luminescence Laboratory of Cologne                                                           |
| Lab-Code:  | LP1_5                                                                                        |
| Location:  | Spain                                                                                        |
| Material:  | Flint                                                                                        |
| Setup:     | Risoe TL/OSL DA-20 reader (Filter: Semrock Brightline, HC475/50, N2, unpolished steel discs) |
| Reference: | unpublished                                                                                  |
| Remarks:   | dataset limited to one position                                                              |

## Note

Please note that this example data cannot be exported to a BIN-file
using the function `writeR2BIN` as it was generated and implemented in
the package long time ago. In the meantime the BIN-file format changed.

## Version

0.1

## References

**CWOSL.SAR.Data**: unpublished data

**TL.SAR.Data**: unpublished data

## Examples

``` r
## show first 5 elements of the METADATA and DATA elements in the terminal
data(ExampleData.BINfileData, envir = environment())
CWOSL.SAR.Data@METADATA[1:5,]
#>   ID  SEL VERSION LENGTH PREVIOUS NPOINTS LTYPE LOW HIGH RATE TEMPERATURE
#> 1  1 TRUE      03   1272        0     250    TL   0  221    5           0
#> 2  2 TRUE      03   1272     1272     250    TL   0  221    5           0
#> 3  3 TRUE      03   1272     1272     250    TL   0  221    5           0
#> 4  4 TRUE      03   1272     1272     250    TL   0  221    5           0
#> 5  5 TRUE      03   1272     1272     250    TL   0  221    5           0
#>   XCOORD YCOORD TOLDELAY TOLON TOLOFF POSITION RUN     TIME   DATE SEQUENCE
#> 1      0      0        0     0      0        1   1 19:14:32 060920 20100906
#> 2      0      0        0     0      0        2   1 19:16:11 060920 20100906
#> 3      0      0        0     0      0        3   1 19:17:50 060920 20100906
#> 4      0      0        0     0      0        4   1 19:19:30 060920 20100906
#> 5      0      0        0     0      0        5   1 19:21:09 060920 20100906
#>      USER   DTYPE IRR_TIME IRR_TYPE IRR_UNIT BL_TIME BL_UNIT AN_TEMP AN_TIME
#> 1 Default Natural        0        0        0       0       0     220      10
#> 2 Default Natural        0        0        0       0       0     220      10
#> 3 Default Natural        0        0        0       0       0     220      10
#> 4 Default Natural        0        0        0       0       0     220      10
#> 5 Default Natural        0        0        0       0       0     220      10
#>   NORM1 NORM2 NORM3 BG SHIFT SAMPLE                                     COMMENT
#> 1     0     0     0  0     0 BT 607 Main Measurement Middle Grain SachsenLoesse
#> 2     0     0     0  0     0 BT 607 Main Measurement Middle Grain SachsenLoesse
#> 3     0     0     0  0     0 BT 607 Main Measurement Middle Grain SachsenLoesse
#> 4     0     0     0  0     0 BT 607 Main Measurement Middle Grain SachsenLoesse
#> 5     0     0     0  0     0 BT 607 Main Measurement Middle Grain SachsenLoesse
#>   LIGHTSOURCE SET TAG GRAIN LPOWER SYSTEMID                   FNAME
#> 1        None   2   0     0      0        0 ExampleData.BINfileData
#> 2        None   2   0     0      0        0 ExampleData.BINfileData
#> 3        None   2   0     0      0        0 ExampleData.BINfileData
#> 4        None   2   0     0      0        0 ExampleData.BINfileData
#> 5        None   2   0     0      0        0 ExampleData.BINfileData
CWOSL.SAR.Data@DATA[1:5]
#> [[1]]
#>   [1]   2   0  13   1   1   5   6  12  12   0   2   4   2   4   6   6   4   8
#>  [19]   6   8   3   2   8   1   4   3   6   8   3   1   3   1   1   1  12   5
#>  [37]   1  10   0   0   2   2   2   1  12   1   3   4   2   6   4   1   3   6
#>  [55]   6   5   5   0   0   5   2   2   0   2   2   3  15   4   0   7   4   4
#>  [73]   0   3  14   4   7   7   4   4   2   4   1   8   3   1   3   2   5   2
#>  [91]  17  12   5   4   3   3  18   3  12   2   4   5   7   4   4   2   8   7
#> [109]   1   9   5   9   5  13   0  10   1   1   1   2   6   6   9   4  12   8
#> [127]   1   8   2   7   6   7   1   2   5   1   6   0  11   5   1   3  13  11
#> [145]   1   3   5   6   5   2   5   0  16   5  10   1   3  17   4   7  10  10
#> [163]   4   6   3   8   4   9   5   7   6   5   8  10   8   3   7  11   4  21
#> [181]  14   3   4  20   9  11   8  11  14  13  12  11  25   7   9  10  18   9
#> [199]  15  11  19  24  29  18  27  26  25  21  17  24  32  22  25  29  35  25
#> [217]  27  45  43  43  44  53  51  42  56  40  49  45  85  66  72  67  83  88
#> [235]  90  78  81  74 108  87 110  99 123 127 144 108 120 129 131  72
#> 
#> [[2]]
#>   [1]   4   2   3   0  20   4   4   2   3  12   3   6   6  15   8   3   1   5
#>  [19]   5   8   1   6   8   6   5  20  12   4   4   8   2   5  11   3   5   6
#>  [37]   1   4  11   5   9   5   4   6   5   5   1   1   4   7   1  12   6   4
#>  [55]   1   0   2   5   5  10  11   4   3   9   3   2   7   2   7  11   8   2
#>  [73]   0   6   6   8   0   8   2   8   8   5  10   4   1  24   7   4   7   5
#>  [91]   6   5  17   1   3   1   2   4   5   6  26   4   7   8  11   5   6   2
#> [109]   6   6   5  14   3  10   2  18   2   0   2   1  10  10   2  11   7   0
#> [127]   2  16   0  13   6  10   0   3   6   6  15   3   2  10   4   3   6   2
#> [145]  11   5   2   7   3   6   2   1   6   4   0   1   3   5   4   8   7   6
#> [163]  10   6   2   7   3   7   3   4  10   7   4  12   6   6  16  11   6   6
#> [181]   1   8   8  11   5  10   6  17   3   6  10   8  11  10   8  15  14  11
#> [199]  14  10   9  12  11  12   8  19  16  41  13  22  20  39  15  27  27  24
#> [217]  25  32  26  20  30  27  37  38  29  31  35  42  49  41  36  60  51  43
#> [235]  41  48  49  55  75  53  76  67  59  61  88  76  95  85 101  41
#> 
#> [[3]]
#>   [1]  11   0   3   5   2   1   8  21   2  10  10   5   5   4   2   2   6   2
#>  [19]   3  14   3   0   2   4   4   2   0   6   2   9   2   1  12   1  10   5
#>  [37]   8   1  10   3   6   9  12   4   3   3   4   1   2   4   0   5   0   6
#>  [55]   5   5   5   7   2   3   0  12   3   0   5   2   7   0   3   9   4   6
#>  [73]   2   4   5   0  10   4   6   3   4   4  11   5   6   6   5   6   2   5
#>  [91]   9   2   3   6   1   5   7   2   1  45   5   0   0   4  11   3   3   2
#> [109]   1   0   1  11   9  10   8   7   9  11   9   7   8   8   7   1  13   3
#> [127]   5  11   2   8   0   9   2   7   6  17   1   5   3   6   2  14   5   1
#> [145]   1   3   2   3   4   3  15   5   1  13  11   5   6   1   4   2  62   5
#> [163]   5   3   9   4   8   3  10   6  17  11   3   5   9   5   4   5   3   2
#> [181]   5   4  10   6   4   8  17   3  10  10   6   4  11   6  15   7  14  15
#> [199]  12   9  16  16  21   9   6  17  11  18  15  18  13  15  18  23  20  22
#> [217]  33  32  22  26  27  23  34  33  26  29  29  32  30  51  36  31  44  56
#> [235]  77  47  76  43  77  57  68  80  82  90  86  57 109  95  95  55
#> 
#> [[4]]
#>   [1] 17  1  0  5  1  5  2  2  8  0  1  4 10  5  0  5  6  3  3  2  8  3  1 47 13
#>  [26]  0  3  2  0  4 12  9  7  1  4  6  4  2  6  0  6  6  8 10  3  8  1  7 13  3
#>  [51]  5 12  8  4  7  4  7  8  8  2  3  4 10  6 12 12  2  1  8  0  3 23  6 13  1
#>  [76]  2 16  2  7  3  9  4 10 12  8  8  5  3  3 10 10  5  1  2  2  4  4 11  3  6
#> [101]  2  4  2  7  2  6  8 16  6  1  8  6  5  9 15  4 13  2  5  5 20  2  3  7  5
#> [126]  6  7  1  4 11  9  3  6  1  7  9  0  3  3  9  0  1  4 10  2  7  2  1  0  1
#> [151]  3  4  4  5  0  6  4  3  5  0  3  3  3  2  5  1  5 18  4  1  8  9  3 20 12
#> [176]  1  0  2  1  1  8  5  9  8  1  5  6  5 12  8  8 17  5  6  4 12  2  8  5  4
#> [201]  4  7 12  3  7 10 10  5  5  4  7  3  7 15  6 11 12  5  6 18  5 18 18  8 13
#> [226] 13 15 11 13 14 11 15 16 12 19 23 24 12 14 16 19 30 27 32 30 41 28 20 27 37
#> 
#> [[5]]
#>   [1]  2  2  3  6  1  2  3  1  1  7  2  5  4  4  0  2  2  5  5  8 11  1 14  5  4
#>  [26]  0  0  3  4  3  2  1  5  3 12  3  3  1  1  6  4  0  1  6  6  3  3  4  3  3
#>  [51]  5  5  9  4  6  0 16  3  5  3  4  1  6  3  0  5  3  3  2  6  3  3  6  1  2
#>  [76]  6  3  9  4  5  7  1  4  5  7  0  9  1  0  3  2  7  8  2  7  2  8  5  6  2
#> [101]  9  7  6  3  0  4  2  5 15 18  1  6  5  3 13  4  6  4  1  2  4  1  5  3  4
#> [126]  2  4  7 13  5  3  4  7  5 20  3  8  1  7  3 10 10  7  2  5 10  4  2  9  5
#> [151]  1  5  2  2  8  7  3  4  5 12  7  7  4  4  3  5  4  6  6  9 11  9  2  3  1
#> [176]  3  9  7  9 10 12  5 17  5  8  4 12  7 12  6  3  9  1  7  7 15  7  8 12 15
#> [201] 14 12 21 20 11 18 11 27 26 16 20 23 23 29 31 23 22 27 31 22 29 43 24 29 25
#> [226] 31 37 41 54 54 52 50 40 51 52 67 61 65 47 62 97 73 67 93 87 68 86 78 77 69
#> 
```
