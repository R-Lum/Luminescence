# General structure function for RLum-class objects

The function provides a generalised access point for specific
[RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
objects. Depending on the input object, the corresponding function will
be selected.

## Usage

``` r
structure_RLum(object, ...)

# S4 method for class 'list'
structure_RLum(object, ...)

# S4 method for class 'RLum.Analysis'
structure_RLum(object, fullExtent = FALSE)
```

## Arguments

- object:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  (**required**): S4 object of class `RLum`

- ...:

  further arguments passed to the specific class method

- fullExtent:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  extends the returned `data.frame` to its full extent, i.e. all info
  elements are part of the return as well. The default value is `FALSE`
  as the data frame might become rather big.

## Value

Returns a [data.frame](https://rdrr.io/r/base/data.frame.html) with
structure of the object.

## Functions

- `structure_RLum(list)`: Returns a list of data frames containing the
  structure of each
  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md)
  object in the input list.

- `structure_RLum(RLum.Analysis)`: Returns the structure of an
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object.

## Function version

0.2.0

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## How to cite

Kreutzer, S., 2025. structure_RLum(): General structure function for
RLum-class objects. Function version 0.2.0. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## Examples

``` r
## load example data
data(ExampleData.XSYG, envir = environment())

## show structure
structure_RLum(OSL.SARMeasurement$Sequence.Object)
#>      id       recordType  curveType protocol.step n.channels x.min   x.max
#> 1     1       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 2     2          TL (NA) predefined          <NA>          5 0.000 268.000
#> 3     3          TL (NA)   measured          <NA>       1740 0.000 173.900
#> 4     4      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 5     5         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 6     6         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 7     7         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 8     8         OSL (NA)   measured          <NA>        150 0.497  74.997
#> 9     9 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 10   10       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 11   11          TL (NA) predefined          <NA>          4 0.000 198.000
#> 12   12          TL (NA)   measured          <NA>        990 0.000  98.900
#> 13   13      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 14   14         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 15   15         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 16   16         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 17   17         OSL (NA)   measured          <NA>        151 0.012  75.012
#> 18   18 irradiation (NA) predefined          <NA>          2 0.000  60.000
#> 19   19       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 20   20          TL (NA) predefined          <NA>          5 0.000 268.000
#> 21   21          TL (NA)   measured          <NA>       1730 0.000 172.900
#> 22   22      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 23   23         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 24   24         OSL (NA)   measured          <NA>       1160 0.000 115.900
#> 25   25         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 26   26         OSL (NA)   measured          <NA>        153 0.012  76.012
#> 27   27 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 28   28       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 29   29          TL (NA) predefined          <NA>          4 0.000 198.000
#> 30   30          TL (NA)   measured          <NA>       1000 0.000  99.900
#> 31   31      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 32   32         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 33   33         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 34   34         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 35   35         OSL (NA)   measured          <NA>        151 0.014  75.014
#> 36   36 irradiation (NA) predefined          <NA>          2 0.000 130.000
#> 37   37       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 38   38          TL (NA) predefined          <NA>          5 0.000 268.000
#> 39   39          TL (NA)   measured          <NA>       1740 0.000 173.900
#> 40   40      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 41   41         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 42   42         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 43   43         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 44   44         OSL (NA)   measured          <NA>        151 0.012  75.012
#> 45   45 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 46   46       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 47   47          TL (NA) predefined          <NA>          4 0.000 198.000
#> 48   48          TL (NA)   measured          <NA>        990 0.000  98.900
#> 49   49      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 50   50         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 51   51         OSL (NA)   measured          <NA>       1130 0.000 112.900
#> 52   52         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 53   53         OSL (NA)   measured          <NA>        147 0.012  73.012
#> 54   54 irradiation (NA) predefined          <NA>          2 0.000 230.000
#> 55   55       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 56   56          TL (NA) predefined          <NA>          5 0.000 268.000
#> 57   57          TL (NA)   measured          <NA>       1740 0.000 173.900
#> 58   58      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 59   59         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 60   60         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 61   61         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 62   62         OSL (NA)   measured          <NA>        151 0.012  75.012
#> 63   63 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 64   64       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 65   65          TL (NA) predefined          <NA>          4 0.000 198.000
#> 66   66          TL (NA)   measured          <NA>        990 0.000  98.900
#> 67   67      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 68   68         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 69   69         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 70   70         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 71   71         OSL (NA)   measured          <NA>        151 0.012  75.012
#> 72   72 irradiation (NA) predefined          <NA>          2 0.000 300.000
#> 73   73       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 74   74          TL (NA) predefined          <NA>          5 0.000 268.000
#> 75   75          TL (NA)   measured          <NA>       1750 0.000 174.900
#> 76   76      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 77   77         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 78   78         OSL (NA)   measured          <NA>       1160 0.000 115.900
#> 79   79         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 80   80         OSL (NA)   measured          <NA>        152 0.481  75.981
#> 81   81 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 82   82       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 83   83          TL (NA) predefined          <NA>          4 0.000 198.000
#> 84   84          TL (NA)   measured          <NA>        990 0.000  98.900
#> 85   85      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 86   86         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 87   87         OSL (NA)   measured          <NA>       1130 0.000 112.900
#> 88   88         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 89   89         OSL (NA)   measured          <NA>        147 0.012  73.012
#> 90   90       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 91   91          TL (NA) predefined          <NA>          5 0.000 268.000
#> 92   92          TL (NA)   measured          <NA>       1740 0.000 173.900
#> 93   93      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 94   94         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 95   95         OSL (NA)   measured          <NA>       1140 0.000 113.900
#> 96   96         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 97   97         OSL (NA)   measured          <NA>        149 0.012  74.012
#> 98   98 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 99   99       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 100 100          TL (NA) predefined          <NA>          4 0.000 198.000
#> 101 101          TL (NA)   measured          <NA>        990 0.000  98.900
#> 102 102      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 103 103         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 104 104         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 105 105         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 106 106         OSL (NA)   measured          <NA>        151 0.012  75.012
#> 107 107 irradiation (NA) predefined          <NA>          2 0.000  60.000
#> 108 108       TL (UVVIS)   measured          <NA>       1180 0.100 118.000
#> 109 109          TL (NA) predefined          <NA>          5 0.000 268.000
#> 110 110          TL (NA)   measured          <NA>       1740 0.000 173.900
#> 111 111      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 112 112         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 113 113         OSL (NA)   measured          <NA>       1150 0.000 114.900
#> 114 114         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 115 115         OSL (NA)   measured          <NA>        151 0.020  75.020
#> 116 116 irradiation (NA) predefined          <NA>          2 0.000  15.000
#> 117 117       TL (UVVIS)   measured          <NA>        680 0.100  68.000
#> 118 118          TL (NA) predefined          <NA>          4 0.000 198.000
#> 119 119          TL (NA)   measured          <NA>        980 0.000  97.900
#> 120 120      OSL (UVVIS)   measured          <NA>        500 0.100  50.000
#> 121 121         OSL (NA) predefined          <NA>          5 0.000 216.000
#> 122 122         OSL (NA)   measured          <NA>       1140 0.000 113.900
#> 123 123         OSL (NA) predefined          <NA>          2 0.000  50.000
#> 124 124         OSL (NA)   measured          <NA>        149 0.012  74.012
#>     y.min  y.max    originator                                 .uid
#> 1       0   82.0 read_XSYG2R()   2016-01-30-10:54.0.226260372670367
#> 2      25  260.0 read_XSYG2R()    2016-01-30-10:54.0.77414278825745
#> 3      26  262.0 read_XSYG2R()   2016-01-30-10:54.0.429038936737925
#> 4       0  891.0 read_XSYG2R()   2016-01-30-10:54.0.336071660276502
#> 5      25  125.0 read_XSYG2R()   2016-01-30-10:54.0.832267640857026
#> 6      43  127.0 read_XSYG2R()   2016-01-30-10:54.0.339185907738283
#> 7      40   40.0 read_XSYG2R()   2016-01-30-10:54.0.517060354584828
#> 8       0   40.2 read_XSYG2R()   2016-01-30-10:54.0.537775189615786
#> 9       1    1.0 read_XSYG2R()    2016-01-30-10:54.0.20047601335682
#> 10      4  555.0 read_XSYG2R()   2016-01-30-10:54.0.868833578424528
#> 11     25  160.0 read_XSYG2R()   2016-01-30-10:54.0.742296873126179
#> 12     34  160.0 read_XSYG2R()   2016-01-30-10:54.0.589681873098016
#> 13      0  122.0 read_XSYG2R()   2016-01-30-10:54.0.630885894177482
#> 14     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.663541194982827
#> 15     42  127.0 read_XSYG2R()   2016-01-30-10:54.0.905940495431423
#> 16     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.381334827048704
#> 17      0   40.2 read_XSYG2R()   2016-01-30-10:54.0.328897463157773
#> 18      1    1.0 read_XSYG2R()   2016-01-30-10:54.0.870911038015038
#> 19     11 1591.0 read_XSYG2R()   2016-01-30-10:54.0.932349309092388
#> 20     25  260.0 read_XSYG2R()   2016-01-30-10:54.0.297850674483925
#> 21     33  262.0 read_XSYG2R()   2016-01-30-10:54.0.605329250684008
#> 22      0  473.0 read_XSYG2R()   2016-01-30-10:54.0.289562621153891
#> 23     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.925942022586241
#> 24     43  128.0 read_XSYG2R()   2016-01-30-10:54.0.291019570548087
#> 25     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.320679558906704
#> 26      0   40.2 read_XSYG2R()   2016-01-30-10:54.0.968130854191259
#> 27      1    1.0 read_XSYG2R()  2016-01-30-10:54.0.0512687624432147
#> 28      3  736.0 read_XSYG2R()    2016-01-30-10:54.0.42879255511798
#> 29     25  160.0 read_XSYG2R()  2016-01-30-10:54.0.0236395532265306
#> 30     33  160.0 read_XSYG2R()   2016-01-30-10:54.0.585047567961738
#> 31      0  149.0 read_XSYG2R()   2016-01-30-10:54.0.522002540295944
#> 32     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.454029348213226
#> 33     42  127.0 read_XSYG2R()   2016-01-30-10:54.0.710487167816609
#> 34     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.830461921636015
#> 35      0   40.1 read_XSYG2R()  2016-01-30-10:54.0.0631769173778594
#> 36      1    1.0 read_XSYG2R()   2016-01-30-10:54.0.593671935843304
#> 37     13 3495.0 read_XSYG2R()   2016-01-30-10:54.0.229677951894701
#> 38     25  260.0 read_XSYG2R()   2016-01-30-10:54.0.921212903689593
#> 39     27  263.0 read_XSYG2R()   2016-01-30-10:54.0.462135725654662
#> 40      0 1152.0 read_XSYG2R()   2016-01-30-10:54.0.173798899399117
#> 41     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.905658074887469
#> 42     43  127.0 read_XSYG2R()   2016-01-30-10:54.0.341222140239552
#> 43     40   40.0 read_XSYG2R()     2016-01-30-10:54.0.4814964144025
#> 44      0   40.2 read_XSYG2R()  2016-01-30-10:54.0.0450054712127894
#> 45      1    1.0 read_XSYG2R()   2016-01-30-10:54.0.268596603535116
#> 46      9  912.0 read_XSYG2R()   2016-01-30-10:54.0.370865847915411
#> 47     25  160.0 read_XSYG2R()   2016-01-30-10:54.0.512568636797369
#> 48     33  160.0 read_XSYG2R()   2016-01-30-10:54.0.869454299798235
#> 49      0  215.0 read_XSYG2R()   2016-01-30-10:54.0.186754534952343
#> 50     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.931624148506671
#> 51     43  127.0 read_XSYG2R()   2016-01-30-10:54.0.561583923641592
#> 52     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.465734504396096
#> 53      0   40.1 read_XSYG2R()   2016-01-30-10:54.0.929129796102643
#> 54      1    1.0 read_XSYG2R()    2016-01-30-10:54.0.29691950744018
#> 55     25 5564.0 read_XSYG2R()   2016-01-30-10:54.0.534651084803045
#> 56     25  260.0 read_XSYG2R()   2016-01-30-10:54.0.792043952038512
#> 57     27  262.0 read_XSYG2R()   2016-01-30-10:54.0.208661240758374
#> 58      0 2346.0 read_XSYG2R()   2016-01-30-10:54.0.937022477388382
#> 59     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.633003524737433
#> 60     43  127.0 read_XSYG2R()   2016-01-30-10:54.0.744804858230054
#> 61     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.451362358173355
#> 62      0   40.3 read_XSYG2R()    2016-01-30-10:54.0.91636705910787
#> 63      1    1.0 read_XSYG2R()  2016-01-30-10:54.0.0463254018686712
#> 64     12 1146.0 read_XSYG2R()   2016-01-30-10:54.0.528479704400524
#> 65     25  160.0 read_XSYG2R()   2016-01-30-10:54.0.478199049830437
#> 66     34  160.0 read_XSYG2R()   2016-01-30-10:54.0.122971840202808
#> 67      0  292.0 read_XSYG2R()   2016-01-30-10:54.0.879391883034259
#> 68     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.242106179939583
#> 69     42  128.0 read_XSYG2R()   2016-01-30-10:54.0.482210234506056
#> 70     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.936310092685744
#> 71      0   40.1 read_XSYG2R()    2016-01-30-10:54.0.58761327737011
#> 72      1    1.0 read_XSYG2R()   2016-01-30-10:54.0.662958577508107
#> 73     47 7423.0 read_XSYG2R()   2016-01-30-10:54.0.349518121918663
#> 74     25  260.0 read_XSYG2R()   2016-01-30-10:54.0.395675572101027
#> 75     25  261.0 read_XSYG2R()   2016-01-30-10:54.0.832506845006719
#> 76      0 3505.0 read_XSYG2R()   2016-01-30-10:54.0.673705363180488
#> 77     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.836654497077689
#> 78     43  127.0 read_XSYG2R()   2016-01-30-10:54.0.655996734043583
#> 79     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.762605228461325
#> 80      0   40.2 read_XSYG2R()   2016-01-30-10:54.0.723475385224447
#> 81      1    1.0 read_XSYG2R()   2016-01-30-10:54.0.953630778472871
#> 82     15 1414.0 read_XSYG2R()   2016-01-30-10:54.0.372155147604644
#> 83     25  160.0 read_XSYG2R()   2016-01-30-10:54.0.212284204317257
#> 84     34  161.0 read_XSYG2R()    2016-01-30-10:54.0.78430981375277
#> 85      0  388.0 read_XSYG2R()   2016-01-30-10:54.0.858382838079706
#> 86     25  125.0 read_XSYG2R()  2016-01-30-10:54.0.0998988668434322
#> 87     42  128.0 read_XSYG2R()   2016-01-30-10:54.0.619967711856589
#> 88     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.079608827130869
#> 89      0   40.2 read_XSYG2R()   2016-01-30-10:54.0.850248564733192
#> 90      0   40.0 read_XSYG2R()   2016-01-30-10:54.0.123581775929779
#> 91     25  260.0 read_XSYG2R()  2016-01-30-10:54.0.0850816639140248
#> 92     38  261.0 read_XSYG2R()   2016-01-30-10:54.0.185026297112927
#> 93      0   27.0 read_XSYG2R()   2016-01-30-10:54.0.738029998959973
#> 94     25  125.0 read_XSYG2R()   2016-01-30-10:54.0.906962538138032
#> 95     43  127.0 read_XSYG2R()   2016-01-30-10:54.0.196863719262183
#> 96     40   40.0 read_XSYG2R()   2016-01-30-10:54.0.223747788928449
#> 97      0   40.2 read_XSYG2R()   2016-01-30-10:54.0.452920723007992
#> 98      1    1.0 read_XSYG2R()   2016-01-30-10:54.0.588007028913125
#> 99     17 1505.0 read_XSYG2R()   2016-01-30-10:54.0.202315265778452
#> 100    25  160.0 read_XSYG2R()   2016-01-30-10:54.0.568469027988613
#> 101    34  160.0 read_XSYG2R()   2016-01-30-10:54.0.770061913877726
#> 102     2  371.0 read_XSYG2R()   2016-01-30-10:54.0.194079604931176
#> 103    25  125.0 read_XSYG2R()   2016-01-30-10:54.0.159584373235703
#> 104    42  127.0 read_XSYG2R()   2016-01-30-10:54.0.883203205419704
#> 105    40   40.0 read_XSYG2R()   2016-01-30-10:54.0.194686755770817
#> 106     0   40.1 read_XSYG2R()    2016-01-30-10:54.0.57188334944658
#> 107     1    1.0 read_XSYG2R()   2016-01-30-10:54.0.310936602763832
#> 108    12 4354.0 read_XSYG2R() 2016-01-30-10:54.0.00642073270864785
#> 109    25  260.0 read_XSYG2R()   2016-01-30-10:54.0.673262577038258
#> 110    32  262.0 read_XSYG2R()   2016-01-30-10:54.0.945887796580791
#> 111     0 1150.0 read_XSYG2R()   2016-01-30-10:54.0.931073016719893
#> 112    25  125.0 read_XSYG2R()   2016-01-30-10:54.0.524589899694547
#> 113    43  127.0 read_XSYG2R()   2016-01-30-10:54.0.901807451387867
#> 114    40   40.0 read_XSYG2R()   2016-01-30-10:54.0.473460933892056
#> 115     0   40.1 read_XSYG2R()  2016-01-30-10:54.0.0573154254816473
#> 116     1    1.0 read_XSYG2R()   2016-01-30-10:54.0.314163006609306
#> 117    10 1540.0 read_XSYG2R()   2016-01-30-10:54.0.891242811921984
#> 118    25  160.0 read_XSYG2R()   2016-01-30-10:54.0.495065419003367
#> 119    33  160.0 read_XSYG2R()  2016-01-30-10:54.0.0757919212337583
#> 120     0  372.0 read_XSYG2R()   2016-01-30-10:54.0.193264133529738
#> 121    25  125.0 read_XSYG2R()   2016-01-30-10:54.0.787167773349211
#> 122    43  126.0 read_XSYG2R()   2016-01-30-10:54.0.778637015959248
#> 123    40   40.0 read_XSYG2R()   2016-01-30-10:54.0.902725383639336
#> 124     0   40.2 read_XSYG2R()   2016-01-30-10:54.0.911328677553684
#>             .pid         info
#> 1   2016-01-.... state, p....
#> 2   2016-01-.... state, p....
#> 3   2016-01-.... state, p....
#> 4   2016-01-.... state, p....
#> 5   2016-01-.... state, p....
#> 6   2016-01-.... state, p....
#> 7   2016-01-.... state, p....
#> 8   2016-01-.... state, p....
#> 9   2016-01-.... state, p....
#> 10  2016-01-.... state, p....
#> 11  2016-01-.... state, p....
#> 12  2016-01-.... state, p....
#> 13  2016-01-.... state, p....
#> 14  2016-01-.... state, p....
#> 15  2016-01-.... state, p....
#> 16  2016-01-.... state, p....
#> 17  2016-01-.... state, p....
#> 18  2016-01-.... state, p....
#> 19  2016-01-.... state, p....
#> 20  2016-01-.... state, p....
#> 21  2016-01-.... state, p....
#> 22  2016-01-.... state, p....
#> 23  2016-01-.... state, p....
#> 24  2016-01-.... state, p....
#> 25  2016-01-.... state, p....
#> 26  2016-01-.... state, p....
#> 27  2016-01-.... state, p....
#> 28  2016-01-.... state, p....
#> 29  2016-01-.... state, p....
#> 30  2016-01-.... state, p....
#> 31  2016-01-.... state, p....
#> 32  2016-01-.... state, p....
#> 33  2016-01-.... state, p....
#> 34  2016-01-.... state, p....
#> 35  2016-01-.... state, p....
#> 36  2016-01-.... state, p....
#> 37  2016-01-.... state, p....
#> 38  2016-01-.... state, p....
#> 39  2016-01-.... state, p....
#> 40  2016-01-.... state, p....
#> 41  2016-01-.... state, p....
#> 42  2016-01-.... state, p....
#> 43  2016-01-.... state, p....
#> 44  2016-01-.... state, p....
#> 45  2016-01-.... state, p....
#> 46  2016-01-.... state, p....
#> 47  2016-01-.... state, p....
#> 48  2016-01-.... state, p....
#> 49  2016-01-.... state, p....
#> 50  2016-01-.... state, p....
#> 51  2016-01-.... state, p....
#> 52  2016-01-.... state, p....
#> 53  2016-01-.... state, p....
#> 54  2016-01-.... state, p....
#> 55  2016-01-.... state, p....
#> 56  2016-01-.... state, p....
#> 57  2016-01-.... state, p....
#> 58  2016-01-.... state, p....
#> 59  2016-01-.... state, p....
#> 60  2016-01-.... state, p....
#> 61  2016-01-.... state, p....
#> 62  2016-01-.... state, p....
#> 63  2016-01-.... state, p....
#> 64  2016-01-.... state, p....
#> 65  2016-01-.... state, p....
#> 66  2016-01-.... state, p....
#> 67  2016-01-.... state, p....
#> 68  2016-01-.... state, p....
#> 69  2016-01-.... state, p....
#> 70  2016-01-.... state, p....
#> 71  2016-01-.... state, p....
#> 72  2016-01-.... state, p....
#> 73  2016-01-.... state, p....
#> 74  2016-01-.... state, p....
#> 75  2016-01-.... state, p....
#> 76  2016-01-.... state, p....
#> 77  2016-01-.... state, p....
#> 78  2016-01-.... state, p....
#> 79  2016-01-.... state, p....
#> 80  2016-01-.... state, p....
#> 81  2016-01-.... state, p....
#> 82  2016-01-.... state, p....
#> 83  2016-01-.... state, p....
#> 84  2016-01-.... state, p....
#> 85  2016-01-.... state, p....
#> 86  2016-01-.... state, p....
#> 87  2016-01-.... state, p....
#> 88  2016-01-.... state, p....
#> 89  2016-01-.... state, p....
#> 90  2016-01-.... state, p....
#> 91  2016-01-.... state, p....
#> 92  2016-01-.... state, p....
#> 93  2016-01-.... state, p....
#> 94  2016-01-.... state, p....
#> 95  2016-01-.... state, p....
#> 96  2016-01-.... state, p....
#> 97  2016-01-.... state, p....
#> 98  2016-01-.... state, p....
#> 99  2016-01-.... state, p....
#> 100 2016-01-.... state, p....
#> 101 2016-01-.... state, p....
#> 102 2016-01-.... state, p....
#> 103 2016-01-.... state, p....
#> 104 2016-01-.... state, p....
#> 105 2016-01-.... state, p....
#> 106 2016-01-.... state, p....
#> 107 2016-01-.... state, p....
#> 108 2016-01-.... state, p....
#> 109 2016-01-.... state, p....
#> 110 2016-01-.... state, p....
#> 111 2016-01-.... state, p....
#> 112 2016-01-.... state, p....
#> 113 2016-01-.... state, p....
#> 114 2016-01-.... state, p....
#> 115 2016-01-.... state, p....
#> 116 2016-01-.... state, p....
#> 117 2016-01-.... state, p....
#> 118 2016-01-.... state, p....
#> 119 2016-01-.... state, p....
#> 120 2016-01-.... state, p....
#> 121 2016-01-.... state, p....
#> 122 2016-01-.... state, p....
#> 123 2016-01-.... state, p....
#> 124 2016-01-.... state, p....
```
