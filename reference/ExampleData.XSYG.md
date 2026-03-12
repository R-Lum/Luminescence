# Example data for a SAR OSL measurement and a TL spectrum using a lexsyg reader

Example data from a SAR OSL measurement and a TL spectrum for package
Luminescence imported from a Freiberg Instruments XSYG file using the
function
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md).

## Format

`OSL.SARMeasurement`: SAR OSL measurement data

The data contain two elements: (a) `$Sequence.Header` is a
[data.frame](https://rdrr.io/r/base/data.frame.html) with metadata from
the measurement,(b) `Sequence.Object` contains an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
object for further analysis.

`TL.Spectrum`: TL spectrum data

[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)
object for further analysis. The spectrum was cleaned from cosmic-rays
using the function

`apply_CosmicRayRemoval`. Note that no quantum efficiency calibration
was performed.

## Source

**OSL.SARMeasurement**

|            |                                                             |
|------------|-------------------------------------------------------------|
| Lab:       | Luminescence Laboratory Giessen                             |
| Lab-Code:  | no code                                                     |
| Location:  | not specified                                               |
| Material:  | Coarse grain quartz on steel cups on lexsyg research reader |
| Reference: | unpublished                                                 |

**TL.Spectrum**

|            |                                                                |
|------------|----------------------------------------------------------------|
| Lab:       | Luminescence Laboratory Giessen                                |
| Lab-Code:  | BT753                                                          |
| Location:  | Dolni Vestonice/Czech Republic                                 |
| Material:  | Fine grain polymineral on steel cups on lexsyg research reader |
| Reference: | Fuchs et al., 2013                                             |
| Spectrum:  | Integration time 19 s, channel time 20 s                       |
| Heating:   | 1 K/s, up to 500 deg. C                                        |

## Version

0.1

## References

Unpublished data measured to serve as example data for that package.
Location origin of sample BT753 is given here:

Fuchs, M., Kreutzer, S., Rousseau, D.D., Antoine, P., Hatte, C.,
Lagroix, F., Moine, O., Gauthier, C., Svoboda, J., Lisa, L., 2013. The
loess sequence of Dolni Vestonice, Czech Republic: A new OSL-based
chronology of the Last Climatic Cycle. Boreas, 42, 664–677.

## See also

[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md),
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md),
[plot_RLum](https://r-lum.github.io/Luminescence/reference/plot_RLum.md),
[plot_RLum.Analysis](https://r-lum.github.io/Luminescence/reference/plot_RLum.Analysis.md),
[plot_RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/plot_RLum.Data.Spectrum.md)

## Examples

``` r
##show data
data(ExampleData.XSYG, envir = environment())

## =========================================
##(1) OSL.SARMeasurement
OSL.SARMeasurement
#> $Sequence.Header
#>                                         
#> state                           finished
#> parentID                1002220348205854
#> name      R Luminescence example dataset
#> position                              30
#> comment                  example dataset
#> startDate                 20140222034820
#> protocol                             SAR
#> mineral                           quartz
#> 
#> $Sequence.Object
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  0
#>   number of records: 124
#>   .. : RLum.Data.Curve : 124
#>   .. .. : #1 TL (UVVIS) <> #2 _TL (NA) <> #3 _TL (NA) 
#>   .. .. : #4 OSL (UVVIS) <> #5 _OSL (NA) <> #6 _OSL (NA) <> #7 _OSL (NA) <> #8 _OSL (NA)
#>   .. .. : #9 irradiation (NA) 
#>   .. .. : #10 TL (UVVIS) <> #11 _TL (NA) <> #12 _TL (NA) 
#>   .. .. : #13 OSL (UVVIS) <> #14 _OSL (NA) <> #15 _OSL (NA) <> #16 _OSL (NA) <> #17 _OSL (NA)
#>   .. .. : #18 irradiation (NA) 
#>   .. .. : #19 TL (UVVIS) <> #20 _TL (NA) <> #21 _TL (NA)
#>   .. .. : #22 OSL (UVVIS) <> #23 _OSL (NA) <> #24 _OSL (NA) <> #25 _OSL (NA) <> #26 _OSL (NA) 
#>   .. .. : #27 irradiation (NA) 
#>   .. .. : #28 TL (UVVIS) <> #29 _TL (NA) <> #30 _TL (NA)
#>   .. .. : #31 OSL (UVVIS) <> #32 _OSL (NA) <> #33 _OSL (NA) <> #34 _OSL (NA) <> #35 _OSL (NA)
#>   .. .. : #36 irradiation (NA) 
#>   .. .. : #37 TL (UVVIS) <> #38 _TL (NA) <> #39 _TL (NA) 
#>   .. .. : #40 OSL (UVVIS) <> #41 _OSL (NA) <> #42 _OSL (NA) <> #43 _OSL (NA) <> #44 _OSL (NA)
#>   .. .. : #45 irradiation (NA) 
#>   .. .. : #46 TL (UVVIS) <> #47 _TL (NA) <> #48 _TL (NA) 
#>   .. .. : #49 OSL (UVVIS) <> #50 _OSL (NA) <> #51 _OSL (NA) <> #52 _OSL (NA) <> #53 _OSL (NA)
#>   .. .. : #54 irradiation (NA) 
#>   .. .. : #55 TL (UVVIS) <> #56 _TL (NA) <> #57 _TL (NA)
#>   .. .. : #58 OSL (UVVIS) <> #59 _OSL (NA) <> #60 _OSL (NA) <> #61 _OSL (NA) <> #62 _OSL (NA) 
#>   .. .. : #63 irradiation (NA)
#>   .. .. : #64 TL (UVVIS) <> #65 _TL (NA) <> #66 _TL (NA) 
#>   .. .. : #67 OSL (UVVIS) <> #68 _OSL (NA) <> #69 _OSL (NA) <> #70 _OSL (NA) <> #71 _OSL (NA)
#>   .. .. : #72 irradiation (NA) 
#>   .. .. : #73 TL (UVVIS) <> #74 _TL (NA) <> #75 _TL (NA) 
#>   .. .. : #76 OSL (UVVIS) <> #77 _OSL (NA) <> #78 _OSL (NA) <> #79 _OSL (NA) <> #80 _OSL (NA)
#>   .. .. : #81 irradiation (NA) 
#>   .. .. : #82 TL (UVVIS) <> #83 _TL (NA) <> #84 _TL (NA)
#>   .. .. : #85 OSL (UVVIS) <> #86 _OSL (NA) <> #87 _OSL (NA) <> #88 _OSL (NA) <> #89 _OSL (NA) 
#>   .. .. : #90 TL (UVVIS) <> #91 _TL (NA) <> #92 _TL (NA)
#>   .. .. : #93 OSL (UVVIS) <> #94 _OSL (NA) <> #95 _OSL (NA) <> #96 _OSL (NA) <> #97 _OSL (NA) 
#>   .. .. : #98 irradiation (NA)
#>   .. .. : #99 TL (UVVIS) <> #100 _TL (NA) <> #101 _TL (NA) 
#>   .. .. : #102 OSL (UVVIS) <> #103 _OSL (NA) <> #104 _OSL (NA) <> #105 _OSL (NA) <> #106 _OSL (NA)
#>   .. .. : #107 irradiation (NA) 
#>   .. .. : #108 TL (UVVIS) <> #109 _TL (NA) <> #110 _TL (NA) 
#>   .. .. : #111 OSL (UVVIS) <> #112 _OSL (NA) <> #113 _OSL (NA) <> #114 _OSL (NA) <> #115 _OSL (NA)
#>   .. .. : #116 irradiation (NA) 
#>   .. .. : #117 TL (UVVIS) <> #118 _TL (NA) <> #119 _TL (NA)
#>   .. .. : #120 OSL (UVVIS) <> #121 _OSL (NA) <> #122 _OSL (NA) <> #123 _OSL (NA) <> #124 _OSL (NA)
#> 

##show $Sequence.Object
OSL.SARMeasurement$Sequence.Object
#> 
#>  [RLum.Analysis-class]
#>   originator: read_XSYG2R()
#>   protocol: SAR
#>   additional info elements:  0
#>   number of records: 124
#>   .. : RLum.Data.Curve : 124
#>   .. .. : #1 TL (UVVIS) <> #2 _TL (NA) <> #3 _TL (NA) 
#>   .. .. : #4 OSL (UVVIS) <> #5 _OSL (NA) <> #6 _OSL (NA) <> #7 _OSL (NA) <> #8 _OSL (NA)
#>   .. .. : #9 irradiation (NA) 
#>   .. .. : #10 TL (UVVIS) <> #11 _TL (NA) <> #12 _TL (NA) 
#>   .. .. : #13 OSL (UVVIS) <> #14 _OSL (NA) <> #15 _OSL (NA) <> #16 _OSL (NA) <> #17 _OSL (NA)
#>   .. .. : #18 irradiation (NA) 
#>   .. .. : #19 TL (UVVIS) <> #20 _TL (NA) <> #21 _TL (NA)
#>   .. .. : #22 OSL (UVVIS) <> #23 _OSL (NA) <> #24 _OSL (NA) <> #25 _OSL (NA) <> #26 _OSL (NA) 
#>   .. .. : #27 irradiation (NA) 
#>   .. .. : #28 TL (UVVIS) <> #29 _TL (NA) <> #30 _TL (NA)
#>   .. .. : #31 OSL (UVVIS) <> #32 _OSL (NA) <> #33 _OSL (NA) <> #34 _OSL (NA) <> #35 _OSL (NA)
#>   .. .. : #36 irradiation (NA) 
#>   .. .. : #37 TL (UVVIS) <> #38 _TL (NA) <> #39 _TL (NA) 
#>   .. .. : #40 OSL (UVVIS) <> #41 _OSL (NA) <> #42 _OSL (NA) <> #43 _OSL (NA) <> #44 _OSL (NA)
#>   .. .. : #45 irradiation (NA) 
#>   .. .. : #46 TL (UVVIS) <> #47 _TL (NA) <> #48 _TL (NA) 
#>   .. .. : #49 OSL (UVVIS) <> #50 _OSL (NA) <> #51 _OSL (NA) <> #52 _OSL (NA) <> #53 _OSL (NA)
#>   .. .. : #54 irradiation (NA) 
#>   .. .. : #55 TL (UVVIS) <> #56 _TL (NA) <> #57 _TL (NA)
#>   .. .. : #58 OSL (UVVIS) <> #59 _OSL (NA) <> #60 _OSL (NA) <> #61 _OSL (NA) <> #62 _OSL (NA) 
#>   .. .. : #63 irradiation (NA)
#>   .. .. : #64 TL (UVVIS) <> #65 _TL (NA) <> #66 _TL (NA) 
#>   .. .. : #67 OSL (UVVIS) <> #68 _OSL (NA) <> #69 _OSL (NA) <> #70 _OSL (NA) <> #71 _OSL (NA)
#>   .. .. : #72 irradiation (NA) 
#>   .. .. : #73 TL (UVVIS) <> #74 _TL (NA) <> #75 _TL (NA) 
#>   .. .. : #76 OSL (UVVIS) <> #77 _OSL (NA) <> #78 _OSL (NA) <> #79 _OSL (NA) <> #80 _OSL (NA)
#>   .. .. : #81 irradiation (NA) 
#>   .. .. : #82 TL (UVVIS) <> #83 _TL (NA) <> #84 _TL (NA)
#>   .. .. : #85 OSL (UVVIS) <> #86 _OSL (NA) <> #87 _OSL (NA) <> #88 _OSL (NA) <> #89 _OSL (NA) 
#>   .. .. : #90 TL (UVVIS) <> #91 _TL (NA) <> #92 _TL (NA)
#>   .. .. : #93 OSL (UVVIS) <> #94 _OSL (NA) <> #95 _OSL (NA) <> #96 _OSL (NA) <> #97 _OSL (NA) 
#>   .. .. : #98 irradiation (NA)
#>   .. .. : #99 TL (UVVIS) <> #100 _TL (NA) <> #101 _TL (NA) 
#>   .. .. : #102 OSL (UVVIS) <> #103 _OSL (NA) <> #104 _OSL (NA) <> #105 _OSL (NA) <> #106 _OSL (NA)
#>   .. .. : #107 irradiation (NA) 
#>   .. .. : #108 TL (UVVIS) <> #109 _TL (NA) <> #110 _TL (NA) 
#>   .. .. : #111 OSL (UVVIS) <> #112 _OSL (NA) <> #113 _OSL (NA) <> #114 _OSL (NA) <> #115 _OSL (NA)
#>   .. .. : #116 irradiation (NA) 
#>   .. .. : #117 TL (UVVIS) <> #118 _TL (NA) <> #119 _TL (NA)
#>   .. .. : #120 OSL (UVVIS) <> #121 _OSL (NA) <> #122 _OSL (NA) <> #123 _OSL (NA) <> #124 _OSL (NA)

##grep OSL curves and plot the first curve
OSLcurve <- get_RLum(OSL.SARMeasurement$Sequence.Object,
recordType="OSL")[[1]]
plot_RLum(OSLcurve)


## =========================================
##(2) TL.Spectrum
TL.Spectrum
#> 
#>  [RLum.Data.Spectrum-class]
#>   recordType: TL (Spectrometer)
#>   curveType: measured
#>   .. recorded frames: 24
#>   .. .. measured values per frame: 1024
#>   .. .. range wavelength/pixel: 296.5 823.126
#>   .. .. range time/temp.: 0.029 460.007
#>   .. .. range count values: 554 65405
#>   additional info elements: 14

##plot simple spectrum (2D)
plot_RLum.Data.Spectrum(TL.Spectrum,
                        plot.type="contour",
                        xlim = c(310,750),
                        ylim = c(0,300),
                        bin.rows=10,
                        bin.cols = 1)
#> Warning: [plot_RLum.Data.Spectrum()] 6 channels removed due to row (wavelength) binning


##plot 3d spectrum (uncomment for usage)
# plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="persp",
# xlim = c(310,750), ylim = c(0,300), bin.rows=10,
# bin.cols = 1)
```
