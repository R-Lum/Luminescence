# Class `"Risoe.BINfileData"`

S4 class object for luminescence data in R. The object is produced as
output of the function
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md).

## Usage

``` r
# S4 method for class 'ANY'
set_Risoe.BINfileData(
  METADATA = data.frame(),
  DATA = list(),
  .RESERVED = list()
)
```

## Arguments

- METADATA:

  Object of class "data.frame" containing the meta information for each
  curve.

- DATA:

  Object of class "list" containing numeric vector with count data.

- .RESERVED:

  Object of class "list" containing list of undocumented raw values for
  internal use only.

## Methods (by generic)

- `set_Risoe.BINfileData(ANY)`: A Risoe.BINfileData object is normally
  produced as output of the function
  [read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md).
  This construction method is intended for internal usage only.

## Slots

- `METADATA`:

  Object of class "data.frame" containing the meta information for each
  curve.

- `DATA`:

  Object of class "list" containing numeric vector with count data.

- `.RESERVED`:

  Object of class "list" containing list of undocumented raw values for
  internal use only.

## Note

**Internal METADATA - object structure**

This structure is compatible with BIN/BINX-files version 03-08, however,
it does not follow (in its sequential arrangement) the manual provided
by the manufacturer, but an own structure accounting for the different
versions.

|         |                   |               |       |                                                                                      |
|---------|-------------------|---------------|-------|--------------------------------------------------------------------------------------|
| **\#**  | **Name**          | **Data Type** | **V** | **Description**                                                                      |
| `[,1]`  | `ID`              | `numeric`     | RLum  | Unique record ID (same ID as in slot `DATA`)                                         |
| `[,2]`  | `SEL`             | `logic`       | RLum  | Record selection, not part official BIN-format, triggered by TAG                     |
| `[,3]`  | `VERSION`         | `raw`         | 03-08 | BIN-file version number                                                              |
| `[,4]`  | `LENGTH`          | `integer`     | 03-08 | Length of this record                                                                |
| `[,5]`  | `PREVIOUS`        | `integer`     | 03-08 | Length of previous record                                                            |
| `[,6]`  | `NPOINTS`         | `integer`     | 03-08 | Number of data points in the record                                                  |
| `[,7]`  | `RECTYPE`         | `integer`     | 08    | Record type                                                                          |
| `[,8]`  | `RUN`             | `integer`     | 03-08 | Run number                                                                           |
| `[,9]`  | `SET`             | `integer`     | 03-08 | Set number                                                                           |
| `[,10]` | `POSITION`        | `integer`     | 03-08 | Position number                                                                      |
| `[,11]` | `GRAIN`           | `integer`     | 03-04 | Grain number                                                                         |
| `[,12]` | `GRAINNUMBER`     | `integer`     | 05-08 | Grain number                                                                         |
| `[,13]` | `CURVENO`         | `integer`     | 05-08 | Curve number                                                                         |
| `[,14]` | `XCOORD`          | `integer`     | 03-08 | X position of a single grain                                                         |
| `[,15]` | `YCOORD`          | `integer`     | 03-08 | Y position of a single grain                                                         |
| `[,16]` | `SAMPLE`          | `factor`      | 03-08 | Sample name                                                                          |
| `[,17]` | `COMMENT`         | `factor`      | 03-08 | Comment name                                                                         |
| `[,18]` | `SYSTEMID`        | `integer`     | 03-08 | Risø system id                                                                       |
| `[,19]` | `FNAME`           | `factor`      | 05-08 | File name (*.bin/*.binx)                                                             |
| `[,20]` | `USER`            | `factor`      | 03-08 | User name                                                                            |
| `[,21]` | `TIME`            | `character`   | 03-08 | Data collection time (`hh-mm-ss`)                                                    |
| `[,22]` | `DATE`            | `factor`      | 03-08 | Data collection date (`ddmmyy`)                                                      |
| `[,23]` | `DTYPE`           | `character`   | 03-08 | Data type                                                                            |
| `[,24]` | `BL_TIME`         | `numeric`     | 03-08 | Bleaching time                                                                       |
| `[,25]` | `BL_UNIT`         | `integer`     | 03-08 | Bleaching unit (mJ, J, s, min, h)                                                    |
| `[,26]` | `NORM1`           | `numeric`     | 03-08 | Normalisation factor (1)                                                             |
| `[,27]` | `NORM2`           | `numeric`     | 03-08 | Normalisation factor (2)                                                             |
| `[,28]` | `NORM3`           | `numeric`     | 03-08 | Normalisation factor (3)                                                             |
| `[,29]` | `BG`              | `numeric`     | 03-08 | Background level                                                                     |
| `[,30]` | `SHIFT`           | `integer`     | 03-08 | Number of channels to shift data                                                     |
| `[,31]` | `TAG`             | `integer`     | 03-08 | Tag, triggers `SEL`                                                                  |
| `[,32]` | `LTYPE`           | `character`   | 03-08 | Luminescence type                                                                    |
| `[,33]` | `LIGHTSOURCE`     | `character`   | 03-08 | Light source                                                                         |
| `[,34]` | `LPOWER`          | `numeric`     | 03-08 | Optical stimulation power                                                            |
| `[,35]` | `LIGHTPOWER`      | `numeric`     | 05-08 | Optical stimulation power                                                            |
| `[,36]` | `LOW`             | `numeric`     | 03-08 | Low (temperature, time, wavelength)                                                  |
| `[,37]` | `HIGH`            | `numeric`     | 03-08 | High (temperature, time, wavelength)                                                 |
| `[,38]` | `RATE`            | `numeric`     | 03-08 | Rate (heating rate, scan rate)                                                       |
| `[,39]` | `TEMPERATURE`     | `integer`     | 03-08 | Sample temperature                                                                   |
| `[,40]` | `MEASTEMP`        | `integer`     | 05-08 | Measured temperature                                                                 |
| `[,41]` | `AN_TEMP`         | `numeric`     | 03-08 | Annealing temperature                                                                |
| `[,42]` | `AN_TIME`         | `numeric`     | 03-08 | Annealing time                                                                       |
| `[,43]` | `TOLDELAY`        | `integer`     | 03-08 | TOL 'delay' channels                                                                 |
| `[,44]` | `TOLON`           | `integer`     | 03-08 | TOL 'on' channels                                                                    |
| `[,45]` | `TOLOFF`          | `integer`     | 03-08 | TOL 'off' channels                                                                   |
| `[,46]` | `IRR_TIME`        | `numeric`     | 03-08 | Irradiation time                                                                     |
| `[,47]` | `IRR_TYPE`        | `integer`     | 03-08 | Irradiation type (alpha, beta or gamma)                                              |
| `[,48]` | `IRR_UNIT`        | `integer`     | 03-04 | Irradiation unit (Gy, rad, s, min, h)                                                |
| `[,49]` | `IRR_DOSERATE`    | `numeric`     | 05-08 | Irradiation dose rate (Gy/s)                                                         |
| `[,50]` | `IRR_DOSERATEERR` | `numeric`     | 06-08 | Irradiation dose rate error (Gy/s)                                                   |
| `[,51]` | `TIMESINCEIRR`    | `integer`     | 05-08 | Time since irradiation (s)                                                           |
| `[,52]` | `TIMETICK`        | `numeric`     | 05-08 | Time tick for pulsing (s)                                                            |
| `[,53]` | `ONTIME`          | `integer`     | 05-08 | On-time for pulsing (in time ticks)                                                  |
| `[,54]` | `OFFTIME`         | `integer`     | 03    | Off-time for pulsed stimulation (in s)                                               |
| `[,55]` | `STIMPERIOD`      | `integer`     | 05-08 | Stimulation period (on+off in time ticks)                                            |
| `[,56]` | `GATE_ENABLED`    | `raw`         | 05-08 | PMT signal gating enabled                                                            |
| `[,57]` | `ENABLE_FLAGS`    | `raw`         | 05-08 | PMT signal gating enabled                                                            |
| `[,58]` | `GATE_START`      | `integer`     | 05-08 | Start gating (in time ticks)                                                         |
| `[,59]` | `GATE_STOP`       | `integer`     | 05-08 | Stop gating (in time ticks), `'Gateend'` for version 04, here only GATE_STOP is used |
| `[,60]` | `PTENABLED`       | `raw`         | 05-08 | Photon time enabled                                                                  |
| `[,61]` | `DTENABLED`       | `raw`         | 05-08 | PMT dead time correction enabled                                                     |
| `[,62]` | `DEADTIME`        | `numeric`     | 05-08 | PMT dead time (s)                                                                    |
| `[,63]` | `MAXLPOWER`       | `numeric`     | 05-08 | Stimulation power to 100 percent (mW/cm^2)                                           |
| `[,64]` | `XRF_ACQTIME`     | `numeric`     | 05-08 | XRF acquisition time (s)                                                             |
| `[,65]` | `XRF_HV`          | `numeric`     | 05-08 | XRF X-ray high voltage (V)                                                           |
| `[,66]` | `XRF_CURR`        | `integer`     | 05-08 | XRF X-ray current (µA)                                                               |
| `[,67]` | `XRF_DEADTIMEF`   | `numeric`     | 05-08 | XRF dead time fraction                                                               |
| `[,68]` | `DETECTOR_ID`     | `raw`         | 07-08 | Detector ID                                                                          |
| `[,69]` | `LOWERFILTER_ID`  | `integer`     | 07-08 | Lower filter ID in reader                                                            |
| `[,70]` | `UPPERFILTER_ID`  | `integer`     | 07-08 | Upper filter ID in reader                                                            |
| `[,71]` | `ENOISEFACTOR`    | `numeric`     | 07-08 | Excess noise filter, usage unknown                                                   |
| `[,72]` | `MARKPOS_X1`      | `numeric`     | 08    | Coordinates marker position 1                                                        |
| `[,73]` | `MARKPOS_Y1`      | `numeric`     | 08    | Coordinates marker position 1                                                        |
| `[,74]` | `MARKPOS_X2`      | `numeric`     | 08    | Coordinates marker position 2                                                        |
| `[,75]` | `MARKPOS_Y2`      | `numeric`     | 08    | Coordinates marker position 2                                                        |
| `[,76]` | `MARKPOS_X3`      | `numeric`     | 08    | Coordinates marker position 3                                                        |
| `[,77]` | `MARKPOS_Y3`      | `numeric`     | 08    | Coordinates marker position 3                                                        |
| `[,78]` | `EXTR_START`      | `numeric`     | 08    | usage unknown                                                                        |
| `[,79]` | `EXTR_END`        | `numeric`     | 08    | usage unknown                                                                        |
| `[,80]` | `SEQUENCE`        | `character`   | 03-04 | Sequence name                                                                        |

V = BIN-file version (RLum means that it does not depend on a specific
BIN-file version)

Note that the `Risoe.BINfileData` object combines all values from
different versions from the BIN/BINX-file, reserved bits are skipped,
however, the function
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)
reset arbitrary reserved bits. Invalid values for a specific version are
set to `NA`. Furthermore, the internal R data types do not necessarily
match the required data types for the BIN-file data import! Data types
are converted during data import.  

**LTYPE** values

|        |          |                                     |
|--------|----------|-------------------------------------|
| VALUE  | TYPE     | DESCRIPTION                         |
| `[0]`  | `TL`     | : Thermoluminescence                |
| `[1]`  | `OSL`    | : Optically stimulated luminescence |
| `[2]`  | `IRSL`   | : Infrared stimulated luminescence  |
| `[3]`  | `M-IR`   | : Infrared monochromator scan       |
| `[4]`  | `M-VIS`  | : Visible monochromator scan        |
| `[5]`  | `TOL`    | : Thermo-optical luminescence       |
| `[6]`  | `TRPOSL` | : Time Resolved Pulsed OSL          |
| `[7]`  | `RIR`    | : Ramped IRSL                       |
| `[8]`  | `RBR`    | : Ramped (Blue) LEDs                |
| `[9]`  | `USER`   | : User defined                      |
| `[10]` | `POSL`   | : Pulsed OSL                        |
| `[11]` | `SGOSL`  | : Single Grain OSL                  |
| `[12]` | `RL`     | : Radio Luminescence                |
| `[13]` | `XRF`    | : X-ray Fluorescence                |

**DTYPE** values

|       |                  |
|-------|------------------|
| VALUE | DESCRIPTION      |
| `[0]` | Natural          |
| `[1]` | N+dose           |
| `[2]` | Bleach           |
| `[3]` | Bleach+dose      |
| `[4]` | Natural (Bleach) |
| `[5]` | N+dose (Bleach)  |
| `[6]` | Dose             |
| `[7]` | Background       |

**LIGHTSOURCE** values

|       |                            |
|-------|----------------------------|
| VALUE | DESCRIPTION                |
| `[0]` | None                       |
| `[1]` | Lamp                       |
| `[2]` | IR diodes/IR Laser         |
| `[3]` | Calibration LED            |
| `[4]` | Blue Diodes                |
| `[5]` | White light                |
| `[6]` | Green laser (single grain) |
| `[7]` | IR laser (single grain)    |

**Internal DATA - object structure**

With version 8 of the BIN/BINX file format, slot `@DATA` (byte array
`DPOINTS`) can contain two different values:

1.  `DPOINTS` (standard for `RECTYPE` := (0,1)): is a vector with the
    length defined through `NPOINTS`. This is the standard for xy-curves
    since version 03. However, recorded are only y-values and x-values
    are calculated during import using information from, amongst others,
    `LOW` and `HIGH`. For instance, a simple TL curve would store
    `LOW = 0` and `HIGH = 400`. This is then becomes the range for the
    temperature values. All values in between are interpolated with a
    length matching the number of corresponding values stored in
    `DPOINTS`.

*Note: The Sequence Editor uses 0 deg. C as the lowest temperature,
although this temperature is usually not available in a laboratory.
This, however, does not mean that the calculated TL curves are invalid,
as the heater would only start the temperature ramp if the ambient
temperature is lower than the target temperature.*

1.  `DPOINTS` (`RECTYPE` := 128) is contains no count values but
    information about the definition of the regions of interest (ROI).
    Each definition is 504 bytes long. The number of definitions is
    defined by `NPOINTS` in `@METADATA`. The record describes basically
    the geometric features of the regions of interest. The
    representation in R is a nested
    [list](https://rdrr.io/r/base/list.html).

|        |             |               |       |                                                                                |
|--------|-------------|---------------|-------|--------------------------------------------------------------------------------|
| **\#** | **Name**    | **Data Type** | **V** | **Description**                                                                |
| `[,1]` | `NOFPOINTS` | `numeric`     | 08    | number of points in the definition (e.g., if the ROI is a rectangle: 4)        |
| `[,2]` | `USEDFOR`   | `logical`     | 08    | samples for which the ROI is used for; a maximum of 48 samples are allowed.    |
| `[,3]` | `SHOWNFOR`  | `logical`     | 08    | samples for which the ROI is shown for; a maximum of 48 samples are allowed.   |
| `[,4]` | `COLOR`     | `numeric`     | 08    | The colour values of the ROI.                                                  |
| `[,5]` | `X`         | `numeric`     | 08    | The x coordinates used to draw the ROI geometry (up to 50 points are allowed). |
| `[,6]` | `Y`         | `numeric`     | 08    | The y coordinates used to draw the ROI geometry (up to 50 points are allowed). |

(information on the BIN/BINX file format are kindly provided by Risø,
DTU Nutech)

## Objects from the Class

Objects can be created by calls of the form
`new("Risoe.BINfileData", ...)`.

## Function version

0.4.1

## How to cite

Kreutzer, S., 2025. Risoe.BINfileData-class(): Class
'Risoe.BINfileData'. Function version 0.4.1. In: Kreutzer, S., Burow,
C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Risø DTU, 2013. The Sequence Editor User Manual - Feb 2013 and Risø DTU,
2016.

The Sequence Editor User Manual - February 2016

<https://www.fysik.dtu.dk>

## See also

[plot_Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/plot_Risoe.BINfileData.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md),
[merge_Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/merge_Risoe.BINfileData.md),
[Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
based on information provided by Torben Lapp and Karsten Bracht Nielsen
(Risø DTU, Denmark) , RLum Developer Team

## Examples

``` r
showClass("Risoe.BINfileData")
#> Class "Risoe.BINfileData" [package "Luminescence"]
#> 
#> Slots:
#>                                        
#> Name:    METADATA       DATA  .RESERVED
#> Class: data.frame       list       list
```
