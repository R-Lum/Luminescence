# Extract Irradiation Times from an XSYG-file or `RLum.Analysis` object

Extracts irradiation times, dose and times since last irradiation, from
a Freiberg Instruments XSYG-file. These information can be further used
to update an existing BINX-file.

## Usage

``` r
extract_IrradiationTimes(
  object,
  file.BINX,
  recordType = c("irradiation (NA)", "IRSL (UVVIS)", "OSL (UVVIS)", "TL (UVVIS)"),
  return_same_as_input = FALSE,
  compatibility.mode = TRUE,
  txtProgressBar = TRUE
)
```

## Arguments

- object:

  [character](https://rdrr.io/r/base/character.html),
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  or [list](https://rdrr.io/r/base/list.html) (**required**): path and
  file name of the XSYG file or an
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  produced by the function
  [read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md);
  alternatively, a `list` of
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  can be provided.

  **Note**: If an
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  is used, any input for the arguments `file.BINX` and `recordType` will
  be ignored!

- file.BINX:

  [character](https://rdrr.io/r/base/character.html) (*optional*): path
  and file name of an existing BINX-file. If a file name is provided the
  file will be updated with the information from the XSYG file in the
  same folder as the original BINX-file.

  **Note:** The XSYG and the BINX-file must originate from the same
  measurement!

- recordType:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  select relevant curves types from the XSYG file or
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object. As the XSYG-file format comprises much more information than
  usually needed for routine data analysis and allowed in the BINX-file
  format, only the relevant curves are selected by using the function
  [get_RLum](https://r-lum.github.io/Luminescence/reference/get_RLum.md).
  The argument `recordType` works as described for this function.

  **Note:** A wrong selection will causes a function error. Please
  change this argument only if you have reasons to do so.

- return_same_as_input:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  set to `TRUE`, an updated
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object (or a [list](https://rdrr.io/r/base/list.html) of it) is
  returned, with each record having gained two new info element fields:
  `IRR_TIME` and `TIMESCINCEIRR`. This makes the
  [RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
  object compatible with external functions that search explicitly for
  `IRR_TIME` and `TIMESCINCEIRR`.

- compatibility.mode:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  whether all position values should be reset to a maximum value of 48
  (see
  [write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)).
  Only used if `file.BINX` is specified.

- txtProgressBar:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable the progress bar during import and export.

## Value

An
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
object is returned with the following structure:

    .. $irr.times (data.frame)

If `return_same_as_input = TRUE` an
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
or a [list](https://rdrr.io/r/base/list.html) of it, but we updated info
elements including irradiation times.

If a BINX-file path and name is set, the output will be additionally
transferred into a new BINX-file (with the function name as suffix)
located in the folder of the input BINX-file. Note that this will not
work if the input object is a file path to an XSYG-file, instead of a
link to only one file. In this case the argument input for `file.BINX`
is ignored.

In the self call mode (input is a `list` of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects a list of
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
is returned.

## Details

The function was written to compensate missing information in the
BINX-file output of Freiberg Instruments lexsyg readers. As all
information are available within the XSYG-file anyway, these information
can be extracted and used for further analysis or/and to stored in a new
BINX-file, which can be further used by other software, e.g., *Analyst*
(Geoff Duller).

Typical application example: *g*-value estimation from fading
measurements using the Analyst or any other self-written script.

Beside some simple data transformation steps, the function relies on
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)
for data import and export.

**Calculation details**

- The value `DURATION.STEP` is calculated as `START` + the end of the
  time axis for all curves except for `TL`, where the function tries to
  extract meta information about the duration.

- The value `END` is calculated as `START` + `DURACTION.STEP`

- All curves for which no prior irradiation was detected receive an `-1`
  (*Analyst* convention)

- Irradiation steps have always `IRR_TIME = 0`

## Note

The function can be also used to extract irradiation times from
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects imported via
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md)
with option `fastForward = TRUE`, or in combination with
[Risoe.BINfileData2RLum.Analysis](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData2RLum.Analysis.md).
Unfortunately the timestamp might not be very precise (or even invalid),
but it allows to essentially treat different formats in a similar
manner.

The produced output object contains still the irradiation steps to keep
the output transparent. However, for the BINX-file export this steps are
removed as the BINX-file format description does not allow irradiations
as separate sequences steps.

**BINX-file 'Time Since Irradiation' value differs from the table
output?**

The way the value 'Time Since Irradiation' is defined differs. In the
BINX-file the 'Time Since Irradiation' is calculated as the 'Time Since
Irradiation' plus the 'Irradiation Time'. The table output returns only
the real 'Time Since Irradiation', i.e. time between the end of the
irradiation and the next step.

**Negative values for `TIMESINCELAST.STEP`?**

Yes, this is possible and not a bug, as in the XSYG-file multiple curves
are stored for one step. Example: TL step may comprise three curves:

- \(a\) counts vs. time,

- \(b\) measured temperature vs. time and

- \(c\) predefined temperature vs. time.

Three curves, but they are all belonging to one TL measurement step, but
with regard to the time stamps this could produce negative values as the
important function
([read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md))
do not change the order of entries for one step towards a correct time
order.

**`TIMESINCELAST.STEP` is odd if TL curves are involved?**

Yes, this is possible! The end time is calculated as start + duration,
and the duration is deduced from the time values of each step. A typical
TL curve, however, does not have a time but a temperature axis. Hence,
the function tries to extract the information from metadata. If that
information is missing, the x-values are presumed time values, which
might still be wrong.

## Function version

0.4.0

## How to cite

Kreutzer, S., 2025. extract_IrradiationTimes(): Extract Irradiation
Times from an XSYG-file or RLum.Analysis object. Function version 0.4.0.
In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C.,
Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## References

Duller, G.A.T., 2015. The Analyst software package for luminescence
data: overview and recent improvements. Ancient TL 33, 35-42.
[doi:10.26034/la.atl.2015.489](https://doi.org/10.26034/la.atl.2015.489)

## See also

[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md),
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[Risoe.BINfileData](https://r-lum.github.io/Luminescence/reference/Risoe.BINfileData-class.md),
[read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.md),
[read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.md),
[write_R2BIN](https://r-lum.github.io/Luminescence/reference/write_R2BIN.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany) , RLum Developer Team

## Examples

``` r
## take system file
xsyg <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")

## the import is automatically
## but you can import it before
irr_times <- extract_IrradiationTimes(xsyg)
#> 
#> [read_XSYG2R()] Importing ...
#>  path:  /home/runner/work/_temp/Library/Luminescence/extdata
#>  file:  XSYG_file.xsyg
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#>   >> 1 of 1 sequence(s) loaded successfully
irr_times$irr.times
#>   POSITION             STEP               START DURATION.STEP
#> 1        1       TL (UVVIS) 2016-02-24 15:51:23          57.0
#> 2        1      OSL (UVVIS) 2016-02-24 15:53:24         129.9
#> 3        1 irradiation (NA) 2016-02-24 15:57:45          80.0
#> 4        1       TL (UVVIS) 2016-02-24 15:58:02          37.0
#>                   END IRR_TIME TIMESINCEIRR TIMESINCELAST.STEP
#> 1 2016-02-24 15:52:20        0           -1                0.0
#> 2 2016-02-24 15:55:33        0           -1               64.0
#> 3 2016-02-24 15:59:05        0           -1              131.1
#> 4 2016-02-24 15:58:39       80          -63              -63.0

if (FALSE) { # \dontrun{
# (1) - example for your own data

# set files and run function
file.XSYG <- file.choose()
file.BINX <- file.choose()

extract_IrradiationTimes(file.XSYG = file.XSYG, file.BINX = file.BINX)

# export results additionally to a CSV-file in the same directory as the XSYG-file
write.table(x = get_RLum(output),
 file = paste0(file.BINX,"_extract_IrradiationTimes.csv"),
 sep = ";",
 row.names = FALSE)
} # }
```
