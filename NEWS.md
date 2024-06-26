




<!-- NEWS.md was auto-generated by NEWS.Rmd. Please DO NOT edit by hand!-->

# Changes in version 0.9.24 (2024-06-07)

**This package version requires R \>= 4.3**

## New functions

- `trim_RLum.Data()`: This new function enables trimming off the number
  of channels of all supported `RLum.Data-class` on the time domain. For
  instance, an OSL curve (`RLum.Data.Curve-class`) has 1000 channels;
  one may want to extract only channels 10 to 100. Moreover, sometimes,
  `RLum.Data-class` objects have a different number of channels but are
  otherwise measured with the exact time resolution. Until now, it was
  possible to merge those curves, but the subsequent analysis usually
  failed because most of the analysis functions check whether the number
  of channels matches.

- `import_Data()`: A convenience wrapper around all functions commencing
  with `read_`. The functions steps through all functions and the one
  that can import a file is chosen. This function simplifies writing of
  scripts for different data formats.

## The Thermochronometry functions (internal)

- Add internal function to import thermochronometry XLSX sheets into R;
  an R implementation of `STAGE1, ExcelToStructure`; the function has no
  user visible use for the moment.

## Bugfixes and changes

### `analyse_IRSARRF()`

- The function crashed for `RF_nat.lim` settings with two parameters;
  fixed (thanks to Mariana Sontag-Gonzalez for reporting)
- The function gained a new argument option for `method`: `"VSLIDE"`. If
  set, the vertical sliding with the range set to `"auto"` is used for
  the estimation of the equivalent dose.

### `analyse_SAR.CWOSL()`

- The function gained a new argument `trim_channels`. The default is
  `FALSE` to do not break existing code. If `TRUE` OSL and IRSL are
  curves are checked for the lowest number of channels on the curves and
  then all curves are trimmed accordingly using `trim_RLum.Data()`. This
  should fix an issue where data could not be analysed due to different
  channel numbers erroneously produced by the luminescence readers.
- The dose points names are returned as `factors` (which they are)
  instead of `characters`, with their class in the correct order. If you
  expect character values this may break existing code.
- The `$data` element gained two new columns `ALQ` and `POS` where the
  functions tries to store data about the running aliquot number and the
  position of the aliquot in the reader. The latter can only be accessed
  if this information was provided in measurement data (e.g., XSYG or
  BIN/BINX)
- The argument `rejection.criteria` gained a new option
  `recuperation_reference` preset to `"Natural"` to specify the
  regeneration reference dose point taken into account for calculating
  the recuperation rate (suggested by Anna-Maartje Boer and Jakob
  Wallinga)

### `calc_Huntely2006()`

- If the age is `NA` due to `Ln/Tn` values lower than the smallest
  simulated `Lx/Tx` value, a warning with an explanation is returned.
  Thanks to Christina Neudorf for providing the test dataset.
- Better catch a few uncontrolled errors and improve the success of the
  fitting (thanks to Salome Oehler for providing scripts and datasets)
- Regression: The changes in the last version led to unexpected function
  behaviour in cases where users tried the fading correction with only a
  few dose points in the lower part of the dose-response curve because
  the fits were no longer forced through the origin. This issue is now
  corrected, and the user can use the argument
  `fit.force_through_origin` (which is passed down to
  `plot_GrowthCurve()`) to force the fit again through the origin
  (thanks to Junjie Zhang for reporting the issue).

### `convert_Activity2Concentration()`

- The function did not work as expected if input was provided as
  abundance and the output data frame was missing values; fixed.
- The parameter `input_unit` now expects either `"activity"` (the
  default) or `"abundance"` instead of `"Bq/kg"` or `"ppm/%'`. The old
  input parameters are still silently accepted.
- **Potentially breaking change**: The naming of the output data frame
  now reports SI unit conform values, i.e. we `ppm` is no longer
  acceptable because it might have different interpretations. To reflect
  this change, in the function title we replaced “concentration” with
  “abundance”.

### `read_BIN2R()`

- Argument `n.records` can now be provided as a vector to specifically
  select records for the import. All other records are skipped and not
  imported. This can be useful in particular for very large
  BIN/BINX-files because allows importing single records very quickly
  without parsing the entire file
- The function is now less talkative for zero byte records and stopped
  spamming the console.
- The function learned about `RECTYPE` 128 records and can now import
  them (before those records were skipped). As it is in the original
  format, these information are only appended to the BIN/BINX file,
  means they do not really represent measurement data. *Please note that
  this support is highly experimental!*  
- The download of files now uses the new internal function
  `.download_file()`

### `read_Daybreak2R()`

- The function gained the `...` argument for compatibility reasons

### `read_RF2R()`

- The function gained the `...` argument for compatibility reasons

### `read_SPE2R()`

- The function gained the `...` argument for compatibility reasons
- The argument verbose was not fully respected; fixed

### `read_TIFF2R()`

- The function gained the `...` argument for compatibility reasons

### `read_XSYG2R()`

- Tries a little bit harder to determine the correct file path.
- Returns the file path as `@info` object in the `RLum.Analysis-class`
  output.
- If the file is not readable it only shows the R error and not anymore
  the internal error.

### `plot_DetPlot()`

- New argument `plot`: The function gained the new parameter `plot` to
  provide the possibility to disable the plot output for more complex
  operations
- Additional input: The function now understands lists of
  `RLum.Analysis-class` objects and iterates over those objects
  automatically.
- New argument `multicore`: As an addition to the list operation, the
  function can let it run in a multi core session
- The legend displayed the wrong labels for the equivalent dose and the
  `Ln/Tn` values; fixed.
- The legend is now disabled by default if no shine-down curve is shown;
  it can be activated again manually using `legend`.
- The par settings were not correctly reset; fixed.
- The function now supports the `trim_channel` argument from
  `analyse_SAR.CWOSL()`

### `plot_GrowthCurve()`

- Edge case: If the `Ln/Tn` values was way below the fit and
  `fit.method = EXP` was chosen for the fitting, the function still
  returned a positive equivalent dose for `mode = interpolation`. Now
  the returned value is `NA` since no interpolation was possible.
- Minor graphical polish: the dashed lines are drawn to the end of the
  plot area.

### `plot_RLum()`

- The function knows how to process results produces by
  `analyse_SAR.CWOSL()`, `analyse_pIRIRSequence()` and
  `analyse_IRSARRF()`; the functionality is very basic and for fast
  reporting situations only.

### `report_RLum()`

- The file `*.Rds` file path download did not function anymore; fixed.
- The function threw an non-conclusive warning; fixed.

### `Risoe.BINfileData-class()`

- The `show` method now knows about `RECTYPE` 128 and will account for
  it.

### `Risoe.BINFfileData2RLum.Analysis()`

- The function learned about `RECTYPE` 128 records. Those records
  contain ROI data from camera measurements and are skipped for the
  moment because they do not really belong to measurement data. This may
  change future.

## Internals

- Add internal download helper `.download_file()` for a more consistent
  approach to detect URL schemes and download files across the package
- Cover tricky edge case for the internal function
  `.expand_parameters()`. The function is used to recycle and expand
  function parameters if the input is a list object. It apparently
  showed odd behaviour if a function was used in a loop with objects of
  its own, e.g., `f(x = i + 1)`, then the function would recycle `i + 1`
  and then evaluate `i + 1` in the function environment. Usually, this
  should throw an error, but in particular, `i` is an object name
  commonly used in loops, so the function went upwards in the
  environments until it found the first `i`. However, this is not the
  `i` inserted by the user. The result was that the loop did not seem to
  work for no apparent reason. Now, `.expand_parameters()` evaluates
  input in the parent environment before passing it down.
