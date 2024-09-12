#' Identify CW-OSL signal components in RLum.Analysis data sets
#'
#' First, all CW-OSL records are combined to one global average CW-OSL curve,
#' then the multi-exponential fitting approach of Bluszcz and Adamiec (2006) is applied.
#' This function processes [RLum.Analysis-class] data sets created within
#' the [Luminescence-package] (Kreutzer et al. 2012).
#'
#' The workflow of this function is as follows:
#'
#' \enumerate{
#'   \item [sum_OSLcurves]: Combine all measurements of type `record_type` to one global average curve.
#'   \item [fit_OSLcurve]: Identify OSL components by a multi-exponential fitting.
#'   \item Create a `html` report to summarize the results (*optional*).
#'}
#'
#' Data sets must be formatted as [RLum.Analysis-class] objects and
#' should have been processed with [RLum.OSL_correction] beforehand.
#' Output objects are also [RLum.Analysis-class] objects and are
#' meant for further analysis with [RLum.OSL_decomposition].
#'
#' If `report = TRUE`, a `html` report of the results is rendered by the [rmarkdown-package]
#' and saved in the working directory, which is usually the directory of the data file.
#' This report can be displayed, shared and published online without any requirements to
#' the operation system or installed software. However, an internet connection is needed to display
#' the *MathJax* encoded equations and special characters.
#' The *Rmarkdown* source code of the report can be found with the following command:
#'
#' `system.file("rmd", "report_Step1.Rmd", package = "OSLdecomposition")`
#'
#'
#'
#' @param object [RLum.Analysis-class] or [list] of [RLum.Analysis-class] (**required**):
#' Data set of one or multiple CW-OSL measured aliquots.
#'
#' @param record_type [character] (*with default*):
#' Type of records, selected by the [RLum.Analysis-class] attribute `@recordType`.
#' Common are: `"OSL"`,`"SGOSL"` or `"IRSL"`.
#'
#' @param K_maximum [numeric] (*with default*):
#' Maximum number of components *K*, see [fit_OSLcurve].
#'
#' @param F_threshold [numeric] (*with default*):
#' Fitting stop criterion, see [fit_OSLcurve].
#'
#' @param stimulation_intensity [numeric] (*with default*):
#' Intensity of optical stimulation in *mW / cm²*. Used to calculate photo-ionisation cross-sections, see [fit_OSLcurve].
#'
#' @param stimulation_wavelength [numeric] (*with default*):
#' Wavelength of optical stimulation in *nm*. Used to calculate photo-ionisation cross-sections, see [fit_OSLcurve].
#'
#' @param report [logical] (*with default*):
#' Creates a `html` report, saves it in the `report_dir` directory.
#' The report contains the results and detailed information on the data processing.
#'
#' @param report_dir [character] (*optional*):
#' Path of output directory if `report = TRUE`. If `report_dir = NULL` (default),
#' a temporary folder is used which is deleted when the R session is closed.
#' File paths are also allowed as parameter, then a new directory named after the OSL data file
#' will be created.
#'
#' @param image_format [character] (*with default*):
#' Image format of the automatically saved graphs if `report = TRUE` and `report_dir` is set.
#' Allowed are `.pdf`, `.eps`, `.svg` (vector graphics), `.jpg`, `.png`, `.bmp` (pixel graphics)
#' and more, see [ggplot2::ggsave]. The images are saved in the `report_dir` subfolder `/report_figures`.
#' Set `image_format = NULL` if no images shall be saved.
#'
#' @param open_report [logical] (*with default*):
#' If set to `TRUE` a browser window displaying the report will be opened automatically.
#'
#' @param rmd_path [character] (*with default*):
#' **For advanced users:** File path to the [rmarkdown] source code file of the report.
#' This allows to execute manipulated versions of the report.
#'
#' @param verbose [logical] (*with default*):
#' Enables console text output.
#'
#'
#' @section Last updates:
#'
#' 2022-05-02, DM: Added new parameter `open_report` to give control over automatic browser opening
#'
#' @author
#' Dirk Mittelstrass, \email{dirk.mittelstrass@@luminescence.de}
#'
#' Please cite the package the following way:
#'
#' Mittelstraß, D., Schmidt, C., Beyer, J., Heitmann, J. and Straessner, A.:
#' R package OSLdecomposition: Automated identification and separation of quartz CW-OSL signal components, *in preparation*.
#'
#' @seealso [RLum.OSL_correction], [RLum.OSL_decomposition], [sum_OSLcurves], [fit_OSLcurve]
#'
#' @references
#'
#' Bluszcz, A., Adamiec, G., 2006. Application of differential evolution to fitting OSL
#' decay curves. Radiation Measurements 41, 886–891.
#'
#' Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M., 2012.
#' Introducing an R package for luminescence dating analysis. Ancient TL, 30 (1), 1-8.
#'
#' @return
#'
#' The input `object`, a [list] of [RLum.Analysis-class] objects is returned but with
#' a new list element `object[["OSL_COMPONENTS"]]`, containing:
#' \itemize{
#'   \item `$decay.rates` [numeric] vector: Decay rates of F-test recommendation or last successful fitting.
#'   \item `$K.selected` [numeric]: Number of components of F-test recommendation or last successful fitting.
#'   \item `$F.test` [data.frame]: F-test table.
#'   \item `$F.test.print` [data.frame]: F-test table but formatted for console output and display with [knitr::kable].
#'   \item `$info.text` [list]: Short process log.
#'   \item `$component.tables` [list] of [data.frame]s: Signal component tables for all curve models.
#'   \item `$curve` [list]: Global average curve created from all  `record_type` curves in the data set.
#'   \item `$components` [data.frame]: Signal component table of F-test recommendation or last successful fitting.
#'   \item `$fit.results` [list]: Returned fitting objects of [DEoptim::DEoptim] and [minpack.lm::nlsLM] for all curve models.
#'   \item `$plot.data` [data.frame]: Model overview table for photo-ionisation cross-section plotting with [plot_PhotoCrosssections].
#'   \item `$parameters` [list]: Input and algorithm parameters.
#' }
#'
#' @examples
#'
#' # 'FB_10Gy' is a dose recovery test with the Fontainebleau quartz
#' # measured in a lexsyg research with green LED stimulation
#' data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
#' data_set <- Luminescence::read_BIN2R(data_path, fastForward = TRUE)
#'
#' # Check data set and perform background correction
#' data_set_corrected <- RLum.OSL_correction(data_set,
#'  background = 11,
#'  remove_light_off = FALSE)
#'
#' # Identify components
#' data_set_fitted <- RLum.OSL_global_fitting(
#'  data_set_corrected,
#'  K_maximum = 2,
#'  stimulation_intensity = 50,
#'  stimulation_wavelength = 530)
#'
#'
#' @md
#' @export

RLum.OSL_global_fitting <- function(object,
                                    record_type = "OSL",
                                    K_maximum = 5,
                                    F_threshold = 150,
                                    stimulation_intensity = 35,
                                    stimulation_wavelength = 470,
                                    report = FALSE,
                                    report_dir = NULL,
                                    image_format = "pdf",
                                    open_report = TRUE,
                                    rmd_path = NULL,
                                    verbose = TRUE){

  # Changelog:
  # * 2020-May  , DM: First reasonable version
  # * 2020-11-06, DM: Added roxygen documentation
  # * 2020-11-23, SK: Moved report call into utils.R
  # * 2021-02-15, DM: Added new parameter `rmd_path`
  # * 2022-05-02, DM: Added new parameter `open_report` to give control over automatic browser opening
  #
  # ToDo:
  # * Get stimulation.intensity from @info[["LPOWER"]]
  # * add 'autoname' and other file handling parameters
  # * add background fitting functionality

  # Hidden parameters
  report_format <- "html"

  # Get name of the input object
  object_name <- deparse(substitute(object))

  # define new list object to safely ignore incompatible list elements
  data_set <- list()
  data_set_overhang <- list()

  # Test if object is a list. If not, create a list
  if (is.list(object)) {

    for (i in 1:length(object)) {

      if (inherits(object[[i]], "RLum.Analysis")) {

        data_set[[length(data_set) + 1]] <- object[[i]]
      } else {

        element_name <- names(object)[i]

        if (element_name == "OSL_COMPONENTS") {

          warning("Input object already contains Step 1 results. Old results were overwritten")
        }else{

          data_set_overhang[[element_name]] <- object[[i]]
          if (!((element_name == "DECOMPOSITION")  || (element_name=="CORRECTION"))) {
            warning("Input object list element ", i, " is not of type 'RLum.Analysis' and was included in the fitting procedure, but was appended to the result list")}}}}

  } else {

    data_set <- list(object)
    warning("Input is not of type list, but output is of type list")}

  if (length(data_set) == 0) stop("Input object contains no RLum.Analysis data")

  # calc arithmetic mean curve
  if(verbose) cat("STEP 1.1 ----- Build global average curve from all CW-OSL curves -----\n")

  # measure computing time
  time.start <- Sys.time()

  global_curve <- sum_OSLcurves(data_set,
                                record_type = record_type,
                                output.plot = FALSE,
                                verbose = verbose)

  if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")


  # find components via fitting and F-statistics
  if(verbose) cat("STEP 1.2 ----- Perform multi-exponential curve fitting -----\n")

  time.start <- Sys.time()

  fit_data <- fit_OSLcurve(global_curve,
                         K.max = K_maximum,
                         F.threshold = F_threshold,
                         stimulation.intensity = stimulation_intensity,
                         stimulation.wavelength = stimulation_wavelength,
                         verbose = verbose,
                         output.complex = TRUE)

  # Add 'record_type' to the argument list
  fit_data$parameters$record_type <- record_type

  if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")


# Report output -----------------------------------------------------------
  if (report) {

    if(verbose) cat("STEP 1.3 ----- Create report -----\n")

    .render_report(
      nature = "global_fitting",
      fit_data = fit_data,
      data_set = data_set,
      object_name = object_name,
      image_format = image_format,
      report_dir = report_dir,
      open_report = open_report,
      rmd_path = rmd_path,
      verbose = verbose)}

# Return ------------------------------------------------------------------
  object <- c(data_set, data_set_overhang, OSL_COMPONENTS = list(fit_data))
  invisible(object)
}
