#' @title Analyse SAR CW-OSL Measurements
#'
#' @description
#' The function performs a SAR CW-OSL analysis on an [Luminescence::RLum.Analysis-class]
#' object, including growth curve fitting.
#'
#' @details
#' The function performs an analysis for standard SAR protocol measurements
#' introduced by Murray and Wintle (2000) with CW-OSL curves. For the
#' calculation of the `Lx/Tx` value the function [Luminescence::calc_OSLLxTxRatio] is
#' used. To **change the way the Lx/Tx error is calculated** use arguments
#' `background.count.distribution` and `sigmab`, which will be passed to
#' [Luminescence::calc_OSLLxTxRatio].
#'
#' **What is part of a SAR sequence?**
#'
#' The function is rather picky when it comes to accepted curve input
#' (OSL, IRSL,...) and structure. A SAR sequence is basically a set of
#' \eqn{L_{x}/T_{x}} curves. Hence, every second curve is considered a
#' shine-down curve related to the test dose. It also means that the number of
#' curves for \eqn{L_{x}} has to be equal to the number of \eqn{T_{x}} curves,
#' and that hot-bleach curves **do not** belong in a SAR sequence; at least
#' not for the analysis. Other curves allowed and processed are preheat curves,
#' or preheat curves measured as TL, and irradiation curves. The latter
#' indicates the duration of the irradiation, the dose and test dose points,
#' e.g., as part of XSYG files.
#'
#' **Argument `object` is of type `list`**
#'
#' If the argument `object` is of type [list] containing **only**
#' [Luminescence::RLum.Analysis-class] objects, the function re-calls itself on each element
#' in the list. This is useful for analysing an entire measurement without
#' writing separate for-loops. To gain full control of the parameters (e.g., `dose.points`) for
#' every aliquot (corresponding to one [Luminescence::RLum.Analysis-class] object in the list), in
#' this case the arguments can be provided as [list]. This `list` should
#' be of similar length as the `list` provided with the argument `object`,
#' otherwise the function will create an own list of the requested length.
#' Function output will be just one single [Luminescence::RLum.Results-class] object.
#'
#' Please be careful when using this option. While it may allow for a fast and
#' efficient data analysis, the function may break with an unclear error
#' message if the input data is misspecified.
#'
#' **Working with IRSL data**
#'
#' The function was originally designed to work just for 'OSL' curves,
#' following the principles of the SAR protocol. An IRSL measurement protocol
#' may follow this procedure, e.g., post-IR IRSL protocol (Thomsen et al.,
#' 2008). Therefore this function has been enhanced to work with IRSL data,
#' however, the function is only capable of analysing curves that follow the
#' SAR protocol structure, i.e., to analyse a post-IR IRSL protocol, curve data
#' have to be pre-selected by the user to fit the standards of the SAR
#' protocol, i.e., Lx,Tx,Lx,Tx and so on.
#'
#' Example: Imagine the measurement contains `pIRIR50` and `pIRIR225` IRSL
#' curves. Only one curve type can be analysed at the same time: either the
#' `pIRIR50` curves or the `pIRIR225` curves.
#'
#' **Supported rejection criteria**
#'
#' `[recycling.ratio]`: calculated for every repeated regeneration dose point.
#'
#' `[recuperation.rate]`: recuperation rate calculated by comparing the
#' `Lx/Tx` values of the zero regeneration point with the `Ln/Tn` value (the
#' `Lx/Tx` ratio of the natural signal). For methodological background see
#' Aitken and Smith (1988). As a variant, `recuperation_reference` can be
#' specified to select another dose point as reference instead of `Ln/Tn`.
#'
#' `[testdose.error]`: set the allowed error for the test dose, which by
#' default should not exceed 10%. The test dose error is calculated as
#' `Tx_net.error/Tx_net`. The calculation of the \eqn{T_{n}} error is detailed
#' in [Luminescence::calc_OSLLxTxRatio].
#'
#' `[palaeodose.error]`: set the allowed error for the De value, which by
#' default should not exceed 10%.
#'
#' `[sn.ratio]`: set the allowed signal/noise ratio, which by default should
#' be at least 50. By default it uses the value from the natural curve, but
#' this can be changed by specifying the `sn_reference` option.
#'
#' By default, the computed values are compared directly to the corresponding
#' thresholds to establish their result status ("OK" or "FAILED"). By setting
#' the option `consider.uncertainties = TRUE` in the `rejection.criteria`
#' list, quantified uncertainties are considered in the computation of the
#' test value before comparing it to the threshold(currently supported
#' only for `recycling.ratio`, `recuperation.rate` and `exceed.max.regpoint`).
#' This reduces tests being marked as "FAILED" when the deviation from the
#' threshold is smaller than the uncertainty margin.
#'
#' **Irradiation times**
#'
#' The function makes two attempts to extract irradiation data (dose points)
#' automatically from the input object, if the argument `dose.points` is not
#' set (aka set to `NULL`).
#'
#' 1. It searches in every curve for an info object called `IRR_TIME`. If this
#' is found, any value set there is taken as dose point.
#'
#' 2. If the object contains curves of type `irradiation`, the function tries
#' to use this information to assign these values to the curves. However, the
#' function does **not** overwrite values preset in `IRR_TIME`.
#'
#' @param object [Luminescence::RLum.Analysis-class] (**required**):
#' input object containing data for analysis, alternatively a [list] of
#' [Luminescence::RLum.Analysis-class] objects can be provided. The object
#' should **only** contain curves considered part of the SAR protocol (see
#' Details). When `OSL.component` is set, the input object must have been
#' processed by `OSLdecomposition::RLum.OSL_decomposition()`.
#'
#' @param signal_integral [integer] (**required**):
#' vector of channels for the signal integral. It can be a [list] of integers,
#' if `object` is a list. If set to `NULL` (default) or `NA`, no integrals are
#' taken into account and their settings are ignored.
#'
#' @param background_integral [integer] (**required**):
#' vector of channels for the background integral. It can be a [list] of
#' integers, if `object` is a list. If set to `NULL` (default), no integrals
#' are taken into account and their settings are ignored. If set to `NA`, no
#' background integral is subtracted.
#'
#' @param signal_integral_Tx [integer] (*optional*):
#' vector of channels for the signal integral for the `Tx` curve. It can be a
#' [list] of integers, if `object` is a list. If `NULL`, the `signal_integral`
#' vector is used also for the `Tx` curve.
#'
#' @param background_integral_Tx [integer] (*optional*):
#' vector of channels for the background integral for the `Tx` curve. It can
#' be a [list] of integers, if `object` is a list. If `NULL`, the
#' `background_integral` vector is used. If set to `NA`, no background integral
#' for the `Tx` curve is subtracted.
#'
#' @param integral_input [character] (*with default*):
#' input type for `signal_integral`, one of `"channel"` (default) or
#' `"measurement"`. If set to `"measurement"`, the best matching channels
#' corresponding to the given time range (in seconds) are selected.
#'
#' @param OSL.component [character] or [integer] (*optional*):
#' single index or a [character] defining the signal component to be evaluated.
#' It requires that the object was processed by `OSLdecomposition::RLum.OSL_decomposition`.
#' This argument can either be the name of the OSL component assigned by
#' `OSLdecomposition::RLum.OSL_global_fitting` or the index in descending
#' order of decay rates. Then `"1"` selects the fastest decaying component, `"2"`
#' the second fastest and so on. Can be a [list] of [integer]s or strings (or mixed)
#' If object is a [list] and this parameter is provided as [list] it alternates over
#' the elements (aliquots) of the object list, e.g., `list(1,2)` processes the first
#' aliquot with component `1` and the second aliquot with component `2`.
#' `NULL` does not process any component.
#'
#' @param rejection.criteria [list] (*with default*):
#' provide a *named* list and set rejection criteria in **percentage**. It can
#' be a nested [list], if `object` is of type [list].
#' Note: *unnamed* list elements are ignored.
#'
#' Allowed options:
#' * `recycling.ratio` [numeric] (default: `10`)
#' * `recuperation.rate` [numeric] (default: `10`)
#' * `palaeodose.error` [numeric] (default: `10`)
#' * `testdose.error` [numeric] (default: `10`)
#' * `sn.ratio` [numeric] (default: `NA`)
#' * `exceed.max.regpoint` [logical] (default: `FALSE`)
#' * `consider.uncertainties` [logical] (default: `FALSE`)
#' * `recuperation_reference` [character] (default: `"Natural"`; set to, e.g.,
#'   `"R1"` for another point)
#' * `sn_reference` [character] (default: `"Natural"`).
#'
#' Example: `rejection.criteria = list(recycling.ratio = 10)`.
#'
#' All numerical criteria can be set to `NA`, in which case values are
#' calculated, but they are not considered, and their corresponding RC.Status
#' is always `"OK"`. In the "Checks" plot, they are shown with a grey circle
#' and only their value is reported (without showing `<= NA` or `>= NA`).
#'
#' If `onlyLxTxTable = TRUE`, the `palaeodose.error` and `exceed.max.regpoint`
#' criteria are not computed.
#'
#' @param dose.points [numeric] (*optional*):
#' a numeric vector containing the dose point values. Using this argument
#' overwrites dose point values extracted from other data. Can be a [list] of
#' [numeric] vectors, if `object` is of type [list].
#'
#' @param dose.points.test [numeric] (*optional*):
#' a numeric vector containing the test dose in the same units as `dose.points`.
#' If length = 1, the values will be recycled. It has only an effect for
#' `fit.method = 'OTORX'`.
#'
#' @param dose_rate_source [numeric] (*optional*):
#' numerical value for the source dose rate, typically in Gy/s. If set, the
#' x-axis default for the dose-response curve changes to `Dose [Gy]`.
#'
#' @param trim_channels [logical] (*with default*):
#' trim channels per record category to the lowest number of channels in the
#' category by using [Luminescence::trim_RLum.Data]. Applies only to `OSL` and `IRSL` curves.
#' For a more granular control use [Luminescence::trim_RLum.Data] before calling this
#' function.
#'
#' @param mtext.outer [character] (*optional*):
#' option to provide an outer margin `mtext`. Can be a [list] of [character]s,
#' if `object` is of type [list]
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param plot_onePage [logical] (*with default*):
#' enable/disable plotting all subplots on one page.
#'
#' @param plot_singlePanels [logical] (*with default*) or [numeric] (*optional*):
#' control the plotting of subplots in single windows (one subplot per page).
#' Using a [numeric] vector allows to select the subplots individually;
#' setting it to `TRUE` corresponds to `plot_singlePanels = 1:8`. For example,
#' `plot_singlePanels = c(1,2,3,4)` will plot the TL and Lx, Tx curves;
#' `plot_singlePanels = c(5,6,7,8)` will plot the legend (5), the dose-response
#' curve (6), the rejection criteria (7), and either the IRSL curve or the
#' single grain (8). It is ignored if `plot = FALSE` or `plot_onePage = TRUE`.
#'
#' @param onlyLxTxTable [logical] (*with default*):
#' If `TRUE` the dose response curve fitting and plotting is skipped, and the
#' `palaeodose.error` and `exceed.max.regpoint` criteria are not computed.
#' This allows to get hands on the `Lx/Tx` table for large datasets
#' without the need for a curve fitting.
#'
#' @param method_control [list] (*optional*):
#' options to control the function behaviour. Currently only the
#' 'auto_curve_removal' (logical) option is supported, which controls whether
#' curves with `recordType` starting with `_` should be automatically removed
#' (`TRUE` by default).
#'
#' @param ... further arguments that will be passed to
#' [Luminescence::fit_DoseResponseCurve], [Luminescence::plot_DoseResponseCurve]
#' or [Luminescence::calc_OSLLxTxRatio] (supported:
#' `background.count.distribution`, `sigmab`, `sig0`, `od_rates`).
#' Additionally, supported are `legend.cex` and `legend.pch` to modify the
#' legend symbols.
#
#' **Note:** If you consider using the early light subtraction method,
#' `sigmab` should be provided.
#'
#' **Note:** `od_rates` can be used to treat uncertainties in Lx/Tx according
#' to Bluszcz et al. (2015) instead of the standard approach of Galbraith
#' (2002, 2014). See [Luminescence::calc_OSLLxTxRatio] for details.
#'
#' @return
#' A plot (*optional*) and an [Luminescence::RLum.Results-class] object is
#' returned containing the following elements:
#'
#' \item{data}{[data.frame] containing De-values, De-error and further parameters}
#' \item{LnLxTnTx.values}{[data.frame] of all calculated Lx/Tx values including signal,
#' background counts and the dose points}
#' \item{rejection.criteria}{[data.frame] with values that might by used as rejection criteria.
#' `NA` is produced if no R0 dose point exists.}
#' \item{Formula}{[formula] formula that have been used for the growth curve fitting}
#' \item{.plot.data}{List used internally for plotting.}
#'
#' The output should be accessed using the function [Luminescence::get_RLum].
#'
#' **The function currently supports only 'OSL', 'IRSL' and 'POSL' data!**
#'
#' @section Function version: 0.13.9
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Luminescence::calc_OSLLxTxRatio], [Luminescence::fit_DoseResponseCurve],
#' [Luminescence::plot_DoseResponseCurve], [Luminescence::RLum.Analysis-class],
#' [Luminescence::RLum.Results-class]
#'
#' @references
#' Aitken, M.J. and Smith, B.W., 1988. Optical dating: recuperation
#' after bleaching. Quaternary Science Reviews 7, 387-393.
#'
#' Duller, G., 2003. Distinguishing quartz and feldspar in single grain
#' luminescence measurements. Radiation Measurements 37 (2), 161-165.
#'
#' Murray, A.S. and Wintle, A.G., 2000. Luminescence dating of quartz using an
#' improved single-aliquot regenerative-dose protocol. Radiation Measurements
#' 32, 57-73.
#'
#' Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. Laboratory
#' fading rates of various luminescence signals from feldspar-rich sediment
#' extracts. Radiation Measurements 43, 1474-1486.
#' doi:10.1016/j.radmeas.2008.06.002
#'
#' Bluszcz, A., Adamiec, G., Herr, A., 2015. Estimation of equivalent dose and
#' its uncertainty in the OSL SAR protocol when count numbers do not follow a
#' Poisson distribution. Radiation Measurements 81, 46-54.
#' doi:10.1016/j.radmeas.2015.01.004
#'
#' @keywords datagen plot
#'
#' @examples
#'
#' ##load data
#' ##ExampleData.BINfileData contains two BINfileData objects
#' ##CWOSL.SAR.Data and TL.SAR.Data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##transform the values from the first position in a RLum.Analysis object
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
#' ##perform SAR analysis and set rejection criteria
#' results <- analyse_SAR.CWOSL(
#' object = object,
#' signal_integral = 1:2,
#' background_integral = 900:1000,
#' log = "x",
#' fit.method = "EXP",
#' plot_onePage = TRUE,
#' rejection.criteria = list(
#'   recycling.ratio = 10,
#'   recuperation.rate = 10,
#'   testdose.error = 10,
#'   palaeodose.error = 10,
#'   recuperation_reference = "Natural",
#'   sn.ratio = 50,
#'   sn_reference = "Natural",
#'   exceed.max.regpoint = TRUE)
#')
#'
#' ##show De results
#' get_RLum(results)
#'
#' ##show LnTnLxTx table
#' get_RLum(results, data.object = "LnLxTnTx.table")
#'
#' ## Run example with special case for
#' ## the OTORX fit
#' \dontrun{
#' results <- analyse_SAR.CWOSL(
#'  object = object,
#'  signal_integral = 1:2,
#'  background_integral = 900:1000,
#'  dose.points.test = 15,
#'  n.MC = 10,
#'  fit.method = "OTORX")
#' }
#'
#' @export
analyse_SAR.CWOSL<- function(
  object,
  signal_integral = NULL,
  background_integral = NULL,
  signal_integral_Tx = NULL,
  background_integral_Tx = NULL,
  integral_input = c("channel", "measurement"),
  OSL.component = NULL,
  rejection.criteria = list(),
  dose.points = NULL,
  dose.points.test = NULL,
  dose_rate_source = NULL,
  trim_channels = FALSE,
  mtext.outer = "",
  plot = TRUE,
  plot_onePage = FALSE,
  plot_singlePanels = FALSE,
  onlyLxTxTable = FALSE,
  method_control = list(),
  ...
) {
  .set_function_name("analyse_SAR.CWOSL")
  on.exit(.unset_function_name(), add = TRUE)

  ## deprecated arguments
  extraArgs <- list(...)
  has_deprecated_args <- any(grepl("[signal|background]\\.integral\\.[min|max]",
                                   names(extraArgs)))
  has_deprecated_values <- !is.null(c(extraArgs$signal.integral.min,
                                      extraArgs$signal.integral.max,
                                      extraArgs$background.integral.min,
                                      extraArgs$background.integral.max))

  if (has_deprecated_args && has_deprecated_values &&
      (!(is.null(signal_integral) || anyNA(signal_integral)) ||
       !(is.null(background_integral) || anyNA(background_integral)))) {
    .throw_error("Convert all integral arguments to the new names, ",
                 "'signal_integral' and 'background_integral'")
  }

  ## Self-call --------------------------------------------------------------
  if (inherits(object, "list")) {
  ##clean object input and expand parameters
  object <- .rm_nonRLum(object)
  parm <- .expand_parameters(length(object))

  ##handle main separately
  if ("main" %in% ...names()) {
    main <- .listify(list(...)$main, length = length(object))
  }else{
    main <- as.list(paste0("ALQ #",1:length(object)))
  }

    ## deprecated arguments
    if (has_deprecated_args) {
      for (name in c("signal.integral.min", "signal.integral.max",
                     "background.integral.min", "background.integral.max"))
        extraArgs[[name]] <- .listify(extraArgs[[name]], length(object))
    }

    ## remove unnamed rejection criteria
    if (!is.null(parm$rejection.criteria)) {
      parm$rejection.criteria <- lapply(parm$rejection.criteria,
                                        .rm_unnamed_elements)
    }

  results <- .warningCatcher(merge_RLum(lapply(seq_along(object), function(x){
    analyse_SAR.CWOSL(
      object = object[[x]],
      signal_integral = parm$signal_integral[[x]],
      background_integral = parm$background_integral[[x]],
      integral_input = parm$integral_input[[x]],
      OSL.component = parm$OSL.component[[x]],
      dose.points = parm$dose.points[[x]],
      dose.points.test = parm$dose.points.test[[x]],
      dose_rate_source = parm$dose_rate_source[[x]],
      trim_channels = parm$trim_channels[[x]],
      mtext.outer = parm$mtext.outer[[x]],
      plot = parm$plot[[x]],
      rejection.criteria = parm$rejection.criteria[[x]],
      plot_singlePanels = parm$plot_singlePanels[[x]],
      plot_onePage = parm$plot_onePage[[x]],
      onlyLxTxTable = parm$onlyLxTxTable[[x]],
      main = main[[x]],
      method_control = method_control,

      ## deprecated arguments
      signal.integral.min = extraArgs$signal.integral.min[[x]],
      signal.integral.max = extraArgs$signal.integral.max[[x]],
      background.integral.min = extraArgs$background.integral.min[[x]],
      background.integral.max = extraArgs$background.integral.max[[x]],

      ## internal information
      .aliquot_number = sprintf("ALQ: #%d | ", x),

      ...)
  })))

  ##return
  if(length(results) == 0) return(NULL)

  ## add aliquot number
  results@data$data$ALQ <- seq_along(object)[1:nrow(results@data$data)]

  return(results)
}

# CONFIG  -----------------------------------------------------------------

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "RLum.Analysis")
  integral_input <- .validate_args(integral_input, c("channel", "measurement"))
  .validate_class(plot_singlePanels, c("logical", "integer", "numeric"))
  .validate_logical_scalar(trim_channels)
  .validate_logical_scalar(plot)
  .validate_logical_scalar(plot_onePage)
  .validate_logical_scalar(onlyLxTxTable)
  .validate_class(rejection.criteria, "list", null.ok = TRUE)
  .validate_class(dose.points, c("numeric", "integer"), null.ok = TRUE)
  .validate_class(dose.points.test, c("numeric", "integer"), null.ok = TRUE)
  .validate_scalar(dose_rate_source, null.ok = TRUE)
  .validate_class(method_control, "list")
  method_control <- modifyList(x = list(auto_curve_removal = TRUE),
                               val = method_control)

  ## Protocol integrity checks ----------------------------------------------

  ## trim OSL or IRSL channels
  record.types <- vapply(object@records, function(x) x@recordType, character(1))
  if (trim_channels) {
    ## fetch names with OSL and IRSL
    tmp_names <- unique(record.types)
    tmp_names <- grep("OSL|IRSL", tmp_names, value = TRUE, perl = TRUE)

    ## trim
    object <- trim_RLum.Data(object, recordType = tmp_names)
  }

  ## remove curves starting with _
  if (isTRUE(method_control[["auto_curve_removal"]])) {
    object <- remove_RLum(object, recordType = "^_")
  }

  ## extract the correct curves for the sequence based on allowed curve types
  ## and the most common curve type (after stripping extra specifiers in the
  ## curve names)
  stripped.curve.types <- regmatches(
      x = names(object),
      m = regexpr("(P?OSL[a-zA-Z]*|IRSL[a-zA-Z]*)", names(object), perl = TRUE))

  if (length(stripped.curve.types) == 0) {
    .throw_message("No record of type 'OSL', 'IRSL', 'POSL' detected, ",
                   "NULL returned")
    return(NULL)
  }

  ## now get the type which is used most
  CWcurve.type <- names(which.max(table(stripped.curve.types)))

  ##check overall structure of the object
  ##every SAR protocol has to have equal number of curves

  ## grep curve types from analysis value and remove unwanted information
  temp.ltype <- sapply(1:length(object@records), function(x) {
    ## export as global variable
    object@records[[x]]@recordType <<- gsub(" .*", "", object@records[[x]]@recordType)
    object@records[[x]]@recordType
  })

  ## FI lexsyg devices provide irradiation information in a separate curve
  if (any("irradiation" %in% temp.ltype)) {
    temp.irradiation <- extract_IrradiationTimes(object)@data$irr.times[["IRR_TIME"]]

    ## add this to the records
    for (i in 1:length(object@records)) {
      if (is.null(object@records[[i]]@info$IRR_TIME))
        object@records[[i]]@info <- c(object@records[[i]]@info,
                                      IRR_TIME = temp.irradiation[i])
    }

    ## remove irradiation curves
    object <- get_RLum(object, record.id = !temp.ltype %in% "irradiation",
                       drop = FALSE)
  }

  ## collect error messages to be reported together
  error.list <- list()

  ## check if the wanted curves are a multiple of two
  if (table(temp.ltype)[CWcurve.type] %% 2 != 0) {
    error.list[[1]] <- "Input OSL/IRSL curves are not a multiple of two"
  }

  ## check if the curve lengths differ
  temp.matrix.length <- unlist(lapply(object@records,
                                      function(x) {
                                        if (x@recordType %in% CWcurve.type)
                                          nrow(x@data)
                                      }))

  if (length(unique(temp.matrix.length)) != 1) {
    ## check if the selected curve type (stripped of specifiers) corresponds to
    ## multiple record types, as that may be problematic if they have different
    ## numbers of channels
    matched.types <- unique(grep(CWcurve.type, record.types,
                                 fixed = TRUE, value = TRUE))
    if (length(matched.types) > 1) {
      .throw_warning("Curve type '", CWcurve.type, "' matches multiple record types: ",
                     .collapse(matched.types), ", please ensure that your curve ",
                     "selection is correct")
    }

    hint <- if (trim_channels) "" else ", consider setting 'trim_channels = TRUE'"
    error.list[[2]] <- paste0("Input curves have different lengths (",
                              .collapse(sort(unique(temp.matrix.length)),
                                        quote = FALSE), ")", hint)
  }

  ## return early in case of errors
  if (length(error.list) > 0) {
    .throw_warning(paste(unlist(error.list), collapse = "\n"),
                   "\n... >> nothing was done here!")
    return(NULL)
  }

  ## maximum number of channels available
  channel.length <- temp.matrix.length[1]

  ## Integrals checks -------------------------------------------------------

  ## deprecated arguments
  if (has_deprecated_args && has_deprecated_values) {
    .deprecated(old = c("signal.integral.min", "signal.integral.max",
                        "background.integral.min", "background.integral.max"),
                new = c("signal_integral", "background_integral"),
                since = "1.2.0")
    if (integral_input != "channel") {
      .throw_error("'integral_input' is not supported with old argument names")
    }
    signal.integral.min <- extraArgs$signal.integral.min %||% NA
    signal.integral.max <- extraArgs$signal.integral.max %||% NA
    background.integral.min <- extraArgs$background.integral.min %||% NA
    background.integral.max <- extraArgs$background.integral.max %||% NA
    signal_integral_Tx <- background_integral_Tx <- NULL
    if (anyNA(c(signal.integral.min, signal.integral.max,
                 background.integral.min, background.integral.max))) {
      signal_integral <- background_integral <- NA
    } else {
      signal_integral <- signal.integral.min[1]:signal.integral.max[1]
      background_integral <- background.integral.min[1]:background.integral.max[1]
      if (length(signal.integral.min) == 2 && length(signal.integral.max) == 2)
        signal_integral_Tx <- signal.integral.min[2]:signal.integral.max[2]
      if (length(background.integral.min) == 2 && length(background.integral.max) == 2)
        background_integral_Tx <- background.integral.min[2]:background.integral.max[2]
    }
  }

  if (is.null(signal_integral) || .strict_na(signal_integral) ||
      is.null(background_integral)) {
    signal_integral <- background_integral <- NA
    signal_integral_Tx <- background_integral_Tx <- NULL
    if (is.null(OSL.component)) {
      .throw_warning("No signal or background integral applied as ",
                     "'signal_integral = NULL' (or NA) or 'background_integral = NULL' ",
                     "(and 'OSL.component' was not specified)")
    }
  } else {
    ## convert integrals to channels
    if (integral_input == "measurement") {
      x.range <- get_RLum(object, recordType = CWcurve.type)[[1]][, 1]
      unit <- "time"
      signal_integral <- .convert_to_channels(x.range, signal_integral, unit,
                                              na.ok = TRUE)
      background_integral <- .convert_to_channels(x.range, background_integral, unit,
                                                  na.ok = TRUE)
      signal_integral_Tx <- .convert_to_channels(x.range, signal_integral_Tx, unit,
                                                 null.ok = TRUE)
      background_integral_Tx <- .convert_to_channels(x.range, background_integral_Tx, unit,
                                                     na.ok = TRUE, null.ok = TRUE)
    }

    signal_integral <- .validate_integral(signal_integral, max = channel.length,
                                          null.ok = TRUE, na.ok = TRUE)
    background_integral <- .validate_integral(background_integral,
                                              null.ok = TRUE, na.ok = TRUE,
                                              min = max(signal_integral) + 1,
                                              max = channel.length)

    if (length(background_integral) == 1 && !.strict_na(background_integral)) {
      ## we subtract 25 to avoid warnings from calc_OSLLxTxRatio()
      background_integral <- background_integral - 25:0
      .throw_warning("Background integral should contain at least two values, reset to ",
                     .format_range(background_integral))
    }

    ## signal integrals for the Tx curve
    signal_integral_Tx <- .validate_integral(signal_integral_Tx,
                                             max = channel.length,
                                             null.ok = TRUE)
    if (is.null(signal_integral_Tx) && !is.null(background_integral_Tx)) {
      signal_integral_Tx <- signal_integral
      .throw_warning("'signal_integral_Tx' set automatically to ",
                     .format_range(signal_integral_Tx))
    }

    ## background integrals for the Tx curve
    background_integral_Tx <- .validate_integral(background_integral_Tx,
                                                 min = max(signal_integral_Tx) + 1,
                                                 max = channel.length,
                                                 na.ok = TRUE, null.ok = TRUE)
    if (!is.null(signal_integral_Tx) && is.null(background_integral_Tx)) {
      background_integral_Tx <- background_integral
      .throw_warning("'background_integral_Tx' set automatically to ",
                     if (.strict_na(background_integral_Tx)) NA
                     else .format_range(background_integral_Tx))
    }
    if (length(background_integral_Tx) == 1 && !.strict_na(background_integral_Tx)) {
      background_integral_Tx <- background_integral_Tx - 25:0
      .throw_warning("Background integral limits for Tx curves cannot be equal, reset to ",
                     .format_range(background_integral_Tx))
    }
  }

  ## Rejection criteria -----------------------------------------------------

  rejection.criteria <- modifyList(x = list(
      recycling.ratio = 10,
      recuperation.rate = 10,
      palaeodose.error = 10,
      testdose.error = 10,
      sn.ratio = NA,
      exceed.max.regpoint = TRUE,
      consider.uncertainties = FALSE,
      sn_reference = "Natural",
      recuperation_reference = "Natural"
    ),
    val = rejection.criteria %||% list(),
    keep.null = TRUE)

  consider.uncertainties <- rejection.criteria$consider.uncertainties
  .validate_logical_scalar(consider.uncertainties,
                           name = "'consider.uncertainties' in 'rejection.criteria'")
  recuperation_reference <- rejection.criteria$recuperation_reference
  .validate_class(recuperation_reference, "character", length = 1,
                  name = "'recuperation_reference' in 'rejection.criteria'")
  sn_reference <- rejection.criteria$sn_reference
  .validate_class(sn_reference, "character", length = 1,
                  name = "'sn_reference' in 'rejection.criteria'")

  ## Deal with extra arguments ----------------------------------------------

  verbose <- .validate_logical_scalar(extraArgs$verbose %||% TRUE, name = "'verbose'")
  main <- extraArgs$main %||% ""
  log <- extraArgs$log %||% ""
  cex <- extraArgs$cex %||% 1
  legend.cex <- extraArgs$legend.cex %||% 4
  legend.pch <- extraArgs$legend.pch %||% 20

  background.count.distribution <-
      extraArgs$background.count.distribution %||% "non-poisson"
  sigmab <- extraArgs$sigmab
  sig0 <- extraArgs$sig0 %||% 0
  od_rates <- extraArgs$od_rates

  # Grep Curves -------------------------------------------------------------
  ## extract relevant curves from RLum.Analysis object
  OSL.Curves.ID <- get_RLum(object, recordType = CWcurve.type, get.index = TRUE)
  TL.Curves.ID <- get_RLum(object, recordType = "TL$", get.index = TRUE)

  ## separate curves by Lx and Tx using vector recycling
  OSL.Curves.ID <- list(Lx = OSL.Curves.ID[c(TRUE, FALSE)], # odd elements
                        Tx = OSL.Curves.ID[c(FALSE, TRUE)]) # even elements

  ## separate TL curves which are always coming before the OSL curve
  ## Note: we do not check anymore whether the sequence makes sense
  TL.Curves.ID <- list(Lx = TL.Curves.ID[TL.Curves.ID %in% (OSL.Curves.ID$Lx - 1)],
                       Tx = TL.Curves.ID[TL.Curves.ID %in% (OSL.Curves.ID$Tx - 1)])

# Calculate LnLxTnTx values  --------------------------------------------------
  ##calculate LxTx values using external function
  if (length(OSL.component) > 0) {
    if (is.null(object@records[[OSL.Curves.ID$Lx[1]]]@info$COMPONENTS)) {
      .throw_warning("'object' does not appear to have been processed by ",
                     "OSLdecomposition::RLum.OSL_decomposition()")
    }
    LnLxTnTx <- try(lapply(seq_along(OSL.Curves.ID$Lx), function(i) {
      get_RLum(
          calc_OSLLxTxDecomposed(
              Lx.data = object@records[[OSL.Curves.ID$Lx[i]]]@info$COMPONENTS,
              Tx.data = object@records[[OSL.Curves.ID$Tx[i]]]@info$COMPONENTS,
              OSL.component = OSL.component,
              digits = 4,
              sig0 = sig0))
    }), silent = TRUE)
  } else {
    LnLxTnTx <- try(get_RLum(
        calc_OSLLxTxRatio(
            Lx.data = object@records[OSL.Curves.ID$Lx],
            Tx.data = object@records[OSL.Curves.ID$Tx],
            signal_integral = signal_integral,
            signal_integral_Tx = signal_integral_Tx,
            background_integral = background_integral,
            background_integral_Tx = background_integral_Tx,
            background.count.distribution = background.count.distribution,
            sigmab = sigmab,
            sig0 = sig0,
            od_rates = od_rates)
    ), silent = TRUE)
  }

  ## catch errors generated in calc_OSLLxTxDecomposed() or calc_OSLLxTxRatio()
  if (inherits(LnLxTnTx, "try-error")) {
    .throw_message("Failed to generate the LxTx table, NULL returned\n",
                   "The original error was: ",
                   ## return the first part of message coming from get_RLum(),
                   ## as it makes the error too long and confusing
                   gsub("^.*\\[", "[", attr(LnLxTnTx, "condition")$message))
    return(NULL)
  }

  ## extract the dose and combine
  LnLxTnTx <- cbind(Dose = sapply(object@records[OSL.Curves.ID$Lx],
                                  function(record) record@info$IRR_TIME %||% NA),
                    data.table::rbindlist(LnLxTnTx))

  ## check whether we have dose points at all
  if (is.null(dose.points) && anyNA(LnLxTnTx$Dose)) {
    .throw_error("'dose.points' contains NA values or was not set")
  }

  ## Set regeneration points ------------------------------------------------
  ## overwrite dose point manually
  if (length(dose.points) > 0) {
      if (length(dose.points) != length(LnLxTnTx$Dose))
        .throw_error("Length of 'dose.points' (", length(dose.points),
                     ") differs from number of curves (", length(LnLxTnTx$Dose), ")")

      LnLxTnTx$Dose <- dose.points
  }
  if (length(dose.points.test) > 1 &&  # allow for recycling
      length(dose.points.test) != length(LnLxTnTx$Dose)) {
    .throw_error("Length of 'dose.points.test' (", length(dose.points.test),
                 ") differs from number of curves (", length(LnLxTnTx$Dose), ")")
  }

  ## set test dose points to -1 if nothing is available
  LnLxTnTx$Test_Dose <- dose.points.test %||% -1

  ## use source dose rate
  if(!is.null(dose_rate_source)) {
    LnLxTnTx$Dose <- LnLxTnTx$Dose * dose_rate_source
    LnLxTnTx$Test_Dose <- ifelse(LnLxTnTx$Test_Dose < 1, -1, LnLxTnTx$Test_Dose  * dose_rate_source)
  }

  ## check whether the first OSL/IRSL curve (i.e., the Natural) has 0 dose. If
  ## not, it is probably a dose recovery test with the given dose being treated
  ## as the unknown dose. We overwrite this value and warn the user.
  if (LnLxTnTx$Dose[1] != 0 && (list(...)$mode %||% "") != "alternate") {
      .throw_warning("The natural signal has a dose of ", LnLxTnTx$Dose[1],
                     " s, which is indicative of a dose recovery test. ",
                     "The natural dose was set to 0.")
      LnLxTnTx$Dose[1] <- 0
    }

  ## Label dose points ------------------------------------------------------
  dose <- LnLxTnTx$Dose
  dose_names <- paste0("R", seq_along(dose) - 1)

    ## identify 0 dose point
    zero_id <- which(dose == 0)
    dose_names[zero_id] <- "R0"
    if (length(zero_id)) dose_names[zero_id[1]] <- "Natural"

    ## check for repeated
    is_repeated <- duplicated(dose)
    is_repeated[dose == 0] <- FALSE

    ## add to data.frame
    LnLxTnTx <- cbind(
      data.frame(
        Name = dose_names,
        Repeated = is_repeated,
        stringsAsFactors = FALSE),
      LnLxTnTx)

  ## Calculate rejection criteria -------------------------------------------

  ## compute the standard error of a ratio
  .se.ratio <- function(num, num.err, den, den.err) {
    num / den * sqrt((num.err / num)^2 + (den.err / den)^2)
  }

  ## compare a single value with a threshold (either can be NA)
  .status_from_threshold <- function(value, threshold, comparator = `<=`) {
    if (is.na(threshold) || isTRUE(comparator(value, threshold)))
      return("OK")
    "FAILED"
  }

  ## Calculate Recycling Ratio ----------------------------------------------
  RecyclingRatio <- NA
  if (any(LnLxTnTx$Repeated)) {
      ## get repeated and previous dose points
      repeated <- LnLxTnTx[LnLxTnTx$Repeated, ]
      previous <- data.table::rbindlist(
        lapply(repeated$Dose, \(x) LnLxTnTx[LnLxTnTx$Dose == x & !LnLxTnTx$Repeated, ][1, ]))

    ratio <- repeated$LxTx / previous$LxTx
    if (consider.uncertainties) {
      uncertainty <- .se.ratio(repeated$LxTx, repeated$LxTx.Error,
                               previous$LxTx, previous$LxTx.Error)
      ## add the uncertainty in the most favourable direction
      ratio <- ifelse(ratio > 1, ratio - uncertainty, ratio + uncertainty)
    }

    ## calculate value and set names
    RecyclingRatio <- t(
        setNames(round(ratio, 4),
          nm = paste0("Recycling ratio (", repeated$Name, "/", previous$Name, ")")))
  }

  ## Recycling Ratio
  recycling.threshold <- rep(rejection.criteria$recycling.ratio / 100,
                             length(RecyclingRatio))

  if (!is.na(rejection.criteria$recycling.ratio)) {
    ## set better ratio by given the absolute margin depending
    ## on whether we have values larger or smaller than 1
    idx.gt1 <- which(RecyclingRatio > 1)
    recycling.threshold[idx.gt1] <- 1 + recycling.threshold[idx.gt1]
    idx.lt1 <- which(RecyclingRatio < 1)
    recycling.threshold[idx.lt1] <- 1 - recycling.threshold[idx.lt1]
  }

  status.RecyclingRatio <- vapply(abs(1 - RecyclingRatio), .status_from_threshold,
                                  threshold = rejection.criteria$recycling.ratio / 100,
                                  FUN.VALUE = character(1))

  ## Calculate Recuperation Rate --------------------------------------------
  Recuperation <- NA
  if (!recuperation_reference %in% LnLxTnTx$Name) {
      .throw_error("Recuperation reference invalid, valid values are: ",
                   .collapse(LnLxTnTx[, "Name"]))
  }

  ## Recuperation Rate (capable of handling multiple type of recuperation values)
  if ("R0" %in% LnLxTnTx$Name) {
    idx.R0 <- LnLxTnTx$Name == "R0"
    idx.Rref <- LnLxTnTx$Name == recuperation_reference
    R0 <- LnLxTnTx$LxTx[idx.R0]
    Rref <- LnLxTnTx$LxTx[idx.Rref]
    ratio <- R0 / Rref
    if (consider.uncertainties) {
      uncertainty <- .se.ratio(R0, LnLxTnTx$LxTx.Error[idx.R0],
                               Rref, LnLxTnTx$LxTx.Error[idx.Rref])
      ratio <- ratio - uncertainty
    }
    labels <- paste0("Recuperation rate (", recuperation_reference, ") ",
                     seq_along(R0))
    Recuperation <- t(setNames(ratio, labels))
  }

  recuperation.threshold <- rep(rejection.criteria$recuperation.rate / 100,
                                length(Recuperation))
  status.Recuperation <- vapply(Recuperation, function(value) {
    if (is.na(value))
      return("OK")
    .status_from_threshold(value, rejection.criteria$recuperation.rate / 100)
  }, FUN.VALUE = character(1))

  ## Calculate Testdose error -----------------------------------------------
  Testdose.error <- (LnLxTnTx$Net_TnTx.Error/LnLxTnTx$Net_TnTx)[1]
  testdose.threshold <- rejection.criteria$testdose.error / 100
  status.Testdose <- .status_from_threshold(Testdose.error, testdose.threshold)

  ## Calculate Signal-to-noise ratio ----------------------------------------
  sn.idx <- match(sn_reference, LnLxTnTx$Name)
  if (is.na(sn.idx)) {
    .throw_error("Signal-to-noise reference invalid, valid values are: ",
                 .collapse(LnLxTnTx[, "Name"]))
  }
  SN.ratio <- LnLxTnTx$SN_RATIO_LnLx[sn.idx]
  SN.threshold <- rejection.criteria$sn.ratio
  status.SN.ratio <- .status_from_threshold(SN.ratio, SN.threshold,
                                            comparator = `>=`)

  RejectionCriteria <- data.frame(
      Criteria = c(colnames(RecyclingRatio) %||% NA_character_,
                   colnames(Recuperation) %||% NA_character_,
                   "Testdose error",
                   "Signal-to-noise ratio"),
      Value = c(RecyclingRatio, Recuperation, Testdose.error, SN.ratio),
      Threshold = c(recycling.threshold, recuperation.threshold,
                    testdose.threshold, SN.threshold),
      Status = c(status.RecyclingRatio, status.Recuperation,
                 status.Testdose, status.SN.ratio),
      stringsAsFactors = FALSE
  )

  ## remove rejection criteria that have a NA value (it can happen for sn.ratio
  ## if OSL.component is used)
  RejectionCriteria <- RejectionCriteria[!is.na(RejectionCriteria$Value), ]

  ## this must be kept in sync with fit_DoseResponseCurve()
  temp.GC.all.na <- data.frame(
      De = NA,
      De.Error = NA,
      D01 = NA,
      D01.ERROR = NA,
      D02 = NA,
      D02.ERROR = NA,
      R = NA,
      R.ERROR = NA,
      Dc = NA,
      D63 = NA,
      n_N = NA,
      De.MC = NA,
      Fit = NA,
      Mode = NA,
      HPDI68_L = NA,
      HPDI68_U = NA,
      HPDI95_L = NA,
      HPDI95_U = NA,
      RC.Status = NA,
      .De.plot = NA,
      .De.raw = NA)

  ## default growth curve results in case no calculation is performed
  temp.GC <- temp.GC.all.na
  temp.GC.fit <- NULL

  ## Calculate the dose-response curve --------------------------------------
  if (!onlyLxTxTable) {
    ## we want to force fit_DoseResponseCurve() to run with verbose = FALSE so
    ## that we can print out the fit message ourselves further down if desired
    extraArgs$verbose <- NULL
    temp.GC.fit <- do.call(fit_DoseResponseCurve,
                           modifyList(list(object = data.frame(
                                               Dose = LnLxTnTx$Dose,
                                               LxTx = LnLxTnTx$LxTx,
                                               LxTx.Error = LnLxTnTx$LxTx.Error,
                                               TnTx = LnLxTnTx$Net_TnTx,
                                               Test_Dose = LnLxTnTx$Test_Dose),
                                           verbose = FALSE),
                                      extraArgs, keep.null = TRUE))

    if (!is.null(temp.GC.fit)) {
      if (verbose)
        .throw_message(extraArgs$.aliquot_number, temp.GC.fit@info$fit_message,
                       error = FALSE)

      temp.GC <- get_RLum(temp.GC.fit)
      De <- temp.GC$De
      De.err <- temp.GC$De.Error

      ## Rejection Criteria for Palaeodose error ----------------------------
      palaeodose.error.calculated <- round(De.err / De, digits = 5)
      palaeodose.error.threshold <- rejection.criteria$palaeodose.error / 100

      palaeodose.error.data.frame <- data.frame(
          Criteria = "Palaeodose error",
          Value = palaeodose.error.calculated,
          Threshold = palaeodose.error.threshold,
          Status = .status_from_threshold(palaeodose.error.calculated,
                                          palaeodose.error.threshold))

      exceed.max.threshold <- if (is.na(rejection.criteria$exceed.max.regpoint)) {
                                NA
                              } else if (!rejection.criteria$exceed.max.regpoint) {
                                Inf
                              } else {
                                as.numeric(max(LnLxTnTx$Dose))
                              }
      exceed.max.regpoint.data.frame <- data.frame(
          Criteria = "De > max. dose point",
          Value = De - ifelse(consider.uncertainties, De.err, 0),
          Threshold = exceed.max.threshold,
          Status = .status_from_threshold(De, exceed.max.threshold))

      ## add to RejectionCriteria data.frame
      RejectionCriteria <- rbind(RejectionCriteria,
                                 palaeodose.error.data.frame,
                                 exceed.max.regpoint.data.frame)
    }
  }

  ## get position numbers
  POSITION <- unique(vapply(object@records,
                            function(x) x@info$POSITION %||% NA,
                            FUN.VALUE = numeric(1)))[1]

  ## get grain numbers
  GRAIN <- unique(vapply(object@records,
                         function(x) x@info$GRAIN %||% NA,
                         FUN.VALUE = numeric(1)))[1]

  ## Results object ---------------------------------------------------------
  UID <- create_UID()
  results <- set_RLum(
      class = "RLum.Results",
      data = list(
          data = cbind(
              temp.GC,
              RC.Status = if ("FAILED" %in% RejectionCriteria$Status) "FAILED" else "OK",
              signal.range = .format_range(signal_integral),
              background.range = .format_range(background_integral),
              signal.range.Tx = .format_range(signal_integral_Tx %||% NA),
              background.range.Tx = .format_range(background_integral_Tx %||% NA),
              ALQ = 1, POS = POSITION, GRAIN = GRAIN, UID = UID),
          LnLxTnTx.table = cbind(LnLxTnTx, UID = UID, stringsAsFactors = FALSE),
          rejection.criteria = cbind(UID, RejectionCriteria),
          Formula = get_RLum(temp.GC.fit, "Formula"),
          .plot.data = list(
              records = object@records,
              signal_integral = signal_integral,
              background_integral = background_integral,
              OSL.Curves.ID = OSL.Curves.ID,
              TL.Curves.ID = TL.Curves.ID,
              curveType = CWcurve.type,
              GC.fit = temp.GC.fit,
              xlab.drc = if (is.null(dose_rate_source)) "Dose [s]" else "Dose [Gy]",
              main = main)
      ),
      info = list(call = sys.call())
  )

  ## Plotting ---------------------------------------------------------------
  if (plot) {
    .plot_SAR.CWOSL(results,
                    mtext.outer = mtext.outer,
                    plot_onePage = plot_onePage,
                    plot_singlePanels = plot_singlePanels,
                    ...)
  }

  ## Return -----------------------------------------------------------------
  invisible(results)
}


## Helper functions ---------------------------------------------------------
## Plot the results of the analysis
.plot_SAR.CWOSL <- function(
  results,
  mtext.outer = "",
  plot_onePage = FALSE,
  plot_singlePanels = FALSE,
  legend.pch = 20,
  legend.cex = 4,
  ...
) {
  curve_args <- results@data$.plot.data
  records <- curve_args$records
  OSL.Curves.ID <- curve_args$OSL.Curves.ID
  TL.Curves.ID <- curve_args$TL.Curves.ID
  GC.fit <- curve_args$GC.fit
  LnLxTnTx <- results@data$LnLxTnTx.table

  ## use the package colours
  col <- get("col", pos = .LuminescenceEnv)
  if (length(OSL.Curves.ID$Lx) > length(col)) {
    .throw_warning("Too many curves, only the first ", length(col),
                   " will be plotted")
  }

  curve_args$col <- col
  curve_args$cex <- list(...)$cex %||% 1
  curve_args$log <- list(...)$log %||% ""

    ## Layout and panel selection -------------------------------------------
    ## 1 -> TL previous LnLx
    ## 2 -> LnLx
    ## 3 -> TL previous TnTx
    ## 4 -> TnTx
    ## 5 -> Legend
    ## 6 -> Dose-response curve
    ## 7 -> Rejection criteria
    ## 8 -> IRSL curve/Single grain

    mar <- c(4, 3, 3, 1)
    if (plot_onePage || identical(plot_singlePanels[1], FALSE)) {
      ## the graphical parameters cannot be restored unconditionally, as that
      ## may affect the analyse_pIRIRSequence() plots
      par.default <- .par_defaults()
      on.exit(par(par.default), add = TRUE)

      layout.matrix <- matrix(c(1, 1, 3, 3, 6, 6, 7,
                                1, 1, 3, 3, 6, 6, 8,
                                2, 2, 4, 4, 9, 9, 10,
                                2, 2, 4, 4, 9, 9, 10,
                                5, 5, 5, 5, 5, 5, 5),
                              nrow = 5, ncol = 7, byrow = TRUE)
      if (plot_onePage) {
        plot_singlePanels <- TRUE
      } else {
        mar <- c(4, 4, 3, 3)
        layout.matrix <- layout.matrix[, 1:4]
      }
      graphics::layout(layout.matrix)
      par(oma = c(0, 0, 0, 0), mar = mar, cex = curve_args$cex * 0.6)
      plot.single.sel <- 1:8
    } else {
      ## this is used when called from analyse_pIRIRSequence()
      par(mar = mar)
      plot.single.sel <- plot_singlePanels
    }

      ## (1) Plotting TL Curves previous LnLx ----------------------------------------
      if (1 %in% plot.single.sel) {
        do.call(.plot_Curves,
                c(curve_args, list(is.TL = TRUE,
                                   curve_ids = TL.Curves.ID$Lx, is.Lx = TRUE)))
      }

      ## (2) Plotting LnLx Curves ----------------------------------------------------
      if (2 %in% plot.single.sel) {
        do.call(.plot_Curves,
                c(curve_args, list(is.TL = FALSE,
                                   curve_ids = OSL.Curves.ID$Lx, is.Lx = TRUE)))

        ##mtext, implemented here, as a plot window has to be called first
        mtext(
          mtext.outer,
          side = 4,
          outer = TRUE,
          line = -1.7,
          cex = curve_args$cex,
          col = "blue")
      }# plot.single.sel

      ## (3) Plotting TL Curves previous TnTx ----------------------------------------
      if (3 %in% plot.single.sel) {
        do.call(.plot_Curves,
                c(curve_args, list(is.TL = TRUE,
                                   curve_ids = TL.Curves.ID$Tx, is.Lx = FALSE)))
      }

      ## (4) Plotting TnTx Curves ----------------------------------------------------
      if (4 %in% plot.single.sel) {
        do.call(.plot_Curves,
                c(curve_args, list(is.TL = FALSE,
                                   curve_ids = OSL.Curves.ID$Tx, is.Lx = FALSE)))
      }# plot.single.sel

      ## (5) Plotting Legend ----------------------------------------
      if (5 %in% plot.single.sel) {
        ## par.old must be assigned before changing the par() values
        ## because `mai` is affected by `mar`, and doing it in one line like
        ## it's done elsewhere would store a modified `mai` value
        par.old <- par("mar", "mai")
        par(mar = c(1,1,1,1), mai = c(0,0,0,0))

        n <- length(OSL.Curves.ID$Lx)
        x <- seq_len(n)
        y <- rep(7, n)

        plot(
          x, y,
          axes = FALSE,
          xlab = "", ylab = "",
          pch = legend.pch,
          type = "p",
          col = unique(col[1:length(OSL.Curves.ID$Lx)]),
          cex = legend.cex,
          ylim = c(0,10))

        ## add legend text
        text(x, y, paste0(LnLxTnTx$Name, "\n(", round(LnLxTnTx$Dose, 2), ")"),
             offset = 1, pos = 1, xpd = NA)

        ##add line
        abline(h = 10,lwd = 0.5)

        #reset margin
        par(par.old)
      }#plot.single.sel

    ## (6) Plot Dose-Response Curve --------------------------------------------
    if (6 %in% plot.single.sel) {
      extraArgs <- list(...)
      if (!is.null(GC.fit)) {
            do.call(plot_DoseResponseCurve, args = modifyList(
              list(
                object = GC.fit,
                xlab = curve_args$xlab.drc,
                plot_singlePanels = plot_onePage || length(plot_singlePanels) > 1,
                cex = ifelse(plot_onePage, 0.6, 1)
              ),
              extraArgs))
      } else {
        ## insert empty plots, otherwise the ordering may get messed up
        shape::emptyplot()
        if (extraArgs$plot_extended %||% TRUE) {
          shape::emptyplot()
          shape::emptyplot()
        }
      }
    }

    ## (7) Plot IRSL curve/Single Grain -------------------------------------
    if (8 %in% plot.single.sel) {
      ## split the device area in two so that the IRSL curve can be plotted
      ## alongside the rejection criteria
      if (!plot_singlePanels[1])
        par(mfrow = c(1,2))

      ## check grain an pos and plot single grain disc marker
      ## if we don't have single grain, we can safely use the other
      ## plot option because this kind of test is almost never
      ## for single grains but on all grains
      POSITION <- results@data$data$POS
      GRAIN <- results@data$data$GRAIN
      if(!is.na(POSITION) & !is.na(GRAIN) && GRAIN > 0) {
        .plot_SGMarker(this_grain = GRAIN, this_pos = POSITION)

      } else {
        ##graphical representation of IR-curve
        IRSL.idx <- grep("IRSL", vapply(records, function(x) x@recordType,
                                        FUN.VALUE = character(1)))
        if (length(IRSL.idx) == 0) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(x = c(1,1), y = c(1, 1), labels = "No IRSL curve detected!")
        } else {
          if (length(IRSL.idx) > 1)
            .throw_warning("Multiple IRSL curves detected (IRSL test), only the last one shown")
          plot_RLum.Data.Curve(records[[tail(IRSL.idx, 1)]],
                               par.local = FALSE, mgp = c(2, 0.7, 0), tcl = -0.4)
        }
      }
    }

    ## (8) Plot rejection criteria ------------------------------------------
    if (7 %in% plot.single.sel) {
      .plot_RCCriteria(results@data$rejection.criteria)
    }
}

## create single grain discs with measured grain labelled
.plot_SGMarker <- function(this_grain = 1, this_pos = 1) {
  par.old <- par(mar = c(3, 3, 3, 3))
  on.exit(par(par.old), add = TRUE)

  ## calculate coordinate matrix
  xy_coord <- matrix(
    data = c(
      rep(seq(0.25,0.75, length.out = 10), 10),
      rep(rev(seq(0.25,0.75, length.out = 10)), each = 10)),
    ncol = 2)

  ## draw disc
  shape::emptyplot(main = "Grain location")

  ## draw super circle
  shape::plotellipse(rx = 0.4, ry = 0.4, mid = c(0.5,0.5), lwd = 1)

  ## draw positioning holes
  for (mid in list(c(0.15, 0.50),
                   c(0.50, 0.85),
                   c(0.85, 0.50))) {
    shape::plotellipse(
               rx = 0.015,
               ry = 0.015,
               mid = mid,
               lwd = 1)
  }

  ## add points
  points(xy_coord, bg = "grey", pch = 21, cex = 1.2)

  ## add the one point
  points(
    x = xy_coord[this_grain,1],
    y = xy_coord[this_grain,2],
    bg = "red", pch = 21, cex = 1.2, col = "darkgreen", lwd = 2)

  ## add text
  mtext(
    side = 3,
    line = -1,
    paste0("pos: #", this_pos, " | ", "grain: #", this_grain),
    cex = 0.7)
}

# create rejection criteria plot
.plot_RCCriteria <- function(x) {
  par.old <- par(mar = c(1, 0, 3.2, 0.35))
  on.exit(par(par.old), add = TRUE)

  ## calculate middle points for the lines
  y_coord <- seq(0.1, 1, length.out = nrow(x) * 2)

  ## open plot
  plot(NA, NA,
    xlim = c(0,1),
    ylim = c(0,1),
    frame = FALSE,
    main = "Checks",
    yaxt = "n",
    xaxt = "n",
    ylab = "",
    xlab = "")

  ## find how many characters can be fitted in the available space
  usr <- par("usr")
  avail.width <- par("pin")[1] / (usr[2] - usr[1])
  longest.label <- x$Criteria[which.max(nchar(x$Criteria))]
  label.length <- nchar(longest.label)
  repeat {
    if (graphics::strwidth(.shorten_filename(longest.label, label.length),
                           cex = 0.9, units = "in") < avail.width)
      break
    label.length <- label.length - 1
  }

  ## plot names
  text(
    x = 0.88,
    y = y_coord[seq(1,length(y_coord),2)],
    labels = .shorten_filename(x$Criteria, label.length),
    cex = 0.9,
    xpd = NA,
    adj = c(1, 0.5))

  ## add lines with criteria
  y_coord_l <- y_coord[seq(2,length(y_coord),2)]
  for(i in 1:nrow(x)) {
    lines(x = c(0.1,1), y = rep(y_coord_l[i],2), lwd = 0.25)
  }

  ## round to the minimum number of digits so that a difference between value
  ## and threshold can be seen
  digits <- pmax(ceiling(-log10(abs(x$Value - x$Threshold))), 0, na.rm = TRUE)

  ## set colours
  col <- ifelse(x$Status == "OK", 3, 2)
  col[is.na(x$Threshold)] <- 24

  ## set labels
  x$Value <- mapply(function(x, d) round(x, digits = d), x$Value, digits)
  x$Threshold <- mapply(function(x, d) round(x, digits = d), x$Threshold, digits)
  x$sign <- ifelse(x$Value > x$Threshold, ">=", "<=")
  x[is.na(x$sign), c("Threshold", "sign")] <- ""

  text(
    x = 0.8,
    y = y_coord_l,
    labels = paste(x$Value, x$sign, x$Threshold),
    cex = 0.7,
    adj = c(1, 1.5))

  ## add final points
  points(
    x = rep(0.95, nrow(x)),
    pch = ifelse(x$Status == "FAILED", 4, 21),
    y = y_coord[seq(1,length(y_coord),2)],
    bg = col,
    col = col,
    cex = 1.3)
}

## Plot TL or OSL curves
.plot_Curves <- function(records, curve_ids, curveType = NULL, is.TL, is.Lx,
                         signal_integral = NULL, background_integral = NULL,
                         main = NULL, cex = 1, col = NULL, log = "", ...) {
  ## TL: empty guard
  if (is.TL && length(curve_ids) == 0) {
    plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), main = "",
         axes = FALSE, ylab = "", xlab = "")
    text(0.5, 0.5, "No TL curve detected")
    return()
  }

  ## OSL: shift curves if x starts at 0 and log-transform is applied
  if (!is.TL && log %in% c("x", "xy")) {
    for (i in curve_ids) {
      x.vals <- records[[i]]@data[, 1]
      if (x.vals[1] == 0) {
        records[[i]]@data[, 1] <- x.vals + x.vals[2] - x.vals[1]
        .throw_warning("Curves shifted by one channel for log-plot")
      }
    }
  }

  ## approximate curve resolution
  x.vals <- records[[curve_ids[[1]]]]@data[, 1]
  resolution <- round(mean(diff(x.vals)), digits = ifelse(is.TL, 1, 2))

  ## get ranges of the curves
  xy_xlim <- matrixStats::rowRanges(vapply(
    X = curve_ids,
    FUN = function(x) apply(records[[x]]@data, 2, range, na.rm = TRUE),
    FUN.VALUE = numeric(4)))

  plot(NA, NA,
       xlab = if (is.TL) "T [\u00B0C]" else "Time [s]",
       ylab = if (is.TL) paste0("TL [cts/", resolution, " \u00B0C]")
              else paste0(curveType, " [cts/", resolution, " s]"),
       xlim = c(min(xy_xlim[1, ]), max(xy_xlim[2, ])),
       ylim = c(if (is.TL) 1 else min(xy_xlim[3, ]),
                max(xy_xlim[4, ])),
       main = main,
       mgp = c(2, 0.7, 0),
       tcl = -0.4,
       log = if (is.TL) gsub("x", "", log) else log)

  prefix <- if (is.TL) "TL previous " else ""
  letter <- if (is.Lx) as.symbol("L") else as.symbol("T")
  mtext(text = eval(bquote(expression(.(prefix) * .(letter)[n] * "," *
                                      .(letter)[x] * " curves"))),
        side = 3, cex = cex * 0.7)
  for (i in seq_along(curve_ids))
    lines(records[[curve_ids[i]]]@data, col = col[i])

  ## mark integration limits for OSL curves
  if (!is.TL)
    abline(v = c(x.vals[range(signal_integral)],
                 x.vals[range(background_integral)]),
           lty = 2, col = "gray")
}
