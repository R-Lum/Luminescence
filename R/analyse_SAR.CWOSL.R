#' @title Analyse SAR CW-OSL Measurements
#'
#' @description
#' The function performs a SAR CW-OSL analysis on an [Luminescence::RLum.Analysis-class]
#' object, including growth curve fitting.
#'
#' @details
#' The function performs an analysis for a standard SAR protocol measurements
#' introduced by Murray and Wintle (2000) with CW-OSL curves. For the
#' calculation of the `Lx/Tx` value the function [Luminescence::calc_OSLLxTxRatio] is
#' used. To **change the way the Lx/Tx error is calculated** use arguments
#' `background.count.distribution` and `sigmab`, which will be passed to
#' [Luminescence::calc_OSLLxTxRatio].
#'
#' **What is part of a SAR sequence?**
#'
#' The function is rather picky when it comes down to accepted curve input
#' (OSL, IRSL,...) and structure. A SAR sequence is basically a set of
#' \eqn{L_{x}/T_{x}} curves. Hence, every second curve is considered a
#' shine-down curve related to the test dose. It also means that the number of
#' curves for \eqn{L_{x}} has to be equal to the number of \eqn{T_{x}} curves,
#' and that hot-bleach curves **do not** belong into a SAR sequence; at least
#' not for the analysis. Other curves allowed and processed are preheat curves,
#' or preheat curves measured as TL, and irradiation curves. The later one
#' indicates the duration of the irradiation, the dose and test dose points,
#' e.g., as part of XSYG files.
#'
#' **Argument `object` is of type `list`**
#'
#' If the argument `object` is of type [list] containing **only**
#' [Luminescence::RLum.Analysis-class] objects, the function re-calls itself on each element
#' in the list. This is useful if to analyse an entire measurement without
#' writing separate for-loops. To gain in full control of the parameters (e.g., `dose.points`) for
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
#' 2008). Therefore this functions has been enhanced to work with IRSL data,
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
#' `[palaeodose.error]`: set the allowed error for the De value, which per
#' default should not exceed 10%.
#'
#' **Irradiation times**
#'
#' The function makes two attempts to extra irradiation data (dose points)
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
#' [Luminescence::RLum.Analysis-class] objects can be provided. The object should **only**
#' contain curves considered part of the SAR protocol (see Details).
#'
#' @param signal.integral.min [integer] (**required**):
#' lower bound of the signal integral. It can be a [list] of integers, if
#' `object` is a list. If the input is a vector (e.g., `c(1,2)`), the second
#' value will be interpreted as the minimum signal integral for the `Tx` curve.
#' It can be set to `NA`, in which case no integrals are taken into account.
#'
#' @param signal.integral.max [integer] (**required**):
#' upper bound of the signal integral. It can be a [list] of integers if
#' `object` is a list. If the input is a vector (e.g., `c(1,2)`), the second
#' value will be interpreted as the maximum signal integral for the `Tx` curve.
#' It can be set to `NA`, in which case no integrals are taken into account.
#'
#' @param background.integral.min [integer] (**required**):
#' lower bound of the background integral. It can be a [list] of integers if
#' `object` is a list. If the input is a vector (e.g., `c(1,2)`), the second
#' value will be interpreted as the minimum background integral for the `Tx`
#' curve. It can be set to `NA`, in which case no integrals are taken into
#' account.
#'
#' @param background.integral.max [integer] (**required**):
#' upper bound of the background integral. It can be a [list] of integers if
#' `object` is a list. If the input is a vector (e.g., `c(1,2)`), the second
#' value will be interpreted as the maximum background integral for the `Tx`
#' curve. It can be set to `NA`, in which case no integrals are taken into
#' account.
#'
#' @param OSL.component [character] or [integer] (*optional*):
#' single index or a [character] defining the signal component to be evaluated.
#' It requires that the object was processed by `OSLdecomposition::RLum.OSL_decomposition`.
#' This argument can either be the name of the OSL component assigned by
#' `OSLdecomposition::RLum.OSL_global_fitting` or the index in the descending
#' order of decay rates. Then `"1"` selects the fastest decaying component, `"2"`
#' the second fastest and so on. Can be a [list] of [integer]s or strings (or mixed)
#' If object is a [list] and this parameter is provided as [list] it alternates over
#' the elements (aliquots) of the object list, e.g., `list(1,2)` processes the first
#' aliquot with component `1` and the second aliquot with component `2`.
#' `NULL` does not process any component.
#'
#' @param rejection.criteria [list] (*with default*):
#' provide a *named* list and set rejection criteria in **percentage**
#' for further calculation. Can be a [list] in a [list], if `object` is of type [list].
#' Note: If an *unnamed* [list] is provided the new settings are ignored!
#'
#' Allowed arguments are `recycling.ratio`, `recuperation.rate`,
#' `palaeodose.error`, `testdose.error`, `exceed.max.regpoint = TRUE/FALSE`,
#' `recuperation_reference` ("Natural" or any other dose point, e.g., `"R1"`).
#' Example: `rejection.criteria = list(recycling.ratio = 10)`.
#' By default, all numerical values are set to 10, `exceed.max.regpoint = TRUE`.
#' Every criterion can be set to `NA`, in which case values are calculated, but
#' they are not considered, i.e. their corresponding RC.Status is always `'OK'`.
#'
#' @param dose.points [numeric] (*optional*):
#' a numeric vector containing the dose points values. Using this argument
#' overwrites dose point values extracted from other data. Can be a [list] of
#' [numeric] vectors, if `object` is of type [list].
#'
#' @param dose.points.test [numeric] (*optional*):
#' a numeric vector containing the test dose in the same units as `dose.points`.
#' If length = 1, the values will be recycled. It has only an effect for
#' `fit.method = 'OTORX'`.
#'
#' @param dose_rate_source [numeric] (*optional*): a numerical value for the source dose rate,
#' typically Gy/s. If set, the x-axis default for the dose-response curve changes to `Dose [Gy]`.
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
#' @param plot_onePage [logical] (*with default*): enable/disable one page
#' plot output.
#'
#' @param plot_singlePanels [logical] (*with default*) or [numeric] (*optional*):
#' single plot output (`TRUE/FALSE`) to allow for plotting the results in single plot windows.
#' If a [numeric] vector is provided the plots can be selected individually, i.e.
#' `plot_singlePanels = c(1,2,3,4)` will plot the TL and Lx, Tx curves but
#' not the legend (5) or the
#' growth curve (6), (7) and (8) belong to rejection criteria plots. Requires
#' `plot = TRUE`.
#'
#' @param onlyLxTxTable [logical] (*with default*): If `TRUE` the dose response
#' curve fitting and plotting is skipped.
#' This allows to get hands on the `Lx/Tx` table for large datasets
#' without the need for a curve fitting.
#'
#' @param ... further arguments that will be passed to the functions
#' [Luminescence::fit_DoseResponseCurve], [Luminescence::plot_DoseResponseCurve]
#' or [Luminescence::calc_OSLLxTxRatio]
#' (supported: `background.count.distribution`, `sigmab`, `sig0`).
#' **Please note** that if you consider to use the early light subtraction
#' method you should provide your own `sigmab` value!
#
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
#'
#' The output should be accessed using the function [Luminescence::get_RLum].
#'
#' **The function currently does support only 'OSL', 'IRSL' and 'POSL' data!**
#'
#' @section Function version: 0.12.0
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
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
#' luminescence measurements. Radiation Measurements, 37 (2), 161-165.
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
#' signal.integral.min = 1,
#' signal.integral.max = 2,
#' background.integral.min = 900,
#' background.integral.max = 1000,
#' log = "x",
#' fit.method = "EXP",
#' plot_onePage = TRUE,
#' rejection.criteria = list(
#'   recycling.ratio = 10,
#'   recuperation.rate = 10,
#'   testdose.error = 10,
#'   palaeodose.error = 10,
#'   recuperation_reference = "Natural",
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
#'  signal.integral.min = 1,
#'  signal.integral.max = 2,
#'  dose.points.test = 15,
#'  background.integral.min = 900,
#'  background.integral.max = 1000,
#'  n.MC = 10,
#'  fit.method = "OTORX")
#' }
#'
#' @export
analyse_SAR.CWOSL<- function(
  object,
  signal.integral.min = NA,
  signal.integral.max = NA,
  background.integral.min = NA,
  background.integral.max = NA,
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
  ...
) {
  .set_function_name("analyse_SAR.CWOSL")
  on.exit(.unset_function_name(), add = TRUE)

# SELF CALL -----------------------------------------------------------------------------------
if(is.list(object)){
  ##clean object input and expand parameters
  object <- .rm_nonRLum(object)
  parm <- .expand_parameters(length(object))

  ##handle main separately
  if ("main" %in% ...names()) {
    main <- .listify(list(...)$main, length = length(object))
  }else{
    main <- as.list(paste0("ALQ #",1:length(object)))
  }

  results <- .warningCatcher(merge_RLum(lapply(seq_along(object), function(x){
    analyse_SAR.CWOSL(
      object = object[[x]],
      signal.integral.min = parm$signal.integral.min[[x]],
      signal.integral.max = parm$signal.integral.max[[x]],
      background.integral.min = parm$background.integral.min[[x]],
      background.integral.max = parm$background.integral.max[[x]],
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
  .validate_class(plot_singlePanels, c("logical", "integer", "numeric"))
  .validate_logical_scalar(trim_channels)
  .validate_logical_scalar(plot)
  .validate_logical_scalar(plot_onePage)
  .validate_logical_scalar(onlyLxTxTable)
  .validate_scalar(dose_rate_source, null.ok = TRUE)

  ## trim OSL or IRSL channels
  if (trim_channels) {
    ## fetch names with OSL and IRSL
    tmp_names <- unique(vapply(object@records, function(x) x@recordType, character(1)))
    tmp_names <- grep("OSL|IRSL", tmp_names, value = TRUE, perl = TRUE)

    ## trim
    object <- trim_RLum.Data(object, recordType = tmp_names)
  }

  ##skip all those tests if signal integral is NA
  if (anyNA(c(signal.integral.min, signal.integral.max,
              background.integral.min, background.integral.max))) {
    signal.integral <- background.integral <- NA
    signal.integral.Tx <- background.integral.Tx <- NULL

    if(is.null(OSL.component))
    .throw_warning("No signal or background integral applied ",
                   "as they were set to NA")
  } else {
    ## build signal and background integrals
    signal.integral <- signal.integral.min[1]:signal.integral.max[1]
    background.integral <- background.integral.min[1]:background.integral.max[1]
    signal.integral.Tx <- NULL
    background.integral.Tx <- NULL

    ## account for the case that Lx and Tx integral differ
    if (length(signal.integral.min) == 2 && length(signal.integral.max) == 2) {
      signal.integral.Tx <- signal.integral.min[2]:signal.integral.max[2]
    }

    if (length(background.integral.min) == 2 && length(background.integral.max) == 2) {
      background.integral.Tx <- background.integral.min[2]:background.integral.max[2]
    }

    ## account for the case that the uset did not provide everything ...
    if (is.null(signal.integral.Tx) && !is.null(background.integral.Tx)) {
          signal.integral.Tx <- signal.integral
          .throw_warning("Background integral for Tx curves set, but not for ",
                         "the signal integral; signal integral for Tx automatically set")
        }

    if (!is.null(signal.integral.Tx) && is.null(background.integral.Tx)) {
        background.integral.Tx <- background.integral
        .throw_warning("Signal integral for Tx curves set, but not for the ",
                       "background integral; background integral for Tx automatically set")
      }

    ##INTEGRAL LIMITS
    if (!is.integer(signal.integral) || !is.integer(background.integral)) {
      .throw_error("'signal.integral' or 'background.integral' is not of type integer")
    }
  }

  ## try to extract the correct curves for the sequence based on allowed curve types and
  ## the curve type used most frequently
  ## now remove all non-allowed curves
  CWcurve.type <- regmatches(
    x = names(object),
    m = regexpr("(OSL[a-zA-Z]*|IRSL[a-zA-Z]*|POSL[a-zA-Z]*)", names(object), perl = TRUE))

  if(length(CWcurve.type) == 0) {
    .throw_message("No record of type 'OSL', 'IRSL', 'POSL' detected, ",
                   "NULL returned")
    return(NULL)
  }

  ## now get the type which is used most
  CWcurve.type <- names(which.max(table(CWcurve.type)))

# Rejection criteria ------------------------------------------------------

  if (class(rejection.criteria)[1] != "list")
    rejection.criteria <- list()

  ##set list
  rejection.criteria <- modifyList(x = list(
      recycling.ratio = 10,
      recuperation.rate = 10,
      palaeodose.error = 10,
      testdose.error = 10,
      exceed.max.regpoint = TRUE,
      recuperation_reference = "Natural"
    ),
    val = rejection.criteria,
    keep.null = TRUE)


# Deal with extra arguments ----------------------------------------------------
  ##deal with addition arguments
  extraArgs <- list(...)

  main <- extraArgs$main %||% ""
  log <- extraArgs$log %||% ""
  cex <- extraArgs$cex %||% 1

  background.count.distribution <-
      extraArgs$background.count.distribution %||% "non-poisson"
  sigmab <- extraArgs$sigmab
  sig0 <- extraArgs$sig0 %||% 0

  ## deprecated argument
  if ("plot.single" %in% names(extraArgs)) {
    plot_singlePanels <- extraArgs$plot.single
    .throw_warning("'plot.single' is deprecated, use 'plot_singlePanels' ",
                   "instead")
  }

# Protocol Integrity Checks --------------------------------------------------
  ##check overall structure of the object
  ##every SAR protocol has to have equal number of curves

  ##grep curve types from analysis value and remove unwanted information
  temp.ltype <- sapply(1:length(object@records), function(x) {
     ##export as global variable
     object@records[[x]]@recordType <<- gsub(" .*", "", object@records[[x]]@recordType)
     object@records[[x]]@recordType
  })

  ##FI lexsyg devices provide irradiation information in a separate curve
  if(any("irradiation" %in% temp.ltype)){
    temp.irradiation <- extract_IrradiationTimes(object)@data$irr.times[["IRR_TIME"]]

    ##write this into the records
    for(i in 1:length(object@records)){
      if(is.null(object@records[[i]]@info$IRR_TIME))
        object@records[[i]]@info <- c(object@records[[i]]@info, IRR_TIME = temp.irradiation[i])
    }

    ## remove irradiation curves
    object <- get_RLum(object, record.id = !temp.ltype %in% "irradiation", drop = FALSE)
  }

  ## collect error messages to be reported together
  error.list <- list()

  ##check if the wanted curves are a multiple of two
  ##gsub removes unwanted information from the curves
  if(table(temp.ltype)[CWcurve.type]%%2!=0){
    error.list[[1]] <- "Input OSL/IRSL curves are not a multiple of two"
  }

  ##check if the curve lengths differ
  temp.matrix.length <- unlist(lapply(object@records,
                                      function(x) {
                                        if (x@recordType %in% CWcurve.type)
                                          nrow(x@data)
                                      }))

  if(length(unique(temp.matrix.length))!=1){
    error.list[[2]] <- paste0("Input curves have different lengths (",
                              .collapse(unique(temp.matrix.length),
                                        quote = FALSE), "), consider setting ",
                              "'trim_channels = TRUE'")
  }

  ## return early in case of errors
  if (length(error.list) > 0) {
    .throw_warning(paste(unlist(error.list), collapse = "\n"),
                   "\n... >> nothing was done here!")
    return(invisible(NULL))
  }

    ##check background integral
    if (!all(is.na(signal.integral)) &&
        max(signal.integral) == min(signal.integral)) {
      signal.integral <- min(signal.integral):(max(signal.integral) + 1)
      .throw_warning("Integral signal limits cannot be equal, reset automatically")
    }

    ##background integral should not be longer than curve channel length
    if (!all(is.na(background.integral)) &&
             max(background.integral) == min(background.integral)) {
      background.integral <-
        c((min(background.integral) - 1) : max(background.integral))
    }

    if (!all(is.na(background.integral)) &&
        max(background.integral) > temp.matrix.length[1]) {
      background.integral <-
          c((temp.matrix.length[1] - length(background.integral)):temp.matrix.length[1])

      ##prevent that the background integral becomes negative
      if(min(background.integral) < max(signal.integral)){
        background.integral <- c((max(signal.integral) + 1):max(background.integral))
      }

      .throw_warning("Background integral out of bounds, set to ",
                     .format_range(background.integral))
    }

    ##Do the same for the Tx-if set
    if (!is.null(background.integral.Tx)) {
      if (max(background.integral.Tx) == min(background.integral.Tx)) {
        background.integral.Tx <-
          c((min(background.integral.Tx) - 1) : max(background.integral.Tx))
      }

      if (max(background.integral.Tx) > temp.matrix.length[2]) {
        background.integral.Tx <-
          c((temp.matrix.length[2] - length(background.integral.Tx)):temp.matrix.length[2])

        ##prevent that the background integral becomes negative
        if (min(background.integral.Tx) < max(signal.integral.Tx)) {
          background.integral.Tx <-
            c((max(signal.integral.Tx) + 1):max(background.integral.Tx))
        }

        .throw_warning("Background integral for Tx out of bounds, set to ",
                       .format_range(background.integral.Tx))
      }
    }

  # Grep Curves -------------------------------------------------------------
  ## extract relevant curves from RLum.Analysis object
  OSL.Curves.ID <- get_RLum(object, recordType = CWcurve.type, get.index = TRUE)

    ##separate curves by Lx and Tx (it makes it much easier)
    OSL.Curves.ID.Lx <-
      OSL.Curves.ID[seq(1,length(OSL.Curves.ID), by = 2)]
    OSL.Curves.ID.Tx <-
      OSL.Curves.ID[seq(2,length(OSL.Curves.ID), by = 2)]

    ##get index of TL curves
    TL.Curves.ID <-
      suppressWarnings(get_RLum(object, recordType = "TL$", get.index = TRUE))

    ##separate TL curves which is always coming before the OSL curve
    ##Note: we do not check anymore whether the sequence makes sense.
    TL.Curves.ID.Lx <- TL.Curves.ID[TL.Curves.ID%in%(OSL.Curves.ID.Lx - 1)]
    TL.Curves.ID.Tx <- TL.Curves.ID[TL.Curves.ID%in%(OSL.Curves.ID.Tx - 1)]

# Calculate LnLxTnTx values  --------------------------------------------------
  ##calculate LxTx values using external function
  LnLxTnTx <- try(lapply(seq(1, length(OSL.Curves.ID), by = 2), function(x) {
      if (length(OSL.component) > 0) {
       temp.LnLxTnTx <- get_RLum(
          calc_OSLLxTxDecomposed(
            Lx.data = object@records[[OSL.Curves.ID[x]]]@info$COMPONENTS,
            Tx.data = object@records[[OSL.Curves.ID[x + 1]]]@info$COMPONENTS,
            OSL.component = OSL.component,
            digits = 4,
            sig0 = sig0))
      } else {
       temp.LnLxTnTx <- get_RLum(
          calc_OSLLxTxRatio(
            Lx.data = object@records[[OSL.Curves.ID[x]]]@data,
            Tx.data = object@records[[OSL.Curves.ID[x + 1]]]@data,
            signal.integral = signal.integral,
            signal.integral.Tx = signal.integral.Tx,
            background.integral = background.integral,
            background.integral.Tx = background.integral.Tx,
            background.count.distribution = background.count.distribution,
            sigmab = sigmab,
            sig0 = sig0))
      }

      temp.Dose <- object@records[[OSL.Curves.ID[x]]]@info$IRR_TIME %||% NA
      temp.LnLxTnTx <- cbind(Dose = temp.Dose, temp.LnLxTnTx)

  }), silent = TRUE)

  ## this is basically for the OSL.component case to avoid that everything
  ## fails if something goes wrong therein
  if (inherits(LnLxTnTx, "try-error")) {
      .throw_message("Something went wrong while generating the LxTx table, ",
                     "NULL returned")
      return(NULL)
    }

    ##combine
    LnLxTnTx <- data.table::rbindlist(LnLxTnTx)

  ## Set regeneration points ------------------------------------------------
  ## overwrite dose point manually
  if (length(dose.points) > 0) {
      if (length(dose.points) != length(LnLxTnTx$Dose))
        .throw_error("Length of 'dose.points' (", length(dose.points),
                     ") differs from number of curves (", length(LnLxTnTx$Dose), ")")

      LnLxTnTx$Dose <- dose.points
  }

  ## set test dose points
  LnLxTnTx$Test_Dose <- rep_len(dose.points.test %||% -1, nrow(LnLxTnTx))

  ## use source dose rate
  if(!is.null(dose_rate_source)) {
    LnLxTnTx$Dose <- LnLxTnTx$Dose * dose_rate_source
    LnLxTnTx$Test_Dose <- LnLxTnTx$Dose * dose_rate_source

  }

  ##check whether we have dose points at all
  if (is.null(dose.points) && anyNA(LnLxTnTx$Dose)) {
    .throw_error("'dose.points' contains NA values or was not set")
  }

    ##check whether the first OSL/IRSL curve (i.e., the Natural) has 0 dose. If not
    ##not, it is probably a Dose Recovery Test with the given dose that is treated as the
    ##unknown dose. We overwrite this value and warn the user.
    if (LnLxTnTx$Dose[1] != 0 &
         (is.null(list(...)$mode) || (!is.null(list(...)$mode) && list(...)$mode != "alternate"))) {
      .throw_warning("The natural signal has a dose of ", LnLxTnTx$Dose[1],
                     " s, which is indicative of a dose recovery test. ",
                     "The natural dose was set to 0.")
      LnLxTnTx$Dose[1] <- 0
    }

  ## Label dose points ------------------------------------------------------
  dose <- LnLxTnTx$Dose
    ## preset names
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
  RecyclingRatio <- Recuperation <- NA

  ## Calculate Recycling Ratio ----------------------------------------------
  if (any(LnLxTnTx$Repeated)) {
      ## get repeated and previous dose points
      repeated <- LnLxTnTx[LnLxTnTx$Repeated, ]
      previous <- data.table::rbindlist(
        lapply(repeated$Dose, \(x) LnLxTnTx[LnLxTnTx$Dose == x & !LnLxTnTx$Repeated, ][1, ]))

      ## calculate value and set names
      RecyclingRatio <- t(
        setNames(
          object = round(repeated$LxTx / previous$LxTx, 4),
          nm = paste0("Recycling ratio (", repeated$Name, "/", previous$Name, ")")))
    }

  ## Calculate Recuperation Rate --------------------------------------------
  if (is.null(rejection.criteria$recuperation_reference) ||
      !rejection.criteria$recuperation_reference[1] %in% LnLxTnTx$Name)
      .throw_error("Recuperation reference invalid, valid values are: ",
                   .collapse(LnLxTnTx[, "Name"]))

  ## Recuperation Rate (capable of handling multiple type of recuperation values)
  if ("R0" %in% LnLxTnTx$Name) {
    Recuperation <- LnLxTnTx$LxTx[LnLxTnTx$Name == "R0"] /
      LnLxTnTx$LxTx[LnLxTnTx$Name == rejection.criteria$recuperation_reference[1]]

     ##transform and name
     Recuperation <- t(setNames(
        object = Recuperation,
        nm = paste0(
          "Recuperation rate (", rejection.criteria$recuperation_reference[1], ") ",
          seq_along(Recuperation))
      ))
    }

    # Evaluate and Combine Rejection Criteria ---------------------------------
    ## set threshold
    temp.threshold <-
      c(
       rep(rejection.criteria$recycling.ratio, length(RecyclingRatio)),
       rep(rejection.criteria$recuperation.rate, length(Recuperation))) / 100


    ## compare a single value with a threshold (either can be NA)
    .status_from_threshold <- function(value, threshold) {
      if (is.na(threshold) || isTRUE(value <= threshold))
        return("OK")
      "FAILED"
    }

    ##RecyclingRatio
    temp.status.RecyclingRatio <- rep("OK", length(RecyclingRatio))
    if (!anyNA(RecyclingRatio) && !is.na(rejection.criteria$recycling.ratio)) {
      temp.status.RecyclingRatio[abs(1 - RecyclingRatio) > (
        rejection.criteria$recycling.ratio / 100)] <- "FAILED"

      ## set better ratio by given the absolute margin depending
      ## on whether we have values larger or smaller than 1
      idx.gt1 <- which(RecyclingRatio > 1)
      temp.threshold[idx.gt1] <- temp.threshold[idx.gt1] + 1
      idx.lt1 <- which(RecyclingRatio < 1)
      temp.threshold[idx.lt1] <- 1 - temp.threshold[idx.lt1]
    }

    ##Recuperation
    temp.status.Recuperation <- sapply(Recuperation, function(value) {
      if (is.na(value))
        return("OK")
      .status_from_threshold(value, rejection.criteria$recuperation.rate / 100)
    })

    # Provide Rejection Criteria for Testdose error --------------------------
    testdose.error.calculated <- (LnLxTnTx$Net_TnTx.Error/LnLxTnTx$Net_TnTx)[1]

    testdose.error.threshold <-
      rejection.criteria$testdose.error / 100

    testdose.error.data.frame <- data.frame(
      Criteria = "Testdose error",
      Value = testdose.error.calculated,
      Threshold = testdose.error.threshold,
      Status = .status_from_threshold(testdose.error.calculated,
                                      testdose.error.threshold),
      stringsAsFactors = FALSE
    )

    RejectionCriteria <- data.frame(
      Criteria = c(colnames(RecyclingRatio) %||% NA_character_,
                   colnames(Recuperation) %||% NA_character_),
      Value = c(RecyclingRatio, Recuperation),
      Threshold = temp.threshold,
      Status = c(temp.status.RecyclingRatio,temp.status.Recuperation),
      stringsAsFactors = FALSE
    )

    RejectionCriteria <- rbind(RejectionCriteria, testdose.error.data.frame)

  ## Plotting ---------------------------------------------------------------
  if (plot) {
      ## the graphical parameters cannot be restored unconditionally, as that
      ## may affect the analyse_pIRIRSequence() plots
      if (plot_onePage || !plot_singlePanels[1]) {
        par.default <- .par_defaults()
        on.exit(par(par.default), add = TRUE)
      }

      ##colours and double for plotting
      col <- get("col", pos = .LuminescenceEnv)

      ## get record list
      record_list <- object@records

    layout.matrix <- matrix(c(1, 1, 3, 3, 6, 6, 7,
                              1, 1, 3, 3, 6, 6, 8,
                              2, 2, 4, 4, 9, 9, 10,
                              2, 2, 4, 4, 9, 9, 10,
                              5, 5, 5, 5, 5, 5, 5),
                            nrow = 5, ncol = 7, byrow = TRUE)

    ## Plotting - one Page config -------------------------------------------
    if (plot_onePage) {
      plot_singlePanels <- TRUE
      graphics::layout(layout.matrix)
      par(oma = c(0, 0, 0, 0),
          mar = c(4, 3, 3, 1),
          cex = cex * 0.6)
      }

    ## Plotting - old way config --------------------------------------------
    if (!plot_singlePanels[1]) {
        graphics::layout(layout.matrix[, 1:4])
        par(
          oma = c(0,0,0,0), mar = c(4,4,3,3), cex = cex * 0.6
        )

        ## 1 -> TL previous LnLx
        ## 2 -> LnLx
        ## 3 -> TL previous TnTx
        ## 4 -> TnTx
        ## 5 -> Legend

        ## set selected curves to allow plotting of all curves
        plot.single.sel <- c(1,2,3,4,5,6,7,8)

      } else {
        plot.single.sel <- 1:8

        ## check for values in the single output of the function and convert
        if (!is.logical(plot_singlePanels)) {
          ## this is used when called from analyse_pIRIRSequence()
          par(mar = c(4, 3, 3, 1))
          plot.single.sel <- plot_singlePanels
        }
      }

      ##warning if number of curves exceed colour values
      if (length(col) < length(OSL.Curves.ID) / 2) {
        .throw_warning("Too many curves, only the first ",
                       length(col), " curves are plotted")
      }

      ##get channel resolution (should be equal for all curves)
      resolution.OSLCurves <- round(object@records[[OSL.Curves.ID[1]]]@data[2,1] -
                                      object@records[[OSL.Curves.ID[1]]]@data[1,1],
                                    digits = 2)

      ## (1) Plotting TL Curves previous LnLx ----------------------------------------
      ##overall plot option selection for plot.single.sel
      if (1 %in% plot.single.sel) {
        ##check if TL curves are available
        if (length(TL.Curves.ID.Lx) > 0) {
          ##It is just an approximation taken from the data
          resolution.TLCurves <-  round(mean(diff(
            round(record_list[[TL.Curves.ID.Lx[[1]]]]@data[,1], digits = 1)
          )), digits = 1)

          ## get value ranges of the curves
          xy_xlim <- matrixStats::rowRanges(vapply(
            X = TL.Curves.ID.Lx,
            FUN = \(x) apply(record_list[[x]]@data, 2, range, na.rm = TRUE),
            FUN.VALUE = numeric(4)))

          xlim_range <- c(min(xy_xlim[1,]), max(xy_xlim[2,]))
          ylim_range <- c(1, max(xy_xlim[4,]))

          plot(
            NA,NA,
            xlab = "T [\u00B0C]",
            ylab = paste0("TL [cts/",resolution.TLCurves," \u00B0C]"),
            xlim = xlim_range,
            ylim = ylim_range,
            main = main,
            mgp = c(2, 0.7, 0),
            tcl = -0.4,
            log = gsub("x", "", log))

          #provide curve information as mtext, to keep the space for the header
          mtext(
            side = 3,
            text = expression(paste("TL previous ", L[n],",",L[x]," curves")),
            cex = cex * 0.7)

          ##plot TL curves
          for (i in seq_along(TL.Curves.ID.Lx)) {
            lines(record_list[[TL.Curves.ID.Lx[i]]]@data, col = col[i])
          }

        }else{
          plot(
            NA,NA,xlim = c(0,1), ylim = c(0,1), main = "",
            axes = FALSE,
            ylab = "",
            xlab = "")
          text(0.5,0.5, "No TL curve detected")
        }
      }#plot.single.sel

      ## (2) Plotting LnLx Curves ----------------------------------------------------
      ##overall plot option selection for plot.single.sel
      if (2 %in% plot.single.sel) {
        .plot_ShineDownCurves(
          record_list,
          curve_ids = OSL.Curves.ID.Lx,
          signal_integral = signal.integral,
          background_integral = background.integral,
          set_main = main,
          set_log = log,
          set_cex = cex,
          set_col = col,
          set_mtext = expression(paste(L[n], ", ", L[x], " curves")),
          set_curveType = CWcurve.type,
          set_curveRes = resolution.OSLCurves)

        ##mtext, implemented here, as a plot window has to be called first
        mtext(
          mtext.outer,
          side = 4,
          outer = TRUE,
          line = -1.7,
          cex = cex,
          col = "blue")

      }# plot.single.sel

      ## (3) Plotting TL Curves previous TnTx ----------------------------------------
      ##overall plot option selection for plot.single.sel
      if (3 %in% plot.single.sel) {
        ##check if TL curves are available
        if (length(TL.Curves.ID.Tx) > 0) {
          ##It is just an approximation taken from the data
          resolution.TLCurves <-  round(mean(diff(
            round(record_list[[TL.Curves.ID.Tx[[1]]]]@data[,1], digits = 1)
          )), digits = 1)

          ## get value ranges of the curves
          xy_xlim <- matrixStats::rowRanges(vapply(
            X = TL.Curves.ID.Tx,
            FUN = \(x) apply(record_list[[x]]@data, 2, range, na.rm = TRUE),
            FUN.VALUE = numeric(4)))

          xlim_range <- c(min(xy_xlim[1,]), max(xy_xlim[2,]))
          ylim_range <- c(1, max(xy_xlim[4,]))

          plot(
            NA,NA,
            xlab = "T [\u00B0C]",
            ylab = paste0("TL [cts/",resolution.TLCurves," \u00B0C]"),
            xlim = xlim_range,
            ylim = ylim_range,
            main = main,
            mgp = c(2, 0.7, 0),
            tcl = -0.4,
            log = gsub("x", "", log))

          #provide curve information as mtext, to keep the space for the header
          mtext(
            side = 3,
            text = expression(paste("TL previous ", T[n],",",T[x]," curves")),
            cex = cex * 0.7)

          ##plot TL curves
          for (i in seq_along(TL.Curves.ID.Tx)) {
            lines(record_list[[TL.Curves.ID.Tx[i]]]@data, col = col[i])
          }

        }else{
          plot(
            NA,NA,xlim = c(0,1), ylim = c(0,1), main = "",
            axes = FALSE,
            ylab = "",
            xlab = ""
          )
          text(0.5,0.5, "No TL curve detected")
        }

      }#plot.single.sel

      ## (4) Plotting TnTx Curves ----------------------------------------------------
      ##overall plot option selection for plot.single.sel
      if (4 %in% plot.single.sel) {
        .plot_ShineDownCurves(
          record_list,
          curve_ids = OSL.Curves.ID.Tx,
          signal_integral = signal.integral,
          background_integral = background.integral,
          set_main = main,
          set_log = log,
          set_cex = cex,
          set_col = col,
          set_mtext = expression(paste(T[n], ", ", T[x], " curves")),
          set_curveType = CWcurve.type,
          set_curveRes = resolution.OSLCurves)

      }# plot.single.sel

      ## (5) Plotting Legend ----------------------------------------
      ##overall plot option selection for plot.single.sel
      if (5 %in% plot.single.sel) {
        ## par.old must be assigned before changing the par() values
        ## because `mai` is affected by `mar`, and doing it in one line like
        ## it's done elsewhere would store a modified `mai` value
        par.old <- par("mar", "mai")
        par(mar = c(1,1,1,1), mai = c(0,0,0,0))

        n <- length(OSL.Curves.ID) / 2
        x <- seq_len(n)
        y <- rep(7, n)

        plot(
          x, y,
          type = "p",
          axes = FALSE,
          xlab = "", ylab = "",
          pch = 20,
          col = unique(col[1:length(OSL.Curves.ID)]),
          cex = 4,
          ylim = c(0,10))

        ## add legend text
        text(x, y, paste0(LnLxTnTx$Name, "\n(", round(LnLxTnTx$Dose, 2), ")"),
             offset = 1, pos = 1, xpd = NA)

        ##add line
        abline(h = 10,lwd = 0.5)

        #reset margin
        par(par.old)
      }#plot.single.sel
    }##end plot

  ## (6) Plot Dose-Response Curve --------------------------------------------
  ## overall plot option selection for plot.single.sel
  plot <- plot && 6 %in% plot.single.sel

  ## if we don't compute the dose-response curve, we'll insert empty subplots
  insert.emptyDRCPlots <- onlyLxTxTable

    ## this must be kept in sync with fit_DoseResponseCurve()
    temp.GC.all.na <- data.frame(
        De = NA,
        De.Error = NA,
        D01 = NA,
        D01.ERROR = NA,
        D02 = NA,
        D02.ERROR = NA,
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
        .De.raw = NA,
        stringsAsFactors = FALSE)

    ##Fit and plot growth curve
    temp.GC <- temp.GC.all.na
    temp.GC.fit.Formula <- NULL

  ## Calculate Dose-response curve ------------------------------------------
  if (!onlyLxTxTable) {

    ## create data.frame
    temp.sample <- data.frame(
        Dose = LnLxTnTx$Dose,
        LxTx = LnLxTnTx$LxTx,
        LxTx.Error = LnLxTnTx$LxTx.Error,
        TnTx = LnLxTnTx$Net_TnTx,
        Test_Dose = LnLxTnTx$Test_Dose
    )

    temp.GC <- do.call(fit_DoseResponseCurve,
                       args = c(list(object = temp.sample), list(...)))

    if (is.null(temp.GC)) {
      temp.GC <- temp.GC.all.na
      temp.GC.fit.Formula <- NA
      insert.emptyDRCPlots <- TRUE
    } else {
          if(plot) {
            do.call(plot_DoseResponseCurve, args = modifyList(
              list(
                object = temp.GC,
                xlab = if(is.null(dose_rate_source)) "Dose [s]" else "Dose [Gy]",
                plot_singlePanels = plot_onePage || length(plot_singlePanels) > 1,
                cex = ifelse(plot_onePage, 0.6, 1)
              ),
              list(...)
            ))
          }

          ##grep information on the fit object
          temp.GC.fit.Formula  <- get_RLum(temp.GC, "Formula")

          ##grep results
          temp.GC <- get_RLum(temp.GC)

          # Provide Rejection Criteria for Palaeodose error --------------------------
          palaeodose.error.calculated <- NA
          De <- as.numeric(temp.GC[, 1])
          if (!is.na(De)) {
            palaeodose.error.calculated <- round(temp.GC[, 2] / De, digits = 5)
          }

          palaeodose.error.threshold <-
            rejection.criteria$palaeodose.error / 100

          palaeodose.error.data.frame <- data.frame(
            Criteria = "Palaeodose error",
            Value = palaeodose.error.calculated,
            Threshold = palaeodose.error.threshold,
            Status = .status_from_threshold(palaeodose.error.calculated,
                                            palaeodose.error.threshold),
            stringsAsFactors = FALSE
          )

          exceed.max.regpoint.data.frame <- data.frame(
            Criteria = "De > max. dose point",
            Value = De,
            Threshold = if(is.na(rejection.criteria$exceed.max.regpoint)){
                NA
              }else if(!rejection.criteria$exceed.max.regpoint){
                Inf
              }else{
                as.numeric(max(LnLxTnTx$Dose))
              },
            Status = .status_from_threshold(De, max(LnLxTnTx$Dose))
          )

          ##add to RejectionCriteria data.frame
          RejectionCriteria <- rbind(RejectionCriteria,
                                     palaeodose.error.data.frame,
                                     exceed.max.regpoint.data.frame)

          ## add rejection status
          status <- if ("FAILED" %in% RejectionCriteria$Status) "FAILED" else "OK"
          temp.GC <- data.frame(temp.GC,
                                RC.Status = status,
                                stringsAsFactors = FALSE)
      }
  }

  ## insert empty plots, otherwise the ordering may get messed up
  if (plot && insert.emptyDRCPlots) {
    shape::emptyplot()
    if (extraArgs$plot_extended %||% TRUE) {
      shape::emptyplot()
      shape::emptyplot()
    }
  }

  ## add information on the integration limits
  temp.GC.extended <- data.frame(
      signal.range = .format_range(signal.integral),
      background.range = .format_range(background.integral),
      signal.range.Tx = .format_range(signal.integral.Tx %||% NA),
      background.range.Tx = .format_range(background.integral.Tx %||% NA),
      stringsAsFactors = FALSE)

# Set return Values -----------------------------------------------------------
    ##generate unique identifier
    UID <- create_UID()

  ## get position numbers
  POSITION <- unique(unlist(lapply(object@records,
                                   function(x) x@info$POSITION %||% NA)))[1]

  ## get grain numbers
  GRAIN <- unique(unlist(lapply(object@records,
                                function(x) x@info$GRAIN %||% NA)))[1]

    temp.results.final <- set_RLum(
      class = "RLum.Results",
      data = list(
        data = as.data.frame(
          c(temp.GC, temp.GC.extended, ALQ = 1,
            POS = POSITION, GRAIN = GRAIN, UID = UID),
          stringsAsFactors = FALSE),
        LnLxTnTx.table = cbind(LnLxTnTx, UID = UID, stringsAsFactors = FALSE),
        rejection.criteria = cbind(UID, RejectionCriteria),
        Formula = temp.GC.fit.Formula
      ),
      info = list(call = sys.call())
    )

  ## (7) Plot IRSL curve/Single Grain ---------------------------------------
  if (plot[1] && 8 %in% plot.single.sel) {
    ## split the device area in two so that the IRSL curve can be plotted
    ## alongside the rejection criteria
    if (!plot_singlePanels[1]) par(mfrow = c(1,2))

      ## check grain an pos and plot single grain disc marker
      ## if we don't have single grain, we can safely use the other
      ## plot option because this kind of test is almost never
      ## for single grains but on all grains
      if(!is.na(POSITION) & !is.na(GRAIN) && GRAIN > 0) {
        .plot_SGMarker(this_grain = GRAIN, this_pos = POSITION)

      } else {
        ##graphical representation of IR-curve
        temp.IRSL <- suppressWarnings(get_RLum(object, recordType = "IRSL"))
        if(length(temp.IRSL) != 0){
          .validate_class(temp.IRSL, c("RLum.Data.Curve", "list"))
          if (inherits(temp.IRSL, "list")) {
            temp.IRSL <- temp.IRSL[[length(temp.IRSL)]]
            .throw_warning("Multiple IRSL curves detected (IRSL test), only the last one shown")
          }
          plot_RLum.Data.Curve(temp.IRSL, par.local = FALSE,
                               mgp = c(2, 0.7, 0), tcl = -0.4)
        }else{
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(x = c(1,1), y = c(1, 1), labels = "No IRSL curve detected!")
        }
      }
    }

  ## (8) Plot rejection criteria --------------------------------------------
  if (plot && 7 %in% plot.single.sel) {
      ##Rejection criteria
      temp.rejection.criteria <- get_RLum(
        temp.results.final,
        data.object = "rejection.criteria")

      .plot_RCCriteria(temp.rejection.criteria)
    }

  ## Return -----------------------------------------------------------------
  invisible(temp.results.final)
}


# Helper functions -------------------------------------------------------------
## create single grain discs with measured grain labelled
.plot_SGMarker <- function(this_grain = 1, this_pos = 1) {
  ## calculate coordinate matrix
  xy_coord <- matrix(
    data = c(
      rep(seq(0.25,0.75, length.out = 10), 10),
      rep(rev(seq(0.25,0.75, length.out = 10)), each = 10)),
    ncol = 2)

  ##set par
  par.old <- par(mar = c(3, 3, 3, 3))
  on.exit(par(par.old), add = TRUE)

  ## draw disc
  shape::emptyplot(main = "Grain location")

  ## draw super circle
  shape::plotellipse(rx = 0.4, ry = 0.4, mid = c(0.5,0.5), lwd = 1)

  ## draw positing holes
  shape::plotellipse(
    rx = 0.015,
    ry = 0.015,
    mid = c(0.15,0.5),
    lwd = 1)
  shape::plotellipse(
    rx = 0.015,
    ry = 0.015,
    mid = c(0.5,0.85),
    lwd = 1)
  shape::plotellipse(
    rx = 0.015,
    ry = 0.015,
    mid = c(0.85,0.5),
    lwd = 1)

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
  ##set par
  par.old <- par(mar = c(1, 0, 3.2, 0.35))
  on.exit(par(par.old), add = TRUE)

  ## determine number of criteria
  n <- nrow(x)

  ## calculate middle points for the lines
  y_coord <- seq(0.1,1,length.out = n * 2)

  # set colours
  pch_set <- vapply(1:nrow(x), function(y) {
    c <- if(x[y,"Status"] == "OK") 3 else 2
    c[is.na(x[y,"Threshold"])] <- 24
    s <- if(x[y,"Status"] == "FAILED") 4 else 21

   c(c,s)
  }, numeric(2))

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

  ## set labels
  x$Value <- round(x$Value, 1)
  x$Threshold <- round(x$Threshold, 2)
  x$sign <- ifelse(x$Value > x$Threshold, ">=", "<=")
  text(
    x = 0.8,
    y = y_coord_l,
    labels = paste(x$Value, x$sign, x$Threshold),
    cex = 0.7,
    adj = c(1, 1.5))

  ## add final points
  points(
    x = rep(0.95, n),
    pch = pch_set[2,],
    y = y_coord[seq(1,length(y_coord),2)],
    bg = pch_set[1,],
    col = pch_set[1,],
    cex = 1.3)
}

# plot the shine-down curves more consistently
.plot_ShineDownCurves <- function(
    record_list,
    curve_ids,
    signal_integral,
    background_integral,
    set_main,
    set_log,
    set_cex,
    set_col,
    set_mtext,
    set_curveType,
    set_curveRes
) {

  ## if we want to apply a log-transform on x and the first time point
  ## is 0, we shift the curves by one channel
  if (set_log == "x" || set_log == "xy") {
    for(i in curve_ids) {
      x.vals <- record_list[[i]]@data[, 1]
      if (x.vals[1] == 0) {
        record_list[[i]]@data[, 1] <- x.vals + x.vals[2] - x.vals[1]
        .throw_warning("Curves shifted by one channel for log-plot")
      }
    }
  }

  ## get value ranges of the curves
  xy_xlim <- matrixStats::rowRanges(vapply(
    X = curve_ids,
    FUN = \(x) apply(record_list[[x]]@data, 2, range, na.rm = TRUE),
    FUN.VALUE = numeric(4)))

  xlim_range <- c(min(xy_xlim[1,]), max(xy_xlim[2,]))
  ylim_range <- c(min(xy_xlim[3,]), max(xy_xlim[4,]))

  #open plot area LnLx
  plot(
    NA,NA,
    xlab = "Time [s]",
    ylab = paste0(set_curveType," [cts/",set_curveRes," s]"),
    xlim = xlim_range,
    ylim = ylim_range,
    main = set_main,
    mgp = c(2, 0.7, 0),
    tcl = -0.4,
    log = set_log)

  #provide curve information as mtext, to keep the space for the header
  mtext(
    side = 3,
    text = set_mtext,
    cex = set_cex * 0.7)

  for (idx in seq_along(curve_ids)) {
    lines(record_list[[curve_ids[idx]]]@data, col = set_col[idx])
  }

  ##mark integration limit Lx curves
  rec <- record_list[[curve_ids[1]]]@data[, 1]
  abline(v = c(rec[range(signal_integral)],
               rec[range(background_integral)]),
    lty = 2,
    col = "gray")
}
