#' @title Compute SAR palaeodoses using natural sensitivity correction (NCF)
#'
#' @description
#' The function computes the natural sensitivity correction factor (NCF) for
#' the assessment of single aliquot regeneration based palaeodoses. This allows
#' to produce \eqn{D_e} values that account for sensitivity changes that
#' may occur during the preheat and readout of the natural OSL.
#'
#' @details
#' This function is a wrapper around [Luminescence::analyse_SAR.CWOSL]: it
#' works in a similar way, but corrects the natural OSL signal according to
#' Singhvi et al. (2011) and by implementing the MatLab code reported in
#' Kaushal et al. (2022).
#'
#' ## The NCF procedure
#'
#' The NCF protocol modifies the standard SAR protocol by adding a small dose
#' before the readout of the natural signal; the corresponding 110 deg. C TL
#' peak in quartz is monitored and compared to the TL peak recorded during the
#' typical cutheat measurement. The additional dose used for the NCF TL curves
#' is subtracted automatically. The published NCF sequence structure must be
#' followed, otherwise the correction will not work.
#'
#' The natural sensitivity correction factor (NCF) is measured as the ratio of
#' the 110 deg. C TL peaks before and after the measurement of natural OSL,
#' accounting for an integration range around the peak, typically 15 deg. C or
#' the full width at half maximum (FWHM).
#'
#' ## TL peak finding
#'
#' The TL peaks are found automatically by finding the position  with highest
#' counts below 160 deg. C (after some smoothing has been applied to the counts
#' data) for each of the two TL curves.
#'
#' The peak search parameters can be controlled via the `method_control`
#' argument. If used, this must be a list with one or more of the following
#' named arguments (e.g. `method_control = list(search.threshold.degrees = 130)`):
#' - `peak.temperature`: this can be used to set the temperature of the TL peak
#'   manually; if set to `NULL` (default), the peak is found automatically
#' - `search.threshold.degrees`: threshold in deg. C below which the peak
#'   is searched (160 by default); this is ignored if `peak.temperature` is
#'   not `NULL`
#'
#' The identified TL curves and the chosen signal integral are shown
#' in the graphical output.
#'
#' @param object [Luminescence::RLum.Analysis-class] or [list] of
#' [Luminescence::RLum.Analysis-class] objects (**required**):
#' input object containing data for analysis.
#' If a [list] is provided the functions tries to iterate over each element
#' in the list.
#'
#' @param TL_peak_range [integer] (**with default**):
#' integration range in deg. C around the identified peak.
#'
#' @param method_control [list] (*optional*):
#' parameters to control the peak-finding step.
#'
#' @inheritParams analyse_SAR.CWOSL
#'
#' @return
#' Plots (*optional*) and an [Luminescence::RLum.Results-class] object is
#' returned containing the following elements:
#'
#' \tabular{lll}{
#' **DATA.OBJECT** \tab **TYPE** \tab **DESCRIPTION** \cr
#' `..$data` : \tab  `data.frame` \tab Table with corrected \eqn{D_e} values \cr
#' `..$LnLxTnTx.table` : \tab `data.frame` \tab corrected `LnLxTnTx` values \cr
#' `..$rejection.criteria` : \tab `data.frame` \tab Rejection criteria \cr
#' `..$Formula` : \tab list \tab Function used for fitting of the dose response curve \cr
#' `..$data_uncor` : \tab  `data.frame` \tab Table with the uncorrected data \cr
#' `..$LnLxTnTx.table_uncor` : \tab `data.frame` \tab uncorrected `LnLxTnTx` values\cr
#' `..$NCF_settings` : \tab list \tab Setting used for TL peak finding \cr
#' }
#'
#' This output is similar to the output of [Luminescence::analyse_SAR.CWOSL]
#' extended by a few more elements.
#'
#' @note
#' This function is a beta version.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#' Sebastian Kreutzer, LIAG Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::analyse_SAR.CWOSL], [Luminescence::fit_DoseResponseCurve]
#'
#' @references
#' Kaushal, R.K., Chauhan, N., Singhvi, A.K., 2022. Luminescence dating of
#' quartz: A MATLAB-based program for computation of SAR paleodoses using
#' natural sensitivity correction (NCF). Ancient TL, 40 (2), 1-7.
#' \doi{10.26034/la.atl.2022.561}
#'
#' Singhvi, A.K., Stokes, S.C., Chauhan, N., Nagar, Y.C., Jaiwal, M.K., 2011.
#' Changes in natural OSL sensitivity during single aliquot regeneration
#' procedure and their implications for equivalent dose determination.
#' Geochronometria, 38 (3), 231-241. \doi{10.2478/s13386-011-0028-3}
#'
#' @keywords datagen plot
#'
#' @examples
#' ## Load example data
#' file <- system.file("extdata/NCF.binx", package = "Luminescence")
#' ncf <- read_BIN2R(file, fastForward = TRUE)
#'
#' results <- analyse_SAR.NCF(
#'  object = ncf,
#'  signal_integral = 1:2,
#'  background_integral = 100:250,
#'  dose_rate_source = 0.1)
#'
#' @export
analyse_SAR.NCF <- function(
  object,
  TL_peak_range = 15,
  method_control = NULL,
  ...
) {
  .set_function_name("analyse_SAR.NCF")
  on.exit(.unset_function_name(), add = TRUE)

  ## Self-call --------------------------------------------------------------
  if (inherits(object, "list")) {
    results <- lapply(seq_along(object), function(x) {
      analyse_SAR.NCF(object[[x]],
                      TL_peak_range = TL_peak_range,
                      method_control = method_control,
                      ...)
    })

    return(results)
  }

  .validate_positive_scalar(TL_peak_range)
  .validate_class(method_control, "list", null.ok = TRUE)

  ## there should be an additional dose recorded for NCF
  additional.dose <- object@records[[1]]@info$IRR_TIME
  if (is.null(additional.dose) || is.na(additional.dose) || additional.dose == 0) {
    .throw_error("No additional dose point found, check that the ",
                 "NCF-SAR protocol was correctly implemented")
  }

  ## default values
  method_control.settings <- list(
      peak.temperature = NULL,
      search.threshold.degrees = 160)

  ## modify list if necessary
  method_control <- modifyList(
      method_control.settings,
      val = as.list(method_control),
      keep.null = TRUE)


  ## Main analysis ----------------------------------------------------------
  ## to suppress the plotting from analyse_SAR.CWOSL(), we need to remove the
  ## plot argument from ..., otherwise it may cause a multiple match error
  extraArgs <- list(...)
  extraArgs$plot <- NULL

  ## this runs the analysis with the original dataset
  cwosl <- do.call(analyse_SAR.CWOSL,
                   c(list(object = object, plot = FALSE, .NCF_mode = TRUE),
                     extraArgs))
  if (is.null(cwosl)) {
    .throw_message("CW-OSL analysis skipped: check your sequence, NULL returned")
    return(NULL)
  }

  ## Identify TL1 and TL2 ---------------------------------------------------
  ## to identify TL1 and TL2, we look for the first two OSL records
  osl.idx <- get_RLum(object, recordType = "OSL", get.index = TRUE)[1:2]
  tl.idx <- get_RLum(object, recordType = "TL", get.index = TRUE)
  TL1 <- object[[min(tl.idx[tl.idx < osl.idx[1]])]] # use min() to skip preheat if present
  TL2 <- object[[max(tl.idx[tl.idx < osl.idx[2]])]] # use max() to skip cut heat if present

  ## compute the natural correction factor
  res1 <- .compute.integrated.counts(TL1, TL_peak_range, method_control)
  res2 <- .compute.integrated.counts(TL2, TL_peak_range, method_control)
  icnt1 <- res1$counts
  icnt2 <- res2$counts
  NCF <- icnt2 / icnt1

  ## extract the computed LnTn
  LnLxTnTx <- cwosl$LnLxTnTx.table[, c("Name", "Dose", "LxTx", "LxTx.Error",
                                       "TnTx", "Test_Dose")]
  LnLxTnTx.uncorrected <- LnLxTnTx
  nat.idx <- grep("Natural", LnLxTnTx$Name, fixed = TRUE)
  LnTn <- LnLxTnTx$LxTx[nat.idx]
  LnTn.Error <- LnLxTnTx$LxTx.Error[nat.idx]

  ## correct LnTn -----------------------------------------------------------
  LnTn.corrected <- LnTn * NCF

  ## error propagation based on the NCFSAR-tool code:
  ## https://github.com/Rahulkaushal009/NCFSAR-tool/blob/main/find_NCF_De_direct.m
  icnt1.err <- sqrt(icnt1)
  icnt2.err <- sqrt(icnt2)
  LnTn.Error.corrected <- sqrt((LnTn.Error / LnTn)^2 +
                               (icnt1.err / icnt1)^2 +
                               (icnt2.err / icnt2)^2) * LnTn.corrected

  ## use the corrected values in the LnLxTnTx data frame
  LnLxTnTx$LxTx[nat.idx] <- LnTn.corrected
  LnLxTnTx$LxTx.Error[nat.idx] <- LnTn.Error.corrected

  ## we want to force fit_DoseResponseCurve to run with verbose = FALSE so
  ## that we can print out the fit message ourselves
  verbose <- extraArgs$verbose %||% TRUE
  extraArgs$verbose <- NULL
  DRC <- do.call(fit_DoseResponseCurve,
                 modifyList(list(LnLxTnTx[, -match("Name", colnames(LnLxTnTx))],
                                 verbose = FALSE),
                            extraArgs))
  if (verbose)
    .throw_message(DRC@info$fit_message, error = FALSE)

  ## Subtract additional dose -----------------------------------------------
  additional.dose <- additional.dose * (extraArgs$dose_rate_source %||% 1)
  cwosl@data$data$De <- cwosl@data$data$De - additional.dose
  DRC@data$De$De <- DRC@data$De$De - additional.dose

  ## Plotting ---------------------------------------------------------------
  if (is.null(extraArgs$plot) || isTRUE(extraArgs$plot)) {
    par.default <- .par_defaults()
    on.exit(par(par.default), add = TRUE)

    ## recreate layout matrix
    layout.matrix <- matrix(c(1, 1, 3, 3,  6,  6, 7,
                              1, 1, 3, 3,  6,  6, 8,
                              2, 2, 4, 4, 10, 10, 9,
                              2, 2, 4, 4, 10, 10, 9,
                              5, 5, 5, 5,  5,  5, 5),
                            nrow = 5, ncol = 7, byrow = TRUE)
    graphics::layout(layout.matrix)
    .plot_SAR.CWOSL(cwosl, plot_singlePanels = 1:7)
    .plot_TL_peaks(TL1, TL2, res1, res2, TL_peak_range)
  }

  ## Return results ---------------------------------------------------------
  ## we return the object from analyse_SAR.CWOSL() and append to it the fields
  ## for the NCF-corrected values
  results <- cwosl

  ## add a slot for the uncorrected De
  results@data$data_uncor <- results@data$data
  results@data$LnLxTnTx.table_uncor <- results@data$LnLxTnTx.table
  results@data$NCF_settings <- method_control

  ## overwrite pre-existing slots with new data
  results@originator <- "analyse_SAR.NCF"
  results@data$data[, 1:length(DRC$De)] <- DRC$De
  results@data$LnLxTnTx.table <- LnLxTnTx

  results
}


## Helpers ------------------------------------------------------------------
.compute.integrated.counts <- function(curve, TL_peak_range, method_control) {
  ## smooth the counts
  data <- curve@data
  data[, 2] <- .smoothing(data[, 2], k = 5, align = "center")

  ## identify the index of the peak as the highest count below the threshold
  peak.temperature <- method_control$peak.temperature
  if (is.null(peak.temperature)) {
    search_range <- data[, 1] < method_control$search.threshold.degrees
    peak.idx <- which.max(data[search_range, 2])
    peak.temperature <- data[peak.idx, 1]
  }

  ## find the ranges of temperatures to integrate
  range.tem <- peak.temperature + c(-1, 1) * TL_peak_range
  range.idx <- between(data[, 1], range.tem[1], range.tem[2])

  ## return the integrated counts using the original data
  list(counts = sum(curve@data[range.idx, 2]),
       peak.temperature = peak.temperature,
       range.idx = which(range.idx))
}

.plot_TL_peaks <- function(TL1, TL2, res1, res2, TL_peak_range) {
  ## first TL
  plot(TL1, type = "l", col = 1, par.local = FALSE,
       main = sprintf("TL Peaks (interval: %g \u00B0C)", TL_peak_range))
  ymin <- par("usr")[3]  ## minimum y coordinate visible on the plot
  abline(v = c(res1$peak.temperature, TL1@data[range(res1$range.idx), 1]),
         lty = c(2, 3, 3), col = 1)
  polygon(x = c(TL1@data[res1$range.idx, 1], rev(TL1@data[res1$range.idx, 1])),
          y = c(TL1@data[res1$range.idx, 2], rep(ymin, length(res1$range.idx))),
          col = rgb(0, 1, 0, 0.1), border = NA, xpd = FALSE)

  ## second TL
  points(TL2@data, type = "l", col = 2)
  abline(v = c(res2$peak.temperature, TL2@data[range(res2$range.idx), 1]),
         lty = c(2, 3, 3), col = 2)
  polygon(x = c(TL2@data[res2$range.idx, 1], rev(TL2@data[res2$range.idx, 1])),
          y = c(TL2@data[res2$range.idx, 2], rep(ymin, length(res2$range.idx))),
          col = rgb(0, 1, 0, 0.1), border = NA, xpd = FALSE)

  ## legend
  legend("topright",
         legend = c(sprintf("TL#1 @ %.1f \u00B0C", res1$peak.tem),
                    sprintf("TL#2 @ %.1f \u00B0C", res2$peak.tem)),
         bty = "n",
         cex = 0.9,
         fill = 1:2)
}
