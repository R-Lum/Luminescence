#' @title Al2O3 Irradiation Time Correction Analysis
#'
#' @description The function provides a very particular analysis to correct the irradiation
#' time while irradiating Al2O3:C chips in a luminescence reader.
#'
#' @details Background: Due to their high dose sensitivity Al2O3:C chips are usually
#' irradiated for only a very short duration or under the closed beta-source
#' within a luminescence reader. However, due to its high dose sensitivity, during
#' the movement towards the beta-source, the pellet already receives and non-negligible
#' dose. Based on measurements following a protocol suggested by Kreutzer et al., 2018,
#' a dose response curve is constructed and the intersection (absolute value) with the time axis
#' is taken as real irradiation time.
#'
#' **`method_control`**
#'
#' To keep the generic argument list as clear as possible, arguments to allow a
#' deeper control of the method are all preset with meaningful default parameters and can be
#' handled using the argument `method_control` only, e.g.,
#' `method_control = list(fit.method = "LIN")`. Supported arguments are:
#'
#' \tabular{lll}{
#' **ARGUMENT** \tab **FUNCTION** \tab **DESCRIPTION**\cr
#' `mode` \tab `fit_DoseResponseCurve` \tab as in [Luminescence::fit_DoseResponseCurve]; sets the mode used for fitting\cr
#' `fit.method` \tab `fit_DoseResponseCurve` \tab as in [Luminescence::fit_DoseResponseCurve]; sets the function applied for fitting\cr
#' }
#'
#' @param object [Luminescence::RLum.Analysis-class] or [list] (**required**):
#' results obtained from the measurement.
#' Alternatively a list of [Luminescence::RLum.Analysis-class] objects can be provided to allow an automatic analysis
#'
#' @param signal_integral [numeric] (*optional*):
#' signal integral, used for the signal and the background.
#' If nothing is provided the full range is used. Argument can be provided as [list].
#'
#' @param dose_points [numeric] (*with default*):
#' vector with dose points, if dose points are repeated, only the general
#' pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., 2018. Argument can be provided as [list].
#'
#' @param recordType [character] (*with default*): input curve selection, which is passed to
#' function [Luminescence::get_RLum]. To deactivate the automatic selection set the argument to `NULL`
#'
#' @param method_control [list] (*optional*):
#' optional parameters to control the calculation.
#' See details for further explanations
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... further arguments that can be passed to the plot output
#'
#' @return
#' Function returns results numerically and graphically:
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' \tabular{lll}{
#'   **Element** \tab **Type** \tab **Description**\cr
#'  `$data` \tab `data.frame` \tab correction value and error \cr
#'  `$table` \tab `data.frame` \tab table used for plotting  \cr
#'  `$table_mean` \tab `data.frame` \tab table used for fitting \cr
#'  `$fit` \tab `lm` or `nls` \tab the fitting as returned by the function [Luminescence::fit_DoseResponseCurve]
#' }
#'
#'**slot:** **`@info`**
#'
#' The original function call
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' - A dose response curve with the marked correction values
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::fit_DoseResponseCurve]
#'
#' @references
#'
#' Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier, N., 2018. Environmental Dose Rate
#' Determination Using a Passive Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
#' Geochronometria 45, 56-67. doi: 10.1515/geochr-2015-0086
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.Al2O3C, envir = environment())
#'
#' ##run analysis
#' analyse_Al2O3C_ITC(data_ITC)
#'
#' @export
analyse_Al2O3C_ITC <- function(
  object,
  signal_integral = NULL,
  dose_points = c(2,4,8,12,16),
  recordType = "OSL (UVVIS)",
  method_control = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("analyse_Al2O3C_ITC")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_not_empty(object) # here for issue 1256

  # SELF CALL -----------------------------------------------------------------------------------
  if (inherits(object, "list")) {
    ##check whether the list contains only RLum.Analysis objects
    lapply(object, .validate_class, "RLum.Analysis",
           name = "All elements of 'object'")

    ## expand input arguments
    rep.length <- length(object)
    signal_integral <- .listify(signal_integral, rep.length)
    dose_points <- .listify(dose_points, rep.length)

    ##run analysis
    results_full <- lapply(1:length(object), function(x){
      ##run analysis
      results <- try(analyse_Al2O3C_ITC(
        object = object[[x]],
        signal_integral = signal_integral[[x]],
        dose_points = dose_points[[x]],
        method_control = method_control,
        verbose = verbose,
        plot = plot,
        main = list(...)$main %||% paste0("ALQ #", x),
        ...
      ), outFile = stdout()) # redirect error messages so they can be silenced

      ##catch error
      if (is.null(results) || inherits(results, "try-error"))
        return(NULL)
      return(results)
    })

    ##return
    return(merge_RLum(results_full))
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "RLum.Analysis", extra = "a 'list' of such objects")
  .validate_class(dose_points, c("numeric", "list"))
  if (is.list(dose_points)) {
    lapply(dose_points, .validate_class, "numeric",
           name = "All elements of 'dose_points'")
  }
  .validate_class(recordType, "character", null.ok = TRUE)
  .validate_class(method_control, "list", null.ok = TRUE)
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(plot)

  # Preparation ---------------------------------------------------------------------------------
  ##select curves based on the recordType selection; if not NULL
  if (!is.null(recordType)) {
    object <- get_RLum(object, recordType = recordType, drop = FALSE)
    if (length(object) == 0)
      .throw_error("'recordType' produced an empty object")
  }

  #set method control
  method_control_settings <- list(
    mode = "extrapolation",
    fit.method = "EXP"
  )

  ## modify on request
  if (!is.null(method_control)) {
    method_control_settings <- modifyList(x = method_control_settings,
                                          val = method_control)
  }

  ##dose points enhancement
  ##make sure that the dose_point is enhanced
  dose_points <- rep(dose_points, times = length(object)/2)

  # Calculation ---------------------------------------------------------------------------------
  ##set signal integral
  max.signal_integral <- nrow(object[[1]][])
  if(is.null(signal_integral)){
    signal_integral <- 1:max.signal_integral
  } else if (min(signal_integral) < 1 ||
             max(signal_integral) > max.signal_integral) {
    ## check whether the input is valid, otherwise make it valid
    signal_integral <- 1:max.signal_integral
    .throw_warning("'signal_integral' corrected to 1:", max.signal_integral)
  }

  ##calculate curve sums, assuming the background
  net_SIGNAL <- vapply(seq(1, length(object), by = 2), function(x) {
    temp_signal <- sum(object[[x]][, 2])
    temp_background <- sum(object[[x + 1]][, 2])
    return(temp_signal - temp_background)
  }, FUN.VALUE = numeric(1))

  ## silence notes raised by R CMD check
  DOSE <- net_SIGNAL_NORM <- NULL

  ## create data.tables
  df <- data.table(
      DOSE = dose_points,
      net_SIGNAL = net_SIGNAL,
      net_SIGNAL.ERROR = 0,
      net_SIGNAL_NORM = net_SIGNAL/max(net_SIGNAL),
      net_SIGNAL_NORM.ERROR = 0
    )

  ## compute means and errors
  df_mean <- as.data.frame(df[, list(net_SIGNAL = mean(net_SIGNAL),
                                     net_SIGNAL.ERROR = sd(net_SIGNAL),
                                     net_SIGNAL_NORM = mean(net_SIGNAL_NORM),
                                     net_SIGNAL_NORM.ERROR = sd(net_SIGNAL_NORM)),
                              by = DOSE])

  ##calculate GC
  GC <- fit_DoseResponseCurve(
    object = df_mean,
    mode = method_control_settings$mode,
    fit.method = method_control_settings$fit.method,
    verbose = FALSE
  )
  if (is.null(GC))
    return(NULL)

  ##output
  if(verbose){
    cat("\n[analyse_Al2O3C_ITC()]\n")
    cat("\n Used fit method:\t\t", method_control_settings$fit.method)
    cat("\n Time correction value:\t", round(GC$De$De, 3), "\u00B1", round(GC$De$De.Error, 3))
    cat("\n\n")
  }


  # Plotting ------------------------------------------------------------------------------------
  if(plot){
    ##set plot settings
    plot_settings <- list(
      xlab = "Dose [s]",
      ylab = "Integrated net GSL [a.u.]",
      main = "Irradiation Time Correction",
      xlim = c(-5, max(df$DOSE)),
      ylim = c(0,max(df$net_SIGNAL)),
      legend.pos = "right",
      legend.text = "dose points",
      mtext = ""
    )

    ##modify list on request
    plot_settings <- modifyList(x = plot_settings, val = list(...))

    ##make plot area
    plot(NA, NA,
         xlim = plot_settings$xlim,
         ylim = plot_settings$ylim,
         xlab = plot_settings$xlab,
         ylab = plot_settings$ylab,
         main = plot_settings$main)

    ##add zero lines
    abline(v = 0)
    abline(h = 0)

    ##add dose points
    points(x = df$DOSE, y = df$net_SIGNAL)

    ##add dose response curve
    x <- seq(min(plot_settings$xlim), max(plot_settings$xlim), length.out = 100)
    lines(
      x = x,
      y = eval(GC$Formula)
    )

    ##show offset
    x <- 0
    lines(x = c(-GC$De[1], -GC$De[1]), y = c(eval(GC$Formula), 0), lty = 2, col = "red")
    shape::Arrows(
      x0 = 0,
      y0 = eval(GC$Formula),
      x1 = as.numeric(-GC$De[1]),
      y1 = eval(GC$Formula),
      arr.type = "triangle",
      arr.adj = -0.5,
      col = 'red',
      cex = par()$cex)

    ##add text
    text(
      x = -GC$De[1] / 2,
      y = eval(GC$Formula),
      pos = 3,
      labels = paste(round(GC$De[1],3), "\u00B1", round(GC$De[2], 3)),
      col = 'red',
      cex = 0.8)

    ##add 2nd x-axis
    axis(
      side = 1,
      at = axTicks(side = 1),
      labels = paste0("(",(axTicks(side = 1) + round(as.numeric(GC$De[1]),2)), ")"),
      line = 1,
      col.axis = "red",
      lwd.ticks = 0,
      lwd = 0,
      cex.axis = 0.9
    )

    ##add legend
    legend(
      plot_settings$legend.pos,
      bty = "n",
      pch = 1,
      legend = plot_settings$legend.text
    )

    ##add mtext
    mtext(side = 3, text = plot_settings$mtext)
  }

  # Output --------------------------------------------------------------------------------------
  set_RLum(
    class = "RLum.Results",
    data = list(
      data = data.frame(
        VALUE = as.numeric(GC$De$De),
        VALUE_ERROR = as.numeric(sd(GC$De.MC))
      ),
      table = as.data.frame(df),
      table_mean = df_mean,
      fit = GC$Fit
    ),
    info = list(call = sys.call())
  )
}
