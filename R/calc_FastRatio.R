#' @title Calculate the Fast Ratio for CW-OSL curves
#'
#' @description
#' Function to calculate the fast ratio of quartz CW-OSL single grain or single
#' aliquot curves after Durcan & Duller (2011).
#'
#' This function follows the equations of Durcan & Duller (2011). The energy
#' required to reduce the fast and medium quartz OSL components to `x` and
#' `x2` % respectively using eq. 3 to determine channels L2 and L3 (start
#' and end). The fast ratio is then calculated from: \eqn{(L1-L3)/(L2-L3)}.
#'
#' @param object [RLum.Analysis-class], [RLum.Data.Curve-class] or [data.frame] (**required**):
#' x, y data of measured values (time and counts).
#'
#' @param stimulation.power [numeric] (*with default*):
#' Stimulation power in mW/cm²
#'
#' @param wavelength [numeric] (*with default*):
#' Stimulation wavelength in nm
#'
#' @param sigmaF [numeric] (*with default*):
#' Photoionisation cross-section (cm²) of the fast component.
#' Default value after Durcan & Duller (2011).
#'
#' @param sigmaM [numeric] (*with default*):
#' Photoionisation cross-section (cm²) of the medium component.
#' Default value after Durcan & Duller (2011).
#'
#' @param Ch_L1 [numeric] (*with default*):
#' An integer specifying the channel for L1.
#'
#' @param Ch_L2 [numeric] (*optional*):
#' An integer specifying the channel for L2.
#'
#' @param Ch_L3 [numeric] (*optional*):
#' A vector of length 2 with integer values specifying the start and end
#' channels for L3 (e.g., `c(40, 50)`), with the second component greater
#' than or equal to the first.
#'
#' @param x [numeric] (*with default*):
#' Percentage of signal remaining from the fast component.
#' Used to define the location of L2 and L3 (start).
#'
#' @param x2 [numeric] (*with default*):
#' Percentage of signal remaining from the medium component.
#' Used to define the location of L3 (end).
#'
#' @param dead.channels [numeric] (*with default*):
#' Vector of length 2 in the form of `c(x, y)`.
#' Channels that do not contain OSL data, i.e. at the start or end of measurement.
#'
#' @param fitCW.sigma [logical] (*optional*):
#' fit CW-OSL curve using [fit_CWCurve] to calculate `sigmaF` and `sigmaM` (**experimental**).
#'
#' @param fitCW.curve [logical] (*optional*):
#' fit CW-OSL curve using [fit_CWCurve] and derive the counts of L2 and L3
#' from the fitted OSL curve (**experimental**).
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... available options: `verbose` ([logical]).
#' Further arguments passed to [fit_CWCurve].
#'
#' @return
#' Returns a plot (*optional*) and an S4 object of type [RLum.Results-class].
#' The slot `data` contains a [list] with the following elements:
#'
#' \item{summary}{[data.frame] summary of all relevant results}
#' \item{data}{the original input data}
#' \item{fit}{[RLum.Results-class] object if either `fitCW.sigma` or `fitCW.curve` is `TRUE`}
#' \item{args}{[list] of used arguments}
#' \item{call}{[call] the function call}
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Georgina E. King, University of Bern (Switzerland) \cr
#' Julie A. Durcan, University of Oxford (United Kingdom) \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @references
#' Durcan, J.A. & Duller, G.A.T., 2011. The fast ratio: A rapid measure for testing
#' the dominance of the fast component in the initial OSL signal from quartz.
#' Radiation Measurements 46, 1065-1072.
#'
#' Madsen, A.T., Duller, G.A.T., Donnelly, J.P., Roberts, H.M. & Wintle, A.G., 2009.
#' A chronology of hurricane landfalls at Little Sippewissett Marsh, Massachusetts, USA,
#' using optical dating. Geomorphology 109, 36-45.
#'
#' **Further reading**
#'
#' Steffen, D., Preusser, F. & Schlunegger, 2009. OSL quartz age underestimation
#' due to unstable signal components. Quaternary Geochronology 4, 353-362.
#'
#'
#' @seealso [fit_CWCurve], [get_RLum], [RLum.Analysis-class],
#' [RLum.Results-class], [RLum.Data.Curve-class]
#'
#' @examples
#' # load example CW-OSL curve
#' data("ExampleData.CW_OSL_Curve")
#'
#' # calculate the fast ratio w/o further adjustments
#' res <- calc_FastRatio(ExampleData.CW_OSL_Curve)
#'
#' # show the summary table
#' get_RLum(res)
#'
#' @md
#' @export
calc_FastRatio <- function(object,
                           stimulation.power = 30.6,
                           wavelength = 470,
                           sigmaF = 2.6E-17,
                           sigmaM = 4.28E-18,
                           Ch_L1 = 1,
                           Ch_L2 = NULL,
                           Ch_L3 = NULL,
                           x = 1,
                           x2 = 0.1,
                           dead.channels = c(0,0),
                           fitCW.sigma = FALSE,
                           fitCW.curve = FALSE,
                           plot = TRUE,
                           ...) {
  .set_function_name("calc_FastRatio")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks - -----------------------------------------------------

  .validate_class(object, c("RLum.Analysis", "RLum.Results", "RLum.Data.Curve",
                            "data.frame", "matrix"))
  .validate_not_empty(object)
  .validate_positive_scalar(Ch_L1, int = TRUE)
  .validate_positive_scalar(Ch_L2, int = TRUE, null.ok = TRUE)
  if (!is.null(Ch_L3)) {
    .validate_class(Ch_L3, c("integer", "numeric"))
    .validate_length(Ch_L3, 2)
    .validate_positive_scalar(Ch_L3[1], int = TRUE, name = "'Ch_L3[1]'")
    .validate_positive_scalar(Ch_L3[2], int = TRUE, name = "'Ch_L3[2]'")
    if (Ch_L3[1] > Ch_L3[2]) {
      .throw_error("'Ch_L3[2]' must be greater than or equal to 'Ch_L3[1]'")
    }
  }
  .validate_positive_scalar(wavelength)
  .validate_positive_scalar(sigmaF)
  .validate_positive_scalar(sigmaM)
  .validate_positive_scalar(x)
  .validate_positive_scalar(x2)
  .validate_class(dead.channels, c("integer", "numeric"))
  .validate_length(dead.channels, 2)
  if (any(dead.channels < 0)) {
    .throw_error("All elements of 'dead.channels' should be non-negative")
  }

  ## Input object handling -----------------------------------------------------
  if (inherits(object, "RLum.Analysis"))
    object <- get_RLum(object)

  if (inherits(object, "RLum.Results"))
    object <- get_RLum(object, "data")

  if ((is.data.frame(object) || is.matrix (object)) && ncol(object) < 2) {
    .throw_error("'object' should have at least two columns")
  }

  if (!inherits(object, "list"))
    object <-list(object)


  ## Settings ------------------------------------------------------------------
  settings <- list(verbose = TRUE,
                   n.components.max = 3,
                   fit.method = "LM",
                   output.terminal = FALSE,
                   info = list(),
                   fit = NULL)

  # override defaults with args in ...
  settings <- modifyList(settings, list(...))


  ## Calculations --------------------------------------------------------------
  # iterate over all user provided objects and calculate the FR
  fast.ratios <- lapply(object, function(obj) {

    if (inherits(obj, "RLum.Data.Curve"))
      A <- get_RLum(obj)
    else
      A <- obj

    ## Energy calculation
    # P = user defined stimulation power in mW
    # lambdaLED = wavelength of stimulation source in nm
    P <- stimulation.power
    lamdaLED <- wavelength

    ## Constants
    ## c = speed of light, h = Planck's constant
    h <- 6.62607004E-34
    c <- 299792458

    I0 <- (P / 1000) / (h * c / (lamdaLED * 10^-9))
    Ch_width <- max(A[ ,1]) / length(A[ ,1])

    # remove dead channels
    A <- as.data.frame(A[(dead.channels[1] + 1):(nrow(A)-dead.channels[2]), ])
    A[ ,1] <- A[ ,1] - A[1,1]

    # estimate the photo-ionisation crossections of the fast and medium
    # component using the fit_CWCurve function
    if (fitCW.sigma | fitCW.curve) {
      fitCW.res <- try(fit_CWCurve(A, n.components.max = settings$n.components.max,
                                   fit.method = settings$fit.method,
                                   LED.power = stimulation.power,
                                   LED.wavelength = wavelength,
                                   verbose = settings$output.terminal,
                                   plot = plot), outFile = stdout())

      if (!inherits(fitCW.res, "try-error")) {
        settings$fit <- fitCW.res
        if (fitCW.sigma) {
          sigmaF <- get_RLum(fitCW.res)$cs1
          sigmaM <- get_RLum(fitCW.res)$cs2
          if (settings$verbose) {
            message("\n [calc_FitCWCurve()]\n")
            message("New value for sigmaF: ", format(sigmaF, digits = 3, nsmall = 2))
            message("New value for sigmaM: ", format(sigmaM, digits = 3, nsmall = 2))
          }
        }

        if (fitCW.curve) {
          nls <- get_RLum(fitCW.res, "fit")
          A[ ,2] <- predict(nls)
        }
      } else {
        settings["fit"] <- list(NULL)
        if (settings$verbose)
          .throw_message("Fitting failed, please call 'fit_CWCurve()' ",
                         "manually before calculating the fast ratio")
      }
    }

    ## The equivalent time in s of L1, L2, L3
    # Use these values to look up the channel
    t_L1 <- 0

    if (is.null(Ch_L2))
      t_L2 <- (log(x / 100)) / (-sigmaF * I0)
    else
      t_L2 <- A[Ch_L2, 1]

    if (is.null(Ch_L3)) {
      t_L3_start <- (log(x / 100)) / (-sigmaM * I0)
      t_L3_end <- (log(x2 / 100)) / (-sigmaM * I0)
    } else {
      if (any(Ch_L3 > nrow(A))) {
        .throw_error("Value in 'Ch_L3' (", .collapse(Ch_L3, quote = FALSE),
                     ") exceeds number of available channels (", nrow(A), ")")
      }
      t_L3_start <- A[Ch_L3[1], 1]
      t_L3_end <- A[Ch_L3[2], 1]
    }

    ## Channel number(s) of L2 and L3
    if (is.null(Ch_L2))
      Ch_L2 <- which.min(abs(A[,1] - t_L2))

    if (Ch_L2 <= 1) {
      msg <- sprintf("Calculated time/channel for L2 is too small (%.f, %.f), NULL returned",
                     t_L2, Ch_L2)
      settings$info <- modifyList(settings$info, list(L2 = msg))
      .throw_warning(msg)
      return(NULL)
    }

    Ch_L3st<- which.min(abs(A[,1] - t_L3_start))
    Ch_L3end <- which.min(abs(A[,1] - t_L3_end))

    ## Counts in channels L1, L2, L3
    # L1 ----
    Cts_L1 <- A[Ch_L1, 2]

    # L2 ----
    if (Ch_L2 > nrow(A)) {
      msg <- sprintf(paste("The calculated channel for L2 (%i) exceeds",
                           "the number of available channels (%i),",
                           "NULL returned"), Ch_L2, nrow(A))
      settings$info <- modifyList(settings$info, list(L2 = msg))
      .throw_warning(msg)
      return(NULL)
    }

    Cts_L2 <- A[Ch_L2, 2]

    # optional: predict the counts from the fitted curve
    if (fitCW.curve) {
      if (!inherits(fitCW.res, "try-error")) {
        nls <- get_RLum(fitCW.res, "fit")
        Cts_L2 <- predict(nls, list(x = t_L2))
      }
    }


    # L3 ----
    if (Ch_L3st >= nrow(A) | Ch_L3end > nrow(A)) {
      msg <- sprintf(paste("The calculated channels for L3 (%i, %i) exceed",
                           "the number of available channels (%i).",
                           "\nThe background has instead been estimated from the last",
                           "5 channels."), Ch_L3st, Ch_L3end, nrow(A))
      settings$info <- modifyList(settings$info, list(L3 = msg))
      .throw_warning(msg)
      Ch_L3st <- max(nrow(A) - 5, 1)
      Ch_L3end <- nrow(A)
      t_L3_start <- A[Ch_L3st,1]
      t_L3_end <- A[Ch_L3end,1]
    }

    Cts_L3 <- mean(A[Ch_L3st:Ch_L3end, 2])

    # optional: predict the counts from the fitted curve
    if (fitCW.curve) {
      if (!inherits(fitCW.res, "try-error")) {
        nls <- get_RLum(fitCW.res, "fit")
        Cts_L3 <- mean(predict(nls, list(x = c(t_L3_start, t_L3_end))))
      }
    }

    # Warn if counts are not in decreasing order
    if (Cts_L3 >= Cts_L2)
      .throw_warning(sprintf("L3 contains more counts (%.f) than L2 (%.f)",
                             Cts_L3, Cts_L2))

    ## Fast Ratio
    FR <- (Cts_L1 - Cts_L3) / (Cts_L2 - Cts_L3)
    if (length(FR) != 1)
      FR <- NA

    ## Fast Ratio - Error calculation
    FR_se <- NA
    FR_rse <- NA
    if (!is.na(FR)) {

      # number of channels the background was derived from
      nBG <- abs(Ch_L3end - Ch_L3st)

      # relative standard errors
      rse_L1 <- sqrt(Cts_L1 + Cts_L3 / nBG) / (Cts_L1 - Cts_L3)
      rse_L2 <- sqrt(Cts_L2 + Cts_L3 / nBG) / (Cts_L2 - Cts_L3)

      # absolute standard errors
      se_L1 <- rse_L1 * (Cts_L1 - Cts_L3)
      se_L2 <- rse_L2 * (Cts_L2 - Cts_L3)

      # absolute standard error on fast ratio
      FR_se <- (sqrt((se_L1 / (Cts_L1 - Cts_L3))^2 + ((se_L2 / (Cts_L2 - Cts_L3))^2) )) * FR
      FR_rse <- FR_se / FR * 100
    }

    ## Return values -----------------------------------------------------------
    summary <- data.frame(fast.ratio = FR,
                          fast.ratio.se = FR_se,
                          fast.ratio.rse = FR_rse,
                          channels = nrow(A),
                          channel.width = Ch_width,
                          dead.channels.start = as.integer(dead.channels[1]),
                          dead.channels.end = as.integer(dead.channels[2]),
                          sigmaF = sigmaF,
                          sigmaM = sigmaM,
                          I0 = I0,
                          stimulation.power = stimulation.power,
                          wavelength = wavelength,
                          t_L1 = t_L1,
                          t_L2 = t_L2,
                          t_L3_start = t_L3_start,
                          t_L3_end = t_L3_end,
                          Ch_L1 = as.integer(Ch_L1),
                          Ch_L2 = as.integer(Ch_L2),
                          Ch_L3_start = as.integer(Ch_L3st),
                          Ch_L3_end = as.integer(Ch_L3end),
                          Cts_L1 = Cts_L1,
                          Cts_L2 = Cts_L2,
                          Cts_L3 = Cts_L3)

    fast.ratio <- set_RLum(class = "RLum.Results",
                           originator = "calc_FastRatio",
                           data = list(summary = summary,
                                       data = obj,
                                       fit = settings$fit,
                                       args = as.list(sys.call(-2L)[-1]),
                                       call = sys.call(-2L)),
                           info = settings$info
    )

    ## Console Output ----------------------------------------------------------
    if (settings$verbose) {

      table.names <- c(
        "Fast Ratio\t", " \U02EA Absolute error", " \U02EA Relative error (%)", "Channels\t",
        "Channel width (s)", "Dead channels start", "Dead channels end",
        "Sigma Fast\t", "Sigma Medium\t", "I0\t\t", "Stim. power (mW/cm^2)", "Wavelength (nm)",
        "-\n Time L1 (s)\t", "Time L2 (s)\t", "Time L3 start (s)", "Time L3 end (s)",
        "-\n Channel L1\t", "Channel L2\t", "Channel L3 start", "Channel L3 end\t",
        "-\n Counts L1\t", "Counts L2\t", "Counts L3\t")

      cat("\n[calc_FastRatio()]\n")
      cat("\n -------------------------------")
      for (i in 1:ncol(summary)) {
        cat(paste0("\n ", table.names[i],"\t: ",
                   format(summary[1, i], digits = 2, nsmall = 2)))
      }
      cat("\n -------------------------------\n\n")
    }

    ## Plotting -------------------------------------------------------------
    if (plot)
      try(plot_RLum.Results(fast.ratio, ...))

    # return
    return(fast.ratio)
  }) # End of lapply

  if (length(fast.ratios) == 1)
    fast.ratios <- fast.ratios[[1]]

  invisible(fast.ratios)
}
