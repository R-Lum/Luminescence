#' Calculate the Fast Ratio for CW-OSL curves
#' 
#' Function to calculate the fast ratio of quartz CW-OSL single grain or single 
#' aliquot curves after Durcan & Duller (2011). 
#' 
#' This function follows the equations of Durcan & Duller (2011). The energy
#' required to reduce the fast and medium quartz OSL components to \code{x} and
#' \code{x2} \% respectively using eq. 3 to determine channels L2 and L3 (start 
#' and end). The fast ratio is then calculated from: \eqn{(L1-L3)/(L2-L3)}. 
#'
#' @param object \code{\linkS4class{RLum.Analysis}}, 
#' \code{\linkS4class{RLum.Data.Curve}} or \code{\link{data.frame}} 
#' (\bold{required}): x, y data of measured values (time and counts).
#' 
#' @param stimulation.power \code{\link{numeric}} (with default): Stimulation power in mW/cm^2
#' 
#' @param wavelength \code{\link{numeric}} (with default): Stimulation wavelength in nm
#' 
#' @param sigmaF \code{\link{numeric}} (with default): Photoionisation cross-section (cm^2) of the
#' fast component. Default value after Durcan & Duller (2011).
#' 
#' @param sigmaM \code{\link{numeric}} (with default): Photoionisation cross-section (cm^2) of the
#' medium component. Default value after Durcan & Duller (2011).
#' 
#' @param Ch_L1 \code{\link{numeric}} (with default): An integer specifying the channel for L1.
#' 
#' @param x \code{\link{numeric}} (with default): \% of signal remaining from the fast component.
#' Used to define the location of L2 and L3 (start).
#' 
#' @param x2 \code{\link{numeric}} (with default): \% of signal remaining from the medium component.
#' Used to define the location of L3 (end). 
#' 
#' @param dead.channels \code{\link{numeric}} (with default): Vector of length 2 in the form of
#' \code{c(x, y)}. Channels that do not contain OSL data, i.e. at the start or end of
#' measurement.
#' 
#' @param fitCW.sigma \code{\link{logical}} (optional): fit CW-OSL curve using \code{\link{fit_CWCurve}}
#' to calculate \code{sigmaF} and \code{sigmaM} (experimental).
#' 
#' @param fitCW.curve \code{\link{logical}} (optional): fit CW-OSL curve using \code{\link{fit_CWCurve}}
#' and derive the counts of L2 and L3 from the fitted OSL curve (experimental).
#' 
#' @param plot \code{\link{logical}} (with default): plot output (\code{TRUE}/\code{FALSE})
#' 
#' @param ... available options: \code{verbose} (\code{\link{logical}}). Further
#' arguments passed to \code{\link{fit_CWCurve}}.
#'
#' @return Returns a plot (optional) and an S4 object of type \code{\linkS4class{RLum.Results}}. 
#' The slot \code{data} contains a \code{\link{list}} with the following elements:\cr
#'
#' \item{summary}{\code{\link{data.frame}} summary of all relevant results}
#' \item{data}{the original input data}
#' \item{fit}{\code{\linkS4class{RLum.Results}} object if either \code{fitCW.sigma} or \code{fitCW.curve} is \code{TRUE}}
#' \item{args}{\code{\link{list}} of used arguments}
#' \item{call}{\code{\link{call}} the function call}
#' 
#' @section Function version: 0.1.0
#'
#' @author 
#' Georgina King, University of Cologne (Germany) \cr
#' Julie A. Durcan, University of Oxford (United Kingdom) \cr
#' Christoph Burow, University of Cologne (Germany) \cr
#'
#' @references 
#' Durcan, J.A. & Duller, G.A.T., 2011. The fast ratio: A rapid measure for testing
#' the dominance of the fast component in the initial OSL signal from quartz.
#' Radiation Measurements 46, 1065-1072. \cr\cr
#' 
#' Madsen, A.T., Duller, G.A.T., Donnelly, J.P., Roberts, H.M. & Wintle, A.G., 2009.
#' A chronology of hurricane landfalls at Little Sippewissett Marsh, Massachusetts, USA,
#' using optical dating. Geomorphology 109, 36-45. \cr\cr
#'
#' \bold{Further reading} \cr\cr
#' 
#' Steffen, D., Preusser, F. & Schlunegger, 2009. OSL quartz age underestimation 
#' due to unstable signal components. Quaternary Geochronology 4, 353-362.
#' 
#'
#' @seealso \code{\link{fit_CWCurve}}, \code{\link{get_RLum}}, \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Results}}, \code{\linkS4class{RLum.Data.Curve}}
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
#' @export
calc_FastRatio <- function(object, 
                           stimulation.power = 30.6, 
                           wavelength = 470,
                           sigmaF = 2.6E-17,
                           sigmaM = 4.28E-18,
                           Ch_L1 = 1,
                           x = 1,
                           x2 = 0.1,
                           dead.channels = c(0,0),
                           fitCW.sigma = FALSE,
                           fitCW.curve = FALSE,
                           plot = TRUE,
                           ...) {
  
  ## Input object handling -----------------------------------------------------
  if (inherits(object, "RLum.Analysis"))
    object <- get_RLum(object)
  
  if (inherits(object, "RLum.Results"))
    object <- get_RLum(object, "data")
  
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
    # h = speed of light, h = Planck's constant
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
                                   output.terminal = settings$output.terminal, 
                                   plot = plot))
      settings$fit <- fitCW.res
      
      if (fitCW.sigma) {
        if (!inherits(fitCW.res, "try-error")) {
          sigmaF <- get_RLum(fitCW.res, "output.table")$cs1
          sigmaM <- get_RLum(fitCW.res, "output.table")$cs2
          if (settings$verbose) {
            message("\n [calc_FitCWCurve()]\n")
            message("New value for sigmaF: ", format(sigmaF, digits = 3, nsmall = 2))
            message("New value for sigmaM: ", format(sigmaM, digits = 3, nsmall = 2))
          }
        } else {
          if (settings$verbose)
            message("Fitting failed! Please call 'fit_CWCurve()' manually before ",
                    "calculating the fast ratio.")
        }
      }
      
      if (fitCW.curve) {
        if (!inherits(fitCW.res, "try-error")) {
          nls <- get_RLum(fitCW.res, "fit")
          A[ ,2] <- predict(nls)
        }
      }

    }
    
    
    ## The equivalent time in s of L1, L2, L3
    # Use these values to look up the channel
    t_L1 <- 0
    t_L2 <- (log(x / 100)) / (-sigmaF * I0)
    t_L3_start <- (log(x / 100)) / (-sigmaM * I0)
    t_L3_end <- (log(x2 / 100)) / (-sigmaM * I0)
    
    ## Channel number(s) of L2 and L3
    Ch_L2 <- which.min(abs(A[,1] - t_L2))
    
    if (Ch_L2 <= 1) {
      msg <- sprintf("Calculated time/channel for L2 is too small (%.f, %.f). Returned NULL.", 
                     t_L2, Ch_L2)
      settings$info <- modifyList(settings$info, list(L2 = msg))
      warning(msg, call. = FALSE)
      return(NULL)
    }
    
    Ch_L3st<- which.min(abs(A[,1] - t_L3_start))
    Ch_L3end <- which.min(abs(A[,1] - t_L3_end))
    
    ## Counts in channels L1, L2, L3
    # L1 ----
    Cts_L1 <- A[Ch_L1, 2]
    
    # L2 ----
    if (Ch_L2 > nrow(A)) {
      msg <- sprintf(paste("The calculated channel for L2 (%i) is equal", 
                           "to or larger than the number of available channels (%i).",
                           "Returned NULL."), Ch_L2, nrow(A))
      settings$info <- modifyList(settings$info, list(L2 = msg))
      warning(msg, call. = FALSE)
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
      msg <- sprintf(paste("The calculated channels for L3 (%i, %i) are equal to or", 
                           "larger than the number of available channels (%i).",
                           "\nThe background has instead been estimated from the last",
                           "5 channels."), Ch_L3st, Ch_L3end, nrow(A))
      settings$info <- modifyList(settings$info, list(L3 = msg))
      warning(msg, call. = FALSE)
      Ch_L3st <- nrow(A) - 5
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
      warning(sprintf("L3 contains more counts (%.f) than L2 (%.f).",
                      Cts_L3, Cts_L2), call. = FALSE)
    
    ## Fast Ratio
    FR <- (Cts_L1 - Cts_L3) / (Cts_L2 - Cts_L3)
    if (length(FR) != 1)
      FR <- NA
    
    ## Fast Ratio - Error calculation
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
      
    } else {
      FR_se <- NA
      FR_rse <- NA
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
    ## Plotting ----------------------------------------------------------------
    if (plot) 
      try(plot_RLum.Results(fast.ratio, ...))

    # return
    return(fast.ratio)
  }) # End of lapply
  
  if (length(fast.ratios) == 1)
    fast.ratios <- fast.ratios[[1]]
  
  invisible(fast.ratios)
}
