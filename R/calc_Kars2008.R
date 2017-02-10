#' Apply the Kars et al. (2008) model
#'
#' A function to calculate the expected sample specific fraction of saturation
#' following Kars et al. (2008) and Huntley (2006).
#'
#' This function applies the approach described in Kars et al. (2008),
#' developed from the model of Huntley (2006) to calculate the expected sample
#' specific fraction of saturation of a feldspar and also to calculate fading
#' corrected age using this model. \eqn{\rho}' (\code{rhop}), the density of recombination
#' centres, is a crucial parameter of this model and must be determined
#' separately from a fading measurement. The function
#' \code{\link[Luminescence]{analyse_FadingMeasurement}}
#' can be used to calculate the sample specific \eqn{\rho}' value.
#'
#' Firstly the unfaded D0 value is determined through applying equation 5 of
#' Kars et al. (2008) to the measured LxTx data as a function of irradiation
#' time, and fitting the data with a single saturating exponential of the form:
#'
#' \deqn{LxTx(t*) = A x \phi(t*) x (1 - exp(-(t* / D0)))}
#'
#' where
#'
#' \deqn{\phi(t*) = exp(-\rho' x ln(1.8 x s_tilde x t*)^3)}
#'
#' after King et al. (2016) where \code{A} is a pre-exponential factor,
#' \code{t*} (s) is the irradiation time, starting at the mid-point of
#' irradiation (Auclair et al. 2003) and \code{s_tilde} (3x10^15 s^-1) is the athermal
#' frequency factor after Huntley (2006). \cr
#'
#' Using fit parameters \code{A} and \code{D0}, the function then computes a natural dose
#' response curve using the environmental dose rate, \code{D_dot} (Gy/s) and equations
#' [1] and [2]. Computed LxTx values are then fitted using the
#' \code{\link[Luminescence]{plot_GrowthCurve}} function and the laboratory measured LnTn can then
#' be interpolated onto this curve to determine the fading corrected
#' De value, from which the fading corrected age is calculated. \cr
#'
#' The \code{calc_Kars2008} function also calculates the level of saturation (n/N)
#' and the field saturation (i.e. athermal steady state, (n/N)_SS) value for
#' the sample under investigation using the sample specific \eqn{\rho}',
#' unfaded \code{D0} and \code{D_dot} values, following the approach of Kars et al. (2008). \cr
#'
#' Uncertainties are reported at 1 sigma and are assumed to be normally
#' distributed and are estimated using monte-carlo resamples (\code{n.MC = 1000})
#' of \eqn{\rho}' and LxTx during dose response curve fitting, and of \eqn{\rho}'
#' in the derivation of (n/N) and (n/N)_SS.
#'
#'
#'
#' @param data \code{\link{data.frame}} (\bold{required}):
#' A three column data frame with numeric values on a) dose (s), b) LxTx and and
#' c) LxTx error. If a two column data frame is provided it is automatically
#' assumed that errors on LxTx are missing. A third column will be attached
#' with an arbitrary 5 \% error on the provided LxTx values.\cr
#' Can also be a wide table, i.e. a \code{\link{data.frame}} with a number of colums divisible by 3
#' and where each triplet has the aforementioned column structure.
#'
#' @param rhop \code{\link{numeric}} (\bold{required}):
#' The density of recombination centres (\eqn{\rho}') and its error (see Huntley 2006),
#' given as numeric vector of length two. Note that \eqn{\rho}' must \bold{not} be
#' provided as the common logarithm. Example: \code{rhop = c(2.92e-06, 4.93e-07)}.
#'
#' @param ddot \code{\link{numeric}} (\bold{required}):
#' Environmental dose rate and its error, given as a numeric vector of length two.
#' Expected unit: Gy/ka. Example: \code{ddot = c(3.7, 0.4)}.
#'
#' @param readerDdot \code{\linkS4class{RLum.Analysis}} (\bold{required}):
#' Dose rate of the irradiation source of the OSL reader and its error,
#' given as a numeric vector of length two.
#' Expected unit: Gy/s. Example: \code{readerDdot = c(0.08, 0.01)}.
#'
#' @param normalise \code{\link{logical}} (with default):
#' If \code{TRUE} (the default) all measured and computed LxTx values are
#' normalised by the pre-exponential factor A (see details).
#'
#' @param summary \code{\link{logical}} (with default):
#' If \code{TRUE} (the default) various parameters provided by the user
#' and calculated by the model are added as text on the right-hand side of the
#' plot.
#'
#' @param plot \code{\link{logical}} (with default): enables/disables plot output.
#'
#' @param ... further arguments passed to \code{\link{plot}} and
#' \code{\link[Luminescence]{plot_GrowthCurve}}.
#'
#' @return An \code{\linkS4class{RLum.Results}} object is returned:
#'
#' Slot: \bold{@data}\cr
#'
#' \tabular{lll}{
#' \bold{OBJECT} \tab \bold{TYPE} \tab \bold{COMMENT}\cr
#' \code{results} \tab \code{data.frame} \tab results of the of Kars et al. 2008 model \cr
#' \code{data} \tab \code{data.frame} \tab original input data \cr
#' \code{Ln} \tab \code{numeric} \tab Ln and its error \cr
#' \code{LxTx_tables} \tab \code{list} \tab A \code{list} of \code{data.frames}
#' containing data on dose, LxTx and LxTx error for each of the dose response curves.
#' Note that these \bold{do not} contain the natural Ln signal, which is provided separately. \cr
#' \code{fits} \tab \code{list} \tab A \code{list} of \code{nls}
#'  objects produced by \code{\link[minpack.lm]{nlsLM}} when fitting the dose response curves \cr
#' }
#'
#' Slot: \bold{@info}\cr
#'
#' \tabular{lll}{
#' \bold{OBJECT} \tab \bold{TYPE} \tab \bold{COMMENT} \cr
#' \code{call} \tab \code{call} \tab the original function call \cr
#' \code{args} \tab \code{list} \tab arguments of the original function call \cr
#'
#' }
#'
#' @section Function version: 0.1.0
#'
#' @author Georgina King, University of Cologne (Germany), \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @note \bold{This function has BETA status and should not be used for publication work!}
#'
#' @keywords datagen
#'
#' @references
#'
#' Kars, R.H., Wallinga, J., Cohen, K.M., 2008. A new approach towards anomalous fading correction for feldspar
#' IRSL dating-tests on samples in field saturation. Radiation Measurements 43, 786-790. doi:10.1016/j.radmeas.2008.01.021
#'
#' Huntley, D.J., 2006. An explanation of the power-law decay of luminescence.
#' Journal of Physics: Condensed Matter 18, 1359-1365. doi:10.1088/0953-8984/18/4/020
#'
#' King, G.E., Herman, F., Lambert, R., Valla, P.G., Guralnik, B., 2016.
#' Multi-OSL-thermochronometry of feldspar. Quaternary Geochronology 33, 76-87. doi:10.1016/j.quageo.2016.01.004
#'
#'
#' \bold{Further reading}
#'
#' Morthekai, P., Jain, M., Cunha, P.P., Azevedo, J.M., Singhvi, A.K., 2011. An attempt to correct
#' for the fading in million year old basaltic rocks. Geochronometria 38(3), 223-230.
#'
#' @examples
#'
#' ## Load example data (sample UNIL/NB123, see ?ExampleData.Fading)
#' data("ExampleData.Fading", envir = environment())
#'
#' ## (1) Set all relevant parameters
#' # a. fading measurement data (IR50)
#' fading_data <- ExampleData.Fading$fading.data$IR50
#'
#' # b. Dose response curve data
#' data <- ExampleData.Fading$equivalentDose.data$IR50
#'
#' ## (2) Define required function parameters
#' ddot <- c(7.00, 0.004)
#' readerDdot <- c(0.134, 0.0067)
#'
#' # Analyse fading measurement and get an estimate of rho'.
#' # Note that the RLum.Results object can be directly used for further processing.
#' # The number of MC runs is reduced for this example
#' rhop <- analyse_FadingMeasurement(fading_data, plot = TRUE, verbose = FALSE, n.MC = 10)
#'
#' ## (3) Apply the Kars et al. (2008) model to the data
#' kars <- calc_Kars2008(data = data,
#'                       rhop = rhop,
#'                       ddot = ddot,
#'                       readerDdot = readerDdot,
#'                       n.MC = 50
#'                       )
#' @export
calc_Kars2008 <- function(data,
                          rhop,
                          ddot,
                          readerDdot,
                          normalise = TRUE,
                          summary = TRUE,
                          plot = TRUE,
                          ...) {

  ## Validate Input ------------------------------------------------------------

  ## Check 'data'
  # must be a data frame
  if (is.data.frame(data)) {

    if (ncol(data) == 2) {
      warning("[calc_Kars2008] 'data' only had two columns. We assumed that",
              " the errors on LxTx were missing and automatically added a",
              " 5 % error.\n Please provide a data frame with three columns",
              " if you wish to use actually measured LxTx errors.", call. = FALSE)
      data[ ,3] <- data[ ,2] * 0.05
    }

    # check number of columns
    if (ncol(data) %% 3 != 0) {
      stop("[calc_Kars2008] the number of columns in 'data' must be a multiple of 3.", 
           call. = FALSE)
    } else {
      # extract all LxTx values
      data_tmp <- do.call(rbind, 
                        lapply(seq(1, ncol(data), 3), function(col) {
                          setNames(data[2:nrow(data), col:c(col+2)], c("dose", "LxTx", "LxTxError")) 
                        })
      )
      # extract the LnTn values (assumed to be the first row) and calculate the column mean
      LnTn_tmp <- do.call(rbind, 
                          lapply(seq(1, ncol(data), 3), function(col) {
                            setNames(data[1, col:c(col+2)], c("dose", "LxTx", "LxTxError")) 
                          })
      )
      
      # check whether the standard deviation of LnTn estimates or the largest
      # individual error is highest, and take the larger one
      LnTn_error_tmp <- max(c(sd(LnTn_tmp[ ,2]), mean(LnTn_tmp[ ,3])), na.rm = TRUE)
      LnTn_tmp <- colMeans(LnTn_tmp)

      # re-bind the data frame
      data <- rbind(LnTn_tmp, data_tmp)
      data[1, 3] <- LnTn_error_tmp
      data <- data[complete.cases(data), ]
    }
    
    
  } else {
    stop("\n[calc_Kars2008] 'data' must be a data frame.",
         call. = FALSE)
  }

  ## Check 'rhop'
  # check if numeric
  if (is.numeric(rhop)) {

    ### TODO: can be of length 2 if error
    if (length(rhop) != 2)
      stop("\n[calc_Kars2008] 'rhop' must be a vector of length two.",
           call. = FALSE)

    # alternatively, and RLum.Results object produced by analyse_FadingMeasurement()
    # can be provided
  } else if (inherits(rhop, "RLum.Results")) {

    if (rhop@originator == "analyse_FadingMeasurement")
      rhop <- c(rhop@data$rho_prime$MEAN,
                rhop@data$rho_prime$SD)
    else
      stop("\n[calc_Kars2008] Only an 'RLum.Results' object produced by",
           " 'analyse_FadingMeasurement()' is allowed as input for 'rhop'.",
           call. = FALSE)
  }

  ## Check ddot & readerDdot
  # check if numeric
  if (any(sapply(list(ddot, readerDdot), is.numeric) == FALSE))
    stop("\n[calc_Kars2008] 'ddot' and 'readerDdot' must be numeric values.",
         call. = FALSE)
  # check if length == 2
  if (any(sapply(list(ddot, readerDdot), function(x) length(x) == 2) == FALSE))
    stop("\n[calc_Kars2008] 'ddot' and 'readerDdot' must be of length 2.",
         call. = FALSE)


  ## Settings ------------------------------------------------------------------
  settings <- list(verbose = TRUE,
                   n.MC = 1000)
  settings <- modifyList(settings, list(...))

  ## Define Constants ----------------------------------------------------------
  kb <- 8.617343 * 1e-5
  alpha <- 1
  Hs <- 3e15 # s value after Huntley (2006)
  Ma <- 1e6 * 365.25 * 24 * 3600 #in seconds
  ka <- Ma / 1000 #in seconds


  ## Define Functions ----------------------------------------------------------
  # fit data using using Eq 5. from Kars et al (2008) employing
  # theta after King et al. (2016)
  theta <- function(t, r) {
    res <- exp(-r * log(1.8 * Hs * (0.5 * t))^3)
    res[!is.finite(res)] <- 0
    return(res)
  }

  ## Preprocessing -------------------------------------------------------------
  readerDdot.error <- readerDdot[2]
  readerDdot <- readerDdot[1]
  ddot.error <- ddot[2]
  ddot <- ddot[1]

  colnames(data) <- c("dose", "LxTx", "LxTx.Error")
  dosetime <- data[["dose"]][2:nrow(data)]
  LxTx.measured <- data[["LxTx"]][2:nrow(data)]
  LxTx.measured.error <- data[["LxTx.Error"]][2:nrow(data)]

  #Keep LnTn separate for derivation of measured fraction of saturation
  Ln <- data[["LxTx"]][1]
  Ln.error <- data[["LxTx.Error"]][1]

  ## (1) MEASURED ----------------------------------------------------
  if (settings$verbose) cat("\n")

  data.tmp <- data
  data.tmp[ ,1] <- data.tmp[ ,1] * readerDdot
  
  GC.settings <- list(sample = data.tmp,
                      mode = "regenerative",
                      fit.method = "EXP",
                      output.plot = plot,
                      main = "Measured dose response curve",
                      xlab = "Dose (Gy)",
                      verbose = FALSE)
  
  GC.settings <- modifyList(GC.settings, list(...))
  GC.settings$verbose <- FALSE

  GC.measured <- try(do.call(plot_GrowthCurve, GC.settings))
  
  if (inherits(GC.measured, "try-error"))
    stop("\n[calc_Kars2008()] Unable to fit growth curve to data", call. = FALSE)

  # extract results and calculate age
  GC.results <- get_RLum(GC.measured)
  fit_measured <- GC.measured@data$Fit
  De.measured <- GC.results$De
  De.measured.error <- GC.results$De.Error
  D0.measured <- GC.results$D01
  D0.measured.error <- GC.results$D01.ERROR
  Age.measured <- De.measured/ ddot
  Age.measured.error <- Age.measured * sqrt( (De.measured.error / De.measured)^2 +
                                               (readerDdot.error / readerDdot)^2 +
                                               (ddot.error / ddot)^2)



  ## (2) SIMULATED -----------------------------------------------------

  # create MC samples
  rhop_MC <- rnorm(n = settings$n.MC, mean = rhop[1], sd = rhop[2])

  #
  fitcoef <- do.call(rbind, sapply(rhop_MC, function(rhop_i) {
    fit_sim <- try(minpack.lm::nlsLM(LxTx.measured ~ a * theta(dosetime, rhop_i) * (1 - exp(-dosetime / D0)),
                     start = list(a = max(LxTx.measured), D0 = D0.measured / readerDdot)))
    if (!inherits(fit_sim, "try-error"))
      coefs <- coef(fit_sim)
    else
      coefs <- c(NA, NA)
    return(coefs)
  }, simplify = FALSE))

  # final fit for export
  fit_simulated <- minpack.lm::nlsLM(LxTx.measured ~ a * theta(dosetime, rhop[1]) * (1 - exp(-dosetime / D0)),
                       start = list(a = max(LxTx.measured), D0 = D0.measured / readerDdot))

  # scaling factor
  A <- mean(fitcoef[, 1], na.rm = TRUE)
  A.error <- sd(fitcoef[ ,1], na.rm = TRUE)

  # derive unfaded D0
  D0.sim <- mean(fitcoef[ ,2], na.rm = TRUE)
  D0.sim.error <- sd(fitcoef[ ,2], na.rm = TRUE)
  D0.sim.Gy <- D0.sim * readerDdot
  D0.sim.Gy.error <- D0.sim.Gy * sqrt( (D0.sim.error / D0.sim)^2 + (readerDdot.error / readerDdot)^2)


  # calculate measured fraction of saturation
  nN <- Ln / A
  nN.error <- sqrt( (Ln.error / Ln)^2 + (A.error / A)^2)

  # compute a natural dose response curve following the assumptions of
  # Morthekai et al. 2011, Geochronometria
  natdosetime <- seq(0, 1e14, length.out = settings$n.MC)
  natdosetimeGray <- natdosetime * ddot / ka

  # calculate D0 dose in seconds
  computedD0 <- (fitcoef[ ,2] * readerDdot) / (ddot / ka)

  # compute natural dose response curve
  LxTx.sim <- A * theta(natdosetime, rhop[1]) * (1 - exp(-natdosetime / mean(computedD0, na.rm = TRUE)))

  # calculate Age
  if (Ln < max(LxTx.sim)) {

    positive <- which(diff(LxTx.sim) > 0)

    data.unfaded <- data.frame(dose = c(0, natdosetime[positive] * ddot / ka),
                               LxTx = c(Ln, LxTx.sim[positive]),
                               LxTx.error = c(Ln.error, LxTx.sim[positive] * A.error/A))

    data.unfaded$LxTx.error[2] <- 0.0001

    GC.settings <- list(sample = data.unfaded,
                        mode = "regenerative",
                        fit.method = "EXP",
                        output.plot = TRUE,
                        verbose = FALSE,
                        main = "Simulated dose response curve",
                        xlab = "Dose (Gy)")
    
    GC.settings <- modifyList(GC.settings, list(...))
    GC.settings$verbose <- FALSE
    
    suppressWarnings(
      GC.unfaded <- try(do.call(plot_GrowthCurve, GC.settings))
    )

    if (!inherits(GC.unfaded, "try-error")) {
      GC.unfaded.results <- get_RLum(GC.unfaded)
      De.sim <- GC.unfaded.results$De
      De.error.sim <- GC.unfaded.results$De.Error
      Age.sim <- De.sim / ddot
      Age.sim.error <- Age.sim * sqrt( ( De.error.sim/ De.sim)^2 +
                                         (readerDdot.error / readerDdot)^2 +
                                         (ddot.error / ddot)^2)


    } else {
      De.sim <- De.error.sim <- Age.sim <- Age.sim.error <- NA
    }

  } else {
    De.sim <- De.error.sim <- Age.sim <- Age.sim.error <- NA
  }

  if (Ln > max(LxTx.sim) * 1.1)
    warning("[calc_Kars2008] Ln is >10 % larger than the maximum computed LxTx value.",
            " The De and age should be regarded as infinite estimates.", call. = FALSE)

  # Estimate nN_(steady state) by Monte Carlo Simulation
  ddot_MC <- rnorm(n = settings$n.MC, mean = ddot, sd = ddot.error)
  UFD0_MC <- rnorm(n = settings$n.MC, mean = D0.sim.Gy, sd = D0.sim.Gy.error)

  nN_SS_MC <- mapply(function(rhop_i, ddot_i, UFD0_i) {
    rprime <- seq(0.01, 5, length.out = settings$n.MC)
    rho <- 3 * alpha^3 * rhop_i / (4 * pi)
    r <- rprime / (4 * pi * rho / 3)^(1 / 3)
    pr <- 3 * rprime^2 * exp(-rprime^3)
    tau <- ((1 / Hs) * exp(1)^(alpha * r)) / ka
    Ls <- 1 / (1 + UFD0_i / (ddot_i * tau))
    Lstrap <- (pr * Ls) / sum(pr)

    # field saturation
    nN_SS_i <- sum(Lstrap)
    return(nN_SS_i)

  }, rhop_MC, ddot_MC, UFD0_MC, SIMPLIFY = TRUE)

  nN_SS <- mean(nN_SS_MC, na.rm = TRUE)
  nN_SS.error <- sd(nN_SS_MC, na.rm = TRUE)

  ## (3) UNFADED ---------------------------------------------------------------
  LxTx.unfaded <- LxTx.measured / theta(dosetime, rhop[1])
  LxTx.unfaded[is.nan((LxTx.unfaded))] <- 0
  LxTx.unfaded[is.infinite(LxTx.unfaded)] <- 0
  dosetimeGray <- dosetime * readerDdot
  fit_unfaded <- minpack.lm::nlsLM(LxTx.unfaded ~ a * (1 - exp(-dosetimeGray / D0)),
                     start = list(a = max(LxTx.unfaded), D0 = D0.measured / readerDdot))
  D0.unfaded <- coef(fit_unfaded)[["D0"]]
  D0.error.unfaded <- summary(fit_unfaded)$coefficients["D0", "Std. Error"]

  ## Create LxTx tables --------------------------------------------------------

  # normalise by A (saturation point of the un-faded curve)
  if (normalise) {
    LxTx.measured.relErr <- (LxTx.measured.error / LxTx.measured)
    LxTx.measured <- LxTx.measured / A
    LxTx.measured.error <- LxTx.measured * LxTx.measured.relErr

    LxTx.sim <- LxTx.sim / A
    LxTx.unfaded <- LxTx.unfaded / A

    Ln.relErr <- Ln.error / Ln
    Ln <- Ln / A
    Ln.error <- Ln * Ln.relErr
  }

  # combine all computed LxTx values
  LxTx_measured <- data.frame(
    dose = dosetimeGray,
    LxTx = LxTx.measured,
    LxTx.Error = LxTx.measured.error)

  LxTx_simulated <- data.frame(
    dose = natdosetimeGray,
    LxTx = LxTx.sim,
    LxTx.Error = LxTx.sim * A.error / A)

  LxTx_unfaded <- data.frame(
    dose = dosetimeGray,
    LxTx = LxTx.unfaded,
    LxTx.Error = LxTx.unfaded * A.error / A)


  ## Plot settings -------------------------------------------------------------
  plot.settings <- list(main = "Dose response curves",
                        xlab = "Dose (Gy)",
                        ylab = ifelse(normalise, "normalised LxTx (a.u.)", "LxTx (a.u.)")
  )
  plot.settings <- modifyList(plot.settings, list(...))

  ## Plotting ------------------------------------------------------------------
  if (plot) {

    # set plot parameters
    par.old.full <- par(no.readonly = TRUE)

    # set graphical parameters
    par(mar = c(5, 4, 4, 4),
        cex = 0.8)
    if (summary)
      par(oma = c(0, 3, 0, 9))
    else
      par(oma = c(0, 9, 0, 9))
    
    # Find a good estimate of the x-axis limits
    xlim <- range(pretty(dosetimeGray))
    if (De.sim > xlim[2])
      xlim <- range(pretty(c(min(dosetimeGray), De.sim)))

    # Create figure after Kars et al. (2008) contrasting the dose response curves
    plot(dosetimeGray, LxTx_measured$LxTx,
         main = plot.settings$main,
         xlab = plot.settings$xlab,
         ylab = plot.settings$ylab,
         pch = 16,
         ylim = c(0, max(do.call(rbind, list(LxTx_measured, LxTx_unfaded))[["LxTx"]])),
         xlim = xlim
    )

    # LxTx error bars
    segments(x0 = dosetimeGray,
             y0 = LxTx_measured$LxTx + LxTx_measured$LxTx.Error,
             x1 = dosetimeGray,
             y1 = LxTx_measured$LxTx - LxTx_measured$LxTx.Error,
             col = "black")

    # re-calculate the measured dose response curve in Gray
    xRange <- range(pretty(dosetimeGray))
    xNew <- seq(xRange[1], xRange[2], length.out = 200)
    yNew <- predict(GC.measured@data$Fit, list(x = xNew))
    if (normalise)
      yNew <- yNew / A

    # add line
    lines(xNew, yNew, col  = "black")

    # add error polygon
    polygon(x = c(natdosetimeGray, rev(natdosetimeGray)),
            y = c(LxTx_simulated$LxTx + LxTx_simulated$LxTx.Error,
                  rev(LxTx_simulated$LxTx - LxTx_simulated$LxTx.Error)),
            col = adjustcolor("grey", alpha.f = 0.5), border = NA)

    # computed LxTx values
    points(natdosetimeGray, LxTx_simulated$LxTx,
           type = "l",
           lty = 2)


    # Ln and DE as points
    points(x = c(0, De.measured),
           y = c(Ln, Ln),
           col = "red", pch = c(1, 16))

    # Ln error bar
    segments(x0 = 0, y0 = Ln - Ln.error,
             x1 = 0, y1 = Ln + Ln.error,
             col = "red")

    # Ln as a horizontal line
    lines(x = c(0, max(c(De.measured, De.sim), na.rm = TRUE)),
          y = c(Ln, Ln),
          col = "black", lty = 3)

    # vertical line of measured DE
    lines(x = c(De.measured, De.measured),
          y = c(0, Ln),
          col = "black", lty = 3)

    # add legends
    legend("bottomright",
           legend = c("Unfaded DRC",
                      "Measured DRC",
                      "Simulated natural DRC"),
           lty = c(5, 1, 2),
           bty = "n")

    # add vertical line of simulated De
    if (!is.na(De.sim)) {
      lines(x = c(De.sim, De.sim),
            y = c(0, Ln),
            col = "black", lty = 3)
      points(x = De.sim,
             y = Ln,
             col = "red" , pch = 16)
    }

    # add unfaded DRC
    xRange <- range(pretty(dosetimeGray))
    xNew <- seq(xRange[1], xRange[2], length.out = 200)
    yNew <- predict(fit_unfaded, list(dosetimeGray = xNew))
    if (normalise)
      yNew <- yNew / A

    lines(xNew, yNew, col  = "black", lty = 5)

    points(x = dosetimeGray,
           y = LxTx_unfaded$LxTx,
           col = "black")

    # LxTx error bars
    segments(x0 = dosetimeGray,
             y0 = LxTx_unfaded$LxTx + LxTx_unfaded$LxTx.Error,
             x1 = dosetimeGray,
             y1 = LxTx_unfaded$LxTx - LxTx_unfaded$LxTx.Error,
             col = "black")

    # add text
    if (summary) {

      # define labels as expressions
      labels.text <- list(
        bquote(dot(D) == .(round(ddot, 2)) %+-% .(round(ddot.error, 2)) ~ frac(Gy, ka)),
        bquote(dot(D)["Reader"] == .(round(readerDdot, 3)) %+-% .(round(readerDdot.error, 3)) ~ frac(Gy, s)),
        bquote(log[10]~(rho~"'") == .(round(log10(rhop[1]), 2)) %+-% .(round(rhop[2] / (rhop[1] * log(10, base = exp(1))), 2)) ),
        bquote(bgroup("(", frac(n, N), ")") == .(round(nN, 2)) %+-% .(round(nN.error, 2)) ),
        bquote(bgroup("(", frac(n, N), ")")[SS] == .(round(nN_SS, 2)) %+-% .(round(nN_SS.error, 2)) ),
        bquote(D["E,sim"] == .(round(De.sim, 2)) %+-% .(round(De.error.sim, 2)) ~ Gy),
        bquote(D["0,sim"] == .(round(D0.sim.Gy, 2)) %+-% .(round(D0.sim.Gy.error, 2)) ~ Gy),
        bquote(Age["sim"] == .(round(Age.sim, 2)) %+-% .(round(Age.sim.error, 2)) ~ ka)
      )

      # each of the labels is positioned at 1/10 of the availalbe y-axis space
      ypos <- seq(range(axTicks(2))[2], range(axTicks(2))[1], length.out = 10)[1:length(labels.text)]

      # allow overprinting
      par(xpd = NA)

      # add labels iteratively
      mapply(function(label, pos) {
        text(x = max(axTicks(1)) * 1.15,
             y = pos,
             labels = label,
             pos = 4)
      }, labels.text, ypos)
    }

    # recover plot parameters
    on.exit(par(par.old.full))

  }

  ## Results -------------------------------------------------------------------
  results <- set_RLum(
    class = "RLum.Results",
    data = list(
      results = data.frame("nN" = nN,
                           "nN.error" = nN.error,
                           "nN_SS" = nN_SS,
                           "nN_SS.error" = nN_SS.error,
                           "Meas_De" = De.measured,
                           "Meas_De.error" = De.measured.error,
                           "Meas_D0" =  D0.measured,
                           "Meas_D0.error" = D0.measured.error,
                           "Meas_Age" = Age.measured,
                           "Meas_Age.error" = Age.measured.error,
                           "Sim_De" = De.sim,
                           "Sim_De.error" = De.error.sim,
                           "Sim_D0" = D0.sim.Gy,
                           "Sim_D0.error" = D0.sim.Gy.error,
                           "Sim_Age" = Age.sim,
                           "Sim_Age.error" = Age.sim.error,
                           "Unfaded_D0" = D0.unfaded,
                           "Unfaded_D0.error" = D0.error.unfaded,
                           row.names = NULL),
      data = data,
      Ln = c(Ln, Ln.error),
      LxTx_tables = list(
        simulated = LxTx_simulated,
        measured = LxTx_measured,
        unfaded = LxTx_unfaded),
      fits = list(
        simulated = fit_simulated,
        measured = fit_measured,
        unfaded = fit_unfaded
      )
    ),
    info = list(call = sys.call(),
                args = as.list(sys.call())[-1])
)

  ## Console output ------------------------------------------------------------
  if (settings$verbose) {
    cat("\n[calc_Kars2008()]\n")
    cat("\n -------------------------------")
    cat("\n (n/N) [-]:\t",
        round(results@data$results$nN, 2), "\u00b1",
        round(results@data$results$nN.error, 2))
    cat("\n (n/N)_SS [-]:\t",
        round(results@data$results$nN_SS, 2),"\u00b1",
        round(results@data$results$nN_SS.error, 2))
    cat("\n\n ---------- Measured -----------")
    cat("\n DE [Gy]:\t",
        round(results@data$results$Meas_De, 2), "\u00b1",
        round(results@data$results$Meas_De.error, 2))
    cat("\n D0 [Gy]:\t",
        round(results@data$results$Meas_D0, 2), "\u00b1",
        round(results@data$results$Meas_D0.error, 2))
    cat("\n Age [ka]:\t",
        round(results@data$results$Meas_Age, 2), "\u00b1",
        round(results@data$results$Meas_Age.error, 2))
    cat("\n\n ---------- Simulated ----------")
    cat("\n DE [Gy]:\t",
        round(results@data$results$Sim_De, 2), "\u00b1",
        round(results@data$results$Sim_De.error, 2))
    cat("\n D0 [Gy]:\t",
        round(results@data$results$Sim_D0, 2), "\u00b1",
        round(results@data$results$Sim_D0.error, 2))
    cat("\n Age [ka]:\t",
        round(results@data$results$Sim_Age, 2), "\u00b1",
        round(results@data$results$Sim_Age.error, 2))
    cat("\n\n ---------- Un-faded -----------")
    cat("\n D0 [Gy]:\t",
        round(results@data$results$Unfaded_D0, 2), "\u00b1",
        round(results@data$results$Unfaded_D0.error, 2))
    cat("\n -------------------------------\n\n")

  }

  ## Return value --------------------------------------------------------------
  return(results)
}
