#' @title Apply the Huntley (2006) model
#'
#' @description
#' A function to calculate the expected sample specific fraction of saturation
#' based on the model of Huntley (2006) using the approach as implemented
#' in Kars et al. (2008) or Guralnik et al. (2015).
#'
#' @details
#'
#' This function applies the approach described in Kars et al. (2008) or Guralnik et al. (2015),
#' which are both developed from the model of Huntley (2006) to calculate the expected sample
#' specific fraction of saturation of a feldspar and also to calculate fading
#' corrected age using this model. \eqn{\rho}' (`rhop`), the density of recombination
#' centres, is a crucial parameter of this model and must be determined
#' separately from a fading measurement. The function [analyse_FadingMeasurement]
#' can be used to calculate the sample specific \eqn{\rho}' value.
#'
#' **Kars et al. (2008) - Single saturating exponential**
#'
#' To apply the approach after Kars et al. (2008) use `fit.method = "EXP"`.
#'
#' Firstly, the unfaded \eqn{D_0} value is determined through applying equation 5 of
#' Kars et al. (2008) to the measured \eqn{\frac{L_x}{T_x}} data as a function of irradiation
#' time, and fitting the data with a single saturating exponential of the form:
#'
#' \deqn{\frac{L_x}{T_x}(t^*) = A  \phi(t^*) \{1 - exp(-\frac{t^*}{D_0}))\}}
#'
#' where
#'
#' \deqn{\phi(t^*) = exp(-\rho' ln(1.8  \tilde{s} t^*)^3)}
#'
#' after King et al. (2016) where \eqn{A} is a pre-exponential factor,
#' \eqn{t^*} (s) is the irradiation time, starting at the mid-point of
#' irradiation (Auclair et al. 2003) and \eqn{\tilde{s}} (\eqn{3\times10^{15}} s\eqn{^{-1}}) is the athermal frequency factor after Huntley (2006). \cr
#'
#' Using fit parameters \eqn{A} and \eqn{D_0}, the function then computes a natural dose
#' response curve using the environmental dose rate, \eqn{\dot{D}} (Gy/s) and equations
#' `[1]` and `[2]`. Computed \eqn{\frac{L_x}{T_x}} values are then fitted using the
#' [plot_GrowthCurve] function and the laboratory measured LnTn can then
#' be interpolated onto this curve to determine the fading corrected
#' \eqn{D_e} value, from which the fading corrected age is calculated.
#'
#' **Guralnik et al. (2015) - General-order kinetics**
#'
#' To apply the approach after Guralnik et al. (2015) use `fit.method = "GOK"`.
#'
#' The approach of Guralnik et al. (2015) is very similar to that of
#' Kars et al. (2008), but instead of using a single saturating exponential
#' the model fits a general-order kinetics function of the form:
#'
#' \deqn{\frac{L_x}{T_x}(t^*) = A \phi (t^*)(1 - (1 + (\frac{1}{D_0}) t^* c)^{-1/c})}
#'
#' where \eqn{A}, \eqn{\phi}, \eqn{t^*} and \eqn{D_0} are the same as above and \eqn{c} is a
#' dimensionless kinetic order modifier (cf. equation 10 in
#' Guralnik et al., 2015).
#'
#' **Level of saturation**
#'
#' The [calc_Huntley2006] function also calculates the level of saturation (\eqn{\frac{n}{N}})
#' and the field saturation (i.e. athermal steady state, (n/N)_SS) value for
#' the sample under investigation using the sample specific \eqn{\rho}',
#' unfaded \eqn{D_0} and \eqn{\dot{D}} values, following the approach of Kars et al. (2008).
#'
#' **Uncertainties**
#'
#' Uncertainties are reported at \eqn{1\sigma} and are assumed to be normally
#' distributed and are estimated using Monte-Carlo re-sampling (`n.MC = 1000`)
#' of \eqn{\rho}' and \eqn{\frac{L_x}{T_x}} during dose response curve fitting, and of \eqn{\rho}'
#' in the derivation of (\eqn{n/N}) and (n/N)_SS.
#'
#' **Age calculated from 2D0 of the simulated natural DRC**
#'
#' In addition to the age calculated from the equivalent dose derived from
#' \eqn{\frac{L_n}{T_n}} projected on the simulated natural dose response curve (DRC), this function
#' also calculates an age from twice the characteristic saturation dose (`D0`)
#' of the simulated natural DRC. This can be a useful information for
#' (over)saturated samples (i.e., no intersect of \eqn{\frac{L_n}{T_n}} on the natural DRC)
#' to obtain at least a "minimum age" estimate of the sample. In the console
#' output this value is denoted by *"Age @2D0 (ka):"*.
#'
#' @param data [data.frame] (**required**):
#' A `data.frame` with one of the following structures:
#' - A **three column** data frame with numeric values on a) dose (s), b) `LxTx` and
#' c) `LxTx` error.
#' - If a **two column** data frame is provided it is automatically
#' assumed that errors on `LxTx` are missing. A third column will be attached
#' with an arbitrary 5 \% error on the provided `LxTx` values.
#' - Can also be a **wide table**, i.e. a [data.frame] with a number of columns divisible by 3
#' and where each triplet has the aforementioned column structure.
#'
#' ```
#'                         (optional)
#'      | dose (s)| LxTx | LxTx error |
#'      |  [ ,1]  | [ ,2]|    [ ,3]   |
#'      |---------|------|------------|
#' [1, ]|  0      | LnTn | LnTn error | (optional, see arg 'LnTn')
#' [2, ]|  R1     | L1T1 | L1T1 error |
#'  ... |    ...  |  ... |     ...    |
#' [x, ]|  Rx     | LxTx | LxTx error |
#'
#' ```
#' **NOTE:** The function assumes the first row of the function to be the
#' `Ln/Tn`-value. If you want to provide more than one `Ln/Tn`-value consider
#' using the argument `LnTn`.
#'
#' @param LnTn [data.frame] (**optional**):
#' This argument should **only** be used to provide more than one `Ln/Tn`-value.
#' It assumes a two column data frame with the following structure:
#'
#' ```
#'      |  LnTn  |  LnTn error  |
#'      |  [ ,1] |      [ ,2]   |
#'      |--------|--------------|
#' [1, ]| LnTn_1 | LnTn_1 error |
#' [2, ]| LnTn_2 | LnTn_2 error |
#'  ... |   ...  |      ...     |
#' [x, ]| LnTn_x | LnTn_x error |
#' ```
#'
#' The function will calculate a **mean** `Ln/Tn`-value and uses either the
#' standard deviation or the highest individual error, whichever is larger. If
#' another mean value (e.g. a weighted mean or median) or error is preferred,
#' this value must be calculated beforehand and used in the first row in the
#' data frame for argument `data`.
#'
#' **NOTE:** If you provide `LnTn`-values with this argument the data frame
#' for the `data`-argument **must not** contain any `LnTn`-values!
#'
#' @param rhop [numeric] (**required**):
#' The density of recombination centres (\eqn{\rho}') and its error (see Huntley 2006),
#' given as numeric vector of length two. Note that \eqn{\rho}' must **not** be
#' provided as the common logarithm. Example: `rhop = c(2.92e-06, 4.93e-07)`.
#'
#' @param ddot [numeric] (**required**):
#' Environmental dose rate and its error, given as a numeric vector of length two.
#' Expected unit: Gy/ka. Example: `ddot = c(3.7, 0.4)`.
#'
#' @param readerDdot [numeric] (**required**):
#' Dose rate of the irradiation source of the OSL reader and its error,
#' given as a numeric vector of length two.
#' Expected unit: Gy/s. Example: `readerDdot = c(0.08, 0.01)`.
#'
#' @param fit.method [character] (*with default*):
#' Fit function of the dose response curve. Can either be `EXP` (the default)
#' or `GOK`. Note that `EXP` (single saturating exponential) is the original
#' function the model after Huntley (2006) and Kars et al. (2008) was
#' designed to use. The use of a general-order kinetics function (`GOK`)
#' is an experimental adaptation of the model and should be used
#' with great care.
#'
#' @param lower.bounds [numeric] (*with default*):
#' Only applicable for `fit.method = 'GOK'`. A vector of length 3 that
#' contains the lower bound values for fitting the general-order kinetics
#' function using [minpack.lm::nlsLM]. In most cases, the default values
#' (c(`-Inf, -Inf, -Inf`)) are appropriate for finding a best fit, but
#' sometimes it may be useful to restrict the lower bounds to e.g.
#' c(`0, 0, 0`). The values of the vector are for parameters
#' `a`, `D0` and `c` in that particular order (see details in
#' [Luminescence::plot_GrowthCurve]).
#'
#' @param normalise [logical] (*with default*): If `TRUE` (the default) all measured and computed \eqn{\frac{L_x}{T_x}} values are normalised by the pre-exponential factor `A` (see details).
#'
#' @param summary [logical] (*with default*):
#' If `TRUE` (the default) various parameters provided by the user
#' and calculated by the model are added as text on the right-hand side of the
#' plot.
#'
#' @param plot [logical] (*with default*):
#' enables/disables plot output.
#'
#' @param ...
#' Further parameters:
#' - `verbose` [logical]: Show or hide console output
#' - `n.MC` [numeric]: Number of Monte Carlo iterations (default = `100000`).
#' **Note** that it is generally advised to have a large number of Monte Carlo
#' iterations for the results to converge. Decreasing the number of iterations
#' will often result in unstable estimates.
#'
#' All other arguments are passed to [plot] and [plot_GrowthCurve] (in particular
#' `mode` for the fit mode and `fit.force_through_origin`)
#'
#' @return An [RLum.Results-class] object is returned:
#'
#' Slot: **@data**\cr
#'
#' \tabular{lll}{
#' **OBJECT** \tab **TYPE** \tab **COMMENT**\cr
#' `results` \tab [data.frame] \tab results of the of Kars et al. 2008 model \cr
#' `data` \tab [data.frame] \tab original input data \cr
#' `Ln` \tab [numeric] \tab Ln and its error \cr
#' `LxTx_tables` \tab `list` \tab A `list` of `data.frames` containing data on dose,
#'  LxTx and LxTx error for each of the dose response curves.
#'  Note that these **do not** contain the natural Ln signal, which is provided separately. \cr
#' `fits` \tab `list` \tab A `list` of `nls` objects produced by [minpack.lm::nlsLM] when fitting the dose response curves \cr
#' }
#'
#' Slot: **@info**\cr
#'
#' \tabular{lll}{
#' **OBJECT** \tab **TYPE** \tab **COMMENT** \cr
#' `call` \tab `call` \tab the original function call \cr
#' `args` \tab `list` \tab arguments of the original function call \cr
#' }
#'
#' @section Function version: 0.4.5
#'
#' @author
#' Georgina E. King, University of Lausanne (Switzerland) \cr
#' Christoph Burow, University of Cologne (Germany) \cr
#' Sebastian Kreutzer, Ruprecht-Karl University of Heidelberg (Germany)
#'
#' @keywords datagen
#'
#' @note This function has BETA status, in particular for the GOK implementation. Please verify
#' your results carefully
#'
#' @references
#'
#' Kars, R.H., Wallinga, J., Cohen, K.M., 2008. A new approach towards anomalous fading correction for feldspar
#' IRSL dating-tests on samples in field saturation. Radiation Measurements 43, 786-790. doi:10.1016/j.radmeas.2008.01.021
#'
#' Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li, S.-H., Pagonis, P.,
#' Herman, F., 2015. Radiation-induced growth and isothermal decay of infrared-stimulated luminescence
#' from feldspar. Radiation Measurements 81, 224-231.
#'
#' Huntley, D.J., 2006. An explanation of the power-law decay of luminescence.
#' Journal of Physics: Condensed Matter 18, 1359-1365. doi:10.1088/0953-8984/18/4/020
#'
#' King, G.E., Herman, F., Lambert, R., Valla, P.G., Guralnik, B., 2016.
#' Multi-OSL-thermochronometry of feldspar. Quaternary Geochronology 33, 76-87. doi:10.1016/j.quageo.2016.01.004
#'
#'
#' **Further reading**
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
#' kars <- calc_Huntley2006(
#'  data = data,
#'  rhop = rhop,
#'  ddot = ddot,
#'  readerDdot = readerDdot,
#'  n.MC = 25)
#'
#'
#' \dontrun{
#' # You can also provide LnTn values separately via the 'LnTn' argument.
#' # Note, however, that the data frame for 'data' must then NOT contain
#' # a LnTn value. See argument descriptions!
#' LnTn <- data.frame(
#'  LnTn = c(1.84833, 2.24833),
#'  nTn.error = c(0.17, 0.22))
#'
#' LxTx <- data[2:nrow(data), ]
#'
#' kars <- calc_Huntley2006(
#'  data = LxTx,
#'  LnTn = LnTn,
#'  rhop = rhop,
#'  ddot = ddot,
#'  readerDdot = readerDdot,
#'  n.MC = 25)
#' }
#' @md
#' @export
calc_Huntley2006 <-
  function(
    data,
    LnTn = NULL,
    rhop,
    ddot,
    readerDdot,
    normalise = TRUE,
    fit.method = c("EXP", "GOK"),
    lower.bounds = c(-Inf, -Inf, -Inf, -Inf),
    summary = TRUE,
    plot = TRUE,
    ...
){
  ## Validate Input ------------------------------------------------------------

  ## Check fit method
  if (!fit.method[1] %in% c("EXP", "GOK"))
    stop("[calc_Huntley2006] Invalid fit option ('", fit.method[1], "'). Only 'EXP' and 'GOK' allowed for argument 'fit.method'.",
         call. = FALSE)

  ## Check length of lower.bounds
  if (fit.method[1] == "GOK" && length(lower.bounds) != 4)
    stop("[calc_Huntley2006] Argument 'lower.bounds' must be of length 3 exactly.",
         call. = FALSE)

  ## Check 'data'
  # must be a data frame
  if (is.data.frame(data)) {

    if (ncol(data) == 2) {
      warning("[calc_Huntley2006] 'data' only had two columns. We assumed that",
              " the errors on LxTx were missing and automatically added a",
              " 5 % error.\n Please provide a data frame with three columns",
              " if you wish to use actually measured LxTx errors.", call. = FALSE)
      data[ ,3] <- data[ ,2] * 0.05
    }

    # Check if 'LnTn' is used and overwrite 'data'
    if (!is.null(LnTn)) {

      if (!is.data.frame(LnTn))
        stop("Value for 'LnTn' must be a data frame!", call. = FALSE)
      if (ncol(LnTn) != 2)
        stop("Data frame for 'LnTn' must have two columns!", call. = FALSE)
      if (ncol(data) > 3)
        stop("Argument 'LnTn' requires the data frame 'data' to have 2 or 3 columns only!", call. = FALSE)

      # case 1: only one LnTn value
      if (nrow(LnTn) == 1) {
        LnTn <- setNames(cbind(0, LnTn), names(data))
        data <- rbind(LnTn, data)

        # case 2: >1 LnTn value
      } else {
        LnTn_mean <- mean(LnTn[ ,1])
        LnTn_sd <- sd(LnTn[ ,1])
        LnTn_error <- max(LnTn_sd, LnTn[ ,2])
        LnTn <- setNames(data.frame(0, LnTn_mean, LnTn_error), names(data))
        data <- rbind(LnTn, data)
      }

    }

    # check number of columns
    if (ncol(data) %% 3 != 0) {
      stop("[calc_Huntley2006] the number of columns in 'data' must be a multiple of 3.",
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
    stop("\n[calc_Huntley2006()] 'data' must be a data frame.",
         call. = FALSE)
  }

  ## Check 'rhop'
  # check if numeric
  if (is.numeric(rhop)) {

    ### TODO: can be of length 2 if error
    if (length(rhop) != 2)
      stop("\n[calc_Huntley2006()] 'rhop' must be a vector of length two.",
           call. = FALSE)

    # alternatively, and RLum.Results object produced by analyse_FadingMeasurement()
    # can be provided
  } else if (inherits(rhop, "RLum.Results")) {

    if (rhop@originator == "analyse_FadingMeasurement")
      rhop <- c(rhop@data$rho_prime$MEAN,
                rhop@data$rho_prime$SD)
    else
      stop("\n[calc_Huntley2006] Only an 'RLum.Results' object produced by",
           " 'analyse_FadingMeasurement()' is allowed as input for 'rhop'.",
           call. = FALSE)
  }

  # check if 'rhop' is actually a positive value
  if (any(is.na(rhop)) || !rhop[1] > 0 || any(is.infinite(rhop))) {
    stop("\n[calc_Huntley2006] 'rhop' must be a positive number. Provided value",
         " was: ", signif(rhop[1], 3), " \u2213 " , signif(rhop[2], 3),
         call. = FALSE)
  }

  ## Check ddot & readerDdot
  # check if numeric
  if (any(sapply(list(ddot, readerDdot), is.numeric) == FALSE))
    stop("\n[calc_Huntley2006] 'ddot' and 'readerDdot' must be numeric values.",
         call. = FALSE)
  # check if length == 2
  if (any(sapply(list(ddot, readerDdot), function(x) length(x) == 2) == FALSE))
    stop("\n[calc_Huntley2006] 'ddot' and 'readerDdot' must be of length 2.",
         call. = FALSE)

  ## Settings ------------------------------------------------------------------
  settings <- modifyList(
    list(
      verbose = TRUE,
      n.MC = 100000),
    list(...))

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

  GC.settings <- list(
    sample = data.tmp,
    mode = "interpolation",
    fit.method = fit.method[1],
    fit.bounds = TRUE,
    output.plot = plot,
    main = "Measured dose response curve",
    xlab = "Dose (Gy)",
    fit.force_through_origin = FALSE,
    verbose = FALSE)

  GC.settings <- modifyList(GC.settings, list(...))
  GC.settings$verbose <- FALSE

  ## take of force_through origin settings
  force_through_origin <- GC.settings$fit.force_through_origin

  ## call the fitting
  GC.measured <- try(do.call(plot_GrowthCurve, GC.settings))

  if (inherits(GC.measured$Fit, "try-error"))
    stop("\n[calc_Huntley2006()] Unable to fit growth curve to measured data. Try to set fit.bounds to FALSE!",
         call. = FALSE)

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

  ## do the fitting
  fitcoef <- do.call(rbind, sapply(rhop_MC, function(rhop_i) {
    if (fit.method[1] == "EXP") {
      fit_sim <- try({
        minpack.lm::nlsLM(
          LxTx.measured ~ a * theta(dosetime, rhop_i) * (1 - exp(-(dosetime + c)/ D0)),
          start = list(
            a = coef(fit_measured)[["a"]],
            c = coef(fit_measured)[["c"]],
            D0 = D0.measured / readerDdot),
          lower = lower.bounds[1:3],
          upper = if(force_through_origin) c(a = Inf, c = 0, D0 = Inf) else rep(Inf,3),
          control = list(maxiter = settings$maxiter))
        }, silent = TRUE)

    } else if (fit.method[1] == "GOK") {
      fit_sim <- try({
        minpack.lm::nlsLM(
          LxTx.measured ~ a * theta(dosetime, rhop_i) * (d-(1+(1/D0)*dosetime*c)^(-1/c)),
          start = list(
            a = coef(fit_measured)[["a"]],
            D0 = D0.measured / readerDdot,
            c = coef(fit_measured)[["c"]] * ddot,
            d = coef(fit_measured)[["d"]]),
          upper = if(force_through_origin) {
             c(a = Inf, D0 = Inf, c = Inf, d = 1)
            } else {
            rep(Inf, 4)},
          lower = lower.bounds,
          control = list(maxiter = settings$maxiter))},
        silent = TRUE)
    }

    if (!inherits(fit_sim, "try-error"))
      coefs <- coef(fit_sim)
    else
      coefs <- c(a = NA, D0 = NA, c = NA, d = NA)
    return(coefs)
  }, simplify = FALSE))

  # final fit for export
  # fit_simulated <- minpack.lm::nlsLM(LxTx.measured ~ a * theta(dosetime, rhop[1]) * (1 - exp(-dosetime / D0)),
  #                      start = list(a = max(LxTx.measured), D0 = D0.measured / readerDdot))

  # scaling factor
  A <- mean(fitcoef[, "a"], na.rm = TRUE)
  A.error <- sd(fitcoef[ ,"a"], na.rm = TRUE)

  # calculate measured fraction of saturation
  nN <- Ln / A
  nN.error <- nN * sqrt( (Ln.error / Ln)^2 + (A.error / A)^2)

  # compute a natural dose response curve following the assumptions of
  # Morthekai et al. 2011, Geochronometria
  # natdosetime <- seq(0, 1e14, length.out = settings$n.MC)
  # natdosetimeGray <- natdosetime * ddot / ka

  # calculate D0 dose in seconds
  computedD0 <- (fitcoef[ ,"D0"] * readerDdot) / (ddot / ka)

  # Legacy code:
  # This is an older approximation to calculate the natural dose response curve,
  # which sometimes tended to slightly underestimate nN_ss. This is now replaced
  # with the newer approach below.
  # compute natural dose response curve
  # LxTx.sim <- A * theta(natdosetime, rhop[1]) * (1 - exp(-natdosetime / mean(computedD0, na.rm = TRUE) ))
  # warning("LxTx Curve: ", round(max(LxTx.sim) / A, 3), call. = FALSE)

  # compute natural dose response curve
  ddots <- ddot / ka
  natdosetimeGray <- c(0, exp(seq(1, log(max(data[ ,1]) * 2), length.out = 999)))
  natdosetime <- natdosetimeGray
  rprime <- seq(0.01, 5, length.out = 500)
  pr <- 3 * rprime^2 * exp(-rprime^3) # Huntley 2006, eq. 3
  K <- Hs * exp(-rhop[1]^-(1/3) * rprime)
  TermA <- matrix(NA, nrow = length(rprime), ncol = length(natdosetime))
  UFD0 <- mean(fitcoef[ ,"D0"], na.rm = TRUE) * readerDdot

  if(fit.method[1] == "EXP")
    c_exp <- mean(fitcoef[ ,"c"], na.rm = TRUE)

  if (fit.method[1] == "GOK") {
    c_gok <- mean(fitcoef[ ,"c"], na.rm = TRUE)

    ## prevent negative c_gok values, which will cause NaN values
    if(c_gok < 0) c_gok <- 1

    d_gok <- mean(fitcoef[ ,"d"], na.rm = TRUE)
  }

  for (j in 1:length(natdosetime)) {
    for (k in 1:length(rprime)) {
      if (fit.method[1] == "EXP") {
        TermA[k,j] <- A * pr[k] *
          ((ddots / UFD0) / (ddots / UFD0 + K[k]) *
             (1 - exp(-(natdosetime[j] + c_exp) * (1 / UFD0 + K[k]/ddots))))
      } else if (fit.method[1] == "GOK") {
        TermA[k,j] <- A * pr[k] * (ddots / UFD0) / (ddots / UFD0 + K[k]) *
          (d_gok-(1+(1/UFD0 + K[k]/ddots) * natdosetime[j] * c_gok)^(-1/c_gok))
      }
    }}

  LxTx.sim <- colSums(TermA) / sum(pr)
  # warning("LxTx Curve (new): ", round(max(LxTx.sim) / A, 3), call. = FALSE)

  # calculate Age
  positive <- which(diff(LxTx.sim) > 0)

  data.unfaded <- data.frame(
    dose = c(0, natdosetimeGray[positive]),
    LxTx = c(Ln, LxTx.sim[positive]),
    LxTx.error = c(Ln.error, LxTx.sim[positive] * A.error/A))

  data.unfaded$LxTx.error[2] <- 0.0001

  GC.settings <- list(
    sample = data.unfaded,
    mode = "interpolation",
    fit.method = fit.method[1],
    fit.bounds = TRUE,
    output.plot = plot,
    fit.force_through_origin = FALSE,
    verbose = FALSE,
    main = "Simulated dose response curve",
    xlab = "Dose (Gy)"
    )

  GC.settings <- modifyList(GC.settings, list(...))

  GC.settings$verbose <- FALSE

  ## calculate simulated DE
  suppressWarnings(
    GC.simulated <- try(do.call(plot_GrowthCurve, GC.settings))
  )

  if (!inherits(GC.simulated, "try-error")) {
    GC.simulated.results <- get_RLum(GC.simulated)
    fit_simulated <- get_RLum(GC.simulated, "Fit")
    De.sim <- GC.simulated.results$De

    De.error.sim <- GC.simulated.results$De.Error

    # derive simulated D0
    D0.sim.Gy <- GC.simulated.results$D01
    D0.sim.Gy.error <- GC.simulated.results$D01.ERROR

    Age.sim <- De.sim / ddot
    Age.sim.error <- Age.sim * sqrt( ( De.error.sim/ De.sim)^2 +
                                       (readerDdot.error / readerDdot)^2 +
                                       (ddot.error / ddot)^2)

    Age.sim.2D0 <- 2 * D0.sim.Gy / ddot
    Age.sim.2D0.error <- Age.sim.2D0 * sqrt( ( D0.sim.Gy.error / D0.sim.Gy)^2 +
                                               (readerDdot.error / readerDdot)^2 +
                                               (ddot.error / ddot)^2)

  } else {
    De.sim <- De.error.sim <- Age.sim <- Age.sim.error <- fit_simulated <- D0.sim.Gy <- D0.sim.Gy.error <-  NA
    Age.sim.2D0 <- Age.sim.2D0.error <- NA

  }

  if (Ln > max(LxTx.sim) * 1.1)
    warning("[calc_Huntley2006()] Ln is >10 % larger than the maximum computed LxTx value.",
            " The De and age should be regarded as infinite estimates.",
            call. = FALSE)

  if (Ln < min(LxTx.sim) * 0.95)
    warning("[calc_Huntley2006()] Ln/Tn is smaller than the minimum computed LxTx value.
            If, in consequence, your age result is NA, either your input values are
            unsuitable, or you should consider using a different model for your dataset!",
            call. = FALSE)



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

  nN_SS <- suppressWarnings(exp(mean(log(nN_SS_MC), na.rm = TRUE)))
  nN_SS.error <- suppressWarnings(nN_SS * abs(sd(log(nN_SS_MC), na.rm = TRUE) / mean(log(nN_SS_MC), na.rm = TRUE)))

  ## legacy code for debugging purposes
  ## nN_SS is often lognormally distributed, so we now take the mean and sd
  ## of the log values.
  # warning(mean(nN_SS_MC, na.rm = TRUE))
  # warning(sd(nN_SS_MC, na.rm = TRUE))

  ## (3) UNFADED ---------------------------------------------------------------
  LxTx.unfaded <- LxTx.measured / theta(dosetime, rhop[1])
  LxTx.unfaded[is.nan((LxTx.unfaded))] <- 0
  LxTx.unfaded[is.infinite(LxTx.unfaded)] <- 0
  dosetimeGray <- dosetime * readerDdot
  if (fit.method[1] == "EXP") {
    fit_unfaded <- minpack.lm::nlsLM(
      LxTx.unfaded ~ a * (1 - exp(-(dosetimeGray + c) / D0)),
      start = list(
        a = coef(fit_simulated)[["a"]],
        c = coef(fit_simulated)[["c"]],
        D0 = D0.measured / readerDdot),
        upper = if(force_through_origin) {
           c(a = Inf, c = 0, D0 = max(dosetimeGray))
          } else {
           c(Inf, Inf, max(dosetimeGray))
          },
        lower = lower.bounds[1:3],
      control = list(maxiter = settings$maxiter))
  } else if (fit.method[1] == "GOK") {
    fit_unfaded <- try(minpack.lm::nlsLM(
      LxTx.unfaded ~ a * (d-(1+(1/D0)*dosetimeGray*c)^(-1/c)),
      start = list(
        a = coef(fit_simulated)[["a"]],
        D0 = coef(fit_simulated)[["b"]] / readerDdot,
        c = coef(fit_simulated)[["c"]],
        d = coef(fit_simulated)[["d"]]),
      upper = if(force_through_origin) {
        c(a = Inf, D0 = max(dosetimeGray), c = Inf, d = 1)
       } else {
        c(Inf, max(dosetimeGray), Inf, Inf)},
      lower = lower.bounds[1:4],
      control = list(maxiter = settings$maxiter)), silent = TRUE)

    if(inherits(fit_unfaded, "try-error"))
      stop("[calc_Huntely2006()] Could not fit simulated curve.
           -> Check suitability of the model and the parameters!",
           call. = FALSE)
  }

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
  plot.settings <- modifyList(list(
    main = "Dose response curves",
    xlab = "Dose (Gy)",
    ylab = ifelse(normalise, "normalised LxTx (a.u.)", "LxTx (a.u.)")
  ), list(...))

  ## Plotting ------------------------------------------------------------------
  if (plot) {
    ### par settings ---------
    # set plot parameters
    par.old.full <- par(no.readonly = TRUE)

    # set graphical parameters
    par(mfrow = c(1,1), mar = c(4.5, 4, 4, 4), cex = 0.8)
    if (summary)
      par(oma = c(0, 3, 0, 9))
    else
      par(oma = c(0, 9, 0, 9))

    # Find a good estimate of the x-axis limits
    if(GC.settings$mode == "extrapolation" & !force_through_origin) {
      dosetimeGray <- c(-De.measured - De.measured.error, dosetimeGray)
      De.measured <- -De.measured
    }

    xlim <- range(pretty(dosetimeGray))
    if (!is.na(De.sim) & De.sim > xlim[2])
      xlim <- range(pretty(c(min(dosetimeGray), De.sim)))

    # Create figure after Kars et al. (2008) contrasting the dose response curves
    ## open plot window ------------
    plot(
      x = dosetimeGray[dosetimeGray >= 0],
      y = LxTx_measured$LxTx,
      main = plot.settings$main,
      xlab = plot.settings$xlab,
      ylab = plot.settings$ylab,
      pch = 16,
      ylim = c(0, max(do.call(rbind, list(LxTx_measured, LxTx_unfaded))[["LxTx"]])),
      xlim = xlim
    )

    ##add ablines for extrapolation
    if(GC.settings$mode == "extrapolation")
      abline(v = 0, h = 0, col = "gray")

    # LxTx error bars
    segments(x0 = dosetimeGray[dosetimeGray >= 0],
             y0 = LxTx_measured$LxTx + LxTx_measured$LxTx.Error,
             x1 = dosetimeGray[dosetimeGray >= 0],
             y1 = LxTx_measured$LxTx - LxTx_measured$LxTx.Error,
             col = "black")

    # re-calculate the measured dose response curve in Gray
    xNew <- seq(par()$usr[1],par()$usr[2], length.out = 200)
    yNew <- predict(GC.measured@data$Fit, list(x = xNew))
    if (normalise)
      yNew <- yNew / A

    ## add measured curve -------
    lines(xNew, yNew, col  = "black")

    # add error polygon
    polygon(x = c(natdosetimeGray, rev(natdosetimeGray)),
            y = c(LxTx_simulated$LxTx + LxTx_simulated$LxTx.Error,
                  rev(LxTx_simulated$LxTx - LxTx_simulated$LxTx.Error)),
            col = adjustcolor("grey", alpha.f = 0.5), border = NA)

    ## add simulated curve -------
    points(
      x = natdosetimeGray,
      y = LxTx_simulated$LxTx,
      type = "l",
      lty = 3)

    # Ln and DE as points
    points(x = if(GC.settings$mode == "extrapolation")
                rep(De.measured, 2)
               else
                 c(0, De.measured),
           y = if(GC.settings$mode == "extrapolation")
                c(0,0)
               else
                c(Ln, Ln),
           col = "red",
           pch = c(2, 16))

    # Ln error bar
    segments(x0 = 0, y0 = Ln - Ln.error,
             x1 = 0, y1 = Ln + Ln.error,
             col = "red")

    # Ln as a horizontal line
    lines(x = if(GC.settings$mode == "extrapolation")
                c(0, min(c(De.measured, De.sim), na.rm = TRUE))
              else
                c(par()$usr[1], max(c(De.measured, De.sim), na.rm = TRUE)),
          y = c(Ln, Ln),
          col = "red ", lty = 3)

    #vertical line of measured DE
    lines(x = c(De.measured, De.measured),
          y = c(par()$usr[3], Ln),
          col = "red",
          lty = 3)

    # add legends
    legend("bottomright",
           legend = c(
             "Unfaded DRC",
             "Measured DRC",
             "Simulated natural DRC"),
           lty = c(5, 1, 3),
           bty = "n",
           cex = 0.8)

    # add vertical line of simulated De
    if (!is.na(De.sim)) {
      lines(x = if(GC.settings$mode == "extrapolation")
                  c(-De.sim, -De.sim)
                else
                  c(De.sim, De.sim),
            y = c(par()$usr[3], Ln),
            col = "red", lty = 3)

      points(x = if(GC.settings$mode == "extrapolation") -De.sim else De.sim,
             y = if(GC.settings$mode == "extrapolation") 0 else Ln,
             col = "red" , pch = 16)
    } else {
      lines(x = c(De.measured, xlim[2]),
            y = c(Ln, Ln),
            col = "black", lty = 3)
    }

    # add unfaded DRC --------
    yNew <- predict(fit_unfaded, list(dosetimeGray = xNew))
    if (normalise)
      yNew <- yNew / A

    lines(xNew, yNew, col  = "black", lty = 5)

    points(x = dosetimeGray[dosetimeGray >= 0],
           y = LxTx_unfaded$LxTx,
           col = "black")

    # LxTx error bars
    segments(
      x0 = dosetimeGray[dosetimeGray >= 0],
      y0 = LxTx_unfaded$LxTx + LxTx_unfaded$LxTx.Error,
      x1 = dosetimeGray[dosetimeGray >= 0],
      y1 = LxTx_unfaded$LxTx - LxTx_unfaded$LxTx.Error,
      col = "black")

    # add text
    if (summary) {
      # define labels as expressions
      labels.text <- list(
        bquote(dot(D) == .(format(ddot, digits = 2, nsmall = 2)) %+-% .(round(as.numeric(format(ddot.error, digits = 3, nsmall = 3)), 3)) ~ frac(Gy, ka)),
        bquote(dot(D)["Reader"] == .(format(readerDdot, digits = 2, nsmall = 2)) %+-% .(round(as.numeric(format(readerDdot.error, digits = 3, nsmall = 3)), 3)) ~ frac(Gy, s)),
        bquote(log[10]~(rho~"'") == .(format(log10(rhop[1]), digits = 2, nsmall = 2)) %+-% .(round(as.numeric(format(rhop[2] / (rhop[1] * log(10, base = exp(1))), digits = 2, nsmall = 2)), 2)) ),
        bquote(bgroup("(", frac(n, N), ")") == .(format(nN, digits = 2, nsmall = 2)) %+-% .(round(as.numeric(format(nN.error, digits = 2, nsmall = 2)), 2)) ),
        bquote(bgroup("(", frac(n, N), ")")[SS] == .(format(nN_SS, digits = 2, nsmall = 2)) %+-% .(round(as.numeric(format(nN_SS.error, digits = 2, nsmall = 2)), 2)) ),
        bquote(D["E,sim"] == .(format(De.sim, digits = 1, nsmall = 0)) %+-% .(format(De.error.sim, digits = 1, nsmall = 0)) ~ Gy),
        bquote(D["0,sim"] == .(format(D0.sim.Gy, digits = 1, nsmall = 0)) %+-% .(format(D0.sim.Gy.error, digits = 1, nsmall = 0)) ~ Gy),
        bquote(Age["sim"] == .(format(Age.sim, digits = 1, nsmall = 0)) %+-% .(format(Age.sim.error, digits = 1, nsmall = 0)) ~ ka)
      )

      # each of the labels is positioned at 1/10 of the available y-axis space
      ypos <- seq(range(axTicks(2))[2], range(axTicks(2))[1], length.out = 10)[1:length(labels.text)]

      # allow overprinting
      par(xpd = NA)

      # add labels iteratively
      mapply(function(label, pos) {
        text(x = max(axTicks(1)) * 1.05,
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
      results = data.frame(
        "nN" = nN,
        "nN.error" = nN.error,
        "nN_SS" = nN_SS,
        "nN_SS.error" = nN_SS.error,
        "Meas_De" = abs(De.measured),
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
        "Sim_Age_2D0" = Age.sim.2D0,
        "Sim_Age_2D0.error" = Age.sim.2D0.error,
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
    info = list(
      call = sys.call(),
      args = as.list(sys.call())[-1])
  )

  ## Console output ------------------------------------------------------------
  if (settings$verbose) {
    cat("\n[calc_Huntley2006()]\n")
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
    if (fit.method[1] == "GOK") {
      cat("\n c [-]:\t\t",
          round(summary(fit_measured)$coefficients["c", "Estimate"], 2), "\u00b1",
          round(summary(fit_measured)$coefficients["c", "Std. Error"], 2))
    }
    cat("\n Age [ka]:\t",
        round(results@data$results$Meas_Age, 2), "\u00b1",
        round(results@data$results$Meas_Age.error, 2))
    cat("\n\n ---------- Un-faded -----------")
    cat("\n D0 [Gy]:\t",
        round(results@data$results$Unfaded_D0, 2), "\u00b1",
        round(results@data$results$Unfaded_D0.error, 2))
    if (fit.method[1] == "GOK") {
      cat("\n c [-]:\t\t",
          round(summary(fit_unfaded)$coefficients["c", "Estimate"], 2), "\u00b1",
          round(summary(fit_unfaded)$coefficients["c", "Std. Error"], 2))
    }
    cat("\n\n ---------- Simulated ----------")
    cat("\n DE [Gy]:\t",
        round(results@data$results$Sim_De, 2), "\u00b1",
        round(results@data$results$Sim_De.error, 2))
    cat("\n D0 [Gy]:\t",
        round(results@data$results$Sim_D0, 2), "\u00b1",
        round(results@data$results$Sim_D0.error, 2))
    if (fit.method[1] == "GOK") {
      cat("\n c [-]:\t\t",
          round(summary(fit_simulated)$coefficients["c", "Estimate"], 2), "\u00b1",
          round(summary(fit_simulated)$coefficients["c", "Std. Error"], 2))
    }
    cat("\n Age [ka]:\t",
        round(results@data$results$Sim_Age, 2), "\u00b1",
        round(results@data$results$Sim_Age.error, 2))
    cat("\n Age @2D0 [ka]:\t",
        round(results@data$results$Sim_Age_2D0, 2), "\u00b1",
        round(results@data$results$Sim_Age_2D0.error, 2))
    cat("\n -------------------------------\n\n")

  }

  ## Return value --------------------------------------------------------------
  return(results)
  }
