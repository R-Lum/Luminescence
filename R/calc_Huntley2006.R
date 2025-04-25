#' @title Apply the Huntley (2006) model
#'
#' @description
#' The function calculates the expected sample specific fraction of saturation
#' based on the model of Huntley (2006), using the approach as implemented
#' in Kars et al. (2008) or Guralnik et al. (2015).
#'
#' @details
#'
#' This function applies the approach described in Kars et al. (2008) or Guralnik et al. (2015),
#' which are both developed from the model of Huntley (2006), to calculate the expected sample
#' specific fraction of saturation of a feldspar and also to calculate fading
#' corrected age using this model. \eqn{\rho}' (`rhop`), the density of recombination
#' centres, is a crucial parameter of this model and must be determined
#' separately from a fading measurement. The function [analyse_FadingMeasurement]
#' can be used to calculate the sample specific \eqn{\rho}' value.
#'
#' **Kars et al. (2008) -- Single saturating exponential**
#'
#' To apply the approach after Kars et al. (2008), use `fit.method = "EXP"`.
#'
#' Firstly, the unfaded \eqn{D_0} value is determined through applying equation 5 of
#' Kars et al. (2008) to the measured \eqn{\frac{L_x}{T_x}} data as a function of irradiation
#' time, and fitting the data with a single saturating exponential of the form:
#'
#' \deqn{\frac{L_x}{T_x}(t^*) = A  \phi(t^*) \{1 - \exp(-\frac{t^*}{D_0}))\}}
#'
#' where
#'
#' \deqn{\phi(t^*) = \exp(-\rho' \ln(1.8  \tilde{s} t^*)^3)}
#'
#' after King et al. (2016) where \eqn{A} is a pre-exponential factor,
#' \eqn{t^*} (s) is the irradiation time, starting at the mid-point of
#' irradiation (Auclair et al. 2003) and \eqn{\tilde{s}} (\eqn{3\times10^{15}} s\eqn{^{-1}})
#' is the athermal frequency factor after Huntley (2006).
#'
#' Using fit parameters \eqn{A} and \eqn{D_0}, the function then computes a natural dose
#' response curve using the environmental dose rate, \eqn{\dot{D}} (Gy/s) and equations
#' `[1]` and `[2]`. Computed \eqn{\frac{L_x}{T_x}} values are then fitted using the
#' [fit_DoseResponseCurve] function and the laboratory measured LnTn can then
#' be interpolated onto this curve to determine the fading corrected
#' \eqn{D_e} value, from which the fading corrected age is calculated.
#'
#' **Guralnik et al. (2015) -- General-order kinetics**
#'
#' To apply the approach after Guralnik et al. (2015) use `fit.method = "GOK"`.
#'
#' The approach of Guralnik et al. (2015) is very similar to that of
#' Kars et al. (2008) but, instead of using a single saturating exponential,
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
#' The computation is done using 1000 equally-spaced points in the interval
#' \[0.01, 3\]. This can be controlled by setting option `rprime`, such as
#' in `rprime = seq(0.01, 3, length.out = 1000)` (the default).
#'
#' **Uncertainties**
#'
#' Uncertainties are reported at \eqn{1\sigma} and are assumed to be normally
#' distributed and are estimated using Monte-Carlo re-sampling (`n.MC = 10000`
#' by default) of \eqn{\rho}' and \eqn{\frac{L_x}{T_x}} during dose response
#' curve fitting, and of \eqn{\rho}' #' in the derivation of (\eqn{n/N}) and
#' (n/N)_SS.
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
#' - **three columns** with numeric values for dose (s), `LxTx` and `LxTx`
#' error, in this order.
#' - **two columns** with numeric values for dose (s) and `LxTx`, in this order.
#' This assumes that errors on `LxTx` are missing, and a third column will be
#' automatically attached with an arbitrary 5 % error on the provided `LxTx`
#' values.
#' - **wide table**, i.e. a [data.frame] with a number of columns divisible by 3
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
#' **NOTE:** The function assumes the first row of the data to be the
#' `Ln/Tn`-value. If you want to provide more than one `Ln/Tn`-values, consider
#' using argument `LnTn`.
#'
#' @param LnTn [data.frame] (*optional*):
#' A two column data frame with the following structure:
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
#' **NOTE:** This argument should **only** be used to provide more than one
#' `Ln/Tn`-value. If you provide `LnTn`-values with this argument, the data
#' frame for the `data`-argument **must not** contain any `LnTn`-values.
#'
#' @param rhop [numeric] (**required**):
#' A vector of length 2 for the density of recombination centres (\eqn{\rho}')
#' and its error (see Huntley 2006). Note that \eqn{\rho}' must **not** be
#' provided as the common logarithm. Example: `rhop = c(2.92e-06, 4.93e-07)`.
#'
#' @param ddot [numeric] (**required**):
#' A vector of length 2 for the environmental dose rate and its error.
#' Expected unit: Gy/ka. Example: `ddot = c(3.7, 0.4)`.
#'
#' @param readerDdot [numeric] (**required**):
#' A vector of length 2 for the dose rate of the irradiation source of the OSL
#' reader and its error.
#' Expected unit: Gy/s. Example: `readerDdot = c(0.08, 0.01)`.
#'
#' @param normalise [logical] (*with default*): If `TRUE` (the default) all
#' measured and computed \eqn{\frac{L_x}{T_x}} values are normalised by the
#' pre-exponential factor `A` (see details).
#'
#' @param fit.method [character] (*with default*):
#' Fit function of the dose response curve. Can either be `"EXP"` (default) or
#' `"GOK"`. Note that `"EXP"` (single saturating exponential) is the original
#' function the model after Huntley (2006) and Kars et al. (2008) was
#' designed to use. The use of a general-order kinetics function (`"GOK"`)
#' is an experimental adaptation of the model and should be used
#' with great care.
#'
#' @param lower.bounds [numeric] (*with default*):
#' A vector of length 4 for the values of the lower bounds to be applied
#' when fitting the models with [minpack.lm::nlsLM]. In most cases, the
#' default values (`c(-Inf, -Inf, -Inf, -Inf)`) are appropriate for finding
#' a best fit, but sometimes it may be useful to restrict the lower bounds to
#' e.g. `c(0, 0, 0, 0)`. The values of the vectors are, respectively, for
#' parameters `a`, `D0`, `c` and `d` in that order (parameter `d` is ignored
#' when `fit.method = "EXP"`). More details can be found in
#' [fit_DoseResponseCurve].
#'
#' @param cores [integer] (*with default*):
#' The number of cores to use. This will be capped to the number of available
#' cores if set to too high.
#'
#' @param summary [logical] (*with default*):
#' If `TRUE` (the default) various parameters provided by the user
#' and calculated by the model are added as text on the right-hand side of the
#' plot.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ...
#' Further parameters:
#' - `verbose` [logical]: Enable/disable output to the terminal (default = `TRUE`)
#' - `n.MC` [numeric]: Number of Monte Carlo iterations (default = 10000)
#' - `cex` [numeric]: Scaling of the plot (default = 1)
#' - `maxiter` [numeric]: Number of iteration limits for nls fitting
#' - `trace` [logical]: Enable/disable value tracing the terminal during fitting
#' **Note** that it is generally advised to have a large number of Monte Carlo
#' iterations for the results to converge. Decreasing the number of iterations
#' will often result in unstable estimates.
#'
#' All other arguments are passed to [plot] and [fit_DoseResponseCurve] (in
#' particular `mode` for the De calculation mode, `fit.force_through_origin`,
#' and `fit.bounds`).
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
#'  Note that these **do not** contain the natural `Ln` signal, which is provided separately. \cr
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
#' @section Function version: 0.4.6
#'
#' @author
#' Georgina E. King, University of Lausanne (Switzerland) \cr
#' Christoph Burow, University of Cologne (Germany) \cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @keywords datagen
#'
#' @note This function has BETA status, in particular for the GOK implementation. Please verify
#' your results carefully.
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
calc_Huntley2006 <- function(
    data,
    LnTn = NULL,
    rhop,
    ddot,
    readerDdot,
    normalise = TRUE,
    fit.method = c("EXP", "GOK"),
    lower.bounds = c(-Inf, -Inf, -Inf, -Inf),
    cores = 1,
    summary = TRUE,
    plot = TRUE,
    ...
) {
  .set_function_name("calc_Huntley2006")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, "data.frame")
  .validate_not_empty(data)
  fit.method <- .validate_args(fit.method, c("EXP", "GOK"))
  .validate_length(lower.bounds, 4)
  .validate_logical_scalar(summary)
  .validate_logical_scalar(plot)

  ## Check 'data'
  if (ncol(data) == 2) {
      .throw_warning("'data' has only two columns: we assume that the errors ",
                     "on LxTx are missing and automatically add a 5% error.\n",
                     "Please provide a data frame with three columns ",
                     "if you wish to use actually measured LxTx errors.")
      data[ ,3] <- data[ ,2] * 0.05
  }

  ## Check if 'LnTn' is used and overwrite 'data'
  if (!is.null(LnTn)) {
    .validate_class(LnTn, "data.frame")
    if (ncol(LnTn) != 2)
      .throw_error("'LnTn' should be a data frame with 2 columns")
    if (ncol(data) > 3)
      .throw_error("When 'LnTn' is specified, 'data' should have only ",
                   "2 or 3 columns")

      # case 1: only one LnTn value
      if (nrow(LnTn) == 1) {
        LnTn <- setNames(cbind(0, LnTn), names(data))

        # case 2: >1 LnTn value
      } else {
        LnTn_mean <- mean(LnTn[ ,1])
        LnTn_sd <- sd(LnTn[ ,1])
        LnTn_error <- max(LnTn_sd, LnTn[ ,2])
        LnTn <- setNames(data.frame(0, LnTn_mean, LnTn_error), names(data))
      }
    data <- rbind(LnTn, data)
  }

    # check number of columns
    if (ncol(data) %% 3 != 0) {
      .throw_error("The number of columns in 'data' must be a multiple of 3.")
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
      data <- data[stats::complete.cases(data), ]
  }

  ## Check 'rhop'
  .validate_class(rhop, c("numeric", "RLum.Results"))

  # check if numeric
  if (is.numeric(rhop)) {
    .validate_length(rhop, 2)
  } else {
    ## alternatively, an RLum.Results object produced by
    ## analyse_FadingMeasurement() can be provided
    if (is.na(rhop@originator) || rhop@originator != "analyse_FadingMeasurement")
      .throw_error("'rhop' accepts only RLum.Results objects produced ",
                   "by 'analyse_FadingMeasurement()'")
    rhop <- c(rhop@data$rho_prime$MEAN, rhop@data$rho_prime$SD)
  }

  # check if 'rhop' is actually a positive value
  if (anyNA(rhop) || !rhop[1] > 0 || any(is.infinite(rhop))) {
    .throw_error("'rhop' must be a positive number, the provided value ",
                 "was ", signif(rhop[1], 3), " \u00B1 ", signif(rhop[2], 3))
  }

  ## Check ddot & readerDdot
  .validate_class(ddot, "numeric")
  .validate_length(ddot, 2)
  .validate_class(readerDdot, "numeric")
  .validate_length(readerDdot, 2)

  ## set up the parallel cluster
  .validate_positive_scalar(cores, int = TRUE)
  available.cores <- parallel::detectCores()
  cores <- min(cores, available.cores)
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  ## Settings ------------------------------------------------------------------
  extraArgs <- list(...)
  settings <- modifyList(
    list(
      verbose = TRUE,
      n.MC = 10000,
      plot_all_DRC = plot,
      maxiter = 500,
      trace = FALSE),
    extraArgs)

  .validate_positive_scalar(settings$n.MC, int = TRUE, name = "'n.MC'")
  if (settings$n.MC == 1) {
    settings$n.MC <- max(settings$n.MC, 2)
    extraArgs$n.MC <- settings$n.MC
  }

  ## Define Constants ----------------------------------------------------------

  Hs <- 3e15 # s value after Huntley (2006)
  ka <- 1e3 * .const$year_s # in seconds

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

  ## set a sensible default for rprime: in most papers the upper boundary is
  ## around 2.2, so setting it to 3 should be enough in general, and 1000
  ## points seem also enough; in any case, we let the user override it
  rprime <- seq(0.01, 3, length.out = 1000)
  if ("rprime" %in% names(extraArgs)) {
    rprime <- extraArgs$rprime
    .validate_class(rprime, "numeric")
  }

  ## (1) MEASURED ----------------------------------------------------
  data.tmp <- data
  data.tmp[ ,1] <- data.tmp[ ,1] * readerDdot

  GC.settings <- list(
    mode = "interpolation",
    fit.method = fit.method[1],
    fit.bounds = TRUE,
    fit.force_through_origin = FALSE,
    verbose = FALSE)

  GC.settings <- modifyList(GC.settings, extraArgs)
  GC.settings$object <- data.tmp
  GC.settings$verbose <- FALSE

  fit.bounds <- GC.settings$fit.bounds
  force_through_origin <- GC.settings$fit.force_through_origin
  mode_is_extrapolation <- GC.settings$mode == "extrapolation"

  ## call the fitting
  GC.measured <- try(do.call(fit_DoseResponseCurve, GC.settings))

  if (inherits(GC.measured$Fit, "try-error")) {
    .throw_error("Unable to fit growth curve to measured data",
                 ifelse(fit.bounds, ", try setting 'fit.bounds = FALSE'", ""))
  }
  if (settings$plot_all_DRC) {
    plot_DoseResponseCurve(GC.measured, main = "Measured dose response curve",
                           xlab = "Dose [Gy]", verbose = FALSE)
  }

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

  if (fit.method == "EXP") {
    model <- LxTx.measured ~ a * theta(dosetime, rhop_i) *
      (1 - exp(-(dosetime + c) / D0))
    start <- list(a = coef(fit_measured)[["a"]],
                  D0 = D0.measured / readerDdot,
                  c = coef(fit_measured)[["c"]])
    lower.bounds <- lower.bounds[1:3]

    ## c = 0 if force_through_origin
    upper.bounds <- c(rep(Inf, 2), if (force_through_origin) 0 else Inf)
  }
  if (fit.method == "GOK") {
    model <- LxTx.measured ~ a * theta(dosetime, rhop_i) *
      (d - (1 + (1 / D0) * dosetime * c)^(-1 / c))
    start <- list(a = coef(fit_measured)[["a"]],
                  D0 = D0.measured / readerDdot,
                  c = coef(fit_measured)[["c"]] * ddot,
                  d = coef(fit_measured)[["d"]])

    ## d = 1 if force_through_origin
    upper.bounds <- c(rep(Inf, 3), if (force_through_origin) 1 else Inf)
  }

  ## do the fitting
  fitcoef <- do.call(rbind, parallel::parLapply(cl, rhop_MC, function(rhop_i) {
    fit_sim <- try({
      minpack.lm::nlsLM(
       formula = model,
       start = start,
       lower = lower.bounds,
       upper = upper.bounds,
       trace = settings$trace,
       control = list(maxiter = settings$maxiter))
    }, silent = TRUE)

    if (!inherits(fit_sim, "try-error"))
      coefs <- coef(fit_sim)
    else {
      ## As the fit from the given starting values failed, try again by fixing
      ## D0 at different values to make the fitting a bit easier. Values are
      ## spaced logarithmically so that the ratio between consecutive values
      ## is constant.
      # all.D0 <- c(start$D0, exp(seq(log(start$D0 / 10), log(start$D0 * 10),
      #                               length.out = 99)))
      ## ALTERNATIVE: sample from a gamma distribution; with only 10, this should
      ## be enough based on tests RLumSK (2025-03-29)
      all.D0 <- stats::rgamma(10, shape = start$D0)
      fit.D0 <- lapply(1:length(all.D0), function(idx) {
        D0 <- all.D0[idx]
        t <- try(minpack.lm::nlsLM(
                  formula = model,
                  start = start[-2],
                  lower = lower.bounds[-2],
                  upper = upper.bounds[-2],
                  control = list(maxiter = settings$maxiter)),
                 silent = TRUE)

        if (inherits(t, "try-error"))
          return(NULL)
        return(t)
      })

      ## pick the one with the best fit after removing those that didn't fit
      fit.D0 <- .rm_NULL_elements(fit.D0)

      ## if also this fails, we should throw an error, but as we are inside
      ## a parallel region, we cannot do that cleanly, so we return NA and
      ## only afterwards we'll throw the error
      if (length(fit.D0) == 0)
        return(NA)

      ## extract the coefficients from the one with the best fit
      fit.D0 <- fit.D0[[which.min(vapply(fit.D0, stats::deviance, numeric(1)))]]
      coefs <- coef(fit.D0)

      ## add back the coefficient for D0
      D0 <- environment(fit.D0$m$predict)$env$D0
      coefs <- c(coefs[1], D0 = D0, coefs[2:length(coefs)])
    }

    return(coefs)
  }))

  ## check if errors had occurred during model fitting
  if (anyNA(fitcoef)) {
    .throw_error("Could not fit simulated curve, check suitability of ",
                 "model and parameters")
  }

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
  pr <- 3 * rprime^2 * exp(-rprime^3) # Huntley 2006, eq. 3
  K <- Hs * exp(-rhop[1]^-(1/3) * rprime)
  TermA <- matrix(NA, nrow = length(rprime), ncol = length(natdosetime))
  UFD0 <- mean(fitcoef[ ,"D0"], na.rm = TRUE) * readerDdot

  c_val <- mean(fitcoef[, "c"], na.rm = TRUE)
  if (fit.method[1] == "GOK") {
    ## prevent negative c values, which will cause NaN values
    if (c_val < 0) c_val <- 1

    d_gok <- mean(fitcoef[ ,"d"], na.rm = TRUE)
  }

  for (k in 1:length(rprime)) {
    if (fit.method[1] == "EXP") {
      TermA[k, ] <- A * pr[k] *
        ((ddots / UFD0) / (ddots / UFD0 + K[k]) *
         (1 - exp(-(natdosetime + c_val) * (1 / UFD0 + K[k] / ddots))))
    } else if (fit.method[1] == "GOK") {
      TermA[k, ] <- A * pr[k] * (ddots / UFD0) / (ddots / UFD0 + K[k]) *
        (d_gok-(1+(1 / UFD0 + K[k] / ddots) * natdosetime * c_val)^(-1 / c_val))
    }
  }

  LxTx.sim <- colSums(TermA) / sum(pr)
  # warning("LxTx Curve (new): ", round(max(LxTx.sim) / A, 3), call. = FALSE)

  # calculate Age
  positive <- which(diff(LxTx.sim) > 0)
  if (length(positive) == 0) {
    .throw_error("All simulated Lx/Tx values are identical and approximately ",
                 "zero. Please verify the accuracy of your rho' value, as this ",
                 "is likely too large and may not be realistic")
  }

  data.unfaded <- data.frame(
    dose = c(0, natdosetimeGray[positive]),
    LxTx = c(Ln, LxTx.sim[positive]),
    LxTx.error = c(Ln.error, LxTx.sim[positive] * A.error/A))
  data.unfaded$LxTx.error[2] <- 0.0001

  ## update the parameter list for fit_DoseResponseCurve()
  GC.settings$object <- data.unfaded

  ## calculate simulated DE
  suppressWarnings(
    GC.simulated <- try(do.call(fit_DoseResponseCurve, GC.settings))
  )

  fit_simulated <- NA
  De.sim <- De.error.sim <- D0.sim.Gy <- D0.sim.Gy.error <- NA
  Age.sim <- Age.sim.error <- Age.sim.2D0 <- Age.sim.2D0.error <- NA
  if (!inherits(GC.simulated, "try-error")) {
    if (settings$plot_all_DRC) {
      plot_DoseResponseCurve(GC.simulated, main = "Simulated dose response curve",
                             xlab = "Dose (Gy)", verbose = FALSE)
    }
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
  }

  if (Ln > max(LxTx.sim) * 1.1)
    .throw_warning("Ln is >10 % larger than the maximum computed LxTx value, ",
                   "the De and age should be regarded as infinite estimates")

  if (Ln < min(LxTx.sim) * 0.95 && !mode_is_extrapolation)
    .throw_warning("Ln/Tn is smaller than the minimum computed LxTx value: ",
                   "if, in consequence, your age result is NA, either your ",
                   "input values are unsuitable, or you should consider using ",
                   "a different model for your data")

  if (is.na(D0.sim.Gy)) {
    .throw_error("Simulated D0 is NA: either your input values are unsuitable, ",
                 "or you should consider using a different model for your data")
  }

  # Estimate nN_(steady state) by Monte Carlo Simulation
  ddot_MC <- rnorm(n = settings$n.MC, mean = ddot, sd = ddot.error)
  UFD0_MC <- rnorm(n = settings$n.MC, mean = D0.sim.Gy, sd = D0.sim.Gy.error)

  ## The original formulation was:
  ##
  ##  (1) rho_i <- 3 * alpha^3 * rhop_MC[i] / (4 * pi)
  ##  (2) r <- rprime / (4 * pi * rho_i / 3)^(1 / 3)
  ##  (3) tau <- ((1 / Hs) * exp(1)^(alpha * r)) / ka
  ##
  ## Substituting the expression for `rho_i` into `r`, many simplifications
  ## can be made, so (2) becomes:
  ##
  ##  (2') r <- rprime / (alpha * (rhop_MC[i])^(1 / 3))
  ##
  ## Now, substituting (2') into (3) we get:
  ##
  ##  (3') tau <- (1 / Hs) * exp(rprime / (rhop_MC[i]^(1 / 3))) / ka
  ##
  ## The current formulation then follows:
  ##
  ##  rho_i <- rhop_MC[i]^(1 / 3)
  ##  tau <- (1 / Hs) * exp(rprime / rho_i) / ka

  rho_MC <- rhop_MC^(1 / 3)
  nN_SS_MC <- mapply(function(rho_i, ddot_i, UFD0_i) {
    tau <- (1 / Hs) * exp(rprime / rho_i) / ka
    Ls <- 1 / (1 + UFD0_i / (ddot_i * tau))
    Lstrap <- (pr * Ls) / sum(pr)

    # field saturation
    nN_SS_i <- sum(Lstrap)
    return(nN_SS_i)

  }, rho_MC, ddot_MC, UFD0_MC, SIMPLIFY = TRUE)

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

  ## run this first model also for GOK as in general it provides more
  ## stable estimates that can be used as starting point for GOK
  if (fit.method[1] == "EXP" || fit.method[1] == "GOK") {
    fit_unfaded <- try(minpack.lm::nlsLM(
      LxTx.unfaded ~ a * (1 - exp(-(dosetimeGray + c) / D0)),
      start = list(
        a = coef(fit_simulated)[["a"]],
        D0 = D0.measured / readerDdot,
        c = coef(fit_simulated)[["c"]]),
        upper = if(force_through_origin) {
           c(a = Inf, D0 = max(dosetimeGray), c = 0)
          } else {
           c(Inf, max(dosetimeGray), Inf)
          },
        lower = lower.bounds[1:3],
        trace = settings$trace,
      control = list(maxiter = settings$maxiter)), silent = TRUE)
  }

  ## if this fit has failed, what we do depends on fit.method:
  ## - for EXP, this error is irrecoverable
  if (inherits(fit_unfaded, "try-error") && fit.method == "EXP") {
    .throw_error("Could not fit unfaded curve, check suitability of ",
                 "model and parameters")
  }

  ## - for GOK, we use the simulated fit to set the starting point
  if (fit.method == "GOK") {
    fit_start <- if (inherits(fit_unfaded, "try-error"))
                   fit_simulated else fit_unfaded

    fit_unfaded <- try(minpack.lm::nlsLM(
      LxTx.unfaded ~ a * (d-(1+(1/D0)*dosetimeGray*c)^(-1/c)),
      start = list(
        a = coef(fit_start)[["a"]],
        D0 = coef(fit_start)[["D0"]],
        c = max(coef(fit_start)[["c"]], 1),
        d = coef(fit_simulated)[["d"]]),
      upper = if(force_through_origin) {
        c(a = Inf, D0 = max(dosetimeGray), c = Inf, d = 1)
       } else {
        c(Inf, max(dosetimeGray), Inf, Inf)},
      lower = lower.bounds[1:4],
      control = list(maxiter = settings$maxiter)), silent = TRUE)

    if(inherits(fit_unfaded, "try-error"))
      .throw_error("Could not fit unfaded curve, check suitability of ",
                   "model and parameters")
  }

  D0.unfaded <- coef(fit_unfaded)[["D0"]]
  D0.error.unfaded <- summary(fit_unfaded)$coefficients["D0", "Std. Error"]

  ## Create LxTx tables --------------------------------------------------------
  # normalise by A (saturation point of the un-faded curve)
  if (normalise) {
    LxTx.measured <- LxTx.measured / A
    LxTx.measured.error <- LxTx.measured.error / A

    LxTx.sim <- LxTx.sim / A
    LxTx.unfaded <- LxTx.unfaded / A

    Ln <- Ln / A
    Ln.error <- Ln.error / A
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
    cex = 1,
    xlab = "Dose [Gy]",
    ylab = ifelse(normalise, "normalised LxTx [a.u.]", "LxTx [a.u.]")
  ), extraArgs)

  ## Plotting ------------------------------------------------------------------
  if (plot) {
    ### par settings ---------
    par.default <- par(no.readonly = TRUE)
    on.exit(par(par.default), add = TRUE)

    # set graphical parameters
    par(mfrow = c(1,1), mar = c(4.5, 4, 4, 4), cex = 0.8 * plot.settings$cex,
        oma = c(0, 0, 0, if (summary) 12 / plot.settings$cex else 0))

    # Find a good estimate of the x-axis limits
    if (mode_is_extrapolation && !force_through_origin) {
      dosetimeGray <- c(-De.measured - De.measured.error, dosetimeGray)
      De.measured <- -De.measured
    }

    xlim <- range(pretty(dosetimeGray, n = 15))
    if (!is.na(De.sim) & De.sim > xlim[2])
      xlim <- range(pretty(c(min(dosetimeGray), De.sim), n = 15))

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

    ## add horizontal line at zero
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
    xNew <- seq(if (mode_is_extrapolation) par()$usr[1] else 0,
                par()$usr[2], length.out = 200)
    yNew <- predict(GC.simulated@data$Fit, list(x = xNew))
    if (normalise)
      yNew <- yNew / A
    points(
      x = xNew,
      y = yNew,
      type = "l",
      lty = 3)

    # Ln and DE as points
    points(x = if (mode_is_extrapolation)
                 rep(De.measured, 2)
               else
                 c(0, De.measured),
           y = if (mode_is_extrapolation)
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
    lines(x = if (mode_is_extrapolation)
                c(0, min(c(De.measured, De.sim), na.rm = TRUE))
              else
                c(par()$usr[1], max(c(De.measured, De.sim, 0), na.rm = TRUE)),
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
           inset = c(0, 0.04),
           lty = c(5, 1, 3),
           bty = "n",
           cex = 0.8)

    # add vertical line of simulated De
    if (!is.na(De.sim)) {
      lines(x = if (mode_is_extrapolation)
                  c(-De.sim, -De.sim)
                else
                  c(De.sim, De.sim),
            y = c(par()$usr[3], Ln),
            col = "red", lty = 3)

      points(x = if (mode_is_extrapolation) -De.sim else De.sim,
             y = if (mode_is_extrapolation) 0 else Ln,
             col = "red" , pch = 16)
    } else {
      lines(x = c(De.measured, xlim[2]),
            y = c(Ln, Ln), col = "black", lty = 3)
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
      ypos <- seq(range(axTicks(2))[2], range(axTicks(2))[1], length.out = 10)
      for (i in seq_along(labels.text))
        mtext(labels.text[[i]], at = ypos[i],
              side = 4, line = 1, las = 1, padj = 1)
    }
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
    cat("\n\n[calc_Huntley2006()]\n")
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
