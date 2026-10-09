#' @title Fit a dose-response curve for luminescence data (Lx/Tx against dose)
#'
#' @description
#' A dose-response curve is produced for luminescence measurements using a
#' regenerative or additive protocol. The function supports interpolation and
#' extrapolation to calculate the equivalent dose.
#'
#' @details
#'
#' ## Implemented fitting methods
#'
#' For all options (except for the `LIN`, `QDR` and the `SSE OR LIN`),
#' the [minpack.lm::nlsLM] function with the evenberg-Marquardt algorithm is
#' used.
#'
#' The solution is found by transforming the function or using [stats::uniroot].
#'
#' **Keyword: `LIN`**
#'
#' Fits a linear function to the data using [lm]:
#' \deqn{y = mx + D_i}
#'
#' **Keyword: `QDR`**
#'
#' Fits a linear function with a quadratic term to the data using  [lm]:
#' \deqn{y = a + bx + cx^2}
#'
#' **Keyword: `SSE`** (formerly `EXP`)
#'
#' Fits a single saturating exponential function of the form:
#' \deqn{y = N (1 - \exp(-\frac{x + D_i}{D_0}))}
#'
#' Parameters \eqn{D_0} and \eqn{D_i} are approximated by a linear fit using [lm].
#'
#' **Keyword: `SSE OR LIN`** (formerly `EXP OR LIN`)
#'
#' Works for some cases where an `SSE` fit fails. If the `SSE` fit fails,
#' a `LIN` fit is done instead, which always works.
#'
#' **Keyword: `SSE+LIN`** (formerly `EXP+LIN`)
#'
#' Tries to fit an exponential plus linear function of the form:
#'
#' \deqn{y = N(1 - \exp(-\frac{x + D_i}{D_0}) + gx)}
#' The \eqn{D_e} is calculated by iteration.
#'
#' **Note:** In the context of luminescence dating, this function has no physical meaning.
#' Therefore, no \eqn{D_0} value is returned.
#'
#' **Keyword: `DSE`** (formerly `EXP+EXP`)
#'
#' Tries to fit a double exponential function of the form:
#'
#' \deqn{y = N_1 (1 - \exp(-\frac{x + D_i}{D0_1})) + N_2 (1 - \exp(-\frac{x + D_i}{D0_2}))}
#'
#' *This fitting procedure is not really robust against wrong start parameters.*
#'
#' **Keyword: `GOK`**
#'
#' Tries to fit the general-order kinetics function following Guralnik et al. (2015)
#' of the form:
#'
#' \deqn{y = a (d - (1 + \frac{1}{D_0} x c)^{-1 / c})}
#'
#' where \eqn{c > 0} is a kinetic order modifier.
#'
#' **Keyword: `OTOR`** (formerly `LambertW`)
#'
#' This tries to fit a dose-response curve based on the Lambert W function
#' and the one trap one recombination centre (OTOR) model according to Pagonis
#' et al. (2020). The function has the form:
#'
#' \deqn{y = (1 + (\mathcal{W}((R - 1) * \exp(R - 1 - (x + D_i) / D_c)) / (1 - R))) * N}
#'
#' with \eqn{W} the Lambert-W function (calculated using [lamW::lambertW0]),
#' \eqn{R} the dimensionless retrapping ratio, \eqn{N} the total concentration
#' of trappings states in cm\eqn{^{-3}}, \eqn{D_{c} = N/R} a constant, and
#' \eqn{D_{i}} is the offset on the x-axis (not part of the original formula in
#' Pagonis et al. 2020). Note that \eqn{R} and \eqn{D_{c}}
#' have a valid physical interpretation only when saturation is reached.
#' Please note that finding the root in `mode = "extrapolation"`
#' is a non-easy task due to the shape of the function and the results might be
#' unexpected.
#'
#' **Keyword: `OTORX`**
#'
#' This adapts extended OTOR (therefore: OTORX) model proposed by Lawless and
#' Timar-Gabor (2024) accounting for retrapping (the equation implemented here
#' is written slightly differently than in the original manuscript):
#'
#' \deqn{F_{OTORX} = 1 + \left[\mathcal{W}\left(-Q * \exp\left(-Q-(1-Q(1-\frac{1}{\exp(1)})) \frac{D + D_i}{D_{63}}\right)\right)\right] / Q}
#'
#' with
#'
#' \deqn{Q = \frac{A_m - A_n}{A_m}\frac{N}{N+N_D}}
#'
#' where \eqn{A_m} and \eqn{A_n} are rate constants for the recombination and
#' the trapping of electrons (\eqn{N}), respectively. \eqn{D_{63}} corresponds to
#' the value at which the trap occupation corresponds to 63% of the saturation
#' value. \eqn{D_i} is an offset: if set to zero, the curve will be forced
#' through the origin as in the original publication.
#'
#' For the implementation the calculation reads further
#'
#' \deqn{y = \frac{F_{OTORX}(((D + D_i)/D_{63}), Q)}{F_{OTORX}((D_{test} + D_i)/D_{63}, Q)}}
#'
#' with \eqn{D_{test}} being the test dose in the same unit (usually s or Gy) as
#' the regeneration dose points. This value is essential and needs to provided
#' along with the usual dose and \eqn{\frac{L_x}{T_x}} values (see `object` parameter input
#' and the example section). For more details see Lawless and Timar-Gabor (2024).
#'
#' The fit also returns the parameter \eqn{R} know from `OTOR`, which is derived
#' as \eqn{R = 1 - Q}.
#'
#' *Note: The offset adder \eqn{D_i} is not part of the formula in Timar-Gabor (2024) and can
#' be set to zero with the option `fit.force_through_origin = TRUE`*
#'
#' **Fit weighting**
#'
#' * `"inverse_var"` (inverse variance weighting - current default)
#'  \deqn{w_i = \frac{1}{\sigma_i^2}}
#'
#' * `"inverse_std"` (inverse standard error)
#' \deqn{w_i = \frac{1}{\sigma_i}}
#'
#' * `"norm_inverse_std"` (normalised inverse standard error weighting - default up to v1.2.1)
#'  \deqn{w_i = \frac{\frac{1}{\sigma_i}}{\Sigma{\frac{1}{\sigma_i}}}}
#' *Although used until Luminescence v1.2.1, this method is no longer
#' recommended, as it does not align with the mathematical approach used in
#' common nls fitting methods.*
#'
#' If the option `fit.weights =  NULL` all weights are set to 1, which disables
#' weighting altogether. If `fit.weights` is a [numeric] vector of correct length
#' (same number of rows as the input `LxTx`), then those fit weights are used.
#' This may be helpful to compare different fitting algorithms that have
#' implemented fit weights differently.
#'
#' **Error estimation using Monte Carlo simulation**
#'
#' Error estimation is done using a parametric bootstrap. A set of
#' \eqn{\frac{L_x}{T_x}} values is constructed by randomly drawing curve data
#' from normal distributions defined by the input values (`mean = value`,
#' `sd = value.error`). A dose-response curve is then fitted for each sampled
#' dataset using the chosen fitting method, producing a distribution of single
#' `De` values. The standard deviation of this distribution is taken as the
#' error of the `De`. With more iterations (`n.MC`) the error estimate
#' stabilizes. However, naturally the error will not decrease with more MC runs.
#'
#' Alternatively, the function returns highest probability density interval
#' estimates as output, users may find more useful under certain circumstances.
#'
#' **Note:** It may take some calculation time with increasing MC runs,
#' especially for the composed functions (`SSE+LIN` and `DSE`).
#'
#' @param object [data.frame] or a [list] of such objects (**required**):
#' data frame with columns for `Dose`, `LxTx`, `LxTx.Error` and `TnTx`
#' (optional). If these column names are used, then they can be passed in
#' whatever order; otherwise columns are taken by position.
#'
#' If `object` is a list, the function is called on each of its elements.
#'
#' If `fit.method = "OTORX"` you have  to provide the test dose in the same unit
#' as the dose in a column called `Test_Dose`. The function searches explicitly
#' for this column name. Only the first value will be used assuming a constant
#' test dose over the measurement cycle.
#'
#' @param mode [character] (*with default*):
#' selects calculation mode of the function.
#' - `"interpolation"` (default) calculates the De by interpolation,
#' - `"extrapolation"` calculates the equivalent dose by extrapolation
#'    (useful for MAAD measurements) and
#' - `"alternate"` calculates no equivalent dose and just fits the data points.
#'
#' Please note that for option `"interpolation"` the first point is considered
#' as natural dose.
#'
#' @param fit.method [character] (*with default*):
#' function used for fitting. Possible options are: `LIN`, `QDR`, `SSE`,
#' `SSE OR LIN`, `SSE+LIN`, `DSE` (not defined for extrapolation), `GOK`,
#' `OTOR` and `OTORX`. See details.
#'
#' @param fit.force_through_origin [logical] (*with default*)
#' allow to force the fitted function through the origin.
#' For `method = "DSE"` the function will be fixed through
#' the origin in either case, so this option will have no effect.
#'
#' @param fit.weights [character] [numeric] (*with default*):
#' weighting approach to be used for the fitting. Options are `inverse_var`
#' (default), `inverse_std`, `norm_inverse_std`, a [numeric] vector, or `NULL`
#' (no weighting). If the input is a numeric vector, it must have length equal
#' to the number of data points to fit (usually the `LxTx` values). See details.
#'
#' @param fit.includingRepeatedRegPoints [logical] (*with default*):
#' includes repeated points for fitting (`TRUE` by default).
#'
#' @param fit.IndexRegPoints [integer] (*optional):
#' indices of the regeneration points to be used in fitting. If `NULL`
#' (default), all regeneration points are used.
#'
#' @param fit.bounds [logical] (*with default*):
#' set lower fit bounds for all fitting parameters to 0. Limited to use
#' with the fit methods `SSE`, `SSE+LIN`, `SSE OR LIN`, `GOK`, `OTOR`, `OTORX`
#' Argument to be inserted for experimental application only!
#'
#' @param n.MC [integer] (*with default*):
#' number of Monte Carlo simulations for error estimation.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar. If `verbose = FALSE` also no
#' `txtProgressBar` is shown.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param ... Further arguments to be passed (currently ignored).
#'
#' @return
#' An [Luminescence::RLum.Results-class] object is returned
#' containing the slot `data` with the
#' following elements:
#'
#' **Overview elements**
#' \tabular{lll}{
#' **DATA.OBJECT** \tab **TYPE** \tab **DESCRIPTION** \cr
#' `..$De` : \tab  `data.frame` \tab Table with De values \cr
#' `..$De.MC` : \tab `numeric` \tab Table with De values from MC runs \cr
#' `..$Fit` : \tab [nls] or [lm] \tab object from the fitting for `SSE`, `SSE+LIN` and `DSE`.
#' In case of a resulting  linear fit when using `LIN`, `QDR` or `SSE OR LIN` \cr
#' `..Fit.Args` : \tab `list` \tab Arguments to the function \cr
#' `..$Formula` : \tab [expression] \tab Fitting formula as R expression \cr
#' }
#'
#' The `@info` slot contains the following elements:
#' \tabular{lll}{
#' **DATA.OBJECT** \tab **TYPE** \tab **DESCRIPTION** \cr
#' `..$fit_message`: \tab `character` \tab The fit message reported \cr
#' `..$call` : \tab `call` \tab The original function call \cr
#' }
#'
#' If `object` is a list, then the function returns a list of
#' [Luminescence::RLum.Results-class]
#' objects as defined above.
#'
#' **Details - `DATA.OBJECT$De`**
#' This object is a [data.frame] with the following columns
#' \tabular{lll}{
#' `De` \tab [numeric] \tab equivalent dose \cr
#' `De.Error` \tab [numeric] \tab standard error the equivalent dose \cr
#' `D01` \tab [numeric] \tab \eqn{D_0} value, curvature parameter of the exponential \cr
#' `D01.ERROR` \tab [numeric] \tab standard error of the \eqn{D_0} value\cr
#' `D02` \tab [numeric] \tab 2nd \eqn{D_0} value, only for `DSE`\cr
#' `D02.ERROR` \tab [numeric] \tab standard error for 2nd \eqn{D_0}; only for `DSE`\cr
#' `R` \tab [numeric] \tab the material specific parameter \eqn{R} (only `OTOR` and `OTORX`)\cr
#' `R.LOWER` \tab [numeric] \tab lower 25% quantile of \eqn{R}\cr
#' `R.UPPER` \tab [numeric] \tab upper 75% quantile of \eqn{R}\cr
#' `Dc` \tab [numeric] \tab value indicating saturation level; only for `OTOR` \cr
#' `Dc.LOWER` \tab [numeric] \tab lower 25% quantile for `Dc`; only for `OTOR` \cr
#' `Dc.UPPER` \tab [numeric] \tab upper 75% quantile for `Dc`; only for `OTOR` \cr
#' `D63` \tab [numeric] \tab the specific saturation level; only for `OTOR`, `OTORX` \cr
#' `D63.LOWER` \ tab [numeric] \tab lower 25% quantile of `D63`; only for `OTOR`, `OTORX` \cr
#' `D63.UPPER` \ tab [numeric] \tab upper 75% quantile of `D63`; only for `OTOR`, `OTORX` \cr
#' `D80` \tab [numeric] \tab the specific saturation level; only for `SSE`, `OTOR`, `OTORX` \cr
#' `D80.LOWER` \ tab [numeric] \tab lower 25% quantile of `D80`; only for `OTOR`, `OTORX` \cr
#' `D80.UPPER` \ tab [numeric] \tab upper 75% quantile of `D80`; only for `OTOR`, `OTORX` \cr
#' `n_N` \tab [numeric] \tab saturation level of dose-response curve derived via integration from the used function; it compares the full integral of the curves (`N`) to the integral until `De` (`n`) (e.g.,  Guralnik et al., 2015)\cr
#' `De.MC` \tab [numeric] \tab equivalent dose derived by Monte-Carlo simulation; ideally identical to `De`\cr
#' `Fit` \tab [character] \tab applied fit function \cr
#' `Mode` \tab [character] \tab mode used in fitting \cr
#' `HPDI68_L` \tab [numeric] \tab highest probability density of the approximated equivalent dose probability curve representing the lower boundary of 68% probability \cr
#' `HPDI68_U` \tab [numeric] \tab same as `HPDI68_L` for the upper bound \cr
#' `HPDI95_L` \tab [numeric] \tab same as `HPDI68_L` but for 95% probability \cr
#' `HPDI95_U` \tab [numeric] \tab same as `HPDI95_L` but for the upper bound \cr
#' `.De.plot` \tab [numeric] \tab equivalent dose used internally for plotting \cr
#' `.De.raw` \tab [numeric] \tab equivalent dose reported 'as is', that is, containing infinities and negative values if they could be calculated. Bear in mind that negative values are meaningless and may be arbitrary.\cr
#' }
#'
#' @section Function version: 1.9
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Michael Dietze, RWTH Aachen (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#'
#' Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46. \doi{10.26034/la.atl.1989.150}
#'
#' Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li, S.-H., Pagonis, P.,
#' Herman, F., 2015. Radiation-induced growth and isothermal decay of infrared-stimulated luminescence
#' from feldspar. Radiation Measurements 81, 224-231. \doi{10.1016/j.radmeas.2015.02.011}
#'
#' Lawless, J.L., Timar-Gabor, A., 2024. A new analytical model to fit both fine and coarse grained quartz luminescence dose response curves. Radiation Measurements 170, 107045. \doi{10.1016/j.radmeas.2023.107045}
#'
#' Pagonis, V., Kitis, G., Chen, R., 2020. A new analytical equation for the dose response of dosimetric materials,
#' based on the Lambert W function. Journal of Luminescence 225, 117333. \doi{10.1016/j.jlumin.2020.117333}
#'
#' @seealso [Luminescence::plot_DoseResponseCurve], [nls],
#' [Luminescence::RLum.Results-class], [Luminescence::get_RLum],
#' [minpack.lm::nlsLM], [lm], [uniroot], [lamW::lambertW0]
#'
#' @examples
#'
#' ##(1) fit growth curve for a dummy data.set and show De value
#' data(ExampleData.LxTxData, envir = environment())
#' temp <- fit_DoseResponseCurve(LxTxData)
#' get_RLum(temp)
#'
#' ##(1b) to access the fitting value try
#' get_RLum(temp, data.object = "Fit")
#'
#' ##(2) fit using the 'extrapolation' mode
#' LxTxData[1,2:3] <- c(0.5, 0.001)
#' print(fit_DoseResponseCurve(LxTxData, mode = "extrapolation"))
#'
#' ##(3) fit using the 'alternate' mode
#' LxTxData[1,2:3] <- c(0.5, 0.001)
#' print(fit_DoseResponseCurve(LxTxData, mode = "alternate"))
#'
#' ##(4) import and fit test data set by Berger & Huntley 1989
#' QNL84_2_unbleached <-
#' read.table(system.file("extdata/QNL84_2_unbleached.txt", package = "Luminescence"))
#'
#' results <- fit_DoseResponseCurve(
#'  QNL84_2_unbleached,
#'  mode = "extrapolation",
#'  verbose = FALSE)
#'
#' #calculate confidence interval for the parameters
#' #as alternative error estimation
#' confint(results$Fit, level = 0.68)
#'
#' \dontrun{
#' ##(5) special case the OTORX model with test dose column
#' df <- cbind(LxTxData, Test_Dose = 15)
#' fit_DoseResponseCurve(object = df, fit.method = "OTORX", n.MC = 10) |>
#'  plot_DoseResponseCurve()
#'
#' QNL84_2_bleached <-
#' read.table(system.file("extdata/QNL84_2_bleached.txt", package = "Luminescence"))
#' STRB87_1_unbleached <-
#' read.table(system.file("extdata/STRB87_1_unbleached.txt", package = "Luminescence"))
#' STRB87_1_bleached <-
#' read.table(system.file("extdata/STRB87_1_bleached.txt", package = "Luminescence"))
#'
#' print(
#'  fit_DoseResponseCurve(
#'  QNL84_2_bleached,
#'  mode = "alternate",
#'  verbose = FALSE)$Fit)
#'
#' print(
#'  fit_DoseResponseCurve(
#'  STRB87_1_unbleached,
#'  mode = "alternate",
#'  verbose = FALSE)$Fit)
#'
#' print(
#'  fit_DoseResponseCurve(
#'  STRB87_1_bleached,
#'  mode = "alternate",
#'  verbose = FALSE)$Fit)
#'  }
#'
#' @export
fit_DoseResponseCurve <- function(
  object,
  mode = c("interpolation", "extrapolation", "alternate"),
  fit.method = c("SSE", "LIN", "QDR", "SSE OR LIN", "SSE+LIN", "DSE",
                 "GOK", "OTOR", "OTORX"),
  fit.force_through_origin = FALSE,
  fit.weights = c("inverse_var", "inverse_std", "norm_inverse_std"),
  fit.includingRepeatedRegPoints = TRUE,
  fit.IndexRegPoints = NULL,
  fit.bounds = TRUE,
  n.MC = 100,
  txtProgressBar = TRUE,
  verbose = TRUE,
  ...
) {
  .set_function_name("fit_DoseResponseCurve")
  on.exit(.unset_function_name(), add = TRUE)

  ## deprecated arguments
  if (is.logical(fit.weights)) {
    fit.weights <- if (isTRUE(fit.weights[1])) "inverse_var" else NULL
    .throw_warning("'fit.weight' no longer accepts a logical value, ",
                   "reset automatically to ", fit.weights %||% "NULL")
  }
  depr.args <- c("fit.NumberRegPoints", "fit.NumberRegPointsReal")
  depr.idx <- which(depr.args %in% ...names())
  if (length(depr.idx) > 0) {
    .deprecated(depr.args[depr.idx], "fit.IndexRegPoints", since = "1.4.0")
  }


  ## Self-call --------------------------------------------------------------
  if (inherits(object, "list")) {
    lapply(object,
           function(x) .validate_class(x, c("data.frame", "matrix"),
                                       name = "All elements of 'object'"))

    results <- lapply(object, function(x) {
      fit_DoseResponseCurve(
          object = x,
          mode = mode,
          fit.method = fit.method,
          fit.force_through_origin = fit.force_through_origin,
          fit.weights = fit.weights,
          fit.includingRepeatedRegPoints = fit.includingRepeatedRegPoints,
          fit.IndexRegPoints = fit.IndexRegPoints,
          fit.bounds = fit.bounds,
          n.MC = n.MC,
          txtProgressBar = txtProgressBar,
          verbose = verbose,
          ...
      )
    })

    return(results)
  }
  ## Self-call end ----------------------------------------------------------

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, c("data.frame", "matrix", "list"))
  .validate_not_empty(object)
  mode <- .validate_args(mode, c("interpolation", "extrapolation", "alternate"))
  interpolation <- mode == "interpolation"
  extrapolation <- mode == "extrapolation"
  alternate <- mode == "alternate"
  fit.method_supported <- c("LIN", "QDR", "SSE", "SSE OR LIN",
                            "SSE+LIN", "DSE", "GOK", "OTOR", "OTORX")
  fit.method_deprecated <- c(SSE = "EXP", "SSE OR LIN" = "EXP OR LIN",
                             "SSE+LIN" = "EXP+LIN", DSE = "EXP+EXP")
  fit.method <- .validate_args(fit.method, c(fit.method_supported, fit.method_deprecated))
  fit.method <- unname(fit.method)
  if (fit.method %in% fit.method_deprecated) {
    new <- names(fit.method_deprecated[match(fit.method, fit.method_deprecated)])
    .deprecated(sprintf("fit.method = \"%s\"", fit.method),
                new = sprintf("fit.method = \"%s\"", new),
                since = "1.3.0")
    fit.method <- new
  }
  if (fit.method == "DSE" && extrapolation)
    .throw_error("Mode 'extrapolation' for fitting method 'DSE' not supported")
  if (fit.method == "OTORX" &&
      (is.null(object$Test_Dose) || all(object$Test_Dose == -1))) {
    .throw_error("Column 'Test_Dose' missing but mandatory for 'OTORX' fitting")
  }
  .validate_logical_scalar(fit.force_through_origin)
  .validate_class(fit.weights, c("character", "numeric"), null.ok = TRUE)
  .validate_logical_scalar(fit.includingRepeatedRegPoints)
  .validate_logical_scalar(fit.bounds)
  if (!is.null(fit.IndexRegPoints)) {
    .validate_class(fit.IndexRegPoints, c("integer", "numeric"), null.ok = TRUE)
    if (any(fit.IndexRegPoints < 1 | fit.IndexRegPoints > nrow(object)))
      .throw_error("All elements of 'fit.IndexRegPoints' should be between 1 and ",
                   nrow(object))

    ## ensure that the natural is included and indices are sorted
    fit.IndexRegPoints <- sort(unique(c(1, fit.IndexRegPoints)))
    object <- object[fit.IndexRegPoints, ]
  }
  .validate_positive_scalar(n.MC, int = TRUE)
  .validate_logical_scalar(txtProgressBar)
  .validate_logical_scalar(verbose)

  ## convert input to data.frame
  if (inherits(object, "matrix"))
    object <- as.data.frame(object)

  ##2.1 check column numbers; we assume that in this particular case no error value
  ##was provided, e.g., set all errors to 0
  if (ncol(object) < 2) {
    .throw_error("'object' should have at least 2 columns")
  }
  if (ncol(object) == 2)
    object <- cbind(object, 0)

  ##2.2 check for inf data in the data.frame
  if (any(is.infinite(unlist(object)))) {
    ## https://stackoverflow.com/questions/12188509/cleaning-inf-values-from-an-r-dataframe
    ## This is slow, but it does not break with previous code
    object <- do.call(data.frame,
                      lapply(object, function(x) replace(x, is.infinite(x), NA)))
      .throw_warning("Inf values found, replaced by NA")
  }

  ##2.2.1 silent column name corrections and ordering

  ## check if all desired column names are present
  ## then sort (either way!)
  default_cln <- c("dose", "lxtx", "lxtx.error", "tntx", "test_dose")
  match.idx <- stats::na.omit(match(default_cln, tolower(colnames(object))))
  if (length(match.idx) >= 3)
    object <- object[, match.idx]

  ## ensure consistent naming of the test dose column
  test_dose.idx <- grep("Test_Dose", colnames(object), ignore.case = TRUE)
  if (!is.null(test_dose.idx))
    colnames(object)[test_dose.idx] <- "Test_Dose"

  ##2.3 check whether the dose value is equal all the time
  if (sum(abs(diff(object[[1]])), na.rm = TRUE) == 0) {
    .throw_message("All points have the same dose, NULL returned")
    return(NULL)
  }

  ## ignore the TnTx column if it only contains NAs
  if (ncol(object) >= 4 && all(is.na(object[[4]]))) {
    object[[4]] <- NULL
  }

  ## count and exclude NA values and print result
  if (sum(!stats::complete.cases(object)) > 0) {
    .throw_warning(sum(!stats::complete.cases(object)),
                   " NA values removed")

    ## exclude NA
    object <- na.exclude(object)

    ## Check if anything is left after removal
    if (nrow(object) == 0) {
      .throw_message("After NA removal, nothing is left from the data set, ",
                     "NULL returned")
      return(NULL)
    }
  }

  ##3. verbose mode
  if(!verbose)
    txtProgressBar <- FALSE

  ##remove rownames from data.frame, as this could causes errors for the reg point calculation
  rownames(object) <- NULL

  ## zero values in the data.frame are not allowed for the y-column
  y.zero <- object[, 2] == 0
  if (sum(y.zero) > 0) {
    .throw_warning(sum(y.zero), " values with 0 for Lx/Tx detected, ",
                   "replaced by ", .Machine$double.eps)
    object[y.zero, 2] <- .Machine$double.eps
  }

  ##1. INPUT
  ## 1.1 Produce data.frame from input values

  ## for interpolation the first point is considered as natural dose
  first.idx <- ifelse(interpolation, 2, 1)
  last.idx <- nrow(object)

  xy <- object[first.idx:last.idx, 1:2]
  colnames(xy) <- c("x", "y")
  y.Error <- object[first.idx:last.idx, 3]

  ##1.1.1 produce weights for weighted fitting; if not do nothing
  ##or hope that the user has provided own weights
  ## reminder: we have already validated the class above

  ## this should prevent problems
  if (!is.null(fit.weights) &&
      (anyNA(y.Error) || any(is.infinite(y.Error)) || any(y.Error == 0))) {
    fit.weights <- NULL
    .throw_warning("Error column invalid, infinite, or contains 0, 'fit.weights' reset to NULL")
  }

  if (is.null(fit.weights)) {
    fit.weights <- rep(1, length(y.Error))

  } else if (inherits(fit.weights, "numeric")) {
    ## if only a scalar is provided, we recycle it
    if (length(fit.weights) == 1) {
      fit.weights <- rep(fit.weights, length(y.Error))
    } else {
      ## we ask the user to provide weights of length corresponding to the
      ## size of the input, but we keep only those we actually need
      .validate_length(fit.weights, nrow(object))
      fit.weights <- fit.weights[first.idx:last.idx]
    }

  } else {
    ## the character case
    .validate_args(fit.weights, c("inverse_var", "inverse_std", "norm_inverse_std"),
                   null.ok = TRUE, extra = "a numeric vector")
    fit.weights <- switch(
      fit.weights[1],
      "inverse_std" = 1 / abs(y.Error),
      "norm_inverse_std" = 1 / abs(y.Error) / sum(1 / abs(y.Error)),
      1 / y.Error^2
    )
  }

  #1.2 Prepare data sets regeneration points for MC Simulation
  ## for interpolation the first point is considered as natural dose
  data.MC <- t(matrix(vapply(
      X = first.idx:last.idx,
      FUN = function(x) {
        sample(rnorm(
          n = 10000,
          mean = object[[2]][x],
          sd = abs(object[[3]][x])
        ),
        size = n.MC,
        replace = TRUE)
      },
      FUN.VALUE = numeric(n.MC)
    ), nrow = n.MC))

  if (interpolation) {
    #1.3 Do the same for the natural signal
    data.MC.De <-
      sample(rnorm(10000, mean = object[1, 2], sd = abs(object[1, 3])),
             n.MC,
             replace = TRUE)
  } else if (extrapolation) {
    data.MC.De <- rep(0, n.MC)
  }

  #1.3 set x.natural
  x.natural <- rep_len(NA_real_, n.MC)

  ## target Lx/Tx for the De solution (natural signal for interpolation,
  ## 0 for extrapolation)
  LnTn <- if (interpolation) object[1, 2] else 0

  ##1.4 set initialise variables
  De <- De.Error <- D01 <- R <- R.LOWER <- R.UPPER <- Dc <- Dc.LOWER <- Dc.UPPER <- NA_real_
  D63 <- D63.LOWER <- D63.UPPER <- D80 <- D80.LOWER <- D80.UPPER <- Di <- N <- NA_real_

  ##1.5 create bindings (we generate this with an internal function klate)
  var.g <- d <- Di <- Q <- NA_real_

  ## FITTING ----------------------------------------------------------------
  ##3. Fitting values with nonlinear least-squares estimation of the parameters
  ## set functions for fitting
  ## REMINDER: DO NOT ADD {} brackets, otherwise the formula construction will not
  ## work

  ## get current environment, we need that later
  currn_env <- environment()

  ## Define functions ---------
  ### SSE ------- (C++ version available)
  fit.functionSSE <- function(N, D0, Di, x)
    N * (1 - exp(-(x + Di) / D0))

  ### SSE+LIN --- (C++ version available)
  fit.functionSSELIN <- function(N, D0, Di, g, x)
    N * (1 - exp(-(x + Di) / D0) + g * x)

  ### DSE ------- (C++ version available)
  fit.functionDSE <- function(N1, N2, D01, D02, x)
    N1 * (1 - exp(-(x + Di) / D01)) + N2 * (1 - exp(-(x + Di) / D02))

  ### GOK ------- (C++ version available)
  fit.functionGOK <- function(a, D0, c, d, x)
    a * (d - (1 + (1 / D0) * x * c)^(-1 / c))

  ### OTOR -------------
  fit.functionOTOR <- function(R, Dc, N, Di, x) (1 + (lamW::lambertW0((R - 1) * exp(R - 1 - ((x + Di) / Dc ))) / (1 - R))) * N

  ### OTORX -------------
  fit.functionOTORX <- function(x, Q, D63, c, Di) .D2nN(x + Di, Q, D63) * c / .D2nN(TEST_DOSE + Di, Q, D63)

  ## input data for fitting; exclude repeated RegPoints
  if (!fit.includingRepeatedRegPoints[1]) {
    is.dup <- duplicated(xy$x)
    fit.weights <- fit.weights[!is.dup]
    data.MC <- data.MC[!is.dup, , drop = FALSE]
    y.Error <- y.Error[!is.dup]
    xy <- xy[!is.dup, , drop = FALSE]
  }
  data <- xy

  ## number of parameters in the non-linear models
  num.params <- switch(fit.method,
                       "QDR" = 3,
                       "SSE" = 3,
                       "SSE OR LIN" = 3,
                       "DSE" = 5,
                       4)
  control_settings <- minpack.lm::nls.lm.control(maxiter = 500)

  ## if the number of data points is smaller than the number of parameters
  ## to fit, the nls() function gets trapped in an infinite loop
  if (fit.method != "LIN" && nrow(data) < num.params) {
    fit.method <- "LIN"
    msg <- paste0("Fitting a non-linear least-squares model requires at least ",
                  num.params, " dose points",
                  if (interpolation) " besides the natural",
                  ", 'fit.method' changed to 'LIN'")
    .throw_warning(msg)
    if (verbose)
      .throw_message(msg, error = FALSE)
  }

  ## helper to report the fit: this assigns the
  fit_message <- ""
  .report_fit <- function(De, ...) {
      fit_message <<- paste0(sprintf("Fit: %6s (%s) | De = %.2f",
                                     fit.method, mode, abs(De)), ...)
      if (verbose)
        writeLines(paste("[fit_DoseResponseCurve()]", fit_message))
  }

  ## helper to report a failure in the fit
  .report_fit_failure <- function(method, mode, ...) {
    fit_message <<- sprintf("Fit failed for %s (%s)", method, mode)
    if (verbose)
      writeLines(paste("[fit_DoseResponseCurve()]", fit_message))
  }

  ## helper to run a generic Monte Carlo fitting loop
  .run_mc_fits <- function(formula, start, lower, upper = NULL) {
    pb <- if (txtProgressBar) {
            cat("\n\t Run Monte Carlo loops for error estimation\n")
            on.exit(close(pb), add = TRUE)
            txtProgressBar(min = 0, max = n.MC, char = "=", style = 3)
          } else NULL

    num.fitted <- 0
    mc_ok <- list()
    for (i in seq_len(n.MC)) {
      try({
        fit.MC_i <- minpack.lm::nlsLM(
          formula = formula,
          data = list(x = xy$x, y = data.MC[, i]),
          start = start,
          weights = fit.weights,
          lower = if (is.function(lower)) lower() else lower,
          upper = upper,
          control = control_settings)
        num.fitted <- num.fitted + 1
        mc_ok[[num.fitted]] <- c(i = i, stats::coef(fit.MC_i))
      }, silent = TRUE)

      if (!is.null(pb)) setTxtProgressBar(pb, i)
    }

    if (num.fitted == 0)
      return(NULL)
    as.data.frame(do.call(rbind, mc_ok))
  }

  ## solve for De
  ## when uniroot fails, 'none' returns NA, 'quiet'/'warn' use optimize()
  .solve_De <- function(f, interval, params, method,
                        fallback = c("none", "quiet", "warn"), ...) {
    fallback <- match.arg(fallback)
    args <- c(list(f = f, interval = interval, tol = 0.001), params, ...)
    de <- try(suppressWarnings(do.call(stats::uniroot, args)$root),
              silent = TRUE)

    ## there are cases where the function cannot calculate the root
    ## due to its shape, here we have to use the minimum
    if (inherits(de, "try-error") && fallback != "none") {
      if (fallback == "warn") {
        .throw_warning(
          "Standard root estimation using stats::uniroot() failed. ",
          "Using stats::optimize() instead, which may lead, however, ",
          "to unexpected and inconclusive results for fit.method = '", method, "'")
      }
      args <- c(list(f = f, interval = interval), params)
      de <- try(suppressWarnings(do.call(stats::optimize, args)$minimum),
                silent = TRUE)
    }

    if (inherits(de, "try-error")) NA else de
  }

  .compute_D80 <- function(D63, R) {
    D63 * (0.809 + 0.800 * R) / (0.368 + 0.632 * R)
  }

  ##START PARAMETER ESTIMATION
  ##general setting of start parameters for fitting

  ## a - estimation for the maximum of the y-values (Lx/Tx)
  a <- max(data[,2])

  ##b - get start parameters from a linear fit of the log(y) data
  ##    (don't even try fitting if no y value is positive)
  b <- 1
  if (any(data$y > 0)) {
    ## this may cause NaN values so we have to handle those later
    fit.lm <- try(stats::lm(suppressWarnings(log(data$y)) ~ data$x,
                            weights = fit.weights),
                  silent = TRUE)

    if (!inherits(fit.lm, "try-error") && !is.na(fit.lm$coefficients[2]))
      b <- as.numeric(1 / fit.lm$coefficients[2])
  }

  ##c - get start parameters from a linear fit - offset on x-axis
  fit.lm <- stats::lm(data$y ~ data$x,
                      weights = fit.weights)
  c <- as.numeric(abs(fit.lm$coefficients[1]/fit.lm$coefficients[2]))

  #take slope from x - y scaling
  g <- max(data[,2]/max(data[,1]))

  ## set D01 and D02 (in case of DSE)
  D01 <- D01.ERROR <- D02 <- D02.ERROR <- NA

  ## Let start parameter vary -------------------------------------------------
  ## to be a little bit more flexible, the start parameters varies within
  ## a normal distribution

  ## draw 50 start values from a normal distribution
  if (!fit.method %in% c("LIN", "QDR", "GOK")) {
    a.MC <- suppressWarnings(rnorm(50, mean = a, sd = a / 100))
    b.MC <- suppressWarnings(rnorm(50, mean = b, sd = b / 100))

    if(fit.force_through_origin)
      c.MC <- rep(0, 50)
    else
      c.MC <- suppressWarnings(rnorm(50, mean = c, sd = c / 100))
    g.MC <- suppressWarnings(rnorm(50, mean = g, sd = g / 1))

    ##set start vector (to avoid errors within the loop)
    N.start <- D0.start <- Di.start <- g.start <- NA
  }

  ## QDR --------------------------------------------------------------------
  if (fit.method == "QDR") {
    ## establish models without and with intercept term
    model.qdr <- stats::update(
      y ~ I(x) + I(x^2),
      stats::reformulate(".", intercept = !fit.force_through_origin))

    upper <- max(object[, 1]) * 1.5

    .fit_qdr_model <- function(model, data, y) {
      fit <- stats::lm(model, data = data, weights = fit.weights)

      ## solve and get De
      success <- TRUE
      if (!alternate) {
        De.fs <- function(fit, x, y) {
          stats::predict(fit, newdata = data.frame(x)) - y
        }

        ## for uniroot() to work, the values at the endpoints (lower and upper)
        ## must have opposite sign: therefore we check if the value at lower
        ## is negative, and if not we decrease it until we find a negative
        ## value or we see that the function is not decreasing
        lower <- 0
        value.lower <- De.fs(fit, lower, y)
        while (value.lower > 0 && lower > -1000) {
          lower <- lower - 10
          temp <- De.fs(fit, lower, y)
          if (temp > value.lower) break
          value.lower <- temp
        }

        De.uniroot <- try(stats::uniroot(De.fs, fit = fit, y = y,
                                  lower = lower, upper = upper),
                          silent = TRUE)

        success <- !inherits(De.uniroot, "try-error")
        if (success) {
          De <- De.uniroot$root
        }
      }
      return(list(fit = fit, De = De, success = success))
    }

    res <- .fit_qdr_model(model.qdr, data, LnTn)
    fit <- res$fit
    De <- res$De
    if (res$success)
      .report_fit(De)
    else
      .report_fit_failure(fit.method, mode) # nocov

    ##set progressbar
    if(txtProgressBar){
      cat("\n\t Run Monte Carlo loops for error estimation of the QDR fit\n")
      pb <- txtProgressBar(min=0,max=n.MC, char="=", style=3)
    }

    ## Monte Carlo Error estimation
    x.natural <- vapply(1:n.MC, function(i) {
      if (txtProgressBar) setTxtProgressBar(pb, i)
      .fit_qdr_model(
        model = model.qdr,
        data = list(x = xy$x, y = data.MC[, i]),
        y = data.MC.De[i])$De
    }, numeric(1))

    if(txtProgressBar) close(pb)
  }

  ## SSE --------------------------------------------------------------------
  if (fit.method %in% c("SSE", "SSE OR LIN", "LIN")) {
    if(fit.method != "LIN"){
      if (anyNA(c(a, b, c))) {
        .throw_message("Fit ", fit.method, " (", mode,
                       ") could not be applied to this data set, NULL returned")
        return(NULL)
      }

      ##FITTING on GIVEN VALUES##
      ##try to create some start parameters from the input values to make
      ## the fitting more stable

      ## prepare what we can outside the loop
      N.start <- D0.start <- Di.start <- numeric(length(a.MC))
      lower_bounds <- c(N = 0, D0 = 1e-6, Di = 0)

      ## loop for better attempt
      for (i in seq_along(a.MC)) {
        ## run fit
        fit.initial <- suppressWarnings(try(minpack.lm::nlsLM(
          formula = y ~ fit_functionSSE_cpp(N, D0, Di, x),
          data = data,
          start = list(N = a.MC[i], D0 = b.MC[i], Di = c.MC[i]),
          lower = lower_bounds,
          control = control_settings)
        , silent = TRUE))

        if(!inherits(fit.initial, "try-error")){
          #get parameters out of it
          parameters <- coef(fit.initial)
          N.start[i] <- as.numeric(parameters["N"])
          D0.start[i] <- as.numeric(parameters["D0"])
          Di.start[i] <- as.numeric(parameters["Di"])
        }
      }

      ##used median as start parameters for the final fitting
      N <- median(N.start, na.rm = TRUE)
      D0 <- mean(b.MC, na.rm = TRUE) # issue 1552
      Di <- median(Di.start, na.rm = TRUE)

      ## set boundaries
      lower <- if (fit.bounds) c(0, 0, 0) else c(-Inf, -Inf, -Inf)
      upper <- if (fit.force_through_origin) c(Inf, Inf, 0) else c(Inf, Inf, Inf)

      #FINAL Fit curve on given values
      fit <- try(minpack.lm::nlsLM(
        formula = y ~ fit_functionSSE_cpp(N, D0, Di, x),
        data = data,
        start = list(N = N, D0 = D0, Di = 0),
        weights = fit.weights,
        lower = lower,
        upper = upper,
        control = control_settings
      ), silent = TRUE)

      if (inherits(fit, "try-error") && inherits(fit.initial, "try-error")) {
        .report_fit_failure(fit.method, mode)

      }else{
        ##this is to avoid the singular convergence failure due to a perfect fit at the beginning
        ##this may happen especially for simulated data
        if (inherits(fit, "try-error") && !inherits(fit.initial, "try-error")) {
          fit <- fit.initial
          rm(fit.initial)
        }

        ## replace with formula so that we can have the C++ version
        f <- function(x) .toFormula(fit.functionSSE, env = currn_env)
        fit$m$formula <- f

        ## put fitted coefficients in the environment
        .get_coef(fit)

        ## calculate D63 and D80 based on approximation in Mauz et al. (submitted)
        D80 <- 1.609 * D0

        ## calculate De
        De <- NA
        if (interpolation || extrapolation) {
          De <- suppressWarnings(-Di - D0 * log(1 - LnTn / N))
        }

        #print D01 value
        D01 <- D0
        .report_fit(De, sprintf(" | D01 = %.2f", D01))

        ## SSE Monte Carlo error estimation
        mc <- .run_mc_fits(
            formula = y ~ fit_functionSSE_cpp(N, D0, Di, x),
            start = list(N = N, D0 = D0, Di = Di),
            lower = lower,
            upper = upper)

        if (!is.null(mc)) {
          D01.ERROR <- sd(mc$D0, na.rm = TRUE)

          if (!alternate) {
            x.natural[mc$i] <- suppressWarnings(
                -mc$Di - mc$D0 * log(1 - data.MC.De[mc$i] / mc$N))
          }
        }

      }#endif::try-error fit
    }#endif:fit.method!="LIN"

    ## LIN ------------------------------------------------------------------
    ## two options: just linear fit or LIN fit after the SSE fit failed

    if ((fit.method == "SSE OR LIN" && inherits(fit, "try-error")) ||
        fit.method == "LIN") {

      ## establish models without and with intercept term
      model.lin <- stats::update(y ~ x,
                          stats::reformulate(".", intercept = !fit.force_through_origin))

      if (fit.force_through_origin)
        De.fs <- function(fit, y) y / coef(fit)[1]
      else
        De.fs <- function(fit, y) (y - coef(fit)[1]) / coef(fit)[2]

      .fit_lin_model <- function(model, data, y) {
        fit <- stats::lm(model, data = data, weights = fit.weights)

        ## solve and get De
        De <- NA
        if (!alternate)
          De <- De.fs(fit, y)

        return(list(fit = fit, De = unname(De)))
      }

      res <- .fit_lin_model(model.lin, data, LnTn)
      fit.lm <- res$fit
      De <- res$De
      .report_fit(De)

      ## Monte Carlo Error estimation
      x.natural <- vapply(1:n.MC, function(i) {
        .fit_lin_model(
          model = model.lin,
          data = list(x = xy$x, y = data.MC[, i]),
          y = data.MC.De[i])$De
      }, numeric(1))

      #correct for fit.method
      fit.method <- "LIN"
      fit <- fit.lm

    } else {
      fit.method <- "SSE"
    }
  } #end if SSE (this includes the LIN fit option)

  ## SSE+LIN ----------------------------------------------------------------
  else if (fit.method == "SSE+LIN") {
    ## set boundaries
    lower <- if (fit.bounds) c(0, 10, 0, 0) else rep(-Inf, 4)
    upper <- if (fit.force_through_origin) c(Inf, Inf, 0, Inf) else rep(Inf, 4)

    ##try some start parameters from the input values to makes the fitting more stable
    for (i in seq_along(a.MC)) {
      N <- a.MC[i]
      D0 <- b.MC[i]
      Di <- c.MC[i]
      g <- max(0, g.MC[i])

      ##---------------------------------------------------------##
      ##start: with SSE function
      fit.SSE <- try({
        suppressWarnings(minpack.lm::nlsLM(
        formula = y ~ fit_functionSSE_cpp(N, D0, Di, x),
        data = data,
        start = c(N = N, D0 = D0, Di = Di),
        lower = c(N = 0, D0 = 10, Di = 0),
        control = minpack.lm::nls.lm.control(
          maxiter=100)
      ))},
      silent=TRUE)

      if (!inherits(fit.SSE, "try-error")) {
        ## put fitted coefficients in the environment
        .get_coef(fit.SSE)
      }

      fit <- try({
        suppressWarnings(minpack.lm::nlsLM(
          formula = y ~ fit_functionSSELIN_cpp(N, D0, Di, g, x),
          data = data,
          start = c(N = N, D0 = D0, Di = Di, g = g),
          lower = lower,
          control = control_settings
          ))
        }, silent=TRUE)

      if(!inherits(fit, "try-error")){
        #get parameters out of it
        parameters <- coef(fit)
        N.start[i] <- parameters[["N"]]
        D0.start[i] <- parameters[["D0"]]
        Di.start[i] <- parameters[["Di"]]
        g.start[i] <- parameters[["g"]]
      }
    }##end for loop

    ## used mean as start parameters for the final fitting
    N <- median(N.start, na.rm = TRUE)
    D0 <- median(D0.start, na.rm = TRUE)
    Di <- median(Di.start, na.rm = TRUE)
    g <- median(g.start, na.rm = TRUE)

    ##perform final fitting
    fit <- try(suppressWarnings(minpack.lm::nlsLM(
      formula = y ~ fit_functionSSELIN_cpp(N, D0, Di, g, x),
      data = data,
      start = list(N = N, D0 = D0, Di = Di, g = g),
      weights = fit.weights,
      lower = lower,
      upper = upper,
      control = control_settings
    )), silent = TRUE)

    #if try error stop calculation
    if(!inherits(fit, "try-error")){
      ## replace with formula so that we can have the C++ version
      f <- function(x) .toFormula(fit.functionSSELIN, env = currn_env)
      fit$m$formula <- f

      ## put fitted coefficients in the environment
      .get_coef(fit)

      #problem: analytically it is not easy to calculate x,
      #use uniroot to solve that problem ... readjust function first
      f.unirootSSELIN <- function(N, D0, Di, g, x, LnTn) {
        fit_functionSSELIN_cpp(N, D0, Di, g, x) - LnTn
      }

      De <- NA
      if (!alternate) {
        de.interval <- c(if (interpolation) 0 else -1e6, max(xy$x) * 1.5)
        De <- .solve_De(
          f = f.unirootSSELIN,
          interval = de.interval,
          params = list(N = N, D0 = D0, Di = Di, g = g, LnTn = LnTn),
          method = "SSE+LIN",
          extendInt = "yes", maxiter = 3000)

        .report_fit(De)

      ## SSE+LIN Monte Carlo error estimation
      mc <- .run_mc_fits(
        formula = y ~ fit_functionSSELIN_cpp(N, D0, Di, g, x),
        start = list(N = N, D0 = D0, Di = Di, g = g),
        lower = lower)

        if (!is.null(mc) && !alternate) {
          ## analytically it is not easy to calculate x, use uniroot to find it
          for (j in seq_len(nrow(mc))) {
            x.natural[mc$i[j]] <- .solve_De(
                f = f.unirootSSELIN,
                interval = de.interval,
                params = list(N = mc$N[j], D0 = mc$D0[j], Di = mc$Di[j],
                              g = mc$g[j], LnTn = data.MC.De[mc$i[j]]),
                method = "SSE+LIN")
          }
        }
      }
    }else{
      .report_fit_failure(fit.method, mode)
    } #end if "try-error" Fit Method
  } # End if SSE+LIN

  ## DSE --------------------------------------------------------------------
  else if (fit.method == "DSE") {
    ## initialise objects
    N1.start <- N2.start <- D01.start <- D02.start <- Di.start <- NA

    ## set fit bounds
    lower <- if (fit.bounds) rep(0, 5) else rep(-Inf, 5)

    ## try to create some start parameters from the input values to make the fitting more stable
    for (i in seq_along(a.MC)) {
      fit.start <- try({
        minpack.lm::nlsLM(
        formula = y ~ fit_functionDSE_cpp(N1, N2, D01, D02, Di, x),
        data = data,
        start = list(N1 = a.MC[i], N2 = a.MC[i] / 2,
                     D01 = b.MC[i], D02 = b.MC[i] / 2,
                     Di = c.MC[i]),
        lower = lower,
        control = control_settings)
      }, silent = TRUE)

      if (!inherits(fit.start, "try-error")) {
        #get parameters out of it
        parameters <- coef(fit.start)
        N1.start[i] <- parameters["N1"]
        N2.start[i] <- parameters["N2"]
        D01.start[i] <- parameters["D01"]
        D02.start[i] <- parameters["D02"]
        Di.start[i] <- parameters["Di"]
      }
    }

    ##perform final fitting
    fit <- try(minpack.lm::nlsLM(
      formula = .toFormula(fit.functionDSE, env = currn_env),
      data = data,
      start = list(N1 = median(N1.start, na.rm = TRUE),
                   N2 = median(N2.start, na.rm = TRUE),
                   D01 = median(D01.start, na.rm = TRUE),
                   D02 = median(D02.start, na.rm = TRUE),
                   Di = median(Di.start, na.rm = TRUE)),
      weights = fit.weights,
      lower = lower,
      control = control_settings
    ), silent = TRUE)

    ##insert if for try-error
    if (!inherits(fit, "try-error")) {
      N1 <- N2 <- NULL # silence notes from R CMD check

      ## put fitted coefficients in the environment
      .get_coef(fit)

      ## analytically it is not easy to calculate x, use uniroot to find it
      De <- NA
      if (interpolation) {
        f.unirootDSE <-
          function(N1, N2, D01, D02, Di, x, LnTn) {
            fit_functionDSE_cpp(N1, N2, D01, D02, Di, x) - LnTn
          }

        de.interval <- c(0, max(xy$x) * 1.5)
        De <- .solve_De(
          f = f.unirootDSE,
          interval = de.interval,
          params = list(N1 = N1, D01 = D01,
                        N2 = N2, D02 = D02,
                        Di = Di, LnTn = LnTn),
          method = "DSE",
          extendInt = "yes", maxiter = 3000)
      }

      #print D0 and De value values
      .report_fit(De, sprintf(" | D01 = %.2f | D02 = %.2f", D01, D02))

      ## DSE Monte Carlo error estimation
      mc <- .run_mc_fits(
        formula = y ~ fit_functionDSE_cpp(N1, N2, D01, D02, Di, x),
        start = list(N1 = N1, N2 = N2, D01 = D01, D02 = D02, Di = Di),
        lower = lower)

      D01 <- round(D01, digits = 2)
      D02 <- round(D02, digits = 2)

      if (!is.null(mc)) {
        D01.ERROR <- sd(mc$D01, na.rm = TRUE)
        D02.ERROR <- sd(mc$D02, na.rm = TRUE)

        if (!alternate) {
          ## analytically it is not easy to calculate x, use uniroot to find it
          for (j in seq_len(nrow(mc))) {
            x.natural[mc$i[j]] <- .solve_De(
              f = f.unirootDSE,
              interval = de.interval,
              params = list(N1 = mc$N1[j], D01 = mc$D01[j],
                            N2 = mc$N2[j], D02 = mc$D02[j],
                            Di = mc$Di[j], LnTn = data.MC.De[mc$i[j]]),
              method = "DSE")
          }
        }
      }

    }else{
      .report_fit_failure(fit.method, mode)
    } #end if "try-error" Fit Method
  }

  ## GOK --------------------------------------------------------------------
  else if (fit.method[1] == "GOK") {
    ## set bounds
    lower <- if (fit.bounds) rep(0, 4) else rep(-Inf, 4)
    upper <- if (fit.force_through_origin) c(Inf, Inf, Inf, 1) else rep(Inf, 4)

    fit <- try(minpack.lm::nlsLM(
      formula = .toFormula(fit.functionGOK, env = currn_env),
      data = data,
      start = list(a = a, D0 = b, c = 1, d = 1),
      weights = fit.weights,
      lower = lower,
      upper = upper,
      control = control_settings
    ), silent = TRUE)

    if (inherits(fit, "try-error")){
      .report_fit_failure(fit.method, mode)

    }else{
      ## put fitted coefficients in the environment
      .get_coef(fit)

      ## calculate De
      De <- if (interpolation || extrapolation) {
        -(D0 * (1 - (d - LnTn / a)^-c)) / c
      } else NA

      #print D01 value
      D01 <- D0
      .report_fit(De, sprintf(" | D01 = %.2f | c = %.2f", D01, c))

      ## GOK Monte Carlo error estimation
      mc <- .run_mc_fits(
        formula = y ~ fit_functionGOK_cpp(a, D0, c, d, x),
        start = list(a = a, D0 = D0, c = 1, d = 1),
        lower = lower,
        upper = upper)

      if (!is.null(mc)) {
        D01.ERROR <- sd(mc$D0, na.rm = TRUE)

        if (!alternate) {
          # calculate x.natural for error calculation
          ## note that data.MC.De contains only 0s for extrapolation
          temp <- mc$d - data.MC.De[mc$i] / mc$a
          x.natural[mc$i] <- -mc$D0 * (1 - temp^-mc$c) / mc$c
        }
      }
    }
  }

  ## OTOR and OTORX ---------------------------------------------------------
  else if (fit.method %in% c("OTOR", "OTORX")) {
    if (fit.method == "OTOR") {
      Di_lower <- 0.01
      if (extrapolation)
        Di_lower <- 50 ##TODO - fragile ... however it is only used by a few

      ## set bounds
      lower <- if (fit.bounds) c(0, 0, 0, Di_lower) else rep(-Inf, 4)
      upper <- if (fit.force_through_origin) c(10, Inf, Inf, 0) else c(10, Inf, Inf, Inf)

      fit.function <- fit.functionOTOR
      start <- list(R = 0, Dc = b, N = b, Di = 0.1)
      mc.start <- list(R = 0, Dc = b, N = 0, Di = 0)

      ## the lower bound for Di is randomised around the fitted value, so that
      ## the Monte-Carlo fits are not all trapped on the same boundary
      mc.lower <- if (fit.bounds) function() c(0, 0, 0, Di * runif(1, 0, 2))
                  else rep(-Inf, 4)

      ## extract the R coefficient
      R.coef <- function(coefs) coefs[["R"]]
    } else { # OTORX
      ## we need a test dose; the default value is -1 because an NA will cause
      ## additional problems
      TEST_DOSE <- object$Test_Dose[[1]]

      ## here we replace TEST_DOSE by an evaluated value
      ## in the function body; this makes things a lot easier below
      body(fit.functionOTORX) <- do.call(
        substitute, list(body(fit.functionOTORX), list(TEST_DOSE = TEST_DOSE)))

      ## set boundaries
      lower <- if (fit.bounds) c(0, 0, 0, 0) else rep(-Inf, 4)
      upper <- rep(Inf, 4)

      ## correct boundaries for origin forced through zero
      if (fit.force_through_origin && interpolation)
        lower[4] <- upper[4] <- 0

      fit.function <- fit.functionOTORX
      start <- list(Q = 1, D63 = b, c = 1, Di = 1)
      mc.start <- start
      mc.lower <- lower

      ## R is not part of the fit but derived from Q, approximation
      ## based on Mauz et al. (submitted)
      R.coef <- function(coefs) 1 - coefs[["Q"]]
    }

    ## solve for De with the parameters passed as one named vector, so that
    ## the same function serves the fitted and the Monte-Carlo parameters
    de.f <- function(x, pars, LnTn)
      do.call(fit.function, c(list(x = x), pars)) - LnTn

    fit <- try(minpack.lm::nlsLM(
      formula = .toFormula(fit.function, env = currn_env),
      data = data,
      start = start,
      weights = fit.weights,
      lower = lower,
      upper = upper,
      control = control_settings
    ), silent = TRUE)

    if (inherits(fit, "try-error")) {
      .report_fit_failure(fit.method, mode)

    } else {
      ## put fitted coefficients in the environment
      .get_coef(fit)

      ## R, D63 and Dc are linked by the approximation given in Mauz et al. (submitted)
      if (fit.method == "OTOR") {
        D63 <- (0.367 + 0.633 * R) * Dc
      } else {
        R <- 1 - Q
        Dc <- D63 / (0.367 + 0.633 * R)
      }
      D80 <- .compute_D80(D63, R)

      ## calculate De
      De <- NA
      if (!alternate) {
        de.interval <- if (interpolation) c(0, max(object[[1]]) * 1.2)
                       else c(-max(object[[1]]), 0)
        De <- .solve_De(
          f = de.f,
          interval = de.interval,
          params = list(pars = coef(fit), LnTn = LnTn),
          method = fit.method,
          fallback = if (extrapolation) "warn" else "none")
      }

      ## report terminal line
      .report_fit(De, sprintf(" | R = %.2f | D63 = %.2f", R, D63))

      ## OTOR/OTORX Monte Carlo error estimation
      mc <- .run_mc_fits(
        formula = .toFormula(fit.function, env = currn_env),
        start = mc.start,
        lower = mc.lower,
        upper = upper)

      if (!is.null(mc)) {
        ## calculate x.natural for error calculation
        if (!alternate) {
          mc.params <- setdiff(names(mc), "i")
          LnTn.MC <- data.MC.De[mc$i]

          x.natural[mc$i] <- vapply(seq_len(nrow(mc)), function(j)
            as.numeric(.solve_De(
              f = de.f,
              interval = de.interval,
              params = list(pars = unlist(mc[j, mc.params, drop = FALSE]),
                            LnTn = LnTn.MC[j]),
              method = fit.method,
              fallback = if (extrapolation) "quiet" else "none")),
            numeric(1))
        }

        R.ERROR <- quantile(R.coef(mc), na.rm = TRUE, probs = c(0.25, 0.75))
        R.LOWER <- R.ERROR[1]
        R.UPPER <- R.ERROR[2]

        if (fit.method == "OTOR") {
          Dc.ERROR <- quantile(mc$Dc, na.rm = TRUE, probs = c(0.25, 0.75))
          Dc.LOWER <- Dc.ERROR[1]
          Dc.UPPER <- Dc.ERROR[2]

          ## calculate the D63 using the approximation in Mauz et al. (submitted)
          D63.ERROR <- (0.367 + 0.633 * R.ERROR) * Dc.ERROR
        } else {
          D63.ERROR <- quantile(mc$D63, na.rm = TRUE, probs = c(0.25, 0.75))
        }
        D63.LOWER <- D63.ERROR[1]
        D63.UPPER <- D63.ERROR[2]

        ## calculate D80 the same way
        D80.LOWER <- .compute_D80(D63.LOWER, R.LOWER)
        D80.UPPER <- .compute_D80(D63.UPPER, R.UPPER)
      }
    }#endif::try-error fit
  }#End if fit.method selection (for all)

  ## Final De.MC ------------------------------------------------------------

  ## get De values from Monte Carlo simulation
  De.MC <- De.MC.NA <- x.natural
  if (interpolation) {
    ## censor negative values
    De.MC <- pmax(x.natural, 0)
    De.MC.NA[x.natural < 0] <- NA
  } else if (extrapolation) {
    ## always return positive values
    De.MC <- De.MC.NA <- x.natural <- abs(x.natural)
  }

  ## calculate mean and sd (ignore NaN values)
  De.MonteCarlo <- mean(De.MC, na.rm = TRUE)

  #De.Error is Error of the whole De (ignore NaN values)
  De.Error <- sd(De.MC.NA, na.rm = TRUE)

  # Formula creation --------------------------------------------------------
  ## This information is part of the fit object output anyway, but
  ## we keep it here for legacy reasons
  fit_formula <- NA
  if(!inherits(fit, "try-error") && !is.na(fit[1]))
    fit_formula <- .replace_coef(fit)

# Output ------------------------------------------------------------------
  ## calculate HPDI
  HPDI <- matrix(c(NA,NA,NA,NA), ncol = 4)
  ## here we use the original x.natural because we need the entire
  ## distribution of De values, not a censored one
  if (sum(!is.na(x.natural)) >= 5) {
    HPDI <- cbind(
        .calc_HPDI(x.natural, prob = 0.68, na.rm = TRUE)[1, , drop = FALSE],
        .calc_HPDI(x.natural, prob = 0.95, na.rm = TRUE)[1, , drop = FALSE])
  }

  ## calculate the n/N value (the relative saturation level)
  ## the absolute intensity is the integral of curve
      ## define the function
      f_int <- function(x) eval(fit_formula)

      ## run integrations (they may fail; so we have to check)
      N <- try({
        suppressWarnings(
          stats::integrate(f_int, lower = 0, upper = max(xy$x, na.rm = TRUE))$value)
      }, silent = TRUE)
      n <- try({
        suppressWarnings(
          stats::integrate(f_int, lower = 0, upper = max(De, na.rm = TRUE))$value)
      }, silent = TRUE)

      if(inherits(N, "try-error") || inherits(n, "try-error"))
        n_N <- NA
      else
        n_N <- n/N

  ## account for the fact that we can still calculate a De that is negative
  ## even it does not make sense for interpolation
  De.raw <- De
  if (interpolation && !is.na(De) && De < 0) {
    De <- NA
  }

  ## if fields in this objects are changed, update also `temp.GC.all.na`
  ## in analyse_SAR.CWOSL()
  output <- try(data.frame(
    De = abs(De),
    De.Error = De.Error,
    D01 = D01,
    D01.ERROR = D01.ERROR,
    D02 = D02,
    D02.ERROR = D02.ERROR,
    R = R,
    R.LOWER = R.LOWER,
    R.UPPER = R.UPPER,
    Dc = Dc,
    Dc.LOWER = Dc.LOWER,
    Dc.UPPER = Dc.UPPER,
    D63 = D63,
    D63.LOWER = D63.LOWER,
    D63.UPPER = D63.UPPER,
    D80 = D80,
    D80.LOWER = D80.LOWER,
    D80.UPPER = D80.UPPER,
    n_N = n_N,
    De.MC = De.MonteCarlo,
    Fit = fit.method,
    Mode = mode,
    HPDI68_L = HPDI[1,1],
    HPDI68_U = HPDI[1,2],
    HPDI95_L = HPDI[1,3],
    HPDI95_U = HPDI[1,4],
    .De.plot = De,    # no absolute value, needed for plot_DoseResposeCurve()
    .De.raw = De.raw, # negative values not set to NA for interpolation
    row.names = NULL
  ), silent = TRUE)

  ##make RLum.Results object
  set_RLum(
    class = "RLum.Results",
    data = list(
      De = output,
      De.MC = De.MC,
      Fit = fit,
      Fit.Args = list(
          object = object,
          fit.method = fit.method,
          mode = mode,
          fit.force_through_origin = fit.force_through_origin,
          fit.includingRepeatedRegPoints = fit.includingRepeatedRegPoints,
          fit.IndexRegPoints = fit.IndexRegPoints,
          fit.weights = fit.weights,
          fit.bounds = fit.bounds,
          n.MC = n.MC
      ),
      Formula = fit_formula
    ),
    info = list(
        fit_message = fit_message,
        call = sys.call()
    )
  )
}

## Helper functions  --------------------------------------------------------

## Write the fitted coefficients into the parent environment
.get_coef <- function(x) {
  coefs <- stats::coef(x)
  mapply(assign, names(coefs), as.vector(coefs), MoreArgs = list(pos = parent.frame()))
  invisible(coefs)
}

#'@title Replace coefficients in formula
#'
#'@description
#'
#'Replace the parameters in a fitting function by the true, fitted values.
#'This way the results can be easily used by the other functions
#'
#'@param f [nls] or [lm] (**required**): the output object of the fitting
#'
#'@returns Returns an [expression]
#'
#'@noRd
.replace_coef <- function(f) {
  ## get formula as character string
  if(inherits(f, "nls")) {
    str <- as.character(f$m$formula())[3]
    param <- coef(f)
  } else {
    str <- "a * x + b * x^2 + n"
    param <- c(n = 0, a = 0, b = 0)
    first.idx <- if ("(Intercept)" %in% names(coef(f))) 0 else 1
    param[first.idx + 1:length(coef(f))] <- coef(f)
  }

  ## if the following assertion is triggered, it means that we have used a C++
  ## function to implement the model but forgot to replace the formula in the
  ## fit object, which can be done with these lines:
  ##   f <- function(x) .toFormula(fit.functionXXX, env = currn_env)
  ##   fit$m$formula <- f
  stopifnot(!startsWith("fit_function", str))

  ## replace parameters with fitted coefficients
  for (par in names(param)) {
    str <- gsub(
      pattern = par,
      replacement = format(param[[par]], digits = 3, scientific = TRUE),
      x = str,
      fixed = TRUE)
  }

  ## return
  parse(text = str)
}

#'@title Convert function to formula
#'
#'@description The fitting functions are provided as functions, however, later is
#'easer to work with them as expressions, this functions converts to formula
#'
#'@param f [function] (**required**): function to be converted
#'
#'@param env [environment] (*with default*): environment for the formula
#'creation. This argument is required otherwise it can cause all kind of
#'very complicated to-track-down errors when R tries to access the function
#'stack
#'
#'@noRd
.toFormula <- function(f, env) {
  ## deparse
  tmp <- deparse(f)

  ## set formula
  ## this is very fragile and works only if the functions are constructed
  ## without {} brackets, otherwise it will not work in combination
  ## of covr and testthat
  tmp_formula <- stats::as.formula(paste0("y ~ ", paste(tmp[-1], collapse = "")),
                                   env = env)
  return(tmp_formula)
}

#'@title Convert n/N ratio to Dose
#'
#'@description Helper function for OTORX model fit according to
#'Lawless & Timar-Gabor (2024) Eq. 9
#'
#'@param nN [numeric] (**required**): n/N ratio value
#'
#'@param Q [numeric] (**required**): product of relative production rates and
#'hole pairs (see Lawless & Timar-Gabor, 2024)
#'
#'@param D63 [numeric] (**required**): characteristic dose
#'
#'@references https://github.com/jll2/LumDRC/blob/main/otorx.py
#'
#'@note Not used here, however, part of the reference implementation.
#'
#'@noRd
.nN2D <- function(nN, Q, D63) D63 * ((-log(1-nN) - Q*nN)/(1 - Q*(1-exp(-1)))) # nocov

#'@title Convert Dose back to n/N ratio
#'
#'@description Return n/N for a given dose D and parameters Q & D63.
#'see Lawless & Timar-Gabor (2024)
#'
#'@param D [numeric] (**required**): dose
#'
#'@param Q [numeric] (**required**): product of relative production rates and
#'hole pairs (see Lawless & Timar-Gabor, 2024)
#'
#'@param D63 [numeric] (**required**): characteristic dose
#'
#'@references https://github.com/jll2/LumDRC/blob/main/otorx.py
#'
#'@noRd
.D2nN <- function(D, Q, D63) {
  if(all(abs(Q) < 1e-06))
    r <- 1 - exp(-D/D63)
  else if (any(abs(Q) < 1e-06))
    .throw_error("Unsupported zero and non-zero Q in .D2nN()")
  else
    r <- 1 + lamW::lambertW0(-Q * exp(-Q - (1 + Q * expm1(-1)) * D / D63)) / Q

  return(r)
}
