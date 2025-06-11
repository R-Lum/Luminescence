#' @title Fit a dose-response curve for luminescence data (Lx/Tx against dose)
#'
#' @description
#'
#' A dose-response curve is produced for luminescence measurements using a
#' regenerative or additive protocol. The function supports interpolation and
#' extrapolation to calculate the equivalent dose.
#'
#' @details
#'
#' ## Implemented fitting methods
#'
#' For all options (except for the `LIN`, `QDR` and the `EXP OR LIN`),
#' the [minpack.lm::nlsLM] function with the `LM` (Levenberg-Marquardt algorithm)
#' algorithm is used. Note: For historical reasons for the Monte Carlo
#' simulations partly the function [nls] using the `port` algorithm.
#'
#' The solution is found by transforming the function or using [stats::uniroot].
#'
#' **Keyword: `LIN`**
#'
#' Fits a linear function to the data using [lm]:
#' \deqn{y = mx + n}
#'
#' **Keyword: `QDR`**
#'
#' Fits a linear function with a quadratic term to the data using  [lm]:
#' \deqn{y = a + bx + cx^2}
#'
#' **Keyword: `EXP`**
#'
#' Adapts a function of the form
#' \deqn{y = a(1 - exp(-\frac{(x+c)}{b}))}
#'
#' Parameters b and c are approximated by a linear fit using [lm]. Note: \eqn{b = D0}
#'
#' **Keyword: `EXP OR LIN`**
#'
#' Works for some cases where an `EXP` fit fails. If the `EXP` fit fails,
#' a `LIN` fit is done instead, which always works.
#'
#' **Keyword: `EXP+LIN`**
#'
#' Tries to fit an exponential plus linear function of the form:
#'
#' \deqn{y = a(1-exp(-\frac{x+c}{b}) + (gx))}
#' The \eqn{D_e} is calculated by iteration.
#'
#' **Note:** In the context of luminescence dating, this function has no physical meaning.
#' Therefore, no \eqn{D_0} value is returned.
#'
#' **Keyword: `EXP+EXP`**
#'
#' Tries to fit a double exponential function of the form
#'
#' \deqn{y = (a_1 (1-exp(-\frac{x}{b_1}))) + (a_2 (1 - exp(-\frac{x}{b_2})))}
#'
#' *This fitting procedure is not really robust against wrong start parameters.*
#'
#' **Keyword: `GOK`**
#'
#' Tries to fit the general-order kinetics function following Guralnik et al. (2015)
#' of the form
#'
#' \deqn{y = a (d - (1 + (\frac{1}{b}) x c)^{(-1/c)})}
#'
#' where \eqn{c > 0} is a kinetic order modifier
#' (not to be confused with **c** in `EXP` or `EXP+LIN`!).
#'
#'**Keyword: `OTOR`** (former `LambertW`)
#'
#' This tries to fit a dose-response curve based on the Lambert W function
#' and the one trap one recombination centre (OTOR)
#' model according to Pagonis et al. (2020). The function has the form
#'
#' \deqn{y = (1 + (\mathcal{W}((R - 1) * exp(R - 1 - ((x + D_{int}) / D_{c}))) / (1 - R))) * N}
#'
#' with \eqn{W} the Lambert W function, calculated using the package [lamW::lambertW0],
#' \eqn{R} the dimensionless retrapping ratio, \eqn{N} the total concentration
#' of trappings states in cm\eqn{^{-3}} and \eqn{D_{c} = N/R} a constant. \eqn{D_{int}} is
#' the offset on the x-axis. Please note that finding the root in `mode = "extrapolation"`
#' is a non-easy task due to the shape of the function and the results might be
#' unexpected.
#'
#' **Keyword: `OTORX`**
#'
#' This adapts extended OTOR (therefore: OTORX) model proposed by Lawless and Timar-Gabor (2024)
#' accounting for retrapping. Mathematically, the implementation reads (the equation here
#' as implemented, it is slightly differently written than in the original manuscript):
#'
#' \deqn{F_{OTORX} = 1 + \left[\mathcal{W}(-Q * exp(-Q-(1-Q*(1-\frac{1}{exp(1)})) * \frac{(D + a)}{D_{63}}))\right] / Q}
#'
#' with
#'
#' \deqn{Q = \frac{A_m - A_n}{A_m}\frac{N}{N+N_D}}
#'
#' where \eqn{A_m} and \eqn{A_n} are rate constants for the recombination and
#' the trapping of electrons (\eqn{N}), respectively. \eqn{D_{63}} corresponds to
#' the value at which the trap occupation corresponds to the 63% of the saturation value.
#' \eqn{a} is in an offset. If set to zero, the curve will be forced through the origin
#' as in the original publication.
#'
#' For the implementation the calculation reads further
#'
#' \deqn{y = \frac{F_{OTORX}(((D + a)/D_{63}), Q)}{F_{OTORX}((D_{test} + a)/D_{63}, Q)}}
#'
#' with \eqn{D_{test}} being the test dose in the same unit (usually s or Gy) as
#' the regeneration dose points. This value is essential and needs to provided
#' along with the usual dose and \eqn{\frac{L_x}{T_x}} values (see `object` parameter input
#' and the example section). For more details see Lawless and Timar-Gabor (2024).
#'
#' *Note: The offset adder \eqn{a} is not part of the formula in Timar-Gabor (2024) and can
#' be set to zero with the option `fit.force_through_origin = TRUE`*
#'
#' **Fit weighting**
#'
#' If the option `fit.weights =  TRUE` is chosen, weights are calculated using
#' provided signal errors (\eqn{\frac{L_x}{T_x}} error):
#' \deqn{fit.weights = \frac{\frac{1}{error}}{\Sigma{\frac{1}{error}}}}
#'
#' **Error estimation using Monte Carlo simulation**
#'
#' Error estimation is done using a parametric bootstrapping approach. A set of
#' \eqn{\frac{L_x}{T_x}} values is constructed by randomly drawing curve data sampled from normal
#' distributions. The normal distribution is defined by the input values (`mean
#' = value`, `sd = value.error`). Then, a dose-response curve fit is attempted for each
#' dataset resulting in a new distribution of single `De` values. The standard
#' deviation of this distribution becomes then the error of the `De`. With increasing
#' iterations, the error value becomes more stable. However, naturally the error
#' will not decrease with more MC runs.
#'
#' Alternatively, the function returns highest probability density interval
#' estimates as output, users may find more useful under certain circumstances.
#'
#' **Note:** It may take some calculation time with increasing MC runs,
#' especially for the composed functions (`EXP+LIN` and `EXP+EXP`).\cr
#' Each error estimation is done with the function of the chosen fitting method.
#'
#' @param object [data.frame] or a [list] of such objects (**required**):
#' data frame with columns for `Dose`, `LxTx`, `LxTx.Error` and `TnTx`.
#' The column for the test dose response is optional, but requires `'TnTx'` as
#' column name if used. For exponential fits at least three dose points
#' (including the natural) should be provided. If `object` is a list,
#' the function is called on each of its elements.
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
#' as natural dose
#'
#' @param fit.method [character] (*with default*):
#' function used for fitting. Possible options are:
#' - `LIN`,
#' - `QDR`,
#' - `EXP`,
#' - `EXP OR LIN`,
#' - `EXP+LIN`,
#' - `EXP+EXP` (not defined for extrapolation),
#' - `GOK`,
#' - `OTOR`,
#' - `OTORX`
#'
#' See details.
#'
#' @param fit.force_through_origin [logical] (*with default*)
#' allow to force the fitted function through the origin.
#' For `method = "EXP+EXP"` the function will be fixed through
#' the origin in either case, so this option will have no effect.
#'
#' @param fit.weights [logical] (*with default*):
#' option whether the fitting is done with or without weights. See details.
#'
#' @param fit.includingRepeatedRegPoints [logical] (*with default*):
#' includes repeated points for fitting (`TRUE`/`FALSE`).
#'
#' @param fit.NumberRegPoints [integer] (*optional*):
#' set number of regeneration points manually. By default the number of all (!)
#' regeneration points is used automatically.
#'
#' @param fit.NumberRegPointsReal [integer] (*optional*):
#' if the number of regeneration points is provided manually, the value of the
#' real, regeneration points = all points (repeated points) including reg 0,
#' has to be inserted.
#'
#' @param fit.bounds [logical] (*with default*):
#' set lower fit bounds for all fitting parameters to 0. Limited for the use
#' with the fit methods `EXP`, `EXP+LIN`, `EXP OR LIN`, `GOK`, `OTOR`, `OTORX`
#' Argument to be inserted for experimental application only!
#'
#' @param n.MC [integer] (*with default*):  number of Monte Carlo simulations
#' for error estimation, see details.
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
#' An `RLum.Results` object is returned containing the slot `data` with the
#' following elements:
#'
#' **Overview elements**
#' \tabular{lll}{
#' **DATA.OBJECT** \tab **TYPE** \tab **DESCRIPTION** \cr
#' `..$De` : \tab  `data.frame` \tab Table with De values \cr
#' `..$De.MC` : \tab `numeric` \tab Table with De values from MC runs \cr
#' `..$Fit` : \tab [nls] or [lm] \tab object from the fitting for `EXP`, `EXP+LIN` and `EXP+EXP`.
#' In case of a resulting  linear fit when using `LIN`, `QDR` or `EXP OR LIN` \cr
#' `..Fit.Args` : \tab `list` \tab Arguments to the function \cr
#' `..$Formula` : \tab [expression] \tab Fitting formula as R expression \cr
#' `..$call` : \tab `call` \tab The original function call\cr
#' }
#'
#' If `object` is a list, then the function returns a list of `RLum.Results`
#' objects as defined above.
#'
#' **Details - `DATA.OBJECT$De`**
#' This object is a [data.frame] with the following columns
#' \tabular{lll}{
#' `De` \tab [numeric] \tab equivalent dose \cr
#' `De.Error` \tab [numeric] \tab standard error the equivalent dose \cr
#' `D01` \tab [numeric] \tab D-naught value, curvature parameter of the exponential \cr
#' `D01.ERROR` \tab [numeric] \tab standard error of the D-naught value\cr
#' `D02` \tab [numeric] \tab 2nd D-naught value, only for `EXP+EXP`\cr
#' `D02.ERROR` \tab [numeric] \tab standard error for 2nd D-naught; only for `EXP+EXP`\cr
#' `Dc` \tab [numeric] \tab value indicating saturation level; only for `OTOR` \cr
#' `D63` \tab [numeric] \tab the specific saturation level; only for `OTORX` \cr
#' `n_N` \tab [numeric] \tab saturation level of dose-response curve derived via integration from the used function; it compares the full integral of the curves (`N`) to the integral until `De` (`n`) (e.g.,  Guralnik et al., 2015)\cr
#' `De.MC` \tab [numeric] \tab equivalent dose derived by Monte-Carlo simulation; ideally identical to `De`\cr
#' `De.plot` \tab [numeric] \tab equivalent dose use for plotting \cr
#' `Fig` \tab [character] \tab applied fit function \cr
#' `HPDI68_L` \tab [numeric] \tab highest probability density of approximated equivalent dose probability curve representing the lower boundary of 68% probability \cr
#' `HPDI68_U` \tab [numeric] \tab same as `HPDI68_L` for the upper bound \cr
#' `HPDI95_L` \tab [numeric] \tab same as `HPDI68_L` but for 95% probability \cr
#' `HPDI95_U` \tab [numeric] \tab same as `HPDI95_L` but for the upper bound \cr
#'
#' }
#'
#' @section Function version: 1.4.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Michael Dietze, GFZ Potsdam (Germany) \cr
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
#' @seealso [plot_GrowthCurve], [nls], [RLum.Results-class], [get_RLum],
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
#' @md
#' @export
fit_DoseResponseCurve <- function(
  object,
  mode = "interpolation",
  fit.method = "EXP",
  fit.force_through_origin = FALSE,
  fit.weights = TRUE,
  fit.includingRepeatedRegPoints = TRUE,
  fit.NumberRegPoints = NULL,
  fit.NumberRegPointsReal = NULL,
  fit.bounds = TRUE,
  n.MC = 100,
  txtProgressBar = TRUE,
  verbose = TRUE,
  ...
) {
  .set_function_name("fit_DoseResponseCurve")
  on.exit(.unset_function_name(), add = TRUE)

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
          fit.NumberRegPoints = fit.NumberRegPoints,
          fit.NumberRegPointsReal = fit.NumberRegPointsReal,
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
  .validate_class(object, c("data.frame", "matrix", "list"))
  .validate_not_empty(object)
  mode <- .validate_args(mode, c("interpolation", "extrapolation", "alternate"))
  fit.method_supported <- c("LIN", "QDR", "EXP", "EXP OR LIN",
                            "EXP+LIN", "EXP+EXP", "GOK", "OTOR", "OTORX")
  fit.method <- .validate_args(fit.method, fit.method_supported)
  .validate_logical_scalar(fit.force_through_origin)
  .validate_logical_scalar(fit.weights)
  .validate_logical_scalar(fit.includingRepeatedRegPoints)
  .validate_logical_scalar(fit.bounds)
  .validate_positive_scalar(fit.NumberRegPoints, int = TRUE, null.ok = TRUE)
  .validate_positive_scalar(fit.NumberRegPointsReal, int = TRUE, null.ok = TRUE)
  .validate_positive_scalar(n.MC, int = TRUE)

  ## convert input to data.frame
  switch(
    class(object)[1],
    data.frame = object,
    matrix = object <- as.data.frame(object),
  )

  ##2.1 check column numbers; we assume that in this particular case no error value
  ##was provided, e.g., set all errors to 0
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

  ##2.3 check whether the dose value is equal all the time
  if (sum(abs(diff(object[[1]])), na.rm = TRUE) == 0) {
    .throw_message("All points have the same dose, NULL returned")
    return(NULL)
  }

  ## count and exclude NA values and print result
  if (sum(!stats::complete.cases(object)) > 0)
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
  #1.0.1 calculate number of reg points if not set
  if(is.null(fit.NumberRegPoints))
    fit.NumberRegPoints <- length(object[-1,1])

  if(is.null(fit.NumberRegPointsReal)){
    fit.RegPointsReal <- which(!duplicated(object[,1]) | object[,1] != 0)
    fit.NumberRegPointsReal <- length(fit.RegPointsReal)
  }

  ## 1.1 Produce data.frame from input values

  ## for interpolation the first point is considered as natural dose
  first.idx <- ifelse(mode == "interpolation", 2, 1)
  last.idx <- fit.NumberRegPoints + 1

  xy <- object[first.idx:last.idx, 1:2]
  colnames(xy) <- c("x", "y")
  y.Error <- object[first.idx:last.idx, 3]

  ##1.1.1 produce weights for weighted fitting
  if(fit.weights){
    fit.weights <- 1 / abs(y.Error) / sum(1 / abs(y.Error))

    if (anyNA(fit.weights)) { # FIXME(mcol): infinities?
      fit.weights <- rep(1, length(y.Error))
      .throw_warning("Error column invalid or 0, 'fit.weights' ignored")
    }
  }else{
    fit.weights <- rep(1, length(y.Error))
  }

  #1.2 Prepare data sets regeneration points for MC Simulation
  ## for interpolation the first point is considered as natural dose
  first.idx <- ifelse(mode == "interpolation", 2, 1)
  last.idx <- fit.NumberRegPoints + 1

  data.MC <- t(vapply(
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
    ))

  if (mode == "interpolation") {
    #1.3 Do the same for the natural signal
    data.MC.De <-
      sample(rnorm(10000, mean = object[1, 2], sd = abs(object[1, 3])),
             n.MC,
             replace = TRUE)
  } else if (mode == "extrapolation") {
    data.MC.De <- rep(0, n.MC)
  }

  #1.3 set x.natural
  x.natural <- rep(NA_real_, n.MC)

  ##1.4 set initialise variables
  De <- De.Error <- D01 <-  R <-  Dc <- D63 <- N <- TEST_DOSE <- NA_real_

  ##1.5 create bindings (we generate this with an internal function klate)
  var.g <- d <- Dint <- Q <- NA_real_

  ## FITTING ----------------------------------------------------------------
  ##3. Fitting values with nonlinear least-squares estimation of the parameters
  ## set functions for fitting
  ## REMINDER: DO NOT ADD {} brackets, otherwise the formula construction will not
  ## work

  ## get current environment, we need that later
  currn_env <- environment()

  ## Define functions ---------
  ### EXP ------- (C++ version available)
  fit.functionEXP <- function(a,b,c,x) a*(1-exp(-(x+c)/b))

  ### EXP+LIN ----------- (C++ version available)
  fit.functionEXPLIN <- function(a,b,c,g,x) a*(1-exp(-(x+c)/b)+(g*x))

  ### EXP+EXP ---------- (C++ version available)
  fit.functionEXPEXP <- function(a1,a2,b1,b2,x) (a1*(1-exp(-(x)/b1)))+(a2*(1-exp(-(x)/b2)))

  ### GOK ---------------- (C++ version available)
  fit.functionGOK <- function(a,b,c,d,x) a*(d-(1+(1/b)*x*c)^(-1/c))

  ### OTOR -------------
  fit.functionOTOR <- function(R, Dc, N, Dint, x) (1 + (lamW::lambertW0((R - 1) * exp(R - 1 - ((x + Dint) / Dc ))) / (1 - R))) * N

  ### OTORX -------------
  fit.functionOTORX <- function(x, Q, D63, c, a) .D2nN(x, Q, D63, a) * c / .D2nN(TEST_DOSE, Q, D63, a)

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
  num.params <- 4
  if (fit.method %in% c("EXP", "EXP OR LIN"))
    num.params <- 3

  ## if the number of data points is smaller than the number of parameters
  ## to fit, the nls() function gets trapped in an infinite loop
  if (!fit.method %in% c("LIN", "QDR") && nrow(data) < num.params) {
    fit.method <- "LIN"
    msg <- paste("Fitting a non-linear least-squares model requires at least",
                 num.params, "dose points, 'fit.method' changed to 'LIN'")
    .throw_warning(msg)
    if (verbose)
      message("[fit_DoseResponseCurve()] ", msg)
  }

  ## helper to report the fit
  .report_fit <- function(De, ...) {
    if (verbose && mode != "alternate") {
      writeLines(paste0("[fit_DoseResponseCurve()] Fit: ", fit.method,
                        " (", mode,") ", "| De = ", round(abs(De), 2),
                        ...))
    }
  }

  ## helper to report a failure in the fit
  .report_fit_failure <- function(method, mode, ...) {
    if (verbose) {
      writeLines(paste0("[fit_DoseResponseCurve()] Fit failed for ",
                        fit.method, " (", mode, ")"))
    }
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
    fit.lm <- try(stats::lm(suppressWarnings(log(data$y)) ~ data$x),
                  silent = TRUE)

    if (!inherits(fit.lm, "try-error") && !is.na(fit.lm$coefficients[2]))
      b <- as.numeric(1 / fit.lm$coefficients[2])
  }

  ##c - get start parameters from a linear fit - offset on x-axis
  fit.lm <- stats::lm(data$y ~ data$x)
  c <- as.numeric(abs(fit.lm$coefficients[1]/fit.lm$coefficients[2]))

  #take slope from x - y scaling
  g <- max(data[,2]/max(data[,1]))

  #set D01 and D02 (in case of EXP+EXP)
  D01 <- D01.ERROR <- D02 <- D02.ERROR <- NA

  ## Let start parameter vary -------------------------------------------------
  ## to be a little bit more flexible, the start parameters varies within
  ## a normal distribution

  ## draw 50 start values from a normal distribution
  if (fit.method != "LIN") {
    a.MC <- suppressWarnings(rnorm(50, mean = a, sd = a / 100))

    if (!is.na(b)) {
      b.MC <- suppressWarnings(rnorm(50, mean = b, sd = b / 100))
    }

    c.MC <- suppressWarnings(rnorm(50, mean = c, sd = c / 100))
    g.MC <- suppressWarnings(rnorm(50, mean = g, sd = g / 1))

    ##set start vector (to avoid errors within the loop)
    a.start <-   b.start <- c.start <- g.start <- NA

  }

  ## QDR --------------------------------------------------------------------
  if (fit.method == "QDR") {
    ## establish models without and with intercept term
    model.qdr <- stats::update(
      y ~ I(x) + I(x^2),
      stats::reformulate(".", intercept = !fit.force_through_origin))

    if (mode == "interpolation") {
      y <- object[1, 2]
      lower <- 0
    } else if (mode == "extrapolation") {
      y <- 0
      lower <- -1e06
    }
    upper <- max(object[, 1]) * 1.5

    .fit_qdr_model <- function(model, data, y) {
      fit <- stats::lm(model, data = data, weights = fit.weights)

      ## solve and get De
      De <- NA
      if (mode != "alternate") {
        De.fs <- function(fit, x, y) {
          predict(fit, newdata = data.frame(x)) - y
        }
        De.uniroot <- try(uniroot(De.fs, fit = fit, y = y,
                                  lower = lower, upper = upper),
                          silent = TRUE)

        if (!inherits(De.uniroot, "try-error")) {
          De <- De.uniroot$root
        }
      }
      return(list(fit = fit, De = De))
    }

    res <- .fit_qdr_model(model.qdr, data, y)
    fit <- res$fit
    De <- res$De
    if (!inherits(fit, "try-error"))
      .report_fit(De)
    else
      .report_fit_failure(fit.method, mode)

    ##set progressbar
    if(txtProgressBar){
      cat("\n\t Run Monte Carlo loops for error estimation of the QDR fit\n")
      pb <- txtProgressBar(min=0,max=n.MC, char="=", style=3)
    }

    ## Monte Carlo Error estimation
    x.natural <- abs(vapply(1:n.MC, function(i) {
      if (txtProgressBar) setTxtProgressBar(pb, i)
      .fit_qdr_model(
        model = model.qdr,
        data = list(x = xy$x, y = data.MC[, i]),
        y = data.MC.De[i])$De
    }, numeric(1)))

    if(txtProgressBar) close(pb)
  }
  ## EXP --------------------------------------------------------------------
  if (any(fit.method %in% c("EXP", "EXP OR LIN", "LIN"))){
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
      a.start <-  b.start <- c.start <- numeric(50)
      lower_bounds <- c(a = 0, b = 1e-6, c = 0)
      control_settings <-  minpack.lm::nls.lm.control(maxiter = 500)

      ## loop for better attempt
      for(i in 1:50){
        ## get start list
        start_list <- list(a = a.MC[i], b = b.MC[i], c = c.MC[i])

        ## run fit
        fit.initial <- suppressWarnings(try(minpack.lm::nlsLM(
          formula = y ~ fit_functionEXP_cpp(a, b, c, x),
          data = data,
          start = start_list,
          trace = FALSE,
          algorithm = "LM",
          lower = lower_bounds,
          control = control_settings)
        , silent = TRUE))

        if(!inherits(fit.initial, "try-error")){
          #get parameters out of it
          parameters <- coef(fit.initial)
          a.start[i] <- as.vector(parameters["a"])
          b.start[i] <- as.vector(parameters["b"])
          c.start[i] <- as.vector(parameters["c"])
        }
      }

      ##used median as start parameters for the final fitting
      a <- median(a.start, na.rm = TRUE)
      b <- median(b.start, na.rm = TRUE)
      c <- median(c.start, na.rm = TRUE)

      ## exception: if b is 1 it is likely to be wrong and should be reset
      if(!is.na(b) && b == 1)
        b <- mean(b.MC)

      ## set boundaries
      lower <- if (fit.bounds) c(0, 0, 0) else c(-Inf, -Inf, -Inf)
      upper <- if (fit.force_through_origin) c(Inf, Inf, 0) else c(Inf, Inf, Inf)

      #FINAL Fit curve on given values
      fit <- try(minpack.lm::nlsLM(
        formula = y ~ fit_functionEXP_cpp(a, b, c, x),
        data = data,
        start = list(a = a, b = b, c = 0),
        weights = fit.weights,
        trace = FALSE,
        algorithm = "LM",
        lower = lower,
        upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = 500)
      ), silent = TRUE)

      if (inherits(fit, "try-error") & inherits(fit.initial, "try-error")){
        .report_fit_failure(fit.method, mode)

      }else{
        ##this is to avoid the singular convergence failure due to a perfect fit at the beginning
        ##this may happen especially for simulated data
        if(inherits(fit, "try-error") & !inherits(fit.initial, "try-error")){
          fit <- fit.initial
          rm(fit.initial)
        }

        ## replace with formula so that we can have the C++ version
        f <- function(x) .toFormula(fit.functionEXP, env = currn_env)
        fit$m$formula <- f

        #get parameters out of it
        .get_coef(fit)

        #calculate De
        De <- NA
        if(mode == "interpolation"){
          De <- suppressWarnings(-c - b * log(1 - object[1, 2] / a))

          ## account for the fact that we can still calculate a De that is negative
          ## even it does not make sense
          if(!is.na(De) && De < 0)
            De <- NA

        }else if (mode == "extrapolation"){
          De <- suppressWarnings(-c-b*log(1-0/a))
        }

        #print D01 value
        D01 <- b

        .report_fit(De, " | D01 = ", round(D01, 2))

        #EXP MC -----
        ##Monte Carlo Simulation
        #	--Fit many curves and calculate a new De +/- De_Error
        #	--take De_Error

        ## preallocate variable
        var.b <- vector(mode="numeric", length=n.MC)

        #start loop
        for (i in 1:n.MC) {
          fit.MC <- try(minpack.lm::nlsLM(
            formula = y ~ fit_functionEXP_cpp(a, b, c, x),
            data = list(x = xy$x,y = data.MC[,i]),
            start = list(a = a, b = b, c = c),
            weights = fit.weights,
            trace = FALSE,
            algorithm = "LM",
            lower = lower,
            upper = upper,
            control = minpack.lm::nls.lm.control(maxiter = 500)
          ), silent = TRUE
          )

          #get parameters out of it including error handling
          if (!inherits(fit.MC, "try-error") & mode != "alternate") {
            #get parameters out
            parameters <- coef(fit.MC)
            var.a <- as.numeric(parameters["a"]) # Imax
            var.b[i] <- as.numeric(parameters["b"]) # D0
            var.c <- as.numeric(parameters["c"])

            #calculate x.natural for error calculation
            x.natural[i] <- suppressWarnings(
                abs(-var.c - var.b[i] * log(1 - data.MC.De[i] / var.a)))
          }

        }#end for loop

        ##write D01.ERROR
        D01.ERROR <- sd(var.b, na.rm = TRUE)

        ##remove values
        rm(var.b)

      }#endif::try-error fit

    }#endif:fit.method!="LIN"

    ## LIN ------------------------------------------------------------------
    ##two options: just linear fit or LIN fit after the EXP fit failed

    #set fit object, if fit object was not set before
    if (!exists("fit")) fit <- NA

    if ((fit.method=="EXP OR LIN" & inherits(fit, "try-error")) |
        fit.method == "LIN") {

      ## establish models without and with intercept term
      model.lin <- stats::update(y ~ x,
                          stats::reformulate(".", intercept = !fit.force_through_origin))

      if (fit.force_through_origin)
        De.fs <- function(fit, y) y / coef(fit)[1]
      else
        De.fs <- function(fit, y) (y - coef(fit)[1]) / coef(fit)[2]

      y <- object[1, 2]
      if (mode == "extrapolation")
        y <- 0

      .fit_lin_model <- function(model, data, y) {
        fit <- stats::lm(model, data = data, weights = fit.weights)

        ## solve and get De
        De <- NA
        if (mode != "alternate")
          De <- De.fs(fit, y)

        return(list(fit = fit, De = unname(De)))
      }

      res <- .fit_lin_model(model.lin, data, y)
      fit.lm <- res$fit
      De <- res$De
      .report_fit(De)

      ## Monte Carlo Error estimation
      x.natural <- abs(vapply(1:n.MC, function(i) {
        .fit_lin_model(
          model = model.lin,
          data = list(x = xy$x, y = data.MC[, i]),
          y = data.MC.De[i])$De
      }, numeric(1)))

      #correct for fit.method
      fit.method <- "LIN"

      ##set fit object
      if(fit.method == "LIN") fit <- fit.lm

    }else{fit.method<-"EXP"}#endif::LIN
  }#end if EXP (this includes the LIN fit option)

  ## EXP+LIN ----------------------------------------------------------------
  else if (fit.method=="EXP+LIN") {
    ##try some start parameters from the input values to makes the fitting more stable
    for(i in 1:length(a.MC)){
      a <- a.MC[i]
      b <- b.MC[i]
      c <- c.MC[i]
      g <- max(0, g.MC[i])

      ##---------------------------------------------------------##
      ##start: with EXP function
      fit.EXP <- try({
        suppressWarnings(minpack.lm::nlsLM(
        formula = y ~ fit_functionEXP_cpp(a, b, c, x),
        data = data,
        start = c(a=a,b=b,c=c),
        trace = FALSE,
        algorithm = "LM",
        lower = c(a = 0, b = 10, c = 0),
        control = minpack.lm::nls.lm.control(
          maxiter=100)
      ))},
      silent=TRUE)

      if(!inherits(fit.EXP, "try-error")){
        #get parameters out of it
        .get_coef(fit.EXP)

        ##end: with EXP function
        ##---------------------------------------------------------##
      }

      fit <- try({
        suppressWarnings(minpack.lm::nlsLM(
          formula = y ~ fit_functionEXPLIN_cpp(a, b, c, g, x),
          data = data,
          start = c(a=a,b=b,c=c,g=g),
          trace = FALSE,
          algorithm = "LM",
          lower = if(fit.bounds){
            c(a = 0, b = 10, c = 0, g = 0)
          } else {
            c(a = -Inf, b = -Inf,c = -Inf,g = -Inf)
          },
          control = minpack.lm::nls.lm.control(
            maxiter = 500) #increase max. iterations
          ))
        }, silent=TRUE)

      if(!inherits(fit, "try-error")){
        #get parameters out of it
        parameters <- coef(fit)
        a.start[i] <- parameters[["a"]]
        b.start[i] <- parameters[["b"]]
        c.start[i] <- parameters[["c"]]
        g.start[i] <- parameters[["g"]]
      }
    }##end for loop

    ## used mean as start parameters for the final fitting
    a <- median(a.start, na.rm = TRUE)
    b <- median(b.start, na.rm = TRUE)
    c <- median(c.start, na.rm = TRUE)
    g <- median(g.start, na.rm = TRUE)

    ## set boundaries
    lower <- if (fit.bounds) c(0, 10, 0, 0) else rep(-Inf, 4)
    upper <- if (fit.force_through_origin) c(Inf, Inf, 0, Inf) else rep(Inf, 4)

    ##perform final fitting
    fit <- try(suppressWarnings(minpack.lm::nlsLM(
      formula = y ~ fit_functionEXPLIN_cpp(a, b, c, g, x),
      data = data,
      start = list(a = a, b = b,c = c, g = g),
      weights = fit.weights,
      trace = FALSE,
      algorithm = "LM",
      lower = lower,
      upper = upper,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    )), silent = TRUE)

    #if try error stop calculation
    if(!inherits(fit, "try-error")){
      ## replace with formula so that we can have the C++ version
      f <- function(x) .toFormula(fit.functionEXPLIN, env = currn_env)
      fit$m$formula <- f

      #get parameters out of it
      .get_coef(fit)

      #problem: analytically it is not easy to calculate x,
      #use uniroot to solve that problem ... readjust function first
      f.unirootEXPLIN <- function(a, b, c, g, x, LnTn) {
        fit_functionEXPLIN_cpp(a, b, c, g, x) - LnTn
      }

      if (mode == "interpolation") {
        LnTn <- object[1, 2]
        min.val <- 0
      } else if (mode == "extrapolation") {
        LnTn <- 0
        min.val <- -1e6
      }

      De <- NA
      if (mode != "alternate") {
        temp.De <-  try(uniroot(
          f = f.unirootEXPLIN,
          interval = c(min.val, max(xy$x) * 1.5),
          tol = 0.001,
          a = a,
          b = b,
          c = c,
          g = g,
          LnTn = LnTn,
          extendInt = "yes",
          maxiter = 3000
        ),
        silent = TRUE)

        if (!inherits(temp.De, "try-error"))
          De <- temp.De$root

        .report_fit(De)
      }

      ##Monte Carlo Simulation for error estimation
      #	--Fit many curves and calculate a new De +/- De_Error
      #	--take De_Error

      ##set progressbar
      if(txtProgressBar){
        cat("\n\t Run Monte Carlo loops for error estimation of the EXP+LIN fit\n")
        pb <- txtProgressBar(min=0,max=n.MC, char="=", style=3)
      }

      ## start Monte Carlo loops
      for(i in  1:n.MC){
        ##perform MC fitting
        fit.MC <- try(suppressWarnings(minpack.lm::nlsLM(
          formula = y ~ fit_functionEXPLIN_cpp(a, b, c, g, x),
          data = list(x=xy$x,y=data.MC[,i]),
          start = list(a = a, b = b,c = c, g = g),
          weights = fit.weights,
          trace = FALSE,
          algorithm = "LM",
          lower = if (fit.bounds) {
            c(0,10,0,0)
          }else{
            c(-Inf,-Inf,-Inf, -Inf)
          },
          control = minpack.lm::nls.lm.control(maxiter = 500)
        )), silent = TRUE)

        #get parameters out of it including error handling
        if (!inherits(fit.MC, "try-error")) {
          .get_coef(fit.MC, pre = "var.")

          min.val <- 0
          if (mode == "extrapolation")
            min.val <- -1e6

          #problem: analytically it is not easy to calculate x,
          #use uniroot to solve this problem
          temp.De.MC <- try(uniroot(
              f = f.unirootEXPLIN,
              interval = c(min.val, max(xy$x) * 1.5),
              tol = 0.001,
              a = var.a,
              b = var.b,
              c = var.c,
              g = var.g,
              LnTn = data.MC.De[i]
            ),
            silent = TRUE)

          if (!inherits(temp.De.MC, "try-error")) {
            x.natural[i] <- temp.De.MC$root
          }
        }
        ##update progress bar
        if(txtProgressBar) setTxtProgressBar(pb, i)

      }#end for loop

      ##close
      if(txtProgressBar) close(pb)

    }else{
      .report_fit_failure(fit.method, mode)
    } #end if "try-error" Fit Method

  } #End if EXP+LIN
  ## EXP+EXP ----------------------------------------------------------------
  else if (fit.method == "EXP+EXP") {
    ## initialise objects
    a1.start <-  a2.start <- b1.start <- b2.start <- NA

    ## try to create some start parameters from the input values to make the fitting more stable
    for(i in 1:50) {
      a1 <- a.MC[i];b1 <- b.MC[i];
      a2 <- a.MC[i] / 2; b2 <- b.MC[i] / 2

      fit.start <- try({
        minpack.lm::nlsLM(
        formula = y ~ fit_functionEXPEXP_cpp(a1, a2, b1, b2, x),
        data = data,
        start = list(a1 = a1,a2 = a2,b1 = b1,b2 = b2),
        trace = FALSE,
        algorithm = "LM",
        lower = c(a1 = 1e-6, a2 = 1e-6, b1 = 1e-6, b2 = 1e-6),
        control = minpack.lm::nls.lm.control(maxiter = 500))
      }, silent = TRUE)

      if (!inherits(fit.start, "try-error")) {
        #get parameters out of it
        parameters <- coef(fit.start)
        a1.start[i] <- as.vector((parameters["a1"]))
        b1.start[i] <- as.vector((parameters["b1"]))
        a2.start[i] <- as.vector((parameters["a2"]))
        b2.start[i] <- as.vector((parameters["b2"]))
      }
    }

    ##use obtained parameters for fit input
    a1.start <- median(a1.start, na.rm = TRUE)
    b1.start <- median(b1.start, na.rm = TRUE)
    a2.start <- median(a2.start, na.rm = TRUE)
    b2.start <- median(b2.start, na.rm = TRUE)

    ## set fit bounds
    lower <- if (fit.bounds) rep(0, 4) else rep(-Inf, 4)

    ##perform final fitting
    fit <- try(minpack.lm::nlsLM(
      formula = .toFormula(fit.functionEXPEXP, env = currn_env),
      data = data,
      start = list(a1 = a1, b1 = b1, a2 = a2, b2 = b2),
      weights = fit.weights,
      trace = FALSE,
      algorithm = "LM",
      lower = lower,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), silent = TRUE)

    ##insert if for try-error
    if (!inherits(fit, "try-error")) {
      #get parameters out of it
      .get_coef(fit)

      ##set D0 values
      D01 <- round(b1,digits = 2)
      D02 <- round(b2,digits = 2)

      #problem: analytically it is not easy to calculate x, use uniroot
      De <- NA
      if (mode == "interpolation") {
        f.unirootEXPEXP <-
          function(a1, a2, b1, b2, x, LnTn) {
            fit_functionEXPEXP_cpp(a1, a2, b1, b2, x) - LnTn
          }

        temp.De <-  try(uniroot(
          f = f.unirootEXPEXP,
          interval = c(0, max(xy$x) * 1.5),
          tol = 0.001,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          LnTn = object[1, 2],
          extendInt = "yes",
          maxiter = 3000
        ),
        silent = TRUE)

        if (!inherits(temp.De, "try-error")) {
          De <- temp.De$root
        }

        ##remove object
        rm(temp.De)

      }else if (mode == "extrapolation"){
        .throw_error("Mode 'extrapolation' for fitting method 'EXP+EXP' ",
                     "not supported")
      }

      #print D0 and De value values
      .report_fit(De, " | D01 = ", D01, " | D02 = ", D02)

      ##Monte Carlo Simulation for error estimation
      #	--Fit many curves and calculate a new De +/- De_Error
      #	--take De_Error from the simulation
      # --comparison of De from the MC and original fitted De gives a value for quality

      ##progress bar
      if(txtProgressBar){
        cat("\n\t Run Monte Carlo loops for error estimation of the EXP+EXP fit\n")
        pb <- txtProgressBar(min=0,max=n.MC, initial=0, char="=", style=3)
      }

      #set variables
      var.b1 <- var.b2 <- vector(mode="numeric", length=n.MC)

      ## start Monte Carlo loops
      for (i in 1:n.MC) {
        #update progress bar
        if(txtProgressBar) setTxtProgressBar(pb,i)

        ##perform final fitting
        fit.MC <- try(minpack.lm::nlsLM(
          formula = y ~ fit_functionEXPEXP_cpp(a1, a2, b1, b2, x),
          data = list(x=xy$x,y=data.MC[,i]),
          start = list(a1 = a1, b1 = b1, a2 = a2, b2 = b2),
          weights = fit.weights,
          trace = FALSE,
          algorithm = "LM",
          lower = lower,
          control = minpack.lm::nls.lm.control(maxiter = 500)
        ), silent = TRUE)

        #get parameters out of it including error handling
        if (!inherits(fit.MC, "try-error")) {
          parameters <- coef(fit.MC)
          var.a1 <- as.numeric(parameters["a1"])
          var.a2 <- as.numeric(parameters["a2"])
          var.b1[i] <- as.vector((parameters["b1"]))
          var.b2[i] <- as.vector((parameters["b2"]))

          #problem: analytically it is not easy to calculate x, here an simple approximation is made
          temp.De.MC <-  try(uniroot(
            f = f.unirootEXPEXP,
            interval = c(0,max(xy$x) * 1.5),
            tol = 0.001,
            a1 = var.a1,
            a2 = var.a2,
            b1 = var.b1[i],
            b2 = var.b2[i],
            LnTn = data.MC.De[i]
          ), silent = TRUE)

          if (!inherits(temp.De.MC, "try-error"))
            x.natural[i] <- temp.De.MC$root

        } #end if "try-error" MC simulation
      } #end for loop

      ##write D01.ERROR
      D01.ERROR <- sd(var.b1, na.rm = TRUE)
      D02.ERROR <- sd(var.b2, na.rm = TRUE)

      ##remove values
      rm(var.b1, var.b2)

    }else{
      .report_fit_failure(fit.method, mode)
    } #end if "try-error" Fit Method

    ##close
    if(txtProgressBar) if(exists("pb")){close(pb)}
  }

  ## GOK --------------------------------------------------------------------
  else if (fit.method[1] == "GOK") {
    ## set bounds
    lower <- if (fit.bounds) rep(0, 4) else rep(-Inf, 4)
    upper <- if (fit.force_through_origin) c(Inf, Inf, Inf, 1) else rep(Inf, 4)

    fit <- try(minpack.lm::nlsLM(
      formula = .toFormula(fit.functionGOK, env = currn_env),
      data = data,
      start = list(a = a, b = b, c = 1, d = 1),
      weights = fit.weights,
      trace = FALSE,
      algorithm = "LM",
      lower = lower,
      upper = upper,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), silent = TRUE)

    if (inherits(fit, "try-error")){
      .report_fit_failure(fit.method, mode)

    }else{
      #get parameters out of it
      .get_coef(fit)

      #calculate De
      y <- object[1, 2]
      De <- switch(
        mode,
        "interpolation" = suppressWarnings(
          -(b * (( (a * d - y)/a)^c - 1) * ( ((a * d - y)/a)^-c  )) / c),
        "extrapolation" = suppressWarnings(
          -(b * (( (a * d - 0)/a)^c - 1) * ( ((a * d - 0)/a)^-c  )) / c),
        NA)

      #print D01 value
      D01 <- b

      .report_fit(De, " | D01 = ", round(D01, 2), " | c = ", round(c, 2))

      #EXP MC -----
      ##Monte Carlo Simulation
      #	--Fit many curves and calculate a new De +/- De_Error
      #	--take De_Error

      ## preallocate variable
      var.b <- vector(mode = "numeric", length = n.MC)

      #start loop
      for (i in 1:n.MC) {
        ##set data set
        fit.MC <- try({
          minpack.lm::nlsLM(
          formula = y ~ fit_functionGOK_cpp(a, b, c, d, x),
          data = list(x = xy$x,y = data.MC[,i]),
          start = list(a = a, b = b, c = 1, d = 1),
          weights = fit.weights,
          trace = FALSE,
          algorithm = "LM",
          lower = lower,
          upper = upper,
          control = minpack.lm::nls.lm.control(maxiter = 500)
        )}, silent = TRUE)

        # get parameters out of it including error handling
        if (!inherits(fit.MC, "try-error") && mode != "alternate") {
          # get parameters out
          parameters<-coef(fit.MC)
          var.a <- as.numeric(parameters["a"]) #Imax
          var.b[i] <- as.numeric(parameters["b"]) #D0
          var.c <- as.numeric(parameters["c"]) #kinetic order modifier
          var.d <- as.numeric(parameters["d"]) #origin

          # calculate x.natural for error calculation
          ## note that data.MC.De contains only 0s for extrapolation
          temp <- (var.a * var.d - data.MC.De[i]) / var.a
          x.natural[i] <- suppressWarnings(
              abs(-(var.b[i] * (temp^var.c - 1) * (temp^-var.c)) / var.c)
          )
        }

      }#end for loop

      ##write D01.ERROR
      D01.ERROR <- sd(var.b, na.rm = TRUE)

      ##remove values
      rm(var.b)
    }
  }

  ## OTOR ---------------------------------------------------------------
  else if (fit.method == "OTOR") {
    Dint_lower <- 0.01
    if(mode == "extrapolation")
      Dint_lower <- 50 ##TODO - fragile ... however it is only used by a few

    ## set bounds
    lower <- if (fit.bounds) c(0, 0, 0, Dint_lower) else rep(-Inf, 4)
    upper <- if (fit.force_through_origin) c(10, Inf, Inf, 0) else c(10, Inf, Inf, Inf)

    fit <- try(minpack.lm::nlsLM(
          formula = .toFormula(fit.functionOTOR, env = currn_env),
          data = data,
          start = list(R = 0, Dc = b, N = b, Dint = 0),
          weights = fit.weights,
          trace = FALSE,
          algorithm = "LM",
          lower = lower,
          upper = upper,
          control = minpack.lm::nls.lm.control(maxiter = 500)
        ), silent = TRUE)

    if (inherits(fit, "try-error")) {
      .report_fit_failure(fit.method, mode)

    } else {
          #get parameters out of it
         .get_coef(fit)

          #calculate De
          De <- NA
          if(mode == "interpolation"){
             De <- try(suppressWarnings(stats::uniroot(
               f = function(x, R, Dc, N, Dint, LnTn) {
                 fit.functionOTOR(R, Dc, N, Dint, x) - LnTn},
               interval = c(0, max(object[[1]]) * 1.2),
               R = R,
               Dc = Dc,
               N = N,
               Dint = Dint,
               LnTn = object[1, 2])$root), silent = TRUE)

          }else if (mode == "extrapolation"){
            De <- try(suppressWarnings(stats::uniroot(
              f = function(x, R, Dc, N, Dint) {
                fit.functionOTOR(R, Dc, N, Dint, x)},
              interval = c(-max(object[[1]]), 0),
              R = R,
              Dc = Dc,
              N = N,
              Dint = Dint)$root), silent = TRUE)

            ## there are cases where the function cannot calculate the root
            ## due to its shape, here we have to use the minimum
            if(inherits(De, "try-error")){
              .throw_warning(
                  "Standard root estimation using stats::uniroot() failed. ",
                  "Using stats::optimize() instead, which may lead, however, ",
                  "to unexpected and inconclusive results for fit.method = 'OTOR'")

              De <- try(suppressWarnings(stats::optimize(
                f = function(x, R, Dc, N, Dint) {
                  fit.functionOTOR(R, Dc, N, Dint, x)},
                interval = c(-max(object[[1]]), 0),
                R = R,
                Dc = Dc,
                N = N,
                Dint = Dint)$minimum), silent = TRUE)
            }
          }

          if (inherits(De, "try-error")) De <- NA # nocov

          .report_fit(De, " | R = ", round(R, 2), " | Dc = ", round(Dc, 2))

          #OTOR MC -----
          ##Monte Carlo Simulation
          #	--Fit many curves and calculate a new De +/- De_Error
          #	--take De_Error
          #set variables
          var.Dc <- vector(mode = "numeric", length = n.MC)

          #start loop
          for (i in 1:n.MC) {
            ##set data set
            fit.MC <- try(minpack.lm::nlsLM(
              formula = .toFormula(fit.functionOTOR, env = currn_env),
              data = list(x = xy$x,y = data.MC[,i]),
              start = list(R = 0, Dc = b, N = 0, Dint = 0),
              weights = fit.weights,
              trace = FALSE,
              algorithm = "LM",
              lower = if (fit.bounds) c(0, 0, 0, Dint*runif(1,0,2)) else c(-Inf,-Inf,-Inf, -Inf),
              upper = upper,
              control = minpack.lm::nls.lm.control(maxiter = 500)
            ), silent = TRUE)

            # get parameters out of it including error handling
            if (!inherits(fit.MC, "try-error")) {
              # get parameters out
              parameters <- coef(fit.MC)
              var.R <- as.numeric(parameters["R"])
              var.Dc[i] <- as.numeric(parameters["Dc"])
              var.N <- as.numeric(parameters["N"])
              var.Dint <- as.numeric(parameters["Dint"])

              # calculate x.natural for error calculation
              if(mode == "interpolation"){
                try <- try({
                  suppressWarnings(stats::uniroot(
                  f = function(x, R, Dc, N, Dint, LnTn) {
                    fit.functionOTOR(R, Dc, N, Dint, x) - LnTn},
                  interval = c(0, max(object[[1]]) * 1.2),
                  R = var.R,
                  Dc = var.Dc[i],
                  N = var.N,
                  Dint = var.Dint,
                  LnTn = data.MC.De[i])$root)
                }, silent = TRUE)

              } else if (mode == "extrapolation"){
                try <- try(
                  suppressWarnings(stats::uniroot(
                    f = function(x, R, Dc, N, Dint) {
                      fit.functionOTOR(R, Dc, N, Dint, x)},
                    interval = c(-max(object[[1]]), 0),
                    R = var.R,
                    Dc = var.Dc[i],
                    N = var.N,
                    Dint = var.Dint)$root),
                  silent = TRUE)

                if(inherits(try, "try-error")){
                  try <- try(suppressWarnings(stats::optimize(
                    f = function(x, R, Dc, N, Dint) {
                      fit.functionOTOR(R, Dc, N, Dint, x)},
                    interval = c(-max(object[[1]]), 0),
                    R = var.R,
                    Dc = var.Dc[i],
                    N = var.N,
                    Dint = var.Dint)$minimum),
                    silent = TRUE)
                }
              }##endif extrapolation
              if(!inherits(try, "try-error") && !inherits(try, "function"))
                x.natural[i] <- try
            }

          }#end for loop

          ##we need absolute numbers
          x.natural <- abs(x.natural)

          ##write Dc.ERROR
          Dc.ERROR <- sd(var.Dc, na.rm = TRUE)

          ##remove values
          rm(var.Dc)

    }#endif::try-error fit

  }  ## OTORX ---------------------------------------------------------------
  else if (fit.method == "OTORX") {
    if(is.null(object$Test_Dose) || all(object$Test_Dose == -1))
      .throw_error("Column 'Test_Dose' missing but mandatory for 'OTORX' fitting!")

    ## we need a test dose; the default value is -1 because an NA will cause
    ## additional problems
      TEST_DOSE <- object$Test_Dose[[1]]

      ## here we replace TEST_DOSE by an evaluated value
      ## in the function body; this makes things ALOT easier below
      body(fit.functionOTORX) <- do.call(
        substitute, list(body(fit.functionOTORX), list(TEST_DOSE = TEST_DOSE)))

    ## set boundaries
    lower <- if (fit.bounds) c(0, 0, 0, 0) else rep(-Inf, 4)
    upper <- c(Inf, Inf, Inf, Inf)

      ## correct boundaries for origin forced through zero
      if (fit.force_through_origin[1] & mode == "interpolation")
        lower[4] <- upper[4] <- 0

    fit <- try(minpack.lm::nlsLM(
      formula = .toFormula(fit.functionOTORX, env = currn_env),
      data = data,
      start = list(Q = 1, D63 = b, c = 1, a = 1),
      weights = fit.weights,
      trace = FALSE,
      algorithm = "LM",
      lower = lower,
      upper = upper,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), silent = TRUE)

    if (inherits(fit, "try-error")) {
      .report_fit_failure(fit.method, mode)

    } else {
      #get parameters out of it
      .get_coef(fit)

      #calculate De
      De <- NA
      if(mode == "interpolation"){
        De <- try(suppressWarnings(stats::uniroot(
          f = function(x, Q, D63, c, a, LnTn) {
            fit.functionOTORX(x, Q, D63, c, a) - LnTn},
          interval = c(0, max(object[[1]]) * 1.2),
          Q = Q,
          D63 = D63,
          c = c,
          a = a,
          LnTn = object[1, 2])$root), silent = TRUE)

      }else if (mode == "extrapolation"){
        De <- try(suppressWarnings(stats::uniroot(
          f = function(x, Q, D63, c, a) {
            fit.functionOTORX(x, Q, D63, c, a)},
          interval = c(-max(object[[1]]), 0),
          Q = Q,
          D63 = D63,
          c = c,
          a = a)$root), silent = TRUE)

        ## there are cases where the function cannot calculate the root
        ## due to its shape, here we have to use the minimum
        if(inherits(De, "try-error")){
          .throw_warning(
            "Standard root estimation using stats::uniroot() failed. ",
            "Using stats::optimize() instead, which may lead, however, ",
            "to unexpected and inconclusive results for fit.method = 'OTORX'")

          De <- try(suppressWarnings(stats::optimize(
            f = function(x, Q, D63, c, a) {
              fit.functionOTORX(x, Q, D63, c, a)},
            interval = c(-max(object[[1]]), 0),
            Q = Q,
            D63 = D63,
            c = c,
            a = a)$minimum), silent = TRUE)
        }
      }

      if (inherits(De, "try-error")) De <- NA # nocov

      .report_fit(De, " | Q = ", round(Q, 2), " | D63 = ", round(D63, 2))

      #OTORX MC -----
      ##Monte Carlo Simulation
      #	--Fit many curves and calculate a new De +/- De_Error
      #	--take De_Error
      #set variables
      var.D63 <- vector(mode = "numeric", length = n.MC)

      #start loop
      for (i in 1:n.MC) {
        ##set data set
        fit.MC <- try(minpack.lm::nlsLM(
          formula = .toFormula(fit.functionOTORX, env = currn_env),
          data = list(x = xy$x,y = data.MC[,i]),
          start = list(Q = 1, D63 = b, c = 1, a = 1),
          weights = fit.weights,
          trace = FALSE,
          algorithm = "LM",
          lower = lower,
          upper = upper,
          control = minpack.lm::nls.lm.control(maxiter = 500)
        ), silent = TRUE)

        # get parameters out of it including error handling
        if (!inherits(fit.MC, "try-error")) {
          # get parameters out
          parameters<-coef(fit.MC)
          var.Q <- as.numeric(parameters["Q"])
          var.D63[i] <- as.numeric(parameters["D63"])
          var.c <- as.numeric(parameters["c"])
          var.a <- as.numeric(parameters["a"])

          # calculate x.natural for error calculation
          if(mode == "interpolation"){
            try <- try(
              {suppressWarnings(stats::uniroot(
                f = function(x, Q, D63, c, a, LnTn) {
                  fit.functionOTORX(x, Q, D63, c, a) - LnTn},
                interval = c(0, max(object[[1]]) * 1.2),
                Q = var.Q,
                D63 = var.D63[i],
                c = var.c,
                a = var.a,
                LnTn = data.MC.De[i])$root)
              }, silent = TRUE)

          }else if(mode == "extrapolation"){
            try <- try(
              suppressWarnings(stats::uniroot(
                f = function(x, Q, D63, c, a, LnTn) {
                  fit.functionOTORX(x, Q, D63, c, a, x)},
                interval = c(-max(object[[1]]), 0),
                Q = var.Q,
                D63 = var.D63[i],
                c = var.c,
                a = var.a)$root),
              silent = TRUE)

            if(inherits(try, "try-error")){
              try <- try(suppressWarnings(stats::optimize(
                f = function(x, Q, D63, c, a) {
                  fit.functionOTOR(x, Q, D63, c, a)},
                interval = c(-max(object[[1]]), 0),
                Q = var.R,
                D63 = var.Dc[i],
                c = var.c,
                a = var.a)$minimum),
                silent = TRUE)
            }
          }##endif extrapolation
          if(!inherits(try, "try-error") && !inherits(try, "function"))
            x.natural[i] <- try
        }

      }#end for loop

      ##we need absolute numbers
      x.natural <- abs(x.natural)

      ##write Dc.ERROR
      D63.ERROR <- sd(var.D63, na.rm = TRUE)

      ##remove values
      rm(var.D63)

    }#endif::try-error fit
  }#End if fit.method selection (for all)

  #Get De values from Monte Carlo simulation

  #calculate mean and sd (ignore NaN values)
  De.MonteCarlo <- mean(x.natural, na.rm = TRUE)

  #De.Error is Error of the whole De (ignore NaN values)
  De.Error <- sd(x.natural, na.rm = TRUE)

  # Formula creation --------------------------------------------------------
  ## This information is part of the fit object output anyway, but
  ## we keep it here for legacy reasons
  fit_formula <- NA
  if(!inherits(fit, "try-error") && !is.na(fit[1]))
    fit_formula <- .replace_coef(fit)

# Output ------------------------------------------------------------------
  ##calculate HPDI
  HPDI <- matrix(c(NA,NA,NA,NA), ncol = 4)
  if (!anyNA(x.natural)) {
    HPDI <- cbind(
      .calc_HPDI(x.natural, prob = 0.68)[1, ,drop = FALSE],
      .calc_HPDI(x.natural, prob = 0.95)[1, ,drop = FALSE])
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

  output <- try(data.frame(
    De = abs(De),
    De.Error = De.Error,
    D01 = D01,
    D01.ERROR = D01.ERROR,
    D02 = D02,
    D02.ERROR = D02.ERROR,
    Dc = Dc,
    D63 = D63,
    n_N = n_N,
    De.MC = De.MonteCarlo,
    De.plot = De, # no absolute value, used for plotting
    Fit = fit.method,
    HPDI68_L = HPDI[1,1],
    HPDI68_U = HPDI[1,2],
    HPDI95_L = HPDI[1,3],
    HPDI95_U = HPDI[1,4],
    row.names = NULL
  ), silent = TRUE)

  ##make RLum.Results object
  output.final <- set_RLum(
    class = "RLum.Results",
    data = list(
      De = output,
      De.MC = x.natural,
      Fit = fit,
      Fit.Args = list(
          object = object,
          fit.method = fit.method,
          mode = mode,
          fit.force_through_origin = fit.force_through_origin,
          fit.includingRepeatedRegPoints = fit.includingRepeatedRegPoints,
          fit.NumberRegPoints = fit.NumberRegPoints,
          fit.NumberRegPointsReal = fit.NumberRegPointsReal,
          fit.weights = fit.weights,
          fit.bounds = fit.bounds,
          n.MC = n.MC
      ),
      Formula = fit_formula
    ),
    info = list(
      call = sys.call()
    )
  )
  invisible(output.final)
}

# Helper functions in fit_DoseResponseCurve() -------------------------------------

#'@title Returns coefficient into parent environment
#'
#'@description Write fitting coefficients into parent environment
#'
#'@param x [stats::nls] (**required**): the fitting output
#'
#'@param pre [character] (*with default*): names prefix
#'
#'@param sufx [character] (*with default*): names suffix
#'
#'@returns New objects into the parent environment
#'
#'@md
#'@noRd
.get_coef <- function(x, pre = "", sufx = "") {
  ## get coefficients and set their names
  tmp <- stats::coef(x)
  names(tmp) <- paste0(pre, names(tmp), sufx)

  ## assign to parent frame
  for (name in names(tmp))
    assign(name, as.vector(tmp[name]), pos = parent.frame())
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
#'@md
#'@noRd
.replace_coef <- function(f) {
  ## get formula as character string
  if(inherits(f, "nls")) {
    str <- as.character(f$m$formula())[3]
    param <- coef(f)
  } else {
    str <- "a * x + b * x^2 + n"
    param <- c(n = 0, a = 0, b = 0)
     if(!"(Intercept)" %in% names(coef(f)))
      param[2:(length(coef(f))+1)] <- coef(f)
    else
      param[1:length(coef(f))] <- coef(f)
  }

  ## if the following assertion is triggered, it means that we have used a C++
  ## function to implement the model but forgot to replace the formula in the
  ## fit object, which can be done with these lines:
  ##   f <- function(x) .toFormula(fit.functionXXX, env = currn_env)
  ##   fit$m$formula <- f
  stopifnot(grepl("^fit_function", str) == FALSE)

  ## replace parameters with fitted coefficients
  for (i in 1:length(param)) {
    str <- gsub(
      pattern = names(param)[i],
      replacement = format(param[i], digits = 3, scientific = TRUE),
      x = str,
      fixed = TRUE)
  }

  ## return
  return(parse(text = str))
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
#'@md
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
#'@md
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
#'@param a [numeric] (**required**): offset parameter
#'
#'@references https://github.com/jll2/LumDRC/blob/main/otorx.py
#'
#'@md
#'@noRd
.D2nN <- function(D, Q, D63, a) {
  if(all(abs(Q) < 1e-06))
    r <- 1 - exp(-D/D63)
  else if (any(abs(Q) < 1e-06))
    stop("[.D2nN()] Unsupported zero and non-zero Q", .call = FALSE)
  else
    r <- 1 + (lamW::lambertW0(-Q * exp(-Q-(1-Q*(1-1/exp(1))) * (D + a) /D63))) / Q

  return(r)
}
