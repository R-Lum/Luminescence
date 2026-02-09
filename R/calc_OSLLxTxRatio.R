#' @title Calculate `Lx/Tx` ratio for CW-OSL curves
#'
#' @description
#' Calculate `Lx/Tx` ratios from a given set of CW-OSL curves assuming late light
#' background subtraction.
#'
#' @details
#' The function checks the integrity of the values chosen for the signal and
#' background integrals; the signal integral limits have to be lower than
#' the background integral limits. If a [vector] is given as input instead
#' of a [data.frame], an artificial [data.frame] is produced. The
#' error calculation is done according to Galbraith (2002).
#'
#' **Please note:** In cases where the calculation results in `NaN` values (for
#' example due to zero-signal, and therefore a division of 0 by 0), these `NaN`
#' values are replaced by 0.
#'
#' **`sigmab`**
#'
#' The default value of `sigmab` is calculated assuming the background is
#' constant and **would not** applicable when the background varies as,
#' e.g., as observed for the early light subtraction method.
#'
#' **`sig0`**
#'
#' This argument allows to add an extra component of error to the final `Lx/Tx`
#' error value. The input will be treated as factor that is multiplied with
#' the already calculated `LxTx` and the result is add up by:
#'
#' \deqn{se(LxTx) = \sqrt(se(LxTx)^2 + (LxTx * sig0)^2)}
#'
#' **`SN_RATIO_LnLx` and `SN_RATIO_TnTx`**
#'
#' For convenience, the function returns the signal-to-noise ratio (`SN_RATIO`)
#' for the `LnLx` and the `TnTx` curves. This is simply the signal divided
#' by the background signal counts normalised to the `k` value (see below).
#'
#' **`background.count.distribution`**
#'
#' This argument allows selecting the distribution assumption that is used for
#' the error calculation. According to Galbraith (2002, 2014) the background
#' counts may be overdispersed (i.e. do not follow a Poisson distribution,
#' which is assumed for the photomultiplier counts). In that case (might be the
#' normal case) it has to be accounted for the overdispersion by estimating
#' \eqn{\sigma^2} (i.e. the overdispersion value). Therefore the relative
#' standard error is calculated as:
#'
#' - `poisson`
#' \deqn{rse(\mu_{S}) \approx \sqrt(Y_{0} + Y_{1}/k^2)/Y_{0} - Y_{1}/k}
#'
#' - `non-poisson`
#' \deqn{rse(\mu_{S}) \approx \sqrt(Y_{0} + Y_{1}/k^2 + \sigma^2(1+1/k))/Y_{0} - Y_{1}/k}
#'
#' **Please note** that when using the early background subtraction method in
#' combination with the 'non-poisson' distribution argument, the corresponding `Lx/Tx` error
#' may considerably increase due to a high `sigmab` value.
#' Please check whether this is valid for your data set and if necessary
#' consider to provide an own `sigmab` value using the corresponding argument `sigmab`.
#'
#' @param Lx.data [Luminescence::RLum.Data.Curve-class], [data.frame], [list] (**required**):
#' requires a CW-OSL shine down curve (x = time, y = counts). Data can also be
#' provided as a list.
#'
#' @param Tx.data [Luminescence::RLum.Data.Curve-class] or [data.frame] (*optional*):
#' requires a CW-OSL shine down curve (x = time, y = counts). If no
#' input is given the `Tx.data` will be treated as `NA` and no `Lx/Tx` ratio
#' is calculated. When `Lx.data` is a list, it must be provided as a list of
#' the same length.
#'
#' @param signal_integral [integer] (**required**):
#' vector of channels for the signal integral. If set to `NA` (alternate mode),
#' no integrals are taken into account and their settings are ignored.
#'
#' @param background_integral [integer] (**required**):
#' vector of channels for the background integral. If set to `NA`, no
#' background integral is subtracted; in this case, the error calculation and
#' the signal-to-noise ratio will report `NA` values.
#'
#' @param signal_integral_Tx [integer] (*optional*):
#' vector of channels for the signal integral for the `Tx` curve.
#' If `NULL`, the `signal_integral` vector is used.
#'
#' @param background_integral_Tx [integer] (*optional*):
#' vector of channels for the background integral for the `Tx` curve.
#' If `NULL`, the `background_integral` vector is used. If set to `NA`, no
#' background integral for the `Tx` curve is subtracted; in this case, the
#' error calculation and the signal-to-noise ratio will report `NA` values.
#'
#' @param background.count.distribution [character] (*with default*):
#' sets the count distribution assumed for the error calculation.
#' Possible arguments are `"poisson"` or `"non-poisson"` (default). See
#' details for further information.
#'
#' @param use_previousBG [logical] (*with default*):
#' If set to `TRUE` the background of the `Lx`-signal is subtracted also
#' from the `Tx`-signal. Please note that in this case separate
#' signal integral limits for the `Tx`-signal are not allowed and will be reset.
#'
#' @param sigmab [numeric] (*optional*):
#' option to set a manual value for the overdispersion (for `LnTx` and `TnTx`),
#' used for the `Lx/Tx` error calculation. The value should be provided as
#' absolute squared count values, e.g. `sigmab = c(300,300)`.
#' **Note:** If only one value is provided this value is taken for both (`LnTx` and `TnTx`) signals.
#'
#' @param sig0 [numeric] (*with default*):
#' allow adding an extra component of error to the final `Lx/Tx` error value
#' (e.g., instrumental error, see details).
#'
#' @param digits [integer] (*with default*):
#' round numbers to the specified digits. If set to `NULL` no rounding occurs.
#'
#' @param ... currently not used.
#'
#' @return
#' Returns an S4 object of type [Luminescence::RLum.Results-class].
#'
#' Slot `data` contains a [list] with the following structure:
#'
#' **@data**
#' ```
#' $LxTx.table (data.frame)
#' .. $ LnLx
#' .. $ LnLx.BG
#' .. $ TnTx
#' .. $ TnTx.BG
#' .. $ Net_LnLx
#' .. $ Net_LnLx.Error
#' .. $ Net_TnTx
#' .. $ Net_TnTx.Error
#' .. $ SN_RATIO_LnLx,
#' .. $ SN_RATIO_TnTx,
#' .. $ LxTx
#' .. $ LxTx.Error
#' $ calc.parameters (list)
#' .. $ sigmab.LnTx
#' .. $ sigmab.TnTx
#' .. $ k
#' ```
#'
#' **@info**
#' ```
#' $ call (original function call)
#' ```
#'
#' @note
#' The results of this function have been cross-checked with the Analyst
#' (version 3.24b). Access to the results object via [Luminescence::get_RLum].
#'
#' **Caution:** If you are using early light subtraction (EBG), please either provide your
#' own `sigmab` value or use `background.count.distribution = "poisson"`.
#'
#' @section Function version: 0.9.4
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::fit_DoseResponseCurve],
#' [Luminescence::analyse_SAR.CWOSL]
#'
#' @references Duller, G., 2018. Analyst v4.57 - User Manual.
#' `https://users.aber.ac.uk/ggd`\cr
#'
#' Galbraith, R.F., 2002. A note on the variance of a background-corrected OSL
#' count. Ancient TL, 20 (2), 49-51. \doi{10.26034/la.atl.2002.348}
#'
#' Galbraith, R.F., 2014. A further note on the variance of a
#' background-corrected OSL count. Ancient TL, 31 (2), 1-3. \doi{10.26034/la.atl.2014.477}
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.LxTxOSLData, envir = environment())
#'
#' ##calculate Lx/Tx ratio
#' results <- calc_OSLLxTxRatio(
#'  Lx.data = Lx.data,
#'  Tx.data = Tx.data,
#'  signal_integral = 1:2,
#'  background_integral = 85:100)
#'
#' ##get results object
#' get_RLum(results)
#'
#' @export
calc_OSLLxTxRatio <- function(
  Lx.data,
  Tx.data = NULL,
  signal_integral = NULL,
  background_integral = NULL,
  signal_integral_Tx = NULL,
  background_integral_Tx = NULL,
  background.count.distribution = "non-poisson",
  use_previousBG = FALSE,
  sigmab = NULL,
  sig0 = 0,
  digits = NULL,
  ...
) {
  .set_function_name("calc_OSLLxTxRatio")
  on.exit(.unset_function_name(), add = TRUE)

  ## Self-call --------------------------------------------------------------
  if (inherits(Lx.data, "list")) {
    if (!is.null(Tx.data) &&
        (!inherits(Tx.data, "list") || length(Lx.data) != length(Tx.data))) {
      .throw_error("When 'Lx.data' is a list, 'Tx.data' should either be ",
                   "a list of the same length or NULL")
    }
    ret <- lapply(seq_along(Lx.data), function(x) {
      calc_OSLLxTxRatio(Lx.data[[x]],
                        Tx.data[[x]],
                        signal_integral = signal_integral,
                        background_integral = background_integral,
                        signal_integral_Tx = signal_integral_Tx,
                        background_integral_Tx = background_integral_Tx,
                        background.count.distribution = background.count.distribution,
                        use_previousBG = use_previousBG,
                        sigmab = sigmab,
                        sig0 = sig0,
                        digits = digits,
                        ...)
    })

    return(ret)
  }

  ## Integrity checks -------------------------------------------------------
  valid.classes <- c("RLum.Data.Curve", "data.frame", "matrix", "numeric")
  .validate_class(Lx.data, valid.classes, extra = "a list of such objects")
  .validate_not_empty(Lx.data)
  .validate_class(Tx.data, valid.classes, null.ok = TRUE,
                  extra = "a list of such objects")
  .validate_class(sigmab, "numeric", null.ok = TRUE, length = 1:2)
  .validate_nonnegative_scalar(sig0)

  .coerce <- function(data) {
    data <- switch(
        class(data)[1],
        RLum.Data.Curve = as(data, "data.frame"),
        matrix = as.data.frame(data),
        data.frame = data,
        data.frame(x = 1:length(data), y = data)
    )
    if (ncol(data) < 2)
      .throw_error(.first_argument(), " should have 2 columns")
    data
  }

  ## Lx - coerce if required
  Lx.data <- .coerce(Lx.data)
  len.Lx <- nrow(Lx.data)

  ## Tx - coerce if required
  if(!is.null(Tx.data)){
    .validate_not_empty(Tx.data)
    Tx.data <- .coerce(Tx.data)
    len.Tx <- nrow(Tx.data)

    ## check channel number
    if (len.Lx != len.Tx) {
      .throw_error("Different number of channels for Lx (", len.Lx, ") ",
                   "and Tx (", len.Tx, ")")
    }
  } else {
    Tx.data <- data.frame(x = NA,y = NA)
  }

  ## deprecated arguments
  extraArgs <- list(...)
  if (any(grepl("[signal|background]\\.integral\\.?[.Tx]?", names(extraArgs))) &&
      !is.null(c(extraArgs$signal.integral, extraArgs$signal.integral.Tx,
                 extraArgs$background.integral, extraArgs$background.integral.Tx))) {
    .deprecated(old = c("signal.integral", "background.integral",
                        "signal.integral.Tx", "background.integral.Tx"),
                new = c("signal_integral", "background_integral",
                        "signal_integral_Tx", "background_integral_Tx"),
                since = "1.2.0")
    signal_integral <- extraArgs$signal.integral
    background_integral <- extraArgs$background.integral
    signal_integral_Tx <- extraArgs$signal.integral.Tx
    background_integral_Tx <- extraArgs$background.integral.Tx
  }

  # Alternate mode ----------------------------------------------------------
  if (.strict_na(signal_integral)) {
    LnLx <- sum(Lx.data[,2])
    TnTx <- sum(Tx.data[,2])

    LnLxTnTx <- data.frame(
      LnLx = LnLx,
      LnLx.BG = 0,
      TnTx = TnTx,
      TnTx.BG = 0,
      Net_LnLx = LnLx,
      Net_LnLx.Error = 0,
      Net_TnTx = TnTx,
      Net_TnTx.Error = 0,
      SN_RATIO_LnLx = 0,
      SN_RATIO_TnTx = 0,
      LxTx = LnLx/TnTx,
      LxTx.Error = 0)

    return(set_RLum(
      class = "RLum.Results",
      data = list(
        LxTx.table = LnLxTnTx,
        calc.parameters = NULL,
      info = list(call = sys.call())
    )))
  }

  # Continue checks ---------------------------------------------------------
  signal_integral <- .validate_integral(signal_integral, na.ok = TRUE,
                                        max = len.Lx)
  background_integral <- .validate_integral(background_integral, na.ok = TRUE,
                                            min = max(signal_integral) + 1,
                                            max = len.Lx)

  if (!anyNA(Tx.data) && xor(is.null(signal_integral_Tx),
                             is.null(background_integral_Tx))) {
    .throw_error("When 'Tx.data' is provided, either both 'signal_integral_Tx' ",
                 "and 'background_integral_Tx' must be provided, or neither")
  }

  any_given <- !is.null(signal_integral_Tx) || !is.null(background_integral_Tx)
  if (!anyNA(Tx.data) && use_previousBG && any_given) {
    .throw_warning("When 'use_previousBG = TRUE', independent Tx integral ",
                   "limits are not allowed, Lx limits will be used for Tx")
  }

  both_given <- !is.null(signal_integral_Tx) && !is.null(background_integral_Tx)
  if (!anyNA(Tx.data) && !use_previousBG && both_given) {
      signal_integral_Tx <- .validate_integral(signal_integral_Tx, max = len.Tx,
                                               null.ok = TRUE)
      background_integral_Tx <- .validate_integral(background_integral_Tx, na.ok = TRUE,
                                                   min = max(signal_integral_Tx) + 1,
                                                   max = len.Tx,
                                                   null.ok = TRUE)
  } else {
    signal_integral_Tx <- signal_integral
    background_integral_Tx <- background_integral
  }

  ##--------------------------------------------------------------------------##
  ##(2) - read data and produce background subtracted values

  ## calculate k value - express the background as multiple value from the number
  ## of signal integral channels, however, it can be < 1 also
  n <- length(signal_integral)
  m <- length(background_integral)
  k <- m/n
  n.Tx <- length(signal_integral_Tx)

  ##use previous BG and account for the option to set different integral limits
  m.Tx <- if (use_previousBG) m else length(background_integral_Tx)
  k.Tx <- m.Tx/n.Tx

  ##LnLx (comments are corresponding variables to Galbraith, 2002)
  Lx.curve <- Lx.data[,2]
  Lx.signal <- sum(Lx.curve[signal_integral])                #Y.0
  Lx.background <- sum(Lx.curve[background_integral]) / k    #Y.1, mu.B
  SN.ratio.LnLx <- Lx.signal / Lx.background
  if (.strict_na(background_integral))
    Lx.background <- 0

  LnLx <- Lx.signal - Lx.background

  ##TnTx
  Tx.curve <- ifelse(is.na(Tx.data[, 1]), NA, Tx.data[, 2])
  Tx.signal <- sum(Tx.curve[signal_integral_Tx])

  ##use previous BG
  if(use_previousBG)
    Tx.background <- Lx.background
  else
    Tx.background <- sum(Tx.curve[background_integral_Tx]) / k.Tx
  SN.ratio.TnTx <- Tx.signal / Tx.background
  if (.strict_na(background_integral_Tx))
    Tx.background <- 0

  TnTx <- (Tx.signal-Tx.background)

  ##--------------------------------------------------------------------------##
  ##(3)
  ## calculate Lx/Tx Errors according Galbraith (2002) and the personal
  ## communication of Galbraith (2014) via e-mail
  ## Nomenclature as stated in the articles

  ##(a)
  ## set Y.0 (sum OSL signal including the background) and
  ## Y.1 (total counts over m later channels)
  Y.0 <- Lx.signal
  Y.0_TnTx <- Tx.signal
  Y.1 <- sum(Lx.curve[background_integral])
  Y.1_TnTx <- sum(Tx.curve[background_integral_Tx])

  ##(b) estimate overdispersion (here called sigmab), see equation (4) in
  ## Galbraith (2002), Galbraith (2014)
  ## If else condition for the case that k < 2
  .calc_sigmab <- function(Lx.curve, signal_integral, background_integral,
                           m, n, k, what, usepreviousBG = FALSE) {
    len.sg.integral <- length(signal_integral)
    min.bg.integral <- min(background_integral)
    if (round(k, digits = 1) >= 2 &&
        min.bg.integral + len.sg.integral * (2 + 1) <= length(Lx.curve)) {

      ## note that m = n*k = multiple of background_integral from signal_integral
      Y.i <- vapply(0:round(k, digits = 0), function(i) {
        sum(Lx.curve[min.bg.integral +
                     (len.sg.integral * i):(len.sg.integral * (i + 1))])
      }, FUN.VALUE = numeric(1))

      Y.i <- na.exclude(Y.i)
      n <- 1
    } else {
      ## warn if m is < 25, as suggested by Rex Galbraith (low number of
      ## degrees of freedom)
      if (m < 25 && !use_previousBG && !.strict_na(background_integral)) {
        .throw_warning("Number of background channels for ", what, " < 25, ",
                       "error estimation might not be reliable")
      }

      Y.i <- Lx.curve[background_integral]
    }

    ## sigmab is denoted as sigma^2 = s.Y^2 - Y.mean, therefore abs() is used
    abs(stats::var(Y.i) - mean(Y.i)) * n
  }

  ##account for a manually set sigmab value
  if (is.null(sigmab)) {
    sigmab.LnLx <- .calc_sigmab(Lx.curve, signal_integral, background_integral,
                                m, n, k, "Lx")
    sigmab.TnTx <- .calc_sigmab(Tx.curve, signal_integral_Tx, background_integral_Tx,
                                m.Tx, n.Tx, k.Tx, "Tx", use_previousBG)
  } else {
    sigmab.LnLx <- sigmab[1]
    sigmab.TnTx <- sigmab[length(sigmab)]
  }

  ##(c)
  ## Calculate relative error of the background subtracted signal
  ## according to Galbraith (2002), equation (6) with changes
  ## from Galbraith (2014), equation 6
  ## Discussion with Rex Galbraith via e-mail (2014-02-27):
  ## Equation 6 is appropriate to be implemented as standard

  ## relative standard error from equation (6)
  ## when sigmab = 0, this reduces to equation (3), valid for poisson
  rse <- function(Y0, Y1, k, sigmab) {
    sqrt(Y0 + Y1 / k^2 + sigmab * (1 + 1 / k)) / (Y0 - Y1 / k)
  }

  if(background.count.distribution == "poisson"){
    used.sigmab.LnLx <- used.sigmab.TnTx <- 0
  }else{
    if(background.count.distribution != "non-poisson"){
      .throw_warning("Unknown method for 'background.count.distribution', ",
                     "a non-poisson distribution is assumed")
    }
    used.sigmab.LnLx <- sigmab.LnLx
    used.sigmab.TnTx <- sigmab.TnTx
  }

  LnLx.relError <- rse(Y.0, Y.1, k, used.sigmab.LnLx)
  TnTx.relError <- rse(Y.0_TnTx, Y.1_TnTx, k, used.sigmab.TnTx)

  ##(d)
  ##calculate absolute standard error
  LnLx.Error <- abs(LnLx*LnLx.relError)
  TnTx.Error <- abs(TnTx*TnTx.relError)

    ##we do not want to have NaN values, as they are mathematically correct, but make
    ##no sense and would result in aliquots that become rejected later
    if(is.nan(LnLx.Error)) LnLx.Error <- 0
    if(is.nan(TnTx.Error)) TnTx.Error <- 0

  ##combine results
  LnLxTnTx <- data.frame(
    LnLx = Lx.signal,
    LnLx.BG = Lx.background,
    TnTx = Tx.signal,
    TnTx.BG = Tx.background,
    Net_LnLx = LnLx,
    Net_LnLx.Error = LnLx.Error,
    Net_TnTx = TnTx,
    Net_TnTx.Error = TnTx.Error,
    SN_RATIO_LnLx = SN.ratio.LnLx,
    SN_RATIO_TnTx = SN.ratio.TnTx)

  ## ------------------------------------------------------------------------
  ## (4) Calculate LxTx error according Galbraith (2014)
  temp <- .calculate_LxTx_error(LnLxTnTx, sig0, digits)

  calc.parameters <- list(
    sigmab.LnLx = sigmab.LnLx,
    sigmab.TnTx = sigmab.TnTx,
    k = k)

  ##set results object
  set_RLum(
      class = "RLum.Results",
      data = list(
        LxTx.table = temp,
        calc.parameters = calc.parameters),
      info = list(call = sys.call())
  )
}
