#' @title Calculate `Lx/Tx` ratio for CW-OSL curves
#'
#' @description
#' Calculate `Lx/Tx` ratios from a given set of CW-OSL curves assuming late light
#' background subtraction.
#'
#' @details
#' The integrity of the chosen values for the signal and background integral is
#' checked by the function; the signal integral limits have to be lower than
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
#' **sig0**
#'
#' This argument allows to add an extra component of error to the final `Lx/Tx`
#' error value. The input will be treated as factor that is multiplied with
#' the already calculated `LxTx` and the result is add up by:
#'
#' \deqn{se(LxTx) = \sqrt(se(LxTx)^2 + (LxTx * sig0)^2)}
#'
#'
#' **background.count.distribution**
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
#' Please check whether this is valid for your data set and  if necessary
#' consider to provide an own `sigmab` value using the corresponding argument `sigmab`.
#'
#' @param Lx.data [RLum.Data.Curve-class] or [data.frame] (**required**):
#' requires a CW-OSL shine down curve (x = time, y = counts)
#'
#' @param Tx.data [RLum.Data.Curve-class] or [data.frame] (*optional*):
#' requires a CW-OSL shine down curve (x = time, y = counts). If no
#' input is given the `Tx.data` will be treated as `NA` and no `Lx/Tx` ratio
#' is calculated.
#'
#' @param signal.integral [numeric] (**required**): vector with the limits for the signal integral.
#' Can be set to `NA` than now integrals are considered and all other integrals are set to `NA` as well.
#'
#' @param signal.integral.Tx [numeric] (*optional*):
#' vector with the limits for the signal integral for the `Tx`-curve. If
#' missing, the value from `signal.integral` is used.
#'
#' @param background.integral [numeric] (**required**):
#' vector with the bounds for the background integral.
#' Can be set to `NA` than now integrals are considered and all other integrals are set to `NA` as well.
#'
#' @param background.integral.Tx [numeric] (*optional*):
#' vector with the limits for the background integral for the `Tx` curve.
#' If missing, the value from `background.integral` is used.
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
#' @return
#' Returns an S4 object of type [RLum.Results-class].
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
#' (version 3.24b). Access to the results object via [get_RLum].
#'
#' **Caution:** If you are using early light subtraction (EBG), please either provide your
#' own `sigmab` value or use `background.count.distribution = "poisson"`.
#'
#' @section Function version: 0.8.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [Analyse_SAR.OSLdata], [fit_DoseResponseCurve],
#' [analyse_SAR.CWOSL]
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
#'  signal.integral = c(1:2),
#'  background.integral = c(85:100))
#'
#' ##get results object
#' get_RLum(results)
#'
#' @md
#' @export
calc_OSLLxTxRatio <- function(
  Lx.data,
  Tx.data = NULL,
  signal.integral,
  signal.integral.Tx = NULL,
  background.integral,
  background.integral.Tx = NULL,
  background.count.distribution = "non-poisson",
  use_previousBG = FALSE,
  sigmab = NULL,
  sig0 = 0,
  digits = NULL
) {
  .set_function_name("calc_OSLLxTxRatio")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(Lx.data, c("RLum.Data.Curve", "data.frame", "numeric", "matrix"))

  ## Lx - coerce if required
  Lx.data <- switch(
    class(Lx.data)[1],
    "RLum.Data.Curve" = as(Lx.data, "data.frame"),
    "matrix" = as.data.frame(Lx.data),
    "data.frame" = Lx.data,
    data.frame(x = 1:length(Lx.data),y = Lx.data)
  )
  len.Lx <- nrow(Lx.data)

  ## Tx - coerce if required
  if(!is.null(Tx.data)){
    .validate_class(Tx.data, c("RLum.Data.Curve", "data.frame", "numeric", "matrix"))

    Tx.data <- switch(
      class(Tx.data)[1],
      "RLum.Data.Curve" = as(Tx.data, "data.frame"),
      "matrix" = as.data.frame(Tx.data),
      "data.frame" = Tx.data,
      data.frame(x = 1:length(Tx.data),y = Tx.data)
    )
    len.Tx <- nrow(Tx.data)

    ## check channel number
    if (len.Lx != len.Tx) {
      .throw_error("Different number of channels for Lx (", len.Lx, ") ",
                   "and Tx (", len.Tx, ")")
    }
  } else {
    Tx.data <- data.frame(x = NA,y = NA)
  }

  # Alternate mode ----------------------------------------------------------
  if (anyNA(c(signal.integral, background.integral))) {
    signal.integral <- background.integral <- NA
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
  ##(e) - check if signal integral is valid
  if (min(signal.integral) < 1 || max(signal.integral) > len.Lx) {
    .throw_error("'signal.integral' is not valid, max: ", len.Lx)
  }

  ##(f) - check if background integral is valid
  if (min(background.integral) < 1 || max(background.integral) > len.Lx) {
    .throw_error("'background.integral' is not valid, max: ", len.Lx)
  }

  ##(g) - check if signal and background integral overlapping
  if(min(background.integral)<=max(signal.integral)){
    .throw_error("'signal.integral' and 'background.integral' overlap")
  }

  ##(h) - similar procedure for the Tx limits
  if(!anyNA(Tx.data) && all(c(!is.null(signal.integral.Tx),!is.null(background.integral.Tx)))){
    if(use_previousBG){
      .throw_warning("With 'use_previousBG = TRUE' independent Lx and Tx ",
                     "integral limits are not allowed. Integral limits ",
                     "of Lx used for Tx.")
      signal.integral.Tx <- signal.integral
      background.integral.Tx <- background.integral
    }

    if (min(signal.integral.Tx) < 1 || max(signal.integral.Tx) > len.Tx) {
      .throw_error("'signal.integral.Tx' is not valid, max: ", len.Tx)
    }
    if (min(background.integral.Tx) < 1 || max(background.integral.Tx) > len.Tx) {
      .throw_error("'background.integral.Tx' is not valid, max: ", len.Tx)
    }

    if(min(background.integral.Tx)<=max(signal.integral.Tx)){
      .throw_error("'signal.integral.Tx' and 'background.integral.Tx' overlap")
    }

  }else if(!anyNA(Tx.data) && !all(c(is.null(signal.integral.Tx),is.null(background.integral.Tx)))){
    .throw_error("You have to provide both 'signal.integral.Tx' and ",
                 "'background.integral.Tx'")
  }else{
    signal.integral.Tx <- signal.integral
    background.integral.Tx <- background.integral
  }

  ##check sigmab
  if (!is.null(sigmab)) {
    .validate_class(sigmab, "numeric")

    if (length(sigmab) > 2) {
      .throw_error("'sigmab' can have at most length 2")
    }
  }

  ##--------------------------------------------------------------------------##
  ##(2) - read data and produce background subtracted values

  ## calculate k value - express the background as multiple value from the number
  ## of signal integral channels, however, it can be < 1 also
  n <- length(signal.integral)
  m <- length(background.integral)
  k <- m/n
  n.Tx <- length(signal.integral.Tx)

  ##use previous BG and account for the option to set different integral limits
  if(use_previousBG){
    m.Tx <- m

  }else{
    m.Tx <- length(background.integral.Tx)
  }

  k.Tx <- m.Tx/n.Tx

  ##LnLx (comments are corresponding variables to Galbraith, 2002)
  Lx.curve <- Lx.data[,2]
  Lx.signal <- sum(Lx.curve[signal.integral])                #Y.0
  Lx.background <- sum(Lx.curve[background.integral])        #Y.1
  Lx.background <- Lx.background*1/k                         #mu.B
  LnLx <- Lx.signal - Lx.background

  ##TnTx
  Tx.curve <- ifelse(is.na(Tx.data[,1])==FALSE, Tx.data[,2], NA)
  Tx.signal <- sum(Tx.curve[signal.integral.Tx])

  ##use previous BG
  if(use_previousBG)
    Tx.background <- Lx.background
  else
    Tx.background <- sum(Tx.curve[background.integral.Tx]) * 1 / k.Tx

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
  Y.1 <- sum(Lx.curve[background.integral])
  Y.1_TnTx <- sum(Tx.curve[background.integral.Tx])

  ##(b) estimate overdispersion (here called sigmab), see equation (4) in
  ## Galbraith (2002), Galbraith (2014)
  ## If else condition for the case that k < 2

  len.sig.integral <- length(signal.integral)
  min.bg.integral <- min(background.integral)
  if (round(k, digits = 1) >= 2 &&
      min.bg.integral + len.sig.integral * (2 + 1) <= len.Lx) {

    ##(b)(1)(1)
    ## note that m = n*k = multiple of background.integral from signal.integral
    Y.i <- vapply(0:round(k,digits=0), function(i){
      sum(Lx.curve[min.bg.integral +
                   (len.sig.integral * i):(len.sig.integral * (i + 1))])
    }, FUN.VALUE = numeric(1))

    Y.i <- na.exclude(Y.i)
    sigmab.LnLx <- abs(stats::var(Y.i) - mean(Y.i))  ##sigmab is denoted as sigma^2 = s.Y^2-Y.mean
    ##therefore here absolute values are given

  }else{
    ## provide warning if m is < 25, as suggested by Rex Galbraith
    ## low number of degree of freedom
    if (m < 25) {
      .throw_warning("Number of background channels for Lx < 25, ",
                     "error estimation might not be reliable")
    }

    sigmab.LnLx <- abs((stats::var(Lx.curve[background.integral]) -
                          mean(Lx.curve[background.integral])) * n)
  }

  len.sig.integral.Tx <- length(signal.integral.Tx)
  min.bg.integral.Tx <- min(background.integral.Tx)
  if (round(k.Tx, digits = 1) >= 2 &&
      min.bg.integral.Tx + len.sig.integral.Tx * (2 + 1) <= length(Tx.curve)) {
    ##(b)(1)(1)
    ## note that m.Tx = n.Tx*k.Tx = multiple of background.integral.Tx from signal.integral.Tx
    ## also for the TnTx signal
    Y.i_TnTx <- vapply(0:round(k.Tx, digits = 0), function(i) {
      sum(Tx.curve[min.bg.integral.Tx +
                   (len.sig.integral.Tx * i):(len.sig.integral.Tx * (i + 1))])
    }, FUN.VALUE = numeric(1))

    Y.i_TnTx <- na.exclude(Y.i_TnTx)
    sigmab.TnTx <- abs(stats::var(Y.i_TnTx) - mean(Y.i_TnTx))

  } else{
    ## provide warning if m is < 25, as suggested by Rex Galbraith
    ## low number of degree of freedom
    if (!anyNA(Tx.data) && m.Tx < 25 && use_previousBG == FALSE) {
      .throw_warning("Number of background channels for Tx < 25, ",
                     "error estimation might not be reliable")
    }

    sigmab.TnTx <- abs((stats::var(Tx.curve[background.integral.Tx]) -
                          mean(Tx.curve[background.integral.Tx])) * n.Tx)
  }

  ##account for a manually set sigmab value
  if (!is.null(sigmab)) {
    sigmab.LnLx <- sigmab[1]
    sigmab.TnTx <- sigmab[length(sigmab)]
  }

  ##(c)
  ## Calculate relative error of the background subtracted signal
  ## according to Galbraith (2002), equation (6) with changes
  ## from Galbraith (2014), equation 6
  ## Discussion with Rex Galbraith via e-mail (2014-02-27):
  ## Equation 6 is appropriate to be implemented as standard

  if(background.count.distribution == "poisson"){
    ##(c.1) estimate relative standard error for assuming a poisson distribution
    LnLx.relError <- sqrt((Y.0 + Y.1/k^2))/(Y.0-Y.1/k)        ##  rse(mu.s)
    TnTx.relError <- sqrt((Y.0_TnTx + Y.1_TnTx/k^2))/(Y.0_TnTx-Y.1_TnTx/k)

  }else{
    ##(c.2) estimate relative standard error for a non-poisson distribution
    if(background.count.distribution != "non-poisson"){
      .throw_warning("Unknown method for 'background.count.distribution', ",
                     "a non-poisson distribution is assumed")
    }

    LnLx.relError <- sqrt(Y.0 + Y.1/k^2 + sigmab.LnLx*(1+1/k))/
      (Y.0 - Y.1/k)
    TnTx.relError <- sqrt(Y.0_TnTx + Y.1_TnTx/k^2 + sigmab.TnTx*(1+1/k))/
      (Y.0_TnTx - Y.1_TnTx/k)
  }

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
    Net_TnTx.Error = TnTx.Error
  )

  ## ------------------------------------------------------------------------
  ## (4) Calculate LxTx error according Galbraith (2014)

  temp <- .calculate_LxTx_error(LnLxTnTx, sig0, digits)

  calc.parameters <- list(
    sigmab.LnLx = sigmab.LnLx,
    sigmab.TnTx = sigmab.TnTx,
    k = k)

  ##set results object
  return(set_RLum(
      class = "RLum.Results",
      data = list(
        LxTx.table = temp,
        calc.parameters = calc.parameters),
      info = list(call = sys.call())
    ))
}
