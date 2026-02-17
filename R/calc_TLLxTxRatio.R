#' @title Calculate the Lx/Tx ratio for a given set of TL curves -beta version-
#'
#' @description
#' Calculate Lx/Tx ratio for a given set of TL curves.
#'
#' @details
#' **Uncertainty estimation**
#'
#' The standard errors are calculated using the following generalised equation:
#'
#' \deqn{SE_{signal} = abs(Signal_{net} * BG_f /BG_{signal})}
#'
#' where \eqn{BG_f} is a term estimated by calculating the standard deviation of the sum of
#' the \eqn{L_x} background counts and the sum of the \eqn{T_x} background counts. However,
#' if both signals are similar the error becomes zero.
#'
#' @param Lx.data.signal [Luminescence::RLum.Data.Curve-class] or [data.frame] (**required**):
#' TL data (x = temperature, y = counts) (TL signal).
#'
#' @param Lx.data.background [Luminescence::RLum.Data.Curve-class] or [data.frame] (*optional*):
#' TL data (x = temperature, y = counts).
#' If no data are provided no background subtraction is performed.
#'
#' @param Tx.data.signal [Luminescence::RLum.Data.Curve-class] or [data.frame] (**required**):
#' TL data (x = temperature, y = counts) (TL test signal).
#'
#' @param Tx.data.background [Luminescence::RLum.Data.Curve-class] or [data.frame] (*optional*):
#' TL data (x = temperature, y = counts).
#' If no data are provided no background subtraction is performed.
#'
#' @param signal_integral [integer] (**required**):
#' vector of channels for the signal integral.
#'
#' @param integral_input [character] (*with default*):
#' input type for `signal_integral`, one of `"channel"` (default) or
#' `"measurement"`. If set to `"measurement"`, the best matching channels
#' corresponding to the given temperature range are selected.
#'
#' @param ... currently not used.
#'
#' @return
#' Returns an S4 object of type [Luminescence::RLum.Results-class].
#' Slot `data` contains a [list] with the following structure:
#'
#' ```
#' $ LxTx.table
#' .. $ LnLx
#' .. $ LnLx.BG
#' .. $ TnTx
#' .. $ TnTx.BG
#' .. $ Net_LnLx
#' .. $ Net_LnLx.Error
#' ```
#'
#' @note
#' **This function has still BETA status!** Please further note that a similar
#' background for both curves results in a zero error and is therefore set to `NA`.
#'
#' @section Function version: 0.3.6
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany) \cr
#' Christoph Schmidt, University of Bayreuth (Germany)
#'
#' @seealso [Luminescence::RLum.Results-class], [Luminescence::analyse_SAR.TL]
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##load package example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##convert Risoe.BINfileData into a curve object
#' temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)
#'
#' Lx.data.signal <- get_RLum(temp, record.id=1)
#' Lx.data.background <- get_RLum(temp, record.id=2)
#' Tx.data.signal <- get_RLum(temp, record.id=3)
#' Tx.data.background <- get_RLum(temp, record.id=4)
#' signal_integral <- 210:230
#'
#' output <- calc_TLLxTxRatio(
#'  Lx.data.signal,
#'  Lx.data.background,
#'  Tx.data.signal,
#'  Tx.data.background,
#'  signal_integral)
#' get_RLum(output)
#'
#' @export
calc_TLLxTxRatio <- function(
  Lx.data.signal,
  Lx.data.background = NULL,
  Tx.data.signal = NULL,
  Tx.data.background = NULL,
  signal_integral = NULL,
  integral_input = c("channel", "measurement"),
  ...
) {
  .set_function_name("calc_TLLxTxRatio")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(Lx.data.signal, c("data.frame", "RLum.Data.Curve"))
  .validate_class(Tx.data.signal, c("data.frame", "RLum.Data.Curve"))

  ##check DATA TYPE differences
   if(is(Lx.data.signal)[1] != is(Tx.data.signal)[1])
     .throw_error("Data types of Lx and Tx data differ")

  ##--------------------------------------------------------------------------##
  ## Type conversion (assuming that all input variables are of the same type)
  if(inherits(Lx.data.signal, "RLum.Data.Curve")){
    Lx.data.signal <- as(Lx.data.signal, "matrix")
    Tx.data.signal <- as(Tx.data.signal, "matrix")

    if (!is.null(Lx.data.background))
      Lx.data.background <- as(Lx.data.background, "matrix")

    if (!is.null(Tx.data.background))
      Tx.data.background <- as(Tx.data.background, "matrix")
  }

  ##(d) - check if Lx and Tx curves have the same channel length
  if (length(Lx.data.signal[, 2]) != length(Tx.data.signal[, 2])) {
    .throw_error("Channel numbers differ for Lx and Tx data")
  }

  integral_input <- .validate_args(integral_input, c("channel", "measurement"))

  ## deprecated argument
  if (any(grepl("signal.integral", ...names(), fixed = TRUE))) {
    .deprecated(old = "signal.integral",
                new = "signal_integral",
                since = "1.2.0")
    if (integral_input != "channel") {
      .throw_error("'integral_input' is not supported with old argument names")
    }
    signal_integral <- list(...)$signal.integral
  }

  if (integral_input == "measurement") {
    signal_integral <- .convert_to_channels(Lx.data.signal[, 1], signal_integral,
                                            "temperature")
  }
  signal_integral <- .validate_integral(signal_integral,
                                        max = length(Lx.data.signal[, 2]))

#  Background Consideration --------------------------------------------------
  LnLx.BG <- TnTx.BG <- NA
  signal.interval <- signal_integral

  ## Lx.data
  if (!is.null(Lx.data.background))
    LnLx.BG <- sum(Lx.data.background[signal.interval, 2])

  ## Tx.data
  if (!is.null(Tx.data.background))
    TnTx.BG <- sum(Tx.data.background[signal.interval, 2])

# Calculate Lx/Tx values --------------------------------------------------
    ## preset variables
    net_LnLx <- net_LnLx.Error <- net_TnTx <- net_TnTx.Error <- NA
    BG.Error <- NA

  ## calculate values
  LnLx <- sum(Lx.data.signal[signal.interval, 2])
  TnTx <- sum(Tx.data.signal[signal.interval, 2])

  ## calculate standard deviation of background
  if (!is.na(LnLx.BG) && !is.na(TnTx.BG)) {
       BG.Error <- sd(c(LnLx.BG, TnTx.BG))

       if(BG.Error == 0) {
         .throw_warning("The background signals for Lx and Tx appear ",
                        "to be similar, no background error was calculated")
         BG.Error <- NA
       }
  }

    ## calculate net LnLx
    if(!is.na(LnLx.BG)){
      net_LnLx <-  LnLx - LnLx.BG
      net_LnLx.Error <- abs(net_LnLx * BG.Error/LnLx.BG)
    }

    ## calculate net TnTx
    if(!is.na(TnTx.BG)){
         net_TnTx <-  TnTx - TnTx.BG
         net_TnTx.Error <- abs(net_TnTx * BG.Error/TnTx.BG)
    }

    ## calculate LxTx
    if(is.na(net_TnTx)){
      LxTx <- LnLx/TnTx
      LxTx.Error <- NA
    }else{
      LxTx <- net_LnLx/net_TnTx
      LxTx.Error <- abs(LxTx*((net_LnLx.Error/net_LnLx) + (net_TnTx.Error/net_TnTx)))
    }

    ##COMBINE into a data.frame
    temp.results <- data.frame(
      LnLx,
      LnLx.BG,
      TnTx,
      TnTx.BG,
      net_LnLx,
      net_LnLx.Error,
      net_TnTx,
      net_TnTx.Error,
      LxTx,
      LxTx.Error
    )

# Return values -----------------------------------------------------------
  set_RLum(
      class = "RLum.Results",
      data = list(LxTx.table = temp.results),
      info = list(call = sys.call())
  )
}
