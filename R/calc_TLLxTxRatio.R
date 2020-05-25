#' Calculate the Lx/Tx ratio for a given set of TL curves -beta version-
#'
#' Calculate Lx/Tx ratio for a given set of TL curves.
#'
#' **Uncertainty estimation**
#'
#' The standard errors are calculated using the following generalised equation:
#'
#' \deqn{SE_{signal} <- abs(Signal_{net} * BG_f /BG_{signal}}
#'
#' where \eqn{BG_f} is a term estimated by calculating the standard deviation of the sum of
#' the \eqn{L_x} background counts and the sum of the \eqn{T_x} background counts. However,
#' if both signals are similar the error becomes zero.
#'
#' @param Lx.data.signal [RLum.Data.Curve-class] or [data.frame] (**required**):
#' TL data (x = temperature, y = counts) (TL signal)
#'
#' @param Lx.data.background [RLum.Data.Curve-class] or [data.frame] (*optional*):
#' TL data (x = temperature, y = counts).
#' If no data are provided no background subtraction is performed.
#'
#' @param Tx.data.signal [RLum.Data.Curve-class] or [data.frame] (**required**):
#' TL data (x = temperature, y = counts) (TL test signal)
#'
#' @param Tx.data.background [RLum.Data.Curve-class] or [data.frame] (*optional*):
#' TL data (x = temperature, y = counts).
#' If no data are provided no background subtraction is performed.
#'
#' @param signal.integral.min [integer] (**required**):
#' channel number for the lower signal integral bound
#' (e.g. `signal.integral.min = 100`)
#'
#' @param signal.integral.max [integer] (**required**):
#' channel number for the upper signal integral bound
#' (e.g. `signal.integral.max = 200`)
#'
#' @return
#' Returns an S4 object of type [RLum.Results-class].
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
#' @section Function version: 0.3.3
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) \cr
#' Christoph Schmidt, University of Bayreuth (Germany)
#'
#' @seealso [RLum.Results-class], [analyse_SAR.TL]
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
#'
#' Lx.data.signal <- get_RLum(temp, record.id=1)
#' Lx.data.background <- get_RLum(temp, record.id=2)
#' Tx.data.signal <- get_RLum(temp, record.id=3)
#' Tx.data.background <- get_RLum(temp, record.id=4)
#' signal.integral.min <- 210
#' signal.integral.max <- 230
#'
#' output <- calc_TLLxTxRatio(Lx.data.signal,
#'                            Lx.data.background,
#'                            Tx.data.signal, Tx.data.background,
#'                            signal.integral.min, signal.integral.max)
#' get_RLum(output)
#'
#' @md
#' @export
calc_TLLxTxRatio <- function(
  Lx.data.signal,
  Lx.data.background = NULL,
  Tx.data.signal,
  Tx.data.background = NULL,
  signal.integral.min,
  signal.integral.max
){


  ##--------------------------------------------------------------------------##
  ##(1) - a few integrity check

     ##check for MISSING objects
     if(missing(Lx.data.signal) | missing(Tx.data.signal) |
        missing(signal.integral.min) |  missing(signal.integral.max)){

       temp.missing <- paste(
                       c(if(missing(Lx.data.signal)){"Lx.data.signal"},
                         if(missing(Tx.data.signal)){"Tx.data.signal"},
                         if(missing(signal.integral.min)){"signal.integral.min"},
                         if(missing(signal.integral.max)){"signal.integral.max"}),
                       collapse = ", ")

          stop(paste("[calc_TLLxTxRatio()] Arguments are missing: ",temp.missing, ".", sep=""), call. = FALSE)

     }


     ##check DATA TYPE differences
     if(is(Lx.data.signal)[1]!=is(Tx.data.signal)[1]){
       stop("[calc_TLLxTxRatio()] Data type of Lx and Tx data differs!")}

     ##check for allowed data.types
     if(!is(Lx.data.signal, "data.frame") &
        !is(Lx.data.signal, "RLum.Data.Curve")){

       stop("[calc_TLLxTxRatio()] Input data type for not allowed. Allowed are 'RLum.Data.Curve' and 'data.frame'")

     }

  ##--------------------------------------------------------------------------##
  ## Type conversion (assuming that all input variables are of the same type)

  if(is(Lx.data.signal, "RLum.Data.Curve")){

    Lx.data.signal <- as(Lx.data.signal, "matrix")
    Tx.data.signal <- as(Tx.data.signal, "matrix")

    if(missing(Lx.data.background) == FALSE && is.null(Lx.data.background) == FALSE){

      Lx.data.background <- as(Lx.data.background, "matrix")

    }

    if(missing(Tx.data.background) == FALSE && is.null(Tx.data.background) == FALSE){

      Tx.data.background <- as(Tx.data.background, "matrix")

    }

  }

  ##(d) - check if Lx and Tx curves have the same channel length
     if(length(Lx.data.signal[,2])!=length(Tx.data.signal[,2])){
       stop("[calc_TLLxTxRatio()] Channel number of Lx and Tx data differs!")}


   ##(e) - check if signal integral is valid
   if(signal.integral.min < 1 | signal.integral.max > length(Lx.data.signal[,2])){
     stop("[calc_TLLxTxRatio()] Signal.integral is not valid!")}




#  Background Consideration --------------------------------------------------

   ##Lx.data
   if(!is.null(Lx.data.background)){
     LnLx.BG <- sum(Lx.data.background[signal.integral.min:signal.integral.max,2])

    }else{
     LnLx.BG <- NA

    }

   ##Tx.data
      if(!is.null(Tx.data.background)){
        TnTx.BG <- sum(Tx.data.background[signal.integral.min:signal.integral.max,2])

      }else{
        TnTx.BG <- NA

      }

# Calculate Lx/Tx values --------------------------------------------------

    LnLx <- sum(Lx.data.signal[signal.integral.min:signal.integral.max,2])
    TnTx <- sum(Tx.data.signal[signal.integral.min:signal.integral.max,2])

     ##calculate variance of background
     if(is.na(LnLx.BG) == FALSE & is.na(TnTx.BG) == FALSE){
       BG.Error <- sd(c(LnLx.BG, TnTx.BG))

       if(BG.Error == 0) {
         warning(
           "[calc_TLLxTxRatio()] The background signals for Lx and Tx appear to be similar, no background error was calculated.",
           call. = FALSE
         )
         BG.Error <- NA

       }

     }


    if(is.na(LnLx.BG) == FALSE){
      net_LnLx <-  LnLx - LnLx.BG
      net_LnLx.Error <- abs(net_LnLx * BG.Error/LnLx.BG)

    }else{
      net_LnLx <- NA
      net_LnLx.Error <- NA

    }

    if(is.na(TnTx.BG) == FALSE){

         net_TnTx <-  TnTx - TnTx.BG
         net_TnTx.Error <- abs(net_TnTx * BG.Error/TnTx.BG)

    }else{

      net_TnTx <- NA
      net_TnTx.Error  <- NA

    }


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
    newRLumResults.calc_TLLxTxRatio <- set_RLum(
      class = "RLum.Results",
      data = list(LxTx.table = temp.results),
      info = list(call = sys.call())
    )

   return(newRLumResults.calc_TLLxTxRatio)

}
