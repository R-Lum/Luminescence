#' Converting values from seconds (s) to gray (Gy)
#'
#' Conversion of absorbed radiation dose in seconds (s) to the SI unit gray
#' (Gy) including error propagation. Normally used for equivalent dose data.
#'
#' Calculation of De values from seconds (s) to gray (Gy) \deqn{De [Gy] = De
#' [s] * Dose Rate [Gy/s])} \cr Provided calculation methods for error
#' calculation: \bold{gaussian} error propagation \cr \deqn{De.error.gray =
#' \sqrt(dose.rate * De.error.seconds)^2 + (De.seconds * dose.rate.error)^2 ))}
#' \bold{absolute} error propagation \cr \deqn{De.error.gray = abs(dose.rate *
#' De.error.seconds) + abs(De.seconds * dose.rate.error)}
#'
#' @param data \code{\link{data.frame}} (\bold{required}): input values,
#' structure: data (\code{values[,1]}) and data error (\code{values [,2]}) are
#' required
#'
#' @param dose.rate \code{\linkS4class{RLum.Results}} or \code{\link{data.frame}} or \code{\link{numeric}}
#' (\bold{required}): \code{RLum.Results} needs to be orginated from the
#' function \code{\link{calc_SourceDoseRate}}, for \code{vector}dose rate in
#' Gy/s and dose rate error in Gy/s
#'
#' @param method \link{character} (with default): method used for error
#' calculation (\code{gaussian} or \code{absolute}), see details for further
#' information
#'
#' @return Returns a \link{data.frame} with converted values.
#'
#' @note If no or a wrong method is given, the execution of the function is
#' stopped. Furthermore, if a \code{data.frame} is provided for the dose rate values is has to
#' be of the same length as the data frame provided with the argument \code{data}
#'
#' @section Function version: 0.5.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France),\cr Michael Dietze, GFZ Potsdam (Germany),\cr Margret C. Fuchs, HZDR,
#' Helmholtz-Institute Freiberg for Resource Technology
#' (Germany)
#'
#' @seealso #
#'
#' @references #
#'
#' @keywords manip
#'
#' @examples
#'
#'
#' ##(A) for known source dose rate at date of measurement
#' ## - load De data from the example data help file
#' data(ExampleData.DeValues, envir = environment())
#' ## - convert De(s) to De(Gy)
#' Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))
#'
#'
#'
#'
#'
#' ##(B) for source dose rate calibration data
#' ## - calculate source dose rate first
#' dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
#'                                   calib.date = "2014-12-19",
#'                                   calib.dose.rate = 0.0438,
#'                                   calib.error = 0.0019)
#' # read example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' # apply dose.rate to convert De(s) to De(Gy)
#' Second2Gray(ExampleData.DeValues$BT998, dose.rate)
#'
#'
Second2Gray <- function(
  data,
  dose.rate,
  method = "gaussian"
){

  # Integrity tests -----------------------------------------------------------------------------

  ##(1) data.frame or RLum.Data.Curve object?
  if(is(data, "data.frame") == FALSE){

    stop("[Second2Gray()] 'data' object has to be of type 'data.frame'!")

  }

  ##(2) numeric, data.frame or RLum.Data.Curve object?
  if(!is(dose.rate, "numeric")  &  !is(dose.rate, "RLum.Results") & !is(dose.rate, "data.frame")){

    stop("[Second2Gray()] 'dose.rate' object has to be of type 'numeric', 'data.frame' or 'RLum.Results'!")

  }


  ##(3) last check to avoid problems
  if(is(dose.rate, "data.frame")){

    if(nrow(dose.rate)!=nrow(data)){

      stop("[Second2Gray()] the data frames in 'data' and 'dose.rate' need to be of similar length!")

    }

  }


  ##(4) check for right orginator
  if(is(dose.rate, "RLum.Results")){

    if(dose.rate@originator != "calc_SourceDoseRate"){

      stop("[Second2Gray()]  Wrong originator for dose.rate 'RLum.Results' object.")

    }else{

      ##check what is what
      if(!is(get_RLum(dose.rate, data.object = "dose.rate"), "data.frame")){

        dose.rate <- data.frame(
          dose.rate  <- as.numeric(get_RLum(dose.rate, data.object = "dose.rate")[1]),
          dose.rate.error <- as.numeric(get_RLum(dose.rate, data.object = "dose.rate")[2])
          )

      }else{

        dose.rate <- get_RLum(dose.rate, data.object = "dose.rate")

      }

    }

  }


  # Calculation ---------------------------------------------------------------------------------


  De.seconds <- data[,1]
  De.error.seconds <- data[,2]

  De.gray <- NA
  De.error.gray <- NA

  if(is(dose.rate,"data.frame")){
    De.gray <- round(De.seconds*dose.rate[,1], digits=2)

  }else{
    De.gray <- round(De.seconds*dose.rate[1], digits=2)

  }


  if(method == "gaussian"){

    if(is(dose.rate,"data.frame")){
       De.error.gray <- round(sqrt((De.seconds*dose.rate[,2])^2+(dose.rate[,1]*De.error.seconds)^2), digits=3)

    }else{
      De.error.gray <- round(sqrt((De.seconds*dose.rate[2])^2+(dose.rate[1]*De.error.seconds)^2), digits=3)

    }

  }else if (method == "absolute"){

    if(is(dose.rate,"data.frame")){
      De.error.gray <- round(abs(dose.rate[,1] * De.error.seconds) + abs(De.seconds * dose.rate[,2]), digits=3)

    }else{
      De.error.gray <- round(abs(dose.rate[1] * De.error.seconds) + abs(De.seconds * dose.rate[2]), digits=3)

    }

  }else{

    stop("[Second2Gray()] unsupported error calculation method!" )

  }

  data <- data.frame(De=De.gray, De.error=De.error.gray)

  return(data)

}
