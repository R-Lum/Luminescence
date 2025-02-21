#' @title Converting equivalent dose values from seconds (s) to Gray (Gy)
#'
#' @description
#' Conversion of absorbed radiation dose in seconds (s) to the SI unit Gray
#' (Gy) including error propagation. Normally used for equivalent dose data.
#'
#' Calculation of De values from seconds (s) to Gray (Gy)
#'
#' \deqn{De [Gy] = De [s] * Dose Rate [Gy/s])}
#'
#' Provided calculation error propagation methods for error calculation
#' (with `'se'` as the standard error and `'DR'` of the dose rate of the beta-source):
#'
#' **(1) `omit`** (default)
#'
#' \deqn{se(De) [Gy] = se(De) [s] * DR [Gy/s]}
#'
#' In this case the standard error of the dose rate of the beta-source is
#' treated as systematic (i.e. non-random), it error propagation is omitted.
#' However, the error must be considered during calculation of the final age.
#' (cf. Aitken, 1985, pp. 242). This approach can be seen as method (2) (gaussian)
#' for the case the (random) standard error of the beta-source calibration is
#' 0. Which particular method is requested depends on the situation and cannot
#' be prescriptive.
#'
#' **(2) `gaussian`** error propagation
#'
#' \deqn{se(De) [Gy] = \sqrt((DR [Gy/s] * se(De) [s])^2 + (De [s] * se(DR) [Gy/s])^2)}
#'
#' Applicable under the assumption that errors of `De` and `se` are uncorrelated.
#'
#' **(3) `absolute`** error propagation
#'
#' \deqn{se(De) [Gy]= abs(DR [Gy/s] * se(De) [s]) + abs(De [s] * se(DR) [Gy/s])}
#'
#' Applicable under the assumption that errors of `De` and `se` are correlated.
#'
#'
#' @param data [data.frame] (**required**):
#' input values, structure: data (`values[,1]`) and data error (`values [,2]`)
#' are required
#'
#' @param dose.rate [RLum.Results-class], [data.frame] or [numeric] (**required**):
#' `RLum.Results` needs to be originated from the function [calc_SourceDoseRate],
#' for `vector` dose rate in Gy/s and dose rate error in Gy/s
#'
#' @param error.propagation [character] (*with default*):
#' error propagation method used for error calculation (`omit`, `gaussian` or
#' `absolute`), see details for further information
#'
#' @return
#' Returns a [data.frame] with converted values.
#'
#' @note
#' If no or a wrong error propagation method is given, the execution of the function is
#' stopped. Furthermore, if a `data.frame` is provided for the dose rate values is has to
#' be of the same length as the data frame provided with the argument `data`
#'
#' @section Function version: 0.6.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Michael Dietze, GFZ Potsdam (Germany)\cr
#' Margret C. Fuchs, HZDR, Helmholtz-Institute Freiberg for Resource Technology (Germany)
#'
#' @seealso [calc_SourceDoseRate]
#'
#' @references
#' Aitken, M.J., 1985. Thermoluminescence dating. Academic Press.
#'
#' @keywords manip
#'
#' @examples
#'
#' ##(A) for known source dose rate at date of measurement
#' ## - load De data from the example data help file
#' data(ExampleData.DeValues, envir = environment())
#' ## - convert De(s) to De(Gy)
#' convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))
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
#' convert_Second2Gray(ExampleData.DeValues$BT998, dose.rate)
#'
#' @md
#' @export
convert_Second2Gray <- function(
  data,
  dose.rate,
  error.propagation = "omit"
) {
  .set_function_name("convert_Second2Gray")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests --------------------------------------------------------

  .validate_class(data, "data.frame")
  .validate_class(dose.rate, c("RLum.Results", "data.frame", "numeric"))

  if (is.data.frame(dose.rate)) {
    if(nrow(dose.rate)!=nrow(data)){
      .throw_error("Data frames in 'data' and 'dose.rate' must have the same length")
    }
  } else if (inherits(dose.rate, "RLum.Results")) {

    ## check for right originator
    if(dose.rate@originator != "calc_SourceDoseRate"){
      .throw_error("Wrong originator for dose.rate 'RLum.Results' object")
    }

    dose.rate <- get_RLum(dose.rate, data.object = "dose.rate")
  }

  error.propagation <- .validate_args(error.propagation,
                                      c("omit", "gaussian", "absolute"))


  ## Calculation ------------------------------------------------------------

  De.seconds <- data[,1]
  De.error.seconds <- data[,2]
  De.gray <- round(De.seconds * dose.rate[[1]], digits = 2)

  if (error.propagation == "omit") {
    De.error.gray <- round(dose.rate[[1]] * De.error.seconds,
                           digits = 3)

  } else if(error.propagation == "gaussian") {
    De.error.gray <- round(sqrt((dose.rate[[1]] * De.error.seconds) ^ 2 +
                                (De.seconds * dose.rate[[2]]) ^ 2),
                           digits = 3)

  } else if (error.propagation == "absolute") {
    De.error.gray <- round(abs(dose.rate[[1]] * De.error.seconds) +
                           abs(De.seconds * dose.rate[[2]]),
                           digits = 3)
  }

  ## Return -----------------------------------------------------------------
  data <- data.frame(De=De.gray, De.error=De.error.gray)

  return(data)
}

#' Converting equivalent dose values from seconds (s) to Gray (Gy)
#'
#' @description
#' This function is defunct, use [convert_Second2Gray] instead.
#'
#' @param ... Unused.
#'
#' @md
#' @export
Second2Gray <- function(...) {
  .Defunct("convert_Second2Gray")
}
