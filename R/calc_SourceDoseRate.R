#' @title Calculation of the source dose rate via the date of measurement
#'
#' @description
#' Calculating the dose rate of the irradiation source via the date of
#' measurement based on: source calibration date, source dose rate, dose rate
#' error. The function returns a data.frame that provides the input argument
#' dose_rate for the function [convert_Second2Gray].
#'
#' Calculation of the source dose rate based on the time elapsed since the last
#' calibration of the irradiation source. Decay parameters assume a Sr-90 beta
#' source. \deqn{dose.rate = D0 * exp(-log(2) / T.1/2 * t)} \cr with: D0 <-
#' calibration dose rate T.1/2 <- half-life of the source nuclide (here in
#' days) t <- time since source calibration (in days) log(2) / T.1/2 equals the
#' decay constant lambda
#'
#' Information on the date of measurements may be taken from the data's
#' original .BIN file (using e.g., `BINfile <- readBIN2R()` and the slot
#' `BINfile@@METADATA$DATE`)
#'
#' **Allowed source types and related values**
#'
#' \tabular{rllll}{
#'  **#** \tab **Source type** \tab **T.1/2** \tab **Reference** \cr
#'  `[1]` \tab Sr-90 \tab 28.90 y \tab NNDC, Brookhaven National Laboratory \cr
#'  `[2]`\tab Am-214 \tab 432.6 y \tab NNDC, Brookhaven National Laboratory \cr
#'  `[3]` \tab Co-60 \tab 5.274 y \tab NNDC, Brookhaven National Laboratory \cr
#'  `[4` \tab Cs-137 \tab 30.08 y \tab NNDC, Brookhaven National Laboratory}
#'
#' @param measurement.date [character] or [Date] (*with default*): Date of measurement in `"YYYY-MM-DD"`.
#' If no value is provided, the date will be set to today. The argument can be provided as vector.
#'
#' @param calib.date [character] or [Date] (**required**):
#' date of source calibration in `"YYYY-MM-DD"`
#'
#' @param calib.dose.rate [numeric] (**required**):
#' dose rate at date of calibration in Gy/s or Gy/min
#'
#' @param calib.error [numeric] (**required**):
#' error of dose rate at date of calibration Gy/s or Gy/min
#'
#' @param source.type [character] (*with default*):
#' specify irradiation source (`Sr-90`, `Co-60`, `Cs-137`, `Am-214`),
#' see details for further information
#'
#' @param dose.rate.unit [character] (*with default*):
#' specify dose rate unit for input (`Gy/min` or `Gy/s`), the output is given in
#' Gy/s as valid for the function [convert_Second2Gray]
#'
#' @param predict [integer] (*with default*):
#' option allowing to predict the dose rate of the source over time in days
#' set by the provided value. Starting date is the value set with
#' `measurement.date`, e.g., `calc_SourceDoseRate(..., predict = 100)` calculates
#' the source dose rate for the next 100 days.
#'
#' @return
#' Returns an S4 object of type [RLum.Results-class].
#' Slot `data` contains a [list] with the following structure:
#'
#' ```
#' $ dose.rate (data.frame)
#' .. $ dose.rate
#' .. $ dose.rate.error
#' .. $ date (corresponding measurement date)
#' $ parameters (list)
#' .. $ source.type
#' .. $ halflife
#' .. $ dose.rate.unit
#' $ call (the original function call)
#' ```
#'
#' The output should be accessed using the function [get_RLum].
#' A plot method of the output is provided via [plot_RLum].
#'
#' @note
#' Please be careful when using the option `predict`, especially when a
#' multiple set for `measurement.date` and `calib.date` is provided. For the
#' source dose rate prediction, the function takes the last `measurement.date`
#' value and predicts from that the source dose rate for the number of days
#' requested, that is: the (multiple) original input will be replaced.
#' However, the function does not change entries for the calibration dates,
#' but mixes them up. Therefore, it is not recommended to use this option
#' when multiple calibration dates (`calib.date`) are provided.
#'
#' @section Function version: 0.3.2
#'
#' @author
#' Margret C. Fuchs, HZDR, Helmholtz-Institute Freiberg for Resource Technology (Germany) \cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'
#' @seealso [convert_Second2Gray], [get_RLum], [plot_RLum]
#'
#' @references
#' NNDC, Brookhaven National Laboratory `http://www.nndc.bnl.gov/`
#'
#' @keywords manip
#'
#' @examples
#'
#' ##(1) Simple function usage
#' ##Basic calculation of the dose rate for a specific date
#' dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
#'                                   calib.date = "2014-12-19",
#'                                   calib.dose.rate = 0.0438,
#'                                   calib.error = 0.0019)
#'
#' ##show results
#' get_RLum(dose.rate)
#'
#' ##(2) Usage in combination with another function (e.g., convert_Second2Gray() )
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## use the calculated variable dose.rate as input argument
#' ## to convert De(s) to De(Gy)
#' convert_Second2Gray(ExampleData.DeValues$BT998, dose.rate)
#'
#' ##(3) source rate prediction and plotting
#' dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
#'                                   calib.date = "2014-12-19",
#'                                   calib.dose.rate = 0.0438,
#'                                   calib.error = 0.0019,
#'                                   predict = 1000)
#' plot_RLum(dose.rate)
#'
#'
#'##(4) export output to a LaTeX table (example using the package 'xtable')
#'\dontrun{
#' xtable::xtable(get_RLum(dose.rate))
#'
#'}
#'
#' @export
calc_SourceDoseRate <- function(
  measurement.date = Sys.Date(),
  calib.date,
  calib.dose.rate,
  calib.error,
  source.type = "Sr-90",
  dose.rate.unit = "Gy/s",
  predict = NULL
) {
  .set_function_name("calc_SourceDoseRate")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_class(measurement.date, c("Date", "character"))
  .validate_class(calib.date, c("Date", "character"))

  if (is(measurement.date, "character")) {
        measurement.date <- as.Date(measurement.date)
      }

  ##calibration date
  if(is(calib.date, "character")) {
    calib.date <- as.Date(calib.date)
  }

  ## source type and dose rate unit
  source.type <- .validate_args(source.type,
                                c("Sr-90", "Am-214", "Co-60", "Cs-137"))
  dose.rate.unit <- .validate_args(dose.rate.unit, c("Gy/s", "Gy/min"))

  # --- if predict is set
  if(!is.null(predict) && predict > 1){
    measurement.date <- seq(tail(measurement.date), by = 1, length = predict)
  }

  # -- calc days since source calibration
  decay.days <- measurement.date - calib.date


  # -- calc dose rate of source at date of measurement, considering the chosen source-type

  ##set halflife
  halflife.years  <- switch(
    source.type,
    "Sr-90" = 28.90,
    "Am-241" = 432.6,
    "Co-60" = 5.274,
    "Cs-137" = 30.08
    )

  halflife.days  <- halflife.years * .const$year_d

  # N(t) = N(0)*e^((lambda * t) with lambda = log(2)/T1.2)
  measurement.dose.rate <- (calib.dose.rate) *
    exp((-log(2) / halflife.days) * as.numeric(decay.days))
  measurement.dose.rate.error <- (calib.error) *
    exp((-log(2) / halflife.days) * as.numeric(decay.days))

  # -- convert to input unit to [Gy/s]
  if(dose.rate.unit == "Gy/min"){
    source.dose.rate <- measurement.dose.rate / 60
    source.dose.rate.error <- source.dose.rate *
      (measurement.dose.rate.error / measurement.dose.rate)

  }else if(dose.rate.unit == "Gy/s"){
    source.dose.rate <- measurement.dose.rate
    source.dose.rate.error <- measurement.dose.rate.error
  }

  # Output --------------------------------------------------------------------------------------

  dose_rate <- data.frame(
    dose.rate = source.dose.rate,
    dose.rate.error = source.dose.rate.error,
    date = measurement.date,
    stringsAsFactors = TRUE
  )

  temp.return <- set_RLum(
    class = "RLum.Results",
    data = list(
      dose.rate = dose_rate,
      parameters = list(source.type = source.type,
                        halflife = halflife.years,
                        dose.rate.unit = dose.rate.unit),
      call = sys.call()
    ))

  return(temp.return)
}
