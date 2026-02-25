#' @title Transform a CW-OSL curve into a pLM-OSL curve via interpolation under linear
#' modulation conditions
#'
#' @description Transforms a conventionally measured continuous-wave (CW) OSL-curve into a
#' pseudo linearly modulated (pLM) curve under linear modulation conditions
#' using the interpolation procedure described by Bos & Wallinga (2012).
#'
#' @details
#' The complete procedure of the transformation is given in Bos & Wallinga
#' (2012). The input `data.frame` consists of two columns: time (t) and
#' count values (CW(t))
#'
#' **Nomenclature**
#'
#' - P = stimulation time (s)
#' - 1/P = stimulation rate (1/s)
#'
#' **Internal transformation steps**
#'
#' (1)
#' log(CW-OSL) values
#'
#' (2)
#' Calculate t' which is the transformed time:
#' \deqn{t' = 1/2*1/P*t^2}
#'
#' (3)
#' Interpolate CW(t'), i.e. use the log(CW(t)) to obtain the count values
#' for the transformed time (t'). Values beyond `min(t)` and `max(t)`
#' produce `NA` values.
#'
#' (4)
#' Select all values for t' < `min(t)`, i.e. values beyond the time resolution
#' of t. Select the first two values of the transformed data set which contain
#' no `NA` values and use these values for a linear fit using [lm].
#'
#' (5)
#' Extrapolate values for t' < `min(t)` based on the previously obtained
#' fit parameters.
#'
#' (6)
#' Transform values using
#' \deqn{pLM(t) = t/P*CW(t')}
#'
#' (7)
#' Combine values and truncate all values for t' > `max(t)`
#'
#' **Note:**
#' The number of values for t' < `min(t)` depends on the stimulation
#' period (P) and therefore on the stimulation rate 1/P. To avoid the
#' production of too many artificial data at the raising tail of the determined
#' pLM curves it is recommended to use the automatic estimation routine for
#' `P`, i.e. provide no own value for `P`.
#'
#' @param object [Luminescence::RLum.Data.Curve-class] or [data.frame] (**required**):
#' [Luminescence::RLum.Data.Curve-class] object or a `data.frame` with measured
#' curve data of type stimulation time (t) (`object[, 1]`) and measured counts
#' (cts) (`object[, 2]`).
#'
#' @param P [numeric] (*optional*):
#' stimulation time in seconds. If set to `NULL`, the optimal value is
#' estimated automatically (see details). Greater values of P produce more
#' points in the rising tail of the curve.
#'
#' @param ... currently not used.
#'
#' @return
#' The function returns the same data type as the input data type with
#' the transformed curve values.
#'
#' **`RLum.Data.Curve`**
#'
#' \tabular{rl}{
#' `$CW2pLMi.x.t` \tab: transformed time values \cr
#' `$CW2pLMi.method` \tab: used method for the production of the new data points
#' }
#'
#' @note
#' According to Bos & Wallinga (2012) the number of extrapolated points
#' should be limited to avoid artificial intensity data. If `P` is
#' provided manually and more than two points are extrapolated, a warning
#' message is returned.
#'
#' @section Function version: 0.3.4
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#' Based on comments and suggestions from:
#' Adrie J.J. Bos, Delft University of Technology, The Netherlands
#'
#' @seealso [Luminescence::convert_CW2pLM], [Luminescence::convert_CW2pHMi], [Luminescence::convert_CW2pPMi],
#' [Luminescence::fit_LMCurve], [Luminescence::RLum.Data.Curve-class]
#'
#' @references
#' Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL
#' signal components. Radiation Measurements 47, 752-758.
#'
#' **Further Reading**
#'
#' Bulur, E., 1996. An Alternative Technique For
#' Optically Stimulated Luminescence (OSL) Experiment. Radiation Measurements
#' 26, 701-709.
#'
#' Bulur, E., 2000. A simple transformation for converting CW-OSL curves to
#' LM-OSL curves. Radiation Measurements 32, 141-145.
#'
#' @keywords manip
#'
#' @examples
#'
#' ##(1)
#' ##load CW-OSL curve data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ##transform values
#' values.transformed <- convert_CW2pLMi(ExampleData.CW_OSL_Curve)
#'
#' ##plot
#' plot(values.transformed$x, values.transformed$y.t, log = "x")
#'
#' ##(2) - produce Fig. 4 from Bos & Wallinga (2012)
#' ##load data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#' values <- CW_Curve.BosWallinga2012
#'
#' ##open plot area
#' plot(NA, NA,
#'      xlim = c(0.001,10),
#'      ylim = c(0,8000),
#'      ylab = "pseudo OSL (cts/0.01 s)",
#'      xlab = "t [s]",
#'      log = "x",
#'      main = "Fig. 4 - Bos & Wallinga (2012)")
#'
#'
#' values.t <- convert_CW2pLMi(values, P = 1/20)
#' lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
#'       col = "red", lwd = 1.3)
#' text(0.03,4500,"LM", col = "red", cex = .8)
#'
#' values.t <- convert_CW2pHMi(values, delta = 40)
#' lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
#'       col = "black", lwd = 1.3)
#' text(0.005,3000,"HM", cex =.8)
#'
#' values.t <- convert_CW2pPMi(values, P = 1/10)
#' lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
#'       col = "blue", lwd = 1.3)
#' text(0.5,6500,"PM", col = "blue", cex = .8)
#'
#' @export
convert_CW2pLMi<- function(
  object,
  P = NULL,
  ...
) {
  .set_function_name("convert_CW2pLMi")
  on.exit(.unset_function_name(), add = TRUE)

  ## deprecated argument
  if ("values" %in% ...names()) {
    object <- list(...)$values
    .deprecated(old = "values", new = "object", since = "1.2.0")
  }

  ## Integrity checks -------------------------------------------------------
  temp.values <- .prepare_CW2pX(object)
  .validate_positive_scalar(P, null.ok = TRUE)

  # (1) Transform values ------------------------------------------------------------------------

  ##(a) log transformation of the CW-OSL count values
  CW_OSL.log<-log(temp.values[,2])

  ##(b) time transformation t >> t'
  t<-temp.values[,1]

  ##set P
  ##if no values for P is set selected a P value for a maximum of
  ##two extrapolation points
  if (is.null(P)) {
    i<-10
    P<-1/i
    t.transformed<-0.5*1/P*t^2

    while(length(t.transformed[t.transformed<min(t)])>2){
      P<-1/i
      t.transformed<-0.5*1/P*t^2
      i<-i+10

    }#end::while
  }else{
    t.transformed<-0.5*1/P*t^2
  }
  #endif

  # (2) Interpolation ---------------------------------------------------------------------------

  ##interpolate values, values beyond the range return NA values
  CW_OSL.interpolated<-approx(t,CW_OSL.log, xout=t.transformed, rule=1 )
  if (all(is.na(CW_OSL.interpolated$y))) {
    .throw_error("All points are outside the interpolation range")
  }

  ## In some cases the interpolation algorithm is not working properly, and
  ## Inf or NaN values are produced
  interpolated <- .fix_interpolation_inf_nan(unlist(CW_OSL.interpolated$y),
                                             warn = FALSE)

  ## combine t.transformed and CW_OSL.interpolated in a data.frame
  temp <- data.frame(x = t.transformed, y = interpolated)

  # (3) Extrapolate first values of the curve ---------------------------------------------------

  res <- .extrapolate_first(temp, t = t[1:2], y = CW_OSL.log[1:2])
  temp <- res$df
  temp.method <- res$method

  # (4) Convert, transform and combine values ---------------------------------------------------

  ##unlog CW-OSL count values, i.e. log(CW) >> CW
  CW_OSL<-exp(temp$y)

  ##transform CW-OSL values to pLM-OSL values
  pLM<-1/P*t*CW_OSL

  ##combine all values and exclude NA values
  temp.values <- data.frame(x=t,y.t=pLM,x.t=t.transformed, method=temp.method)
  temp.values <- na.exclude(temp.values)

  # (5) Return values ---------------------------------------------------------------------------

  ##returns the same data type as the input
  if (is.data.frame(object)) {
    return(temp.values)
  }

    ##add old info elements to new info elements
    temp.info <- c(object@info,
                   CW2pLMi.x.t = list(temp.values$x.t),
                   CW2pLMi.method = list(temp.values$method))

  set_RLum(
      class = "RLum.Data.Curve",
      recordType = object@recordType,
      data = as.matrix(temp.values[,1:2]),
      info = temp.info)
}

.prepare_CW2pX <- function(object) {
  .validate_class(object, c("data.frame", "RLum.Data.Curve"))
  .validate_not_empty(object)
  if (ncol(object) < 2) {
    .throw_error("'object' should have 2 columns")
  }

  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if (inherits(object, "RLum.Data.Curve")) {
    if (!grepl("OSL", object@recordType) && !grepl("IRSL", object@recordType)) {
      .throw_error("recordType ", object@recordType,
                   " is not allowed for the transformation")
    }

    object <- as(object, "data.frame")
  }
  if (isTRUE(any(object[, 1] < 0))) {
    .throw_error("'object' cannot contain negative times")
  }
  if (isTRUE(any(object[, 2] < 0))) {
    .throw_error("'object' cannot contain negative counts")
  }

  ## remove NAs
  object <- na.exclude(object)
  if (nrow(object) < 2) {
    .throw_error("'object' should have at least 2 non-missing values")
  }

  object
}

.fix_interpolation_inf_nan <- function(values, warn) {
  invalid.idx <- which(is.infinite(values) | is.nan(values))
  if (length(invalid.idx) == 0)
    return(values)
  if (all(is.infinite(values) | is.na(values)))
    .throw_error("All interpolated values are Inf/NaN/NA, check your data")

  ## replace invalid values with mean of the value before and the value after
  values[invalid.idx] <- sapply(invalid.idx,
                                function(x) mean(values[c(x - 1, x + 1)]))

  if (warn) {
    .throw_warning(length(invalid.idx), " invalid values found, ",
                   "replaced by the mean of the nearest values")
  }

  values
}

.extrapolate_first <- function(df, t, y) {
  ##(a) - find index of first rows which contain NA values (needed for extrapolation)
  temp.sel.id <- min(which(!is.na(df[, 2])))

  ##(b) - fit linear function
  fit.lm <- stats::lm(y ~ x, data.frame(x = t, y = y))

  ## select values to extrapolate and predict (extrapolate) values based on
  ## the fitted function
  x.i <- data.frame(x = df[1:(min(temp.sel.id) - 1), 1])
  y.i <- predict(fit.lm, x.i)

  ## replace NA values by extrapolated values
  df[1:length(y.i), 2] <- y.i

  ##set method values
  temp.method <- c(rep("extrapolation", length(y.i)),
                   rep("interpolation", length(df[, 2]) - length(y.i)))

  ## print a warning message for more than two extrapolation points
  if (length(y.i) > 2) {
    .throw_warning("t' is beyond the time resolution and more than two ",
                   "data points have been extrapolated")
  }

  list(df = df, method = temp.method)
}
