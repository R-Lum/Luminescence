#' Transform a CW-OSL curve into a pHM-OSL curve via interpolation under
#' hyperbolic modulation conditions
#'
#' This function transforms a conventionally measured continuous-wave (CW)
#' OSL-curve to a pseudo hyperbolic modulated (pHM) curve under hyperbolic
#' modulation conditions using the interpolation procedure described by Bos &
#' Wallinga (2012).
#'
#' The complete procedure of the transformation is described in Bos & Wallinga
#' (2012). The input `data.frame` consists of two columns: time (t) and
#' count values (CW(t))
#'
#' **Internal transformation steps**
#'
#' (1) log(CW-OSL) values
#'
#' (2)
#' Calculate t' which is the transformed time:
#' \deqn{t' = t-(1/\delta)*log(1+\delta*t)}
#'
#' (3)
#' Interpolate CW(t'), i.e. use the log(CW(t)) to obtain the count values
#' for the transformed time (t'). Values beyond `min(t)` and `max(t)`
#' produce `NA` values.
#'
#' (4)
#' Select all values for t' < `min(t)`, i.e. values beyond the time
#' resolution of t. Select the first two values of the transformed data set
#' which contain no `NA` values and use these values for a linear fit
#' using [lm].
#'
#' (5)
#' Extrapolate values for t' < `min(t)` based on the previously
#' obtained fit parameters.
#'
#' (6)
#' Transform values using
#' \deqn{pHM(t) = (\delta*t/(1+\delta*t))*c*CW(t')}
#' \deqn{c = (1+\delta*P)/\delta*P}
#' \deqn{P = length(stimulation~period)}
#'
#' (7) Combine all values and truncate all values for t' > `max(t)`
#'
#'
#' **NOTE:**
#' The number of values for t' < `min(t)` depends on the stimulation rate
#' parameter `delta`. To avoid the production of too many artificial data
#' at the raising tail of the determined pHM curve, it is recommended to use
#' the automatic estimation routine for `delta`, i.e. provide no value for
#' `delta`.
#'
#' @param values [RLum.Data.Curve-class] or [data.frame] (**required**):
#' [RLum.Data.Curve-class] or [data.frame] with measured curve data of type
#' stimulation time (t) (`values[,1]`) and measured counts (cts) (`values[,2]`).
#'
#' @param delta [vector] (*optional*):
#' stimulation rate parameter, if no value is given, the optimal value is
#' estimated automatically (see details). Smaller values of delta produce more
#' points in the rising tail of
#' the curve.
#'
#' @return
#' The function returns the same data type as the input data type with
#' the transformed curve values.
#'
#'
#' **`RLum.Data.Curve`**
#'
#' \tabular{ll}{
#' `$CW2pHMi.x.t` \tab: transformed time values \cr
#' `$CW2pHMi.method` \tab: used method for the production of the new data points
#' }
#'
#' **`data.frame`**
#'
#' \tabular{ll}{
#' `$x` \tab: time\cr
#' `$y.t` \tab: transformed count values\cr
#' `$x.t` \tab: transformed time values \cr
#' `$method` \tab: used method for the production of the new data points
#' }
#'
#' @note
#' According to Bos & Wallinga (2012), the number of extrapolated points
#' should be limited to avoid artificial intensity data. If `delta` is
#' provided manually and more than two points are extrapolated, a warning
#' message is returned.
#'
#' The function [approx] may produce some `Inf` and `NaN` data.
#' The function tries to manually interpolate these values by calculating
#' the `mean` using the adjacent channels. If two invalid values are succeeding,
#' the values are removed and no further interpolation is attempted.
#' In every case a warning message is shown.
#'
#' @section Function version: 0.2.3
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Based on comments and suggestions from:\cr
#' Adrie J.J. Bos, Delft University of Technology, The Netherlands
#'
#' @seealso [convert_CW2pLM], [convert_CW2pLMi], [convert_CW2pPMi],
#' [fit_LMCurve], [lm], [RLum.Data.Curve-class]
#'
#' @references
#' Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL
#' signal components. Radiation Measurements, 47, 752-758.\cr
#'
#' **Further Reading**
#'
#' Bulur, E., 1996. An Alternative Technique For
#' Optically Stimulated Luminescence (OSL) Experiment. Radiation Measurements,
#' 26, 701-709.
#'
#' Bulur, E., 2000. A simple transformation for converting CW-OSL curves to
#' LM-OSL curves. Radiation Measurements, 32, 141-145.
#'
#' @keywords manip
#'
#' @examples
#'
#' ##(1) - simple transformation
#'
#' ##load CW-OSL curve data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ##transform values
#' values.transformed <- convert_CW2pHMi(ExampleData.CW_OSL_Curve)
#'
#' ##plot
#' plot(values.transformed$x, values.transformed$y.t, log = "x")
#'
#' ##(2) - load CW-OSL curve from BIN-file and plot transformed values
#'
#' ##load BINfile
#' #BINfileData<-readBIN2R("[path to BIN-file]")
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##grep first CW-OSL curve from ALQ 1
#' curve.ID<-CWOSL.SAR.Data@@METADATA[CWOSL.SAR.Data@@METADATA[,"LTYPE"]=="OSL" &
#'                                     CWOSL.SAR.Data@@METADATA[,"POSITION"]==1
#'                                   ,"ID"]
#'
#' curve.HIGH<-CWOSL.SAR.Data@@METADATA[CWOSL.SAR.Data@@METADATA[,"ID"]==curve.ID[1]
#'                                     ,"HIGH"]
#'
#' curve.NPOINTS<-CWOSL.SAR.Data@@METADATA[CWOSL.SAR.Data@@METADATA[,"ID"]==curve.ID[1]
#'                                        ,"NPOINTS"]
#'
#' ##combine curve to data set
#'
#' curve<-data.frame(x = seq(curve.HIGH/curve.NPOINTS,curve.HIGH,
#'                           by = curve.HIGH/curve.NPOINTS),
#'                   y=unlist(CWOSL.SAR.Data@@DATA[curve.ID[1]]))
#'
#'
#' ##transform values
#'
#' curve.transformed <- convert_CW2pHMi(curve)
#'
#' ##plot curve
#' plot(curve.transformed$x, curve.transformed$y.t, log = "x")
#'
#'
#' ##(3) - produce Fig. 4 from Bos & Wallinga (2012)
#'
#' ##load data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#' values <- CW_Curve.BosWallinga2012
#'
#' ##open plot area
#' plot(NA, NA,
#'      xlim=c(0.001,10),
#'      ylim=c(0,8000),
#'      ylab="pseudo OSL (cts/0.01 s)",
#'      xlab="t [s]",
#'      log="x",
#'      main="Fig. 4 - Bos & Wallinga (2012)")
#'
#' values.t <- convert_CW2pLMi(values, P = 1/20)
#' lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
#'       col="red" ,lwd=1.3)
#' text(0.03,4500,"LM", col="red" ,cex=.8)
#'
#' values.t <- convert_CW2pHMi(values, delta = 40)
#' lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
#'       col="black", lwd=1.3)
#' text(0.005,3000,"HM", cex=.8)
#'
#' values.t <- convert_CW2pPMi(values, P = 1/10)
#' lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
#'       col="blue", lwd=1.3)
#' text(0.5,6500,"PM", col="blue" ,cex=.8)
#'
#' @export
convert_CW2pHMi<- function(
  values,
  delta
) {
  .set_function_name("convert_CW2pHMi")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  ##(1) data.frame or RLum.Data.Curve object?
  .validate_class(values, c("data.frame", "RLum.Data.Curve"))
  .validate_not_empty(values)
  if (ncol(values) < 2) {
    .throw_error("'values' should have 2 columns")
  }

  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if (inherits(values, "RLum.Data.Curve")) {
    if(!grepl("OSL", values@recordType) & !grepl("IRSL", values@recordType)){

      .throw_error("recordType ", values@recordType,
                   " is not allowed for the transformation")
    }

    temp.values <- as(values, "data.frame")

  }else{

    temp.values <- values
  }

  ## remove NAs
  temp.values <- na.exclude(temp.values)
  if (nrow(temp.values) < 2) {
    .throw_error("'values' should have at least 2 non-missing values")
  }

  # (1) Transform values ------------------------------------------------------

  ##log transformation of the CW-OSL count values
  CW_OSL.log<-log(temp.values[,2])

  ##time transformation t >> t'
  t<-temp.values[,1]

  ##set delta
  ##if no values for delta is set selected a delta value for a maximum of
  ##two extrapolation points
  if(missing(delta)==TRUE){
    i<-10
    delta<-i
    t.transformed<-t-(1/delta)*log(1+delta*t)

    while(length(t.transformed[t.transformed<min(t)])>2){

      delta<-i
      t.transformed<-t-(1/delta)*log(1+delta*t)
      i<-i+10
    }
  }else{

    t.transformed<-t-(1/delta)*log(1+delta*t)
  }

  # (2) Interpolation ---------------------------------------------------------

  ##interpolate values, values beyond the range return NA values
  CW_OSL.interpolated <- approx(t,CW_OSL.log, xout=t.transformed, rule=1)


  ##combine t.transformed and CW_OSL.interpolated in a data.frame
  temp <- data.frame(x=t.transformed, y=unlist(CW_OSL.interpolated$y))

  ##Problem: In some cases the interpolation algorithm is not working properly
  ##and Inf or NaN values are returned

  ##fetch row number of the invalid values
  invalid_values.id <- c(which(is.infinite(temp[,2]) | is.nan(temp[,2])))

  if(length(invalid_values.id) > 0){
    .throw_warning(length(invalid_values.id), " invalid values have been found ",
                   "and replaced by the mean of the nearest values")
  }

  ##interpolate between the lower and the upper value
  invalid_values.interpolated <- sapply(invalid_values.id,
                                        function(x) mean(temp[c(x - 1, x + 1), 2]))

  ##replace invalid values in data.frame with newly interpolated values
  if(length(invalid_values.id)>0){
    temp[invalid_values.id,2]<-invalid_values.interpolated
  }

  # (3) Extrapolate first values of the curve ---------------------------------

  ##(a) - find index of first rows which contain NA values (needed for extrapolation)
  temp.sel.id<-min(which(is.na(temp[,2])==FALSE))

  ##(b) - fit linear function
  fit.lm <- stats::lm(y ~ x, data.frame(x = t[1:2], y = CW_OSL.log[1:2]))

  ##select values to extrapolate and predict (extrapolate) values based on the fitted function
  x.i<-data.frame(x=temp[1:(min(temp.sel.id)-1),1])
  y.i<-predict(fit.lm,x.i)

  ##replace NA values by extrapolated values
  temp[1:length(y.i),2]<-y.i

  ##set method values
  temp.method<-c(rep("extrapolation",length(y.i)),rep("interpolation",(length(temp[,2])-length(y.i))))

  ##print a warning message for more than two extrapolation points
  if (length(y.i) > 2) {
    .throw_warning("t' is beyond the time resolution and more than ",
                   "two data points have been extrapolated")
  }

  # (4) Convert, transform and combine values ---------------------------------

  ##unlog CW-OSL count values, i.e. log(CW) >> CW
  CW_OSL<-exp(temp$y)

  ##set values for c and P

  ##P is the stimulation period
  P<-max(temp.values[,1])

  ##c is a dimensionless constant
  c<-(1+(delta*P))/(delta*P)

  ##transform CW-OSL values to pLM-OSL values
  pHM<-((delta*t)/(1+(delta*t)))*c*CW_OSL

  ##combine all values and exclude NA values
  temp.values <- data.frame(x=t,y.t=pHM,x.t=t.transformed,method=temp.method)
  temp.values <- na.exclude(temp.values)

  # (5) Return values ---------------------------------------------------------

  ##returns the same data type as the input
  if(is(values, "data.frame") == TRUE){

    values <- temp.values
    return(values)

  }else{

    ##add old info elements to new info elements
    temp.info <- c(values@info,
                   CW2pHMi.x.t = list(temp.values$x.t),
                   CW2pHMi.method = list(temp.values$method))

    newRLumDataCurves.CW2pHMi <- set_RLum(
      class = "RLum.Data.Curve",
      recordType = values@recordType,
      data = as.matrix(temp.values[,1:2]),
      info = temp.info)
    return(newRLumDataCurves.CW2pHMi)
  }
}

#' @rdname convert_CW2pHMi
#' @export
CW2pHMi <- function(values, delta) {
  .Deprecated("convert_CW2pHMi", old = "CW2pHMi")
  convert_CW2pHMi(values, delta)
}
