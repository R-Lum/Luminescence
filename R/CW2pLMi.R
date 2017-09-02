#' Transform a CW-OSL curve into a pLM-OSL curve via interpolation under linear
#' modulation conditions
#'
#' Transforms a conventionally measured continuous-wave (CW) OSL-curve into a
#' pseudo linearly modulated (pLM) curve under linear modulation conditions
#' using the interpolation procedure described by Bos & Wallinga (2012).
#'
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
#'
#' **NOTE:**
#' The number of values for t' < `min(t)` depends on the stimulation
#' period (P) and therefore on the stimulation rate 1/P. To avoid the
#' production of too many artificial data at the raising tail of the determined
#' pLM curves it is recommended to use the automatic estimation routine for
#' `P`, i.e. provide no own value for `P`.
#'
#' @param values [RLum.Data.Curve-class] or [data.frame] (**required**):
#' [RLum.Data.Curve-class] or `data.frame` with measured curve data of type 
#' stimulation time (t) (`values[,1]`) and measured counts (cts) (`values[,2]`)
#' 
#' @param P [vector] (*optional*): 
#' stimulation time in seconds. If no value is given the optimal value is 
#' estimated automatically (see details). Greater values of P produce more 
#' points in the rising tail of the curve.
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
#' @section Function version: 0.3.1
#' 
#' @author 
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#'
#' Based on comments and suggestions from:\cr 
#' Adrie J.J. Bos, Delft University of Technology, The Netherlands
#' 
#' @seealso [CW2pLM], [CW2pHMi], [CW2pPMi], [fit_LMCurve], 
#' [RLum.Data.Curve-class]
#' 
#' @references 
#' Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL
#' signal components. Radiation Measurements, 47, 752-758.
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
#' ##(1)
#' ##load CW-OSL curve data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ##transform values
#' values.transformed <- CW2pLMi(ExampleData.CW_OSL_Curve)
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
#' values.t <- CW2pLMi(values, P = 1/20)
#' lines(values[1:length(values.t[,1]),1],CW2pLMi(values, P = 1/20)[,2],
#'       col = "red", lwd = 1.3)
#' text(0.03,4500,"LM", col = "red", cex = .8)
#'
#' values.t <- CW2pHMi(values, delta = 40)
#' lines(values[1:length(values.t[,1]),1],CW2pHMi(values, delta = 40)[,2],
#'       col = "black", lwd = 1.3)
#' text(0.005,3000,"HM", cex =.8)
#'
#' values.t <- CW2pPMi(values, P = 1/10)
#' lines(values[1:length(values.t[,1]),1], CW2pPMi(values, P = 1/10)[,2],
#'       col = "blue", lwd = 1.3)
#' text(0.5,6500,"PM", col = "blue", cex = .8)
#'
#' @md
#' @export
CW2pLMi<- function(
  values,
  P
){

  # (0) Integrity checks -------------------------------------------------------

  ##(1) data.frame or RLum.Data.Curve object?
  if(is(values, "data.frame") == FALSE & is(values, "RLum.Data.Curve") == FALSE){
    stop("[CW2pLMi()] 'values' object has to be of type 'data.frame' or 'RLum.Data.Curve'!", call. = FALSE)

  }

  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if(is(values, "RLum.Data.Curve") == TRUE){

    if(!grepl("OSL", values@recordType) & !grepl("IRSL", values@recordType)){

      stop(paste("[CW2pLMi()] recordType ",values@recordType, " is not allowed for the transformation!",
                 sep=""), call. = FALSE)

    }else{

      temp.values <- as(values, "data.frame")

    }

  }else{

    temp.values <- values

  }


  # (1) Transform values ------------------------------------------------------------------------


  ##(a) log transformation of the CW-OSL count values
  CW_OSL.log<-log(temp.values[,2])

  ##(b) time transformation t >> t'
  t<-temp.values[,1]

  ##set P
  ##if no values for P is set selected a P value for a maximum of
  ##two extrapolation points
  if(missing(P)==TRUE){

    i<-10
    P<-1/i
    t.transformed<-0.5*1/P*t^2

    while(length(t.transformed[t.transformed<min(t)])>2){

      P<-1/i
      t.transformed<-0.5*1/P*t^2
      i<-i+10

    }#end::while
  }else{

    if(P==0){stop("[CW2pLMi] P has to be > 0!", call. = FALSE)}
    t.transformed<-0.5*1/P*t^2

  }
  #endif

  # (2) Interpolation ---------------------------------------------------------------------------

  ##interpolate values, values beyond the range return NA values
  CW_OSL.interpolated<-approx(t,CW_OSL.log, xout=t.transformed, rule=1 )

  ##combine t.transformed and CW_OSL.interpolated in a data.frame
  temp<-data.frame(x=t.transformed, y=unlist(CW_OSL.interpolated$y))

  ##Problem: I rare cases the interpolation is not working properely and Inf or NaN values are returned

  ##Fetch row number of the invalid values
  invalid_values.id<-c(which(is.infinite(temp[,2]) | is.nan(temp[,2])))

  ##interpolate between the lower and the upper value
  invalid_values.interpolated<-sapply(1:length(invalid_values.id),
                                      function(x) {
                                        mean(c(temp[invalid_values.id[x]-1,2],temp[invalid_values.id[x]+1,2]))
                                      }
  )

  ##replace invalid values in data.frame with newly interpolated values
  if(length(invalid_values.id)>0){
    temp[invalid_values.id,2]<-invalid_values.interpolated
  }

  # (3) Extrapolate first values of the curve ---------------------------------------------------


  ##(a) - find index of first rows which contain NA values (needed for extrapolation)
  temp.sel.id<-min(which(is.na(temp[,2])==FALSE))

  ##(b) - fit linear function
  fit.lm<-lm(y ~ x,data.frame(x=t[1:2],y=CW_OSL.log[1:2]))

  ##select values to extrapolate and predict (extrapolate) values based on the fitted function
  x.i<-data.frame(x=temp[1:(min(temp.sel.id)-1),1])
  y.i<-predict(fit.lm,x.i)

  ##replace NA values by extrapolated values
  temp[1:length(y.i),2]<-y.i

  ##set method values
  temp.method<-c(rep("extrapolation",length(y.i)),rep("interpolation",(length(temp[,2])-length(y.i))))

  ##print a warning message for more than two extrapolation points
  if(length(y.i)>2){warning("t' is beyond the time resolution and more than two data points have been extrapolated!")}

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
  if(is(values, "data.frame") == TRUE){

    values <- temp.values
    return(values)

  }else{


    ##add old info elements to new info elements
    temp.info <- c(values@info,
                   CW2pLMi.x.t = list(temp.values$x.t),
                   CW2pLMi.method = list(temp.values$method))

    newRLumDataCurves.CW2pLMi <- set_RLum(
      class = "RLum.Data.Curve",
      recordType = values@recordType,
      data = as.matrix(temp.values[,1:2]),
      info = temp.info)
    return(newRLumDataCurves.CW2pLMi)

  }

}
