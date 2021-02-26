#' Transform a CW-OSL curve into a pLM-OSL curve
#'
#' Transforms a conventionally measured continuous-wave (CW) curve into a
#' pseudo linearly modulated (pLM) curve using the equations given in Bulur
#' (2000).
#'
#' According to Bulur (2000) the curve data are transformed by introducing two
#' new parameters `P` (stimulation period) and `u` (transformed time):
#'
#' \deqn{P=2*max(t)} \deqn{u=\sqrt{(2*t*P)}}
#'
#' The new count values are then calculated by
#' \deqn{ctsNEW = cts(u/P)}
#'
#' and the returned `data.frame` is produced by: `data.frame(u,ctsNEW)`
#'
#' The output of the function can be further used for LM-OSL fitting.
#'
#' @param values [RLum.Data.Curve-class] or [data.frame] (**required**):
#' `RLum.Data.Curve` data object. Alternatively, a `data.frame` of the measured
#' curve data of type stimulation time (t) (`values[,1]`) and measured counts (cts)
#' (`values[,2]`) can be provided.
#'
#' @return
#' The function returns the same data type as the input data type with
#' the transformed curve values ([data.frame] or [RLum.Data.Curve-class]).
#'
#' @note
#' The transformation is recommended for curves recorded with a channel
#' resolution of at least 0.05 s/channel.
#'
#' @section Function version: 0.4.1
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [CW2pHMi], [CW2pLMi], [CW2pPMi], [fit_LMCurve], [lm],
#' [RLum.Data.Curve-class]
#'
#'
#' @references
#' Bulur, E., 2000. A simple transformation for converting CW-OSL
#' curves to LM-OSL curves. Radiation Measurements, 32, 141-145.
#'
#' **Further Reading**
#'
#' Bulur, E., 1996. An Alternative Technique For Optically Stimulated
#' Luminescence (OSL) Experiment. Radiation Measurements, 26, 701-709.
#'
#' @keywords manip
#'
#' @examples
#'
#' ##read curve from CWOSL.SAR.Data transform curve and plot values
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##read id for the 1st OSL curve
#' id.OSL <- CWOSL.SAR.Data@@METADATA[CWOSL.SAR.Data@@METADATA[,"LTYPE"] == "OSL","ID"]
#'
#' ##produce x and y (time and count data for the data set)
#' x<-seq(CWOSL.SAR.Data@@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@@METADATA[id.OSL[1],"NPOINTS"],
#'        CWOSL.SAR.Data@@METADATA[id.OSL[1],"HIGH"],
#'        by = CWOSL.SAR.Data@@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@@METADATA[id.OSL[1],"NPOINTS"])
#' y <- unlist(CWOSL.SAR.Data@@DATA[id.OSL[1]])
#' values <- data.frame(x,y)
#'
#' ##transform values
#' values.transformed <- CW2pLM(values)
#'
#' ##plot
#' plot(values.transformed)
#'
#' @md
#' @export
CW2pLM <- function(
  values
){

  # Integrity Checks --------------------------------------------------------

  ##(1) data.frame or RLum.Data.Curve object?
  if(is(values, "data.frame") == FALSE & is(values, "RLum.Data.Curve") == FALSE){
    stop("[CW2pLM()] 'values' object has to be of type 'data.frame' or 'RLum.Data.Curve'!", call. = FALSE)

  }

  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if(is(values, "RLum.Data.Curve") == TRUE){

    if(!grepl("OSL", values@recordType) & !grepl("IRSL", values@recordType)){

      stop(paste("[CW2pLM()] recordType ",values@recordType, " is not allowed for the transformation!",
                 sep=""), call. = FALSE)

    }else{

      temp.values <- as(values, "data.frame")

    }

  }else{

    temp.values <- values


  }


  # Calculation -------------------------------------------------------------


  ##curve transformation
  P<-2*max(temp.values[,1])
  u<-((2*temp.values[,1]*P)^0.5)

  ##cw >> plm conversion, according Bulur, 2000
  temp.values[,2]<-temp.values[,2]*(u/P)
  temp.values<-data.frame(u,temp.values[,2])


  # Return values -----------------------------------------------------------

  ##returns the same data type as the input

  if(is(values, "data.frame") == TRUE){

    values <- temp.values
    return(values)

  }else{

    newRLumDataCurves.CW2pLM <- set_RLum(
      class = "RLum.Data.Curve",
      recordType = values@recordType,
                                                    data = as.matrix(temp.values),
                                                    info = values@info)
    return(newRLumDataCurves.CW2pLM)

  }

}
