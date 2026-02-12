#' @title Transform a CW-OSL curve into a pLM-OSL curve
#'
#' @description Transforms a conventionally measured continuous-wave (CW) curve into a
#' pseudo linearly modulated (pLM) curve using the equations given in Bulur
#' (2000).
#'
#' @details
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
#' @inheritParams convert_CW2pLMi
#'
#' @return
#' The function returns the same data type as the input data type with
#' the transformed curve values ([data.frame] or [Luminescence::RLum.Data.Curve-class]).
#'
#' @note
#' The transformation is recommended for curves recorded with a channel
#' resolution of at least 0.05 s/channel.
#'
#' @section Function version: 0.4.2
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::convert_CW2pHMi], [Luminescence::convert_CW2pLMi], [Luminescence::convert_CW2pPMi],
#' [Luminescence::fit_LMCurve], [lm], [Luminescence::RLum.Data.Curve-class]
#'
#' @references
#' Bulur, E., 2000. A simple transformation for converting CW-OSL
#' curves to LM-OSL curves. Radiation Measurements 32, 141-145.
#'
#' **Further Reading**
#'
#' Bulur, E., 1996. An Alternative Technique For Optically Stimulated
#' Luminescence (OSL) Experiment. Radiation Measurements 26, 701-709.
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
#' values.transformed <- convert_CW2pLM(values)
#'
#' ##plot
#' plot(values.transformed)
#'
#' @export
convert_CW2pLM <- function(
  values
) {
  .set_function_name("convert_CW2pLM")
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

  # Calculation -------------------------------------------------------------

  ##curve transformation
  P<-2*max(temp.values[,1])
  u<-((2*temp.values[,1]*P)^0.5)

  ##cw >> plm conversion, according Bulur, 2000
  temp.values[,2]<-temp.values[,2]*(u/P)
  temp.values<-data.frame(u,temp.values[,2])


  # Return values -----------------------------------------------------------

  ##returns the same data type as the input
  if (is.data.frame(values)) {
    return(temp.values)
  }

  set_RLum(
      class = "RLum.Data.Curve",
      recordType = values@recordType,
      data = as.matrix(temp.values),
      info = values@info)
}
