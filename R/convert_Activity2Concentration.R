#' Convert Nuclide Activities to Concentrations and Vice Versa
#'
#'
#' @param data \code{\link{data.frame}} \bold{(required)}: provide dose rate data (activity or concentration)
#' in three columns. The first column indicates the nuclides, the 2nd column measured value and
#' in the 3rd column its error value. Allowed nuclide data are \code{'U-238'}, \code{'Th-232'} and \code{'K-40'}.
#' See examples for an example.
#'
#' @param input_unit \code{\link{character}} (with default): specify unit of input data given in the
#' dose rate data frame, choose between \code{'Bq/kg'} and \code{'ppm/\%'} the default is \code{'Bq/kg'}
#'
#' @param verbose \code{\link{logical}} (with default): enable or disable verbose mode
#'
#' @section Function version: 0.1.0
#'
#' @author Margret C. Fuchs, Helmholtz-Institut Freiberg for Resource Technology (Germany)
#'
#' @references -
#'
#' @keywords IO
#'
#' @examples
#'
#' ##construct data.frame
#' data <- data.frame(
#'  NUCLIDES = c("U-238", "Th-232", "K-40"),
#'  VALUE = c(40,80,100),
#'  VALUE_ERROR = c(4,8,10),
#'  stringsAsFactors = FALSE)
#'
#' ##perform analysis
#' convert_Activity2Concentration(data)
#'
#' @export
convert_Activity2Concentration <- function(
  data,
  input_unit = "Bq/kg",
  verbose = TRUE

){

  ##TODO
  ## - add references
  ## - add details to the conversion
  ## - add unit test after references have been provided (@RLumSK)

  # Integrity checks ----------------------------------------------------------------------------
  if(missing(data)){
    stop("[convert_Activity2Concentration()] I'm still waiting for input data ...", call. = FALSE)

  }

  if(ncol(data)<3){
    stop("[convert_Activity2Concentration()] Input data.frame should have at least three columns.", call. = FALSE)

  }


  # Set output data.frame -----------------------------------------------------------------------
  output <- data.frame(
    NUCLIDE = rep(NA, nrow(data)),
    ACTIVIY = rep(NA, nrow(data)),
    ACTIVIY_ERROR = rep(NA, nrow(data)),
    CONC = rep(NA, nrow(data)),
    CONC_ERROR = rep(NA, nrow(data)),
    stringsAsFactors = FALSE
  )

  ##set column names
  colnames(output) <- c(
    "NUCLIDE",
    "ACTIVIY (Bq/kg)", "ACTIVIY ERROR (Bq/kg)",
    "CONC. (ppm/%)",
    "CONC. ERROR (ppm/%)")

  ##set column for output
  output$NUCLIDE = data[[1]]

  # Set conversion factors ----------------------------------------------------------------------

  #############################################################################
  u <- which(data$NUCLIDE == "U-238")
  t <- which(data$NUCLIDE == "Th-232")
  k <- which(data$NUCLIDE == "K-40")

  convers.factor.U238   <- 12.35
  convers.factor.Th232  <- 4.057
  convers.factor.K40    <- 309

  # Run conversion ------------------------------------------------------------------------------

  ##Activity to concentration
  if(input_unit == "Bq/kg"){
    output[u,4:5] <- data[u,2:3] / convers.factor.U238
    output[t,4:5] <- data[t,2:3] / convers.factor.Th232
    output[k,4:5] <- data[k,2:3] / convers.factor.K40

    output[u,2:3] <- data[u,2:3]
    output[t,2:3] <- data[t,2:3]
    output[k,2:3] <- data[k,2:3]

  }

  ##Concentration to activity
  if(input_unit == "ppm/%"){
    data[u,2:3] <- data[u,2:3] * convers.factor.U238
    data[t,2:3] <- data[t,2:3] * convers.factor.Th232
    data[k,2:3] <- data[k,2:3] * convers.factor.K40

    output[u,5:6] <- data[u,2:3]
    output[t,5:6] <- data[t,2:3]
    output[k,5:6] <- data[k,2:3]

  }

  # Return value --------------------------------------------------------------------------------
  if(verbose){
    print(output)

  }

  invisible(set_RLum(
    class = "RLum.Results",
    data = list(data = output),
    info = list(call = sys.call())
    ))

}
