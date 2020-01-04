#' @title Convert Nuclide Activities to Concentrations and Vice Versa
#'
#' @description The function performs the conversion of the specific activities into
#' concentrations and vice versa for the nuclides U-238, Th-232 and K-40 to
#' harmonise the measurement unit with the required data input unit of
#' potential analytical tools for, e.g. dose rate calculation or related
#' functions such as [use_DRAC].
#'
#' @details The conversion from nuclide activity of a sample to nuclide concentration
#' is performed using conversion factors that are based on the mass-related
#' specific activity of the respective nuclides.
#' The factors can be calculated using the equation:
#'
#' \deqn{
#' A = avogadronumber * N.freq / N.mol.mass   *  ln(2) / N.half.life
#' }
#'
#' \deqn{
#' f = A / 10^6
#' }
#'
#' where:
#'
#' - `A` - specific activity of the nuclide
#' - `N.freq` - natural frequency of the isotop
#' - `N.mol.mass` molare mass
#' - `n.half.life` half-life of the nuclide
#'
#' example for U238:
#'
#' - \eqn{avogadronumber = 6.02214199*10^23}
#' - \eqn{uran.half.life = 1.41*10^17} (in s)
#' - \eqn{uran.mol.mass = 0.23802891} (in kg/mol)
#' - \eqn{uran.freq = 0.992745} (in mol)
#'
#' - \eqn{A.U = avogadronumber * uran.freq / uran.mol.mass * ln(2) / uran.half.life} (specific activity in Bq/kg)
#' - \eqn{f.U = A.kg / 10^6}
#'
#' @param data [data.frame] **(required)**:
#' provide dose rate data (activity or concentration) in three columns.
#' The first column indicates the nuclides, the 2nd column measured value and
#' in the 3rd column its error value. Allowed nuclide data are
#' `'U-238'`, `'Th-232'` and `'K-40'`. See examples for an example.
#'
#' @param input_unit [character] (*with default*):
#' specify unit of input data given in the dose rate data frame, choose between
#' 'Bq/kg' and 'ppm/%' the default is 'Bq/kg'
#'
#' @param verbose [logical] (*with default*):
#' enable or disable verbose mode
#'
#' @section Function version: 0.1.0
#'
#' @author Margret C. Fuchs, Helmholtz-Institut Freiberg for Resource Technology (Germany)
#'
#' @references
#' Debertin, K., Helmer, R.G., 1988. Gamma- and X-ray Spectrometry with
#' Semiconductor Detectors, Elsevier Science Publishers, p.283
#'
#' Wiechen, A., Ruehle, H., Vogl, K., 2013. Bestimmung der massebezogenen
#' Aktivitaet von Radionukliden. AEQUIVAL/MASSAKT, ISSN 1865-8725,
#' [https://www.bmu.de/fileadmin/Daten_BMU/Download_PDF/Strahlenschutz/aequival-massakt_v2013-07_bf.pdf]()
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
#' @md
#' @export
convert_Activity2Concentration <- function(
  data,
  input_unit = "Bq/kg",
  verbose = TRUE

){


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
