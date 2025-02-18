#' @title Convert Nuclide Activities to Abundance and Vice Versa
#'
#' @description The function performs the conversion of the specific activities into
#' mass abundance and vice versa for the radioelements U, Th, and K to
#' harmonise the measurement unit with the required data input unit of
#' potential analytical tools for, e.g. dose rate calculation or related
#' functions such as [use_DRAC].
#'
#' @details The conversion from nuclide activity of a sample to nuclide concentration
#' is performed using conversion factors that are based on the mass-related
#' specific activity of the respective nuclide.
#'
#' Constants used in this function were obtained from `https://physics.nist.gov/cuu/Constants/`
#' all atomic weights and composition values from
#' `https://www.nist.gov/pml/atomic-weights-and-isotopic-compositions-relative-atomic-masses`
#' and the nuclide data from `https://www.iaea.org/resources/databases/livechart-of-nuclides-advanced-version`
#'
#' The factors can be calculated using the equation:
#'
#' \deqn{
#' A = N_A \frac{N_{abund}}{N_{mol.mass}}  ln(2) / N.half.life
#' }
#'
#' to convert in µg/g we further use:
#'
#' \deqn{
#' f = A / 10^6
#' }
#'
#' where:
#'
#' - `N_A` - Avogadro constant in 1/mol
#' - `A` - specific activity of the nuclide in Bq/kg
#' - `N.abund` - relative natural abundance of the isotope
#' - `N.mol.mass` molar mass in kg/mol
#' - `N.half.life` half-life of the nuclide in s
#'
#' example for calculating the activity of the radionuclide U-238:
#'
#'  * `N_A` = 6.02214076e+23 (1/mol)
#'  * `T_0.5` = 1.41e+17 (s)
#'  * `m_U_238` = 0.23802891 (kg/mol)
#'  * `U_abund` = 0.992745 (unitless)
#'
#' \deqn{A_{U} = N_{A} * U_{abund} / m_{U_238} * ln(2) / T_{1/2} = 2347046} (Bq/kg)
#'
#' \deqn{f.U = A_{U} / 10^6}
#'
#' @param data [data.frame] (**required**):
#' provide dose rate data (activity or concentration) in three columns.
#' The first column indicates the nuclide, the 2nd column measured value and
#' in the 3rd column its error value. Allowed nuclide data are
#' `'U-238'`, `'Th-232'` and `'K-40'`. See examples for an example.
#'
#' @param input_unit [character] (*with default*):
#' specify unit of input data given in the dose rate data frame, choose between
#' `"activity"` (considered as given Bq/kg) and `"abundance"` (considered as given in mug/g or mass. %).
#' The default value is `"activity"`
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @section Function version: 0.1.2
#'
#' @author Margret C. Fuchs, Helmholtz-Institute Freiberg for Resource Technology (Germany)
#'
#' @references
#' Debertin, K., Helmer, R.G., 1988. Gamma- and X-ray Spectrometry with
#' Semiconductor Detectors, Elsevier Science Publishers, p.283
#'
#' Wiechen, A., Ruehle, H., Vogl, K., 2013. Bestimmung der massebezogenen
#' Aktivitaet von Radionukliden. AEQUIVAL/MASSAKT, ISSN 1865-8725,
#' [https://www.bmuv.de/fileadmin/Daten_BMU/Download_PDF/Strahlenschutz/aequival-massakt_v2013-07_bf.pdf]()
#'
#' @keywords IO
#'
#' @note Although written otherwise for historical reasons. Input values must be element values.
#' For instance, if a value is provided for U-238 the function assumes that this value
#' represents the sum (activity or abundance) of U-238, U-235 and U-234.
#' In other words, 1 µg/g of U means that this is the composition of 0.992 parts of U-238,
#' 0.000054 parts of U-234, and 0.00072 parts of U-235.
#'
#' @return Returns an [RLum.Results-class] object with a [data.frame] containing
#' input and newly calculated values. Please not that in the column header µg/g
#' is written as `mug/g` due to the R requirement to maintain packages portable using
#' ASCII characters only.
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
  input_unit = "activity",
  verbose = TRUE

) {
  .set_function_name("convert_Activity2Concentration")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests --------------------------------------------------------
  .validate_class(data, "data.frame")

  if(ncol(data)<3)
    .throw_error("'data' should have at least 3 columns")

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
    "ACTIVIY (Bq/kg)",
    "ACTIVIY ERROR (Bq/kg)",
    "ABUND. (mug/g or mass. %)",
    "ABUND. ERROR (mug/g or mass. %)")

  ##set column for output
  output$NUCLIDE = data[[1]]

  ## check input unit
  ## we silently let the old input values unflagged for back compatibility reasons
  if (!tolower(input_unit[1]) %in% c("bq/kg", "ppm/%")) {
    input_unit <- .validate_args(tolower(input_unit), c("activity", "abundance"))
  }

  # Set conversion factors ----------------------------------------------------------------------

  #############################################################################
  ### conversion factors
  mass_constant <- 1.66053906660e-27  # in kg

  ## set conversion factors ... this are the expected activity per kg of the radionuclide
  ## a = log(2) / ((unified_atomic_mass * mass_constant) / abundance) * T_0.5
  convers.factor.U238 <- log(2) / (((238.0507884 * mass_constant)/ 0.992742) * 1.409963e+17) / 1e+06
  convers.factor.Th232 <- log(2) / (((232.0380558 * mass_constant)/ 1) * 4.41797e+17) / 1e+06
  convers.factor.K40 <- log(2) / (((39.963998166 * mass_constant)/ 0.000117) * 3.9383e+16) / 1e+02

  # Run conversion ------------------------------------------------------------------------------
  U <- which(data$NUCLIDE == "U-238")
  Th <- which(data$NUCLIDE == "Th-232")
  K <- which(data$NUCLIDE == "K-40")

  ##Activity to concentration
  if(input_unit == "activity" || input_unit == "bq/kg"){
    output[U,4:5] <- data[U,2:3] / convers.factor.U238
    output[Th,4:5] <- data[Th,2:3] / convers.factor.Th232
    output[K,4:5] <- data[K,2:3] / convers.factor.K40

    output[U,2:3] <- data[U,2:3]
    output[Th,2:3] <- data[Th,2:3]
    output[K,2:3] <- data[K,2:3]
  }

  ##Concentration to activity
  if(input_unit == "abundance" || input_unit == "ppm/%"){
    output[U,2:3] <- data[U,2:3] * convers.factor.U238
    output[Th,2:3] <- data[Th,2:3] * convers.factor.Th232
    output[K,2:3] <- data[K,2:3] * convers.factor.K40

    output[U,4:5] <- data[U,2:3]
    output[Th,4:5] <- data[Th,2:3]
    output[K,4:5] <- data[K,2:3]
  }

  # Return value --------------------------------------------------------------------------------
  if(verbose) print(output)

  invisible(set_RLum(
    class = "RLum.Results",
    data = list(data = output),
    info = list(call = sys.call())))
}
