#'@title Dose-rate conversion function
#'
#'@description This function converts radionuclide concentrations
#'(K in %, Th and U in ppm) into dose rates (Gy/ka). Beta-dose rates are also
#'attenuated for the grain size. Beta and gamma-dose rates are corrected
#'for the water content. This function converts concentrations into dose rates
#'(Gy/ka) and corrects for grain size attenuation and water content
#'
#'Dose rate conversion factors can be chosen from Adamiec and Aitken (1998),
#'Guerin et al. (2011), Liritzis et al. (201) and Cresswell et al. (2018).
#'Default is Guerin et al. (2011).
#'
#'Grain size correction for beta dose rates is achieved using the correction
#'factors published by Guérin et al. (2012).
#'
#'Water content correction is based on factors provided by Aitken (1985),
#'with the factor for beta dose rate being 1.25 and for gamma 1.14.
#'
#'@details
#'
#'**The input data**
#'
#'\tabular{lll}{
#'COLUMN \tab DATA TYPE \tab DESCRIPTION\cr
#'`Mineral` \tab `character` \tab `'FS'` for feldspar, `'Q'` for quartz\cr
#'`K` \tab `numeric` \tab K nuclide content in %\cr
#'`K_SE` \tab `numeric` \tab error on K nuclide content in %\cr
#'`Th` \tab `numeric` \tab Th nuclide content in ppm\cr
#'`Th_SE` \tab `numeric` error on Th nuclide content in ppm\cr
#'`U` \tab `numeric` U nuclide content in ppm\cr
#'`U_SE` \tab `numeric` \tab error on U nuclide content in ppm\cr
#'`GrainSize` \tab `numeric` \tab average grain size in µm\cr
#'`WaterContent` \tab `numeric` \tab mean water content in %\cr
#'`WaterContent_SE` \tab `numeric` \tab relative error on water content
#'}
#'
#'
#'**Water content**
#'The water content provided by the user should be calculated according to:
#'
#'\deqn{(Wet_weight - Dry_weight) / Dry_weight * 100}
#'
#'The unit for the weight is gram (g).
#'
#' @param input [data.frame] (*optional*): a table containing all relevant
#' information for each individual layer. If nothing is provided, the function
#' returns a template data frame, the values of which need to be filled in by
#' the user. Please note that only one dataset per input is supported.
#'
#' @param conversion [character] (*with default*):
#' dose rate conversion factors to use, by default those by Guérin et al. (2011).
#' For accepted values see [BaseDataSet.ConversionFactors].
#'
#'@return The function returns an [RLum.Results-class] object for which the first
#'element is [matrix] with the converted values. If no input is provided, the
#'function returns a template [data.frame] that can be used as input.
#'
#'@section Function version: 0.1.0
#'
#'@author Svenja Riedesel, Aberystwyth University (United Kingdom) \cr
#'Martin Autzen, DTU NUTECH Center for Nuclear Technologies (Denmark)
#'
#'@references
#'Adamiec, G., Aitken, M.J., 1998. Dose-rate conversion factors: update. Ancient TL 16, 37-46.
#'
#'Cresswell., A.J., Carter, J., Sanderson, D.C.W., 2018. Dose rate conversion parameters:
#'Assessment of nuclear data. Radiation Measurements 120, 195-201.
#'
#'Guerin, G., Mercier, N., Adamiec, G., 2011. Dose-rate conversion factors: update.
#'Ancient TL, 29, 5-8.
#'
#'Guerin, G., Mercier, N., Nathan, R., Adamiec, G., Lefrais, Y., 2012. On the use
#'of the infinite matrix assumption and associated concepts: A critical review.
#'Radiation Measurements, 47, 778-785.
#'
#'Liritzis, I., Stamoulis, K., Papachristodoulou, C., Ioannides, K., 2013.
#'A re-evaluation of radiation dose-rate conversion factors. Mediterranean
#'Archaeology and Archaeometry 13, 1-15.
#'
#'@keywords datagen
#'
#'@examples
#'
#'## create input template
#'input <- convert_Concentration2DoseRate()
#'
#'## fill input
#'input$Mineral <- "FS"
#'input$K <- 2.13
#'input$K_SE <- 0.07
#'input$Th <- 9.76
#'input$Th_SE <- 0.32
#'input$U <- 2.24
#'input$U_SE <- 0.12
#'input$GrainSize <- 200
#'input$WaterContent <- 30
#'input$WaterContent_SE <- 5
#'
#'## convert
#'convert_Concentration2DoseRate(input)
#'
#'@export
convert_Concentration2DoseRate <- function(
  input,
  conversion = "Guerinetal2011"
) {
  .set_function_name("convert_Concentration2DoseRate")
  on.exit(.unset_function_name(), add = TRUE)

  template <- data.frame(
      Mineral = NA_character_,
      K = NA_integer_,
      K_SE = NA_integer_,
      Th = NA_integer_,
      Th_SE = NA_integer_,
      U = NA_integer_,
      U_SE = NA_integer_,
      GrainSize = NA_integer_,
      WaterContent = NA_integer_,
      WaterContent_SE = NA_integer_)

  ## return a template if no input is given
  if (missing(input)) {
    .throw_message("Input template returned, please fill this data frame ",
                   "and use it as input to the function", error = FALSE)
    return(template)
  }

  ## Load datasets ----------------------------------------------------------

  ## silence notes raised by R CMD check
  BaseDataSet.ConversionFactors <- BaseDataSet.GrainSizeAttenuation <- NA

  load(system.file("data", "BaseDataSet.ConversionFactors.rda",
                   package = "Luminescence"))
  load(system.file("data", "BaseDataSet.GrainSizeAttenuation.rda",
                   package = "Luminescence"))

  ## we do this to be consistent with the code written by Svenja and Martin
  GSA <- BaseDataSet.GrainSizeAttenuation

  ## Integrity checks -------------------------------------------------------

  .validate_class(input, c("data.frame", "matrix"))

  if (ncol(input) != ncol(template))
    .throw_error("'input' should have ", ncol(template), " columns")
  if (nrow(input) > 1)
    .throw_error("'input' should have only one row")
  if (anyNA(input))
    .throw_error("'input' should not contain NA values")
  for (idx in 2:ncol(input)) {
    .validate_class(input[, idx], c("numeric", "integer"),
                    name = "Each element of 'input' other than the first")
  }

  ## conversion factors: we do not use BaseDataSet.ConversionFactors directly
  ## as it is in alphabetical level, but we want to have 'Guerinetal2011'
  ## in first position, as that is our default value
  valid_conversion_factors <- c("Guerinetal2011", "Cresswelletal2018",
                                "AdamiecAitken1998", "Liritzisetal2013")
  stopifnot(all(names(BaseDataSet.ConversionFactors) %in%
                valid_conversion_factors))
  conversion <- .validate_args(conversion, valid_conversion_factors)

  if(!any(input[,1] %in% c("FS","Q")))
    .throw_error("As mineral only 'FS' or 'Q' is supported")

  ## Convert ----------------------------------------------------------------

    InfDR <- matrix(data = NA, nrow = 2, ncol = 6)
    colnames(InfDR) <- c("K","SE","Th","SE","U","SE")
    rownames(InfDR) <- c("Beta","Gamma")

    ### --- BETA DOSE RATES
    for (i in 1:3){
      if (i == 1){
        Col <- "K"
      } else if (i == 2){
        Col <- "Th"
      } else {
        Col <- "U"
      }

      for (j in 1:2){
        if (j== 1){
          Temp = "beta"
        } else {
          Temp = "gamma"
        }

        ConvFactor <- BaseDataSet.ConversionFactors[[conversion]][[Temp]][[Col]]
        Nuclide <- i * 2
        N <- 2 * i - 1
        Error <- Nuclide + 1

        ## calculate dose rate
        InfDR[j, N] <- input[1, Nuclide] * ConvFactor[1]

        # calculate error
        InfDR[j, Nuclide] <- sqrt((input[1, Error] / input[1, Nuclide])^2 +
                                  (ConvFactor[2] / ConvFactor[1])^2)
      }
    }

    ##### --- dose rate for grain size --- #####

    if (input[1,1] == "FS") {                                       # FELDSPAR
      KFit <- approx(GSA$GrainSize, GSA$FS_K, n = 981, method = "linear")
      ThFit <- approx(GSA$GrainSize, GSA$FS_Th,n = 981, method = "linear")
      UFit <- approx(GSA$GrainSize, GSA$FS_U, n = 981, method = "linear")

    } else if (input[1,1] == "Q")  {                                # QUARTZ
      KFit <- approx(GSA$GrainSize, GSA$Q_K, n = 981, method = "linear")
      ThFit <- approx(GSA$GrainSize, GSA$Q_Th, n = 981, method = "linear")
      UFit <- approx(GSA$GrainSize, GSA$Q_U, n = 981, method = "linear")
    }

    Temp <- which(KFit$x == input[1, 8])
    if (length(Temp) == 0) {
      .throw_error("No attenuation data available for the grain size provided (",
                   input[1, 8], ")")
    }

    InfDR[1, 1] <- InfDR[1, 1] * (1 - KFit$y[Temp])   # K
    InfDR[1, 3] <- InfDR[1, 3] * (1 - ThFit$y[Temp])  # Th
    InfDR[1, 5] <- InfDR[1, 5] * (1 - UFit$y[Temp])   # U

    ##### --- Correct beta sediment dose rate for water content --- #####
    InfDRG <- matrix(data = NA, nrow = 2, ncol = 6)
    colnames(InfDRG) <- c("K", "SE", "Th", "SE", "U", "SE")
    rownames(InfDRG) <- c("Beta", "Gamma")

    WC <- input[1, 9] / 100
    WCerr <- input[1, 10] / 100

    for (i in 1:6){
      for (j in 1:2){
        if (j == 1){
          k = 1.25 #Water content correction for beta
        } else {
          k = 1.14 #Water content correction for gamma
        }

        Remain <- i %% 2

        if (Remain == 1){
          InfDRG[j,i] <- InfDR[j,i]/(1 + k*WC)
        } else if (Remain == 0){
          Temp <- (InfDR[j,i]/(1 + k*WC)) - (InfDR[j,i]/(1+k*(WC+WCerr)))
          InfDRG[j,i] <- InfDRG[j,i-1]*sqrt(InfDR[j,i]^2+(Temp/InfDR[j,i-1])^2)
        }
      }
    }
    InfDRG <- round(InfDRG, digits = 3)

# Return ------------------------------------------------------------------
  return(
    set_RLum(
      class = "RLum.Results",
      data = list(
        InfDRG = InfDRG,
        input_data = input
      ),
      info = list(
        call = sys.call()
      )))
}
