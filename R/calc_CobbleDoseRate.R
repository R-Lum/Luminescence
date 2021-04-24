#'@title Calculate dose rate of slices in a spherical cobble
#'
#'@description
#'
#'Calculates the dose rate profile through the cobble based on Riedesel and Autzen (2020).
#'
#'Corrects the beta dose rate in the cobble for the grain size following results
#'of Guérin et al. (2012). Sediment beta and gamma dose rates are corrected
#'for the water content of the sediment using the correction factors of Aitken (1985).
#'Water content in the cobble is assumed to be 0.
#'
#'
#'@details
#'
#'**The input table layout**
#'
#'\tabular{lll}{
#'COLUMN \tab DATA TYPE \tab DESCRIPTION\cr
#'`Distance` \tab `numeric` \tab distance from the surface of the cobble to the top of each rock slice in mm. The distance for each slice will be listed in this column\cr
#'`DistanceError` \tab `numeric` \tab Error on the distance in mm\cr
#'`Thickness` \tab `numeric` \tab Thickness of each slice in mm\cr
#'`TicknessError` \tab `numeric` \tab uncertainty of the thickness in mm.\cr
#'`Mineral` \tab `character` \tab `'FS'` for feldspar, `'Q'` for quartz, depending which mineral in the cobble is used for dating\cr
#'`Cobble_K` \tab `numeric` \tab K nuclide content in % of the bulk cobble\cr
#'`Cobble_K_SE` \tab `numeric` \tab error on K nuclide content in % of the bulk cobble\cr
#'`Cobble_Th` \tab `numeric` \tab Th nuclide content in ppm of the bulk cobble\cr
#'`Cobble_Th_SE` \tab `numeric` \tab error on Th nuclide content in ppm of the bulk cobble\cr
#'`Cobble_U` \tab `numeric` \tab U nuclide content in ppm of the bulk cobble\cr
#'`CobbleU_SE` \tab `numeric` \tab error on U nuclide content in ppm of the bulk cobble\cr
#'`GrainSize` \tab `numeric` \tab average grain size in µm of the grains used for dating\cr
#'`Density` \tab `numeric` \tab Density of the cobble. Default is 2.7 g cm^-3\cr
#'`CobbleDiameter` \tab `numeric` \tab Diameter of the cobble in cm.\cr
#'`Sed_K` \tab `numeric` \tab K nuclide content in % of the sediment matrix\cr
#'`Sed_K_SE` \tab `numeric` \tab error on K nuclide content in % of the sediment matrix\cr
#'`Sed_Th` \tab `numeric` \tab Th nuclide content in ppm of the sediment matrix\cr
#'`Sed_Th_SE` \tab `numeric` \tab error on Th nuclide content in ppm of the sediment matrix\cr
#'`Sed_U` \tab `numeric` \tab U nuclide content in ppm of the sediment matrix\cr
#'`Sed_U_SE` \tab `numeric` \tab error on U nuclide content in ppm of the sediment matrix\cr
#'`GrainSize` \tab `numeric` \tab average grain size of the sediment matrix\cr
#'`WaterContent` \tab `numeric` \tab mean water content of the sediment matrix in %\cr
#'`WaterContent_SE` \tab `numeric` \tab relative error on water content
#'}
#'
#'**Water content**
#'The water content provided by the user should be calculated according to:
#'
#'\deqn{(Wet_weight - Dry_weight) / Dry_weight * 100}
#'
#'@param input [data.frame] (**required**): A table containing all relevant information
#'for each individual layer. For the table layout see details.
#'
#'@param conversion Which dose rate conversion factors to use. For accepted values see [BaseDataSet.ConversionFactors]
#'
#'@references
#'Riedesel, S., Autzen, M., 2020. Beta and gamma dose rate attenuation in rocks and sediment.
#'Radiation Measurements 133, 106295.
#'
#'@section Function version: 0.1.0
#'
#'@author Svenja Riedesel, Aberystwyth University (United Kingdom) \cr
#'Martin Autzen, DTU NUTECH Center for Nuclear Technologies (Denmark)
#'
#'@return The function returns an [RLum.Results-class] object for which the first element
#'is a [matrix] (`DataIndividual`) that gives the dose rate results for each slice
#'for each decay chain individually, for both, the cobble dose rate and the sediment
#'dose rate. The second element is also a [matrix] (`DataComponent`) that gives
#'the total beta and gamma-dose rates for the cobble and the adjacent sediment
#'for each slice of the cobble.
#'
#'@keywords datagen
#'
#'@seealso [convert_Concentration2DoseRate]
#'
#'@examples
#'## load example data
#'data("ExampleData.CobbleData", envir = environment())
#'
#'## run function
#'calc_CobbleDoseRate(ExampleData.CobbleData)
#'
#'@md
#'@export
calc_CobbleDoseRate <- function(input,conversion = "Guerinetal2011"){

  # Integrity tests ---------------------------------------------------------
  if ((max(input[,1])>input$CobbleDiameter[1]*10) ||
      ((max(input[,1]) + input[length(input[,1]),3]) > input$CobbleDiameter[1]*10))
    stop("[calc_CobblDoseRate()] Slices outside of cobble. Please check your distances and make sure they are in mm and diameter is in cm!", call. = FALSE)


  # Calculate Dose Rate -----------------------------------------------------
  SedDoseData <- matrix(data = NA, nrow = 1, ncol = 10)
  CobbleDoseData <- matrix(data = 0, nrow = 1, ncol = 10)

  CobbleDoseData <- input[1,5:12]
  CobbleDoseData <- cbind(CobbleDoseData,0,0)
  SedDoseData <- cbind(input[1,5],input[1,15:20],input[1,12],input[1,23:24])

  CobbleDoseRate <- get_RLum(convert_Concentration2DoseRate(
    input = CobbleDoseData, conversion = conversion))
  SedDoseRate <- get_RLum(
    convert_Concentration2DoseRate(input = SedDoseData, conversion = conversion))

  ## Distance should be from the surface of the rock to the top of the slice. Distances and thicknesses are in mm
  N <- length(input$Distance)

  Diameter <- input$CobbleDiameter[1]

  ### Calculate gamma attenuation coefficient for the cobbles internal dose rate
  if (Diameter<25){
    CobbleGammaAtt <-
      (0.55 * exp(-0.45 * Diameter) + 0.09 * exp(-0.06 * Diameter)) * 10
  }else {
    CobbleGammaAtt <- 0.02
  }

  ## Scale the density and infinite matrix gamma dose rates ----
  Scaling <- input$Density[1] / 2.7
  GammaEdge <- 0.5 * (1 - exp(-0.039 * Diameter))
  GammaCentre <- 2 * GammaEdge

  DiameterSeq <-
    seq(0, Diameter * 10, by = 0.01) #Converts diameter into integer of 10 um

  ### Create matrices for use ----
  Temp <- matrix(data = NA, nrow = length(DiameterSeq), ncol = 9)
  DistanceError <- matrix(data = NA, nrow = N, ncol = 8)
  ThicknessError <- matrix(data = NA, nrow = N, ncol = 8)
  DataIndividual <- matrix(data = NA, nrow = N, ncol = 25)
  DataComponent <- matrix(data = NA, nrow = N, ncol = 9)
  DoseRates <- matrix(data = NA, nrow = 1, ncol = 24)
  output <- matrix(list(), nrow = 2, ncol = 1)

  ### Calculate dose rate profiles through the rock ----
  t <- Diameter * 10 - DiameterSeq
  tGamma <- t

  #Beta and gamma functions for the cobbles own dose rate
  KBetaCobble <- function(x) (1 - 0.5 * exp(-3.77 * DiameterSeq))+(1-0.5*exp(-3.77*t))-1
  ThBetaCobble_short <- function(x) (1 - 0.5 * exp(-5.36 * x * Scaling))+(1-0.5*exp(-5.36*t*Scaling))-1
  ThBetaCobble_long <- function(x) (1 - 0.33 * exp(-2.36 * x * Scaling))+(1-0.33*exp(-2.36*t*Scaling))-1
  UBetaCobble_short <- function(x) (1 - 0.5 * exp(-4.15 * x * Scaling))+(1-0.5*exp(-4.15*t*Scaling))-1
  UBetaCobble_long <- function(x) (1 - 0.33 * exp(-2.36 * x * Scaling))+(1-0.33*exp(-2.36*t*Scaling))-1

  GammaCobble <- function(x) {
    (GammaCentre - GammaEdge * exp(-CobbleGammaAtt * x * Scaling)) +
    (GammaCentre - GammaEdge * exp(-CobbleGammaAtt * tGamma * Scaling)) -
    GammaCentre
  }

  #Beta and gamma functions for the sediment dose rates into the cobble
  KBetaSed <- function(x) 2 - (1 - 0.5 * exp(-3.77 * x * Scaling)) - (1 - 0.5 * exp(-3.77 * t * Scaling))
  ThBetaSed_short <- function(x) 2 - (1 - 0.5 * exp(-5.36 * x * Scaling)) - (1 - 0.5 * exp(-5.36 * t * Scaling))
  ThBetaSed_long <- function(x) 2 - (1 - 0.33 * exp(-2.36 * x * Scaling)) - (1 - 0.33 * exp(-2.36 * t * Scaling))
  UBetaSed_short <- function(x) 2 - (1 - 0.5 * exp(-4.15 * x * Scaling)) - (1 - 0.5 * exp(-4.15 * t * Scaling))
  UBetaSed_long <- function(x) 2 - (1 - 0.33 * exp(-2.36 * x * Scaling)) - (1 - 0.33 * exp(-2.36 * t * Scaling))

  GammaSed <- function(x) 2 - (1 - 0.5 * exp(-0.02 * x * Scaling)) - (1 - 0.5 * exp(-0.02 * tGamma *
                                                                                      Scaling))
  Temp[, 1] <- DiameterSeq
  Temp[, 2] <- KBetaCobble(DiameterSeq)
  Temp[, 3] <- ThBetaCobble_long(DiameterSeq)
  Temp[, 4] <- UBetaCobble_long(DiameterSeq)
  Temp[, 5] <- GammaCobble(DiameterSeq)
  Temp[, 6] <- KBetaSed(DiameterSeq)
  Temp[, 7] <- ThBetaSed_long(DiameterSeq)
  Temp[, 8] <- UBetaSed_long(DiameterSeq)
  Temp[, 9] <- GammaSed(DiameterSeq)

  TempThCob <- ThBetaCobble_short(DiameterSeq)
  TempUCob <- UBetaCobble_short(DiameterSeq)
  TempThSed <- ThBetaSed_short(DiameterSeq)
  TempUSed <- UBetaSed_short(DiameterSeq)

  n <- which(DiameterSeq >= (max(DiameterSeq)-0.15))[1]
  Max <- length(DiameterSeq)

  ## Create the full matrix based on the short and long beta attenuations
  Temp[0:16, 3] <- TempThCob[0:16]
  Temp[n:Max, 3] <- TempThCob[n:Max]

  Temp[0:16, 7] <- TempThSed[0:16]
  Temp[n:Max, 7] <- TempThSed[n:Max]

  Temp[0:16, 4] <- TempUCob[0:16]
  Temp[n:Max, 4] <- TempUCob[n:Max]

  Temp[0:16, 8] <- TempUSed[0:16]
  Temp[n:Max, 8] <- TempUSed[n:Max]

  colnames(Temp) <- c(
    "Distance",
    "KBetaCob",
    "ThBetaCob",
    "UBetaCob",
    "GammaCob",
    "KBetaSed",
    "ThBetaSed",
    "UBetaSed",
    "GammaSed"
  )

  ### Create data output matrices ----
  Distances <- input$Distance / 0.01 + 1
  Thicknesses <- input$Thickness / 0.01

  MinDistance <- (input$Distance - input$DistanceError) / 0.01 + 1
  MaxDistance <- (input$Distance + input$DistanceError) / 0.01 + 1

  MinThickness <- (input$Thickness - input$ThicknessError) / 0.01
  MaxThickness <- (input$Thickness + input$ThicknessError) / 0.01

  for (i in 1:N){
    Start <- Distances[i]
    End <- Start+Thicknesses[i]

    d_min <- MinDistance[i]
    d_max <- MaxDistance[i]

    t_min <- MinThickness[i]
    t_max <- MaxThickness[i]

    #Calculate errors ----
    #Check if minimum distance from top is less than 0
    if (MinDistance[i]<0){
      d_min <- 0
    }

    j <- d_min+Thicknesses[i]
    k <- d_max+Thicknesses[i]

    for (l in 1:8){
      m <- l + 1
      if (d_min == Start){
        DistanceError[i,l]<- abs(
          (mean(Temp[d_max:k,m])-mean(Temp[Start:End,m]))/(2*mean(Temp[Start:End,m])))
      } else if (k > Max){
        DistanceError[i,l] <- abs(
          (mean(Temp[Start:End,m])-mean(Temp[d_min:j,m]))/(2*mean(Temp[Start:End,m])))
      } else {
        DistanceError[i,l] <- abs(
          mean((mean(Temp[d_max:k,m])-mean(Temp[Start:End,m])):(mean(Temp[Start:End,m])-mean(Temp[d_min:j,m])))/(2*mean(Temp[Start:End,m])))
      }

      j2 <- Start+t_min
      k2 <- Start+t_max

      if (k2 > Max){
        ThicknessError[i,l] <- abs(
          (mean(Temp[Start:End,m])-mean(Temp[Start:j2,m]))/(2*mean(Temp[Start:End,m])))
      } else {
        ThicknessError[i,l] <- abs(
          mean((mean(Temp[Start:k2,m])-mean(Temp[Start:End,m])):(mean(Temp[Start:End,m])-mean(Temp[Start:j2,m])))/(2*mean(Temp[Start:End,m])))
      }
    }
    ### Calculate average dose rates ----

    DataIndividual[i, 1] <- input[i, 1]
    # Cobble K Beta
    DataIndividual[i, 2] <- mean(Temp[Start:End, 2]) * CobbleDoseRate[1, 1]
    DataIndividual[i, 3] <-
      DataIndividual[i, 2] * sqrt(DistanceError[i, 1] ^ 2 + ThicknessError[i, 1] ^
                                    2 + (CobbleDoseRate[1, 2] / CobbleDoseRate[1, 1]) ^ 2)
    # Cobble Th Beta
    DataIndividual[i, 4] <- mean(Temp[Start:End, 3]) * CobbleDoseRate[1, 3]
    DataIndividual[i, 5] <-
      DataIndividual[i, 4] * sqrt(DistanceError[i, 2] ^ 2 + ThicknessError[i, 2] ^
                                    2 + (CobbleDoseRate[1, 4] / CobbleDoseRate[1, 3]) ^ 2)
    # Cobble U Beta
    DataIndividual[i, 6] <- mean(Temp[Start:End, 4]) * CobbleDoseRate[1, 5]
    DataIndividual[i, 7] <- DataIndividual[i, 6] * sqrt(DistanceError[i, 3] ^ 2 + ThicknessError[i, 3] ^
                                                          2 + (CobbleDoseRate[1, 6] / CobbleDoseRate[1, 5]) ^ 2)
    # Cobble K Gamma
    DataIndividual[i, 8] <- mean(Temp[Start:End, 5]) * CobbleDoseRate[2, 1]
    DataIndividual[i, 9] <- DataIndividual[i, 8] * sqrt(DistanceError[i, 4] ^ 2 + ThicknessError[i, 4] ^
                                                          2 + (CobbleDoseRate[2, 2] / CobbleDoseRate[2, 1]) ^ 2)
    # Cobble Th Gamma
    DataIndividual[i, 10] <- mean(Temp[Start:End, 5]) * CobbleDoseRate[2, 3]
    DataIndividual[i, 11] <-
      DataIndividual[i, 10] * sqrt(DistanceError[i, 4] ^ 2 + ThicknessError[i, 4] ^
                                     2 + (CobbleDoseRate[2, 4] / CobbleDoseRate[2, 3]) ^ 2)
    # Cobble U Gamma
    DataIndividual[i, 12] <- mean(Temp[Start:End, 5]) * CobbleDoseRate[2, 5]
    DataIndividual[i, 13] <-
      DataIndividual[i, 12] * sqrt(DistanceError[i, 4] ^ 2 + ThicknessError[i, 4] ^
                                     2 + (CobbleDoseRate[2, 6] / CobbleDoseRate[2, 5]) ^ 2)

    # Sediment K Beta
    DataIndividual[i, 14] <- mean(Temp[Start:End, 6]) * SedDoseRate[1, 1]
    DataIndividual[i, 15] <-
      DataIndividual[i, 14] * sqrt(DistanceError[i, 5] ^ 2 + ThicknessError[i, 5] ^
                                     2 + (SedDoseRate[1, 2] / SedDoseRate[1, 1]) ^ 2)
    # Sediment Th Beta
    DataIndividual[i, 16] <- mean(Temp[Start:End, 7]) * SedDoseRate[1, 3]
    DataIndividual[i, 17] <-
      DataIndividual[i, 16] * sqrt(DistanceError[i, 6] ^ 2 + ThicknessError[i, 6] ^
                                     2 + (SedDoseRate[1, 4] / SedDoseRate[1, 3]) ^ 2)
    # Sediment U Beta
    DataIndividual[i, 18] <- mean(Temp[Start:End, 8]) * SedDoseRate[1, 5]
    DataIndividual[i, 19] <-
      DataIndividual[i, 18] * sqrt(DistanceError[i, 7] ^ 2 + ThicknessError[i, 7] ^
                                     2 + (SedDoseRate[1, 6] / SedDoseRate[1, 5]) ^ 2)
    # Sediment K Gamma
    DataIndividual[i, 20] <- mean(Temp[Start:End, 9]) * SedDoseRate[2, 1]
    DataIndividual[i, 21] <-
      DataIndividual[i, 20] * sqrt(DistanceError[i, 8] ^ 2 + ThicknessError[i, 8] ^
                                     2 + (SedDoseRate[2, 2] / SedDoseRate[2, 1]) ^ 2)
    # Sediment Th Gamma
    DataIndividual[i, 22] <- mean(Temp[Start:End, 9]) * SedDoseRate[2, 3]
    DataIndividual[i, 23] <-
      DataIndividual[i, 22] * sqrt(DistanceError[i, 8] ^ 2 + ThicknessError[i, 8] ^
                                     2 + (SedDoseRate[2, 4] / SedDoseRate[2, 3]) ^ 2)
    # Sediment U Gamma
    DataIndividual[i, 24] <- mean(Temp[Start:End, 9]) * SedDoseRate[2, 5]
    DataIndividual[i, 25] <-
      DataIndividual[i, 24] * sqrt(DistanceError[i, 8] ^ 2 + ThicknessError[i, 8] ^
                                     2 + (SedDoseRate[2, 6] / SedDoseRate[2, 5]) ^ 2)

    ### Sum data into beta and gamma dose rates from cobble and sediment ----
    DataComponent[i, 1] <- input[i, 1]
    DataComponent[i, 2] <- DataIndividual[i, 2] + DataIndividual[i, 4] + DataIndividual[i, 6]
    DataComponent[i, 3] <- DataComponent[i,2]*sqrt((DataIndividual[i,3]/DataIndividual[i,2])^2+(DataIndividual[i,5]/DataIndividual[i,4])^2+(DataIndividual[i,7]/DataIndividual[i,6])^2)
    DataComponent[i, 4] <- DataIndividual[i, 8] + DataIndividual[i, 10] + DataIndividual[i, 12]
    DataComponent[i, 5] <- DataComponent[i,4]*sqrt((DataIndividual[i,9]/DataIndividual[i,8])^2+(DataIndividual[i,11]/DataIndividual[i,10])^2+(DataIndividual[i,13]/DataIndividual[i,12])^2)

    DataComponent[i, 6] <- DataIndividual[i, 14] + DataIndividual[i, 16] + DataIndividual[i, 18]
    DataComponent[i, 7] <- DataComponent[i,6]*sqrt((DataIndividual[i,15]/DataIndividual[i,14])^2+(DataIndividual[i,17]/DataIndividual[i,16])^2+(DataIndividual[i,19]/DataIndividual[i,18])^2)
    DataComponent[i, 8] <- DataIndividual[i, 20] + DataIndividual[i, 22] + DataIndividual[i, 24]
    DataComponent[i, 9] <- DataComponent[i,8]*sqrt((DataIndividual[i,21]/DataIndividual[i,20])^2+(DataIndividual[i,23]/DataIndividual[i,22])^2 + (DataIndividual[i,25]/DataIndividual[i,24])^2)
  }

  colnames(DataIndividual) <-
    c(
      "Distance.",
      "K Beta cobble",
      "SE",
      "Th Beta cobble",
      "SE",
      "U Beta cobble",
      "SE",
      "K Gamma cobble",
      "SE",
      "Th Gamma cobble",
      "SE",
      "U Gamma cobble",
      "SE",
      "K Beta sed.",
      "SE",
      "Th Beta sed.",
      "SE",
      "U Beta sed.",
      "SE",
      "K Gamma sed.",
      "SE",
      "Th Gamma sed.",
      "SE",
      "U Gamma sed.",
      "SE"
    )

  colnames(DataComponent) <-
    c(
      "Distance",
      "Total Cobble Beta",
      "SE",
      "Total Cobble Gamma",
      "SE",
      "Total Beta Sed.",
      "SE",
      "Total Gamma Sed.",
      "SE"
    )


  DataIndividual[is.na(DataIndividual)] <- 0
  DataComponent[is.na(DataComponent)] <- 0

  # Return ------------------------------------------------------------------
  return(
    set_RLum(
      class = "RLum.Results",
      data = list(
        DataIndividual = DataIndividual,
        DataComponent = DataComponent,
        input = input
      ),
      info = list(
        call = sys.call()
      )))

}
