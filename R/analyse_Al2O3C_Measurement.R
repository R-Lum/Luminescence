#' Al2O3:C Passive Dosimeter Measurement Analysis
#'
#' The function provides the analysis routines for measurements on a
#' FI lexsyg SMART reader using Al2O3:C chips according to Kreutzer et al., 2018
#'
#' **Working with a travel dosimeter**
#'
#' The function allows to define particular aliquots as travel dosimeters. For example:
#' `travel_dosimeter = c(1,3,5)` sets aliquots 1, 3 and 5 as travel dosimeters. These dose values
#' of this dosimeters are combined and automatically subtracted from the obtained dose values
#' of the other dosimeters.
#'
#' **Calculate TL dose **
#'
#' The argument `calculate_TL_dose` provides the possibility to experimentally calculate a TL-dose,
#' i.e. an apparent dose value derived from the TL curve ratio. However, it should be noted that
#' this value is only a fall back in case something went wrong during the measurement of the optical
#' stimulation. The TL derived dose value is corrected for cross-talk and for the irradiation time,
#' but not considered if a travel dosimeter is defined.
#'
#' **Test parameters**
#'
#' `TL_peak_shift` [numeric] (default: `15`):
#'
#' Checks whether the TL peak shift is bigger > 15 K, indicating a problem with the
#' thermal contact of the chip.
#'
#' `stimulation_power` [numeric] (default: `0.05`):
#'
#' So far available, information on the delivered optical stimulation are compared. Compared are
#' the information from the first curves with all others. If the ratio differs more from
#' unity than the defined by the threshold, a warning is returned.
#'
#'
#' @param object [RLum.Analysis-class] **(required)**:
#' measurement input
#'
#' @param signal_integral [numeric] (*optional*): signal integral, used for the signal
#' and the background. Example: `c(1:10)` for the first 10 channels.
#' If nothing is provided the full range is used
#'
#' @param dose_points [numeric] (*with default*):
#' vector with dose points, if dose points are repeated, only the general
#' pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., 2018
#'
#' @param recordType [character] (*with default*): input curve selection, which is passed to
#' function [get_RLum]. To deactivate the automatic selection set the argument to `NULL`
#'
#' @param irradiation_time_correction [numeric] or [RLum.Results-class] (*optional*):
#' information on the used irradiation time correction obtained by another experiments.
#' I a `numeric` is provided it has to be of length two: mean, standard error
#'
#' @param calculate_TL_dose [logical] (*with default*): Enables/disables experimental dose estimation
#' based on the TL curves. Taken is the ratio of the peak sums of each curves +/- 5 channels.
#'
#' @param cross_talk_correction [numeric] or [RLum.Results-class] (*optional*):
#' information on the used irradiation time correction obtained by another experiments.
#' If a `numeric` vector is provided it has to be of length three:
#' mean, 2.5 % quantile, 97.5 % quantile.
#'
#' @param travel_dosimeter [numeric] (*optional*): specify the position of the travel dosimeter
#' (so far measured a the same time). The dose of travel dosimeter will be subtracted from all
#' other values.
#'
#' @param test_parameters [list] (*with default*):
#' set test parameters. Supported parameters are: `TL_peak_shift` All input: [numeric]
#' values, `NA` and `NULL` (s. Details)
#'
#' @param verbose [logical] (*with default*):
#' enable/disable verbose mode
#'
#' @param plot [logical] (*with default*): enable/disable plot output, if `object` is of type [list],
#' a [numeric] vector can be provided to limit the plot output to certain aliquots
#'
#' @param ... further arguments that can be passed to the plot output, supported are `norm`, `main`, `mtext`,
#' `title` (for self-call mode to specify, e.g., sample names)
#'
#' @return Function returns results numerically and graphically:
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$data` \tab `data.frame` \tab the estimated equivalent dose \cr
#'  `$data_table` \tab `data.frame` \tab full dose and signal table \cr
#'  `test_parameters` \tab `data.frame` \tab results with test parameters \cr
#'  `data_TDcorrected` \tab `data.frame` \tab travel dosimeter corrected results (only if TD was provided)\cr
#' }
#'
#' *Note: If correction the irradiation time and the cross-talk correction method is used, the De
#' values in the table `data` table are already corrected, i.e. if you want to get an uncorrected value,
#' you can use the column `CT_CORRECTION` remove the correction*
#'
#'**slot:** **`@info`**
#'
#' The original function call
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' - OSL and TL curves, combined on two plots.
#'
#'
#' @section Function version: 0.2.5
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [analyse_Al2O3C_ITC]
#'
#' @references
#'
#' Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier, N., 2018. Environmental Dose Rate
#' Determination Using a Passive Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
#' Geochronometria 45, 56-67.
#'
#' @keywords datagen
#'
#' @examples
#' ##load data
#' data(ExampleData.Al2O3C, envir = environment())
#'
#' ##run analysis
#' analyse_Al2O3C_Measurement(data_CrossTalk)
#'
#' @md
#' @export
analyse_Al2O3C_Measurement <- function(
  object,
  signal_integral = NULL,
  dose_points = c(0,4),
  recordType = c("OSL (UVVIS)", "TL (UVVIS)"),
  calculate_TL_dose = FALSE,
  irradiation_time_correction = NULL,
  cross_talk_correction = NULL,
  travel_dosimeter = NULL,
  test_parameters = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
){


  # Self call -----------------------------------------------------------------------------------
  if(is(object, "list")){
    if(!all(unlist(lapply(object, function(x){is(x, "RLum.Analysis")})))){
        stop("[analyse_Al2O3C_Measurement()] The elements in 'object' are not all of type 'RLum.Analsyis'", call. = FALSE)

    }

    ##expand input arguments
    if(!is.null(signal_integral)){
      signal_integral <- rep(list(signal_integral), length = length(object))
    }

    ##dose points
    if(is(dose_points, "list")){
      dose.points <- rep(dose_points, length = length(object))

    }else{
      dose_points <- rep(list(dose_points), length = length(object))

    }

    ##irradiation time correction
    if(is(irradiation_time_correction, "list")){
      irradiation_time_correction <- rep(irradiation_time_correction, length = length(object))

    }else{
      irradiation_time_correction <- rep(list(irradiation_time_correction), length = length(object))

    }

    ##cross talk correction
    if(is( cross_talk_correction, "list")){
      cross_talk_correction <- rep( cross_talk_correction, length = length(object))

    }else{
      cross_talk_correction <- rep(list( cross_talk_correction), length = length(object))

    }

    ##test_parameters
    if(is(test_parameters[[1]], "list")){
      test_parameters <- rep(test_parameters, length = length(object))

    }else{
      test_parameters <- rep(list(test_parameters), length = length(object))

    }

    ##verbose

    ##plot
    if(is(plot, "logical")){
      plot <- rep(x = plot, length(object))

    }else{
      plot <- 1:length(object)%in%plot

    }

    ##run analyis
    results <- lapply(1:length(object), function(x) {
      temp <- analyse_Al2O3C_Measurement(
        object = object[[x]],
        signal_integral = signal_integral[[x]],
        dose_points = dose_points[[x]],
        irradiation_time_correction = irradiation_time_correction[[x]],
        cross_talk_correction = cross_talk_correction[[x]],
        test_parameters = test_parameters[[x]],
        calculate_TL_dose = calculate_TL_dose,
        verbose = verbose,
        plot = plot[x],
        ...

      )

     ##adjusting the terminal output, to avoid confusions
     if(verbose)
       cat(" ... (#",x, " | ALQ POS: ", temp$data$POSITION,")\n", sep = "")

     ##add running number to the plot, but only of we had a plot here...
     if(plot[x]){
       title(main = paste0(list(...)$title[x], " ","#", x), adj = 1, line = 3)

     }

     return(temp)
    })

    ##merge results
    results <- merge_RLum(results)

    ##correct sys.call, otherwise it gets a little bit strange
    ##why this is not implemented in the merge_RLum() method ... because here it would be wrong!
    results@info[names(results@info) == "call"] <- NULL
    results@info$call <- sys.call()

    ##travel dosimeter
    ##check for travel dosimeter and subtract the values so far this is meaningful at all
    if(!is.null(travel_dosimeter)){
      ##check data type
      if(!is(travel_dosimeter, "numeric"))
        stop("[analyse_Al2O3C_Measurement()] Input for `travel_dosimeter` is not numeric!",
             call. = FALSE)

      ##check whether everything is subtracted from everything ... you never know, users do weird stuff
      if(length(travel_dosimeter) == nrow(results$data))
        try(stop("[analyse_Al2O3C_Measurement()] You specified every position as travel dosimeter, nothing corrected!",
                 call. = FALSE))

      ##check if the position is valid
      if(!any(travel_dosimeter%in%results$data$POSITION))
        try(stop("[analyse_Al2O3C_Measurement()] Invalid position in 'travel_dosimeter', nothing corrected!",
                 call. = FALSE))

      ##correct for the travel dosimeter calculating the weighted mean and the sd (as new error)
      ##if only one value is given just take it
      if(length(travel_dosimeter) == 1 && nrow(results$data[travel_dosimeter==results$data$POSITION,c(1,2)]) == 1){
        correction <- as.numeric(results$data[travel_dosimeter==results$data$POSITION,c(1,2)])

      }else{
        temp.correction <- results$data[results$data$POSITION%in%travel_dosimeter,c(1,2)]
        correction <- c(
          stats::weighted.mean(
            x = temp.correction[[1]],
            w = if(all(temp.correction[[2]]==0)){rep(1, length(temp.correction[[2]]))} else {temp.correction[[2]]}),
          sd(temp.correction[,1]))
        rm(temp.correction)

      }

      ##subtract all the values, in a new data frame, we do not touch the original data
      data_TDcorrected <- data.frame(
        DE = results@data$data[!results$data$POSITION%in%travel_dosimeter,1] - correction[1],
        DE_ERROR = sqrt(results@data$data[!results$data$POSITION%in%travel_dosimeter,2]^2 + correction[2]^2),
        POSITION = results@data$data[!results$data$POSITION%in%travel_dosimeter, "POSITION"]
      )

      ##however, we set information on the travel dosimeter in the corresponding column
      results@data$data$TRAVEL_DOSIMETER <- results$data$POSITION%in%travel_dosimeter

      ##attach the new element to the results output
      results@data <- c(results@data,  list(data_TDcorrected = data_TDcorrected))

      ##return message
      if(verbose)
        cat("\n ...+ travel dosimeter correction applied.\n ...+ results stored in object $data_TDcorrected.\n\n")

    } ##end travel dosimeter

    ##return results
    return(results)

  }

  # Integrity check  ---------------------------------------------------------------------------

  ##TODO ... do more, push harder
  ##Add sufficient unit tests

  # Preparation ---------------------------------------------------------------------------------

  ##select curves based on the recordType selection; if not NULL
  if(!is.null(recordType)){
    object_raw <- object
    object <- get_RLum(object, recordType = recordType, drop = FALSE)

  }

  ##set signal integral
  if(is.null(signal_integral)){
    signal_integral <- c(1:nrow(object[[1]][]))

  }else{
    ##check whether the input is valid, otherwise make it valid
    if(min(signal_integral) < 1 | max(signal_integral) > nrow(object[[1]][])){
      signal_integral <- c(1:nrow(object[[1]][]))
      warning(
        paste0(
          "[analyse_Al2O3C_Measurement()] Input for 'signal_integral' corrected to 1:", nrow(object[[1]][])
        ),
        call. = FALSE
      )
    }

  }


  ## Set Irradiation Time Correction ---------------
  if (!is.null(irradiation_time_correction)) {
    if (is(irradiation_time_correction, "RLum.Results")) {
      if (irradiation_time_correction@originator == "analyse_Al2O3C_ITC") {
        irradiation_time_correction <- get_RLum(irradiation_time_correction)

        ##consider the case for more than one observation ...
        if(nrow(irradiation_time_correction)>1){
          irradiation_time_correction <- c(mean(irradiation_time_correction[[1]]), sd(irradiation_time_correction[[1]]))

        }else{
          irradiation_time_correction <- c(irradiation_time_correction[[1]], irradiation_time_correction[[2]])

        }

      } else{
        stop(
          "[analyse_Al2O3C_Measurement()] The object provided for the argument 'irradiation_time_correction' was created by an unsupported function!",
          call. = FALSE
        )

      }
    }
  }


  ## Set Cross Talk Correction ---------------
  ##check wehther the information on the position was stored in the input
  ##object
  if(!is.null(get_RLum(object = object[[1]], info.object = "position"))){
    POSITION <- get_RLum(object = object[[1]], info.object = "position")

  }else{
    message("[analyse_Al2O3_Measurement()] Aliquot position number was not found. No cross talk correction was applied!")
    cross_talk_correction <- c(0,0,0)
    POSITION <- NA

  }


  if(is.null(cross_talk_correction)){
    cross_talk_correction <- c(0,0,0)

  }else{

    ##check whether the input is of type RLum.Results and check orignator
    if (is(cross_talk_correction, "RLum.Results") &&
        cross_talk_correction@originator == "analyse_Al2O3C_CrossTalk") {


        ##grep cross talk correction and calculate values for
        ##this particular carousel position
        cross_talk_correction <-
          as.numeric(predict(cross_talk_correction$fit,
                  newdata = data.frame(x = POSITION),
                  interval = "confidence"))


    }else{
      stop(
        "[analyse_Al2O3C_Measurement()] The object provided for the argument 'cross_talk_correction' was created by an unsupported function or has a wrong originator!",
        call. = FALSE
      )

    }

  }

  # Calculation ---------------------------------------------------------------------------------
  ##we have two dose points, and one background curve, we do know only the 2nd dose

  ##set test parameters
  test_parameters.default <- list(
    TL_peak_shift = 15,
    stimulation_power = 0.05
  )

  ##modify default values by given input
  if(!is.null(test_parameters)){
    test_parameters <- modifyList(test_parameters.default, test_parameters)

    ##remove NULL elements from list
    test_parameters <- test_parameters[!sapply(test_parameters, is.null)]

  }else{
    test_parameters <- test_parameters.default

  }

  ##calculate integrated light values
  NATURAL <- sum(object@records[[1]]@data[signal_integral, 2])
  REGENERATED <- sum(object@records[[3]]@data[signal_integral, 2])
  BACKGROUND <- sum(object@records[[5]]@data[signal_integral, 2])

  ##do the same for the TL
  if(calculate_TL_dose){
    NATURAL_TL <- try(sum(
      object@records[[2]]@data[
        (which.max(object@records[[2]]@data[,2])-5):(which.max(object@records[[2]]@data[,2])+5),2]), silent = TRUE)
    REGENERATED_TL <- try(sum(
      object@records[[4]]@data[
        (which.max(object@records[[4]]@data[,2])-5):(which.max(object@records[[4]]@data[,2])+5),2]), silent = TRUE)

    ##catch errors if the integration fails
    if(inherits(NATURAL_TL, "try-error")){
      NATURAL_TL <- NA
      warning("[analyse_Al2O3_Measurement()] Natural TL signal out of bounds, NA returned!", call. = FALSE, immediate. = TRUE)

    }

    if(inherits(REGENERATED_TL, "try-error")){
      REGENERATED_TL <- NA
      warning("[analyse_Al2O3_Measurement()] Regenerated TL signal out of bounds, NA returned!", call. = FALSE, immediate. = TRUE)

    }

  }else{
    NATURAL_TL <- NA
    REGENERATED_TL <- NA

  }

  ##combine into data.frame
  temp_df <- data.frame(
      POSITION = POSITION,
      DOSE = if(!is.null(irradiation_time_correction)){
        dose_points + irradiation_time_correction[1]
        }else{
         dose_points
        },
      DOSE_ERROR = if(!is.null(irradiation_time_correction)){
        dose_points * irradiation_time_correction[2]/irradiation_time_correction[1]
        }else{
        0
        },
      STEP = c("NATURAL", "REGENERATED"),
      INTEGRAL = c(NATURAL, REGENERATED),
      BACKGROUND = c(BACKGROUND, BACKGROUND),
      NET_INTEGRAL = c(NATURAL - BACKGROUND, REGENERATED - BACKGROUND),
      NATURAL_TL = NATURAL_TL,
      REGENERATED_TL = REGENERATED_TL,
      row.names = NULL
    )

     ##0 dose points should not be biased by the correction ..
     ##Note: it does not mean that 0 s beneath the source has a dose of 0, however, in the certain
     ##case aliquot was never moved under the source
     id_zero <- which(dose_points == 0)
     temp_df$DOSE[id_zero] <- 0
     temp_df$DOSE_ERROR[id_zero] <- 0


   ##calculate DE by using the irradiation time correction AND the cross talk correction

   ##(1) sample dose point values with irradiation time corrections (random)
   if(!is.null(irradiation_time_correction)){
    DOSE_MC <- rnorm(1000, mean = temp_df$DOSE[2], sd = temp_df$DOSE_ERROR[2])

   }else{
    DOSE_MC <- temp_df$DOSE[2]

  }

   ##(2) random sampling from cross-irradiation
   CT <- runif(1000, min = cross_talk_correction[2], max = cross_talk_correction[3])

   ##(3) signal ratio
   INTEGRAL_RATIO <- temp_df$NET_INTEGRAL[1]/temp_df$NET_INTEGRAL[2]

   ##(4) calculate DE
   temp_DE <- (DOSE_MC * INTEGRAL_RATIO)

   ##(5) substract cross-talk value from DE
   temp_DE  <- temp_DE  - CT

     ##(5.1) calculate TL based DE
     ##calculate a dose based on TL
     ##Note: we use irradiation time correction and CT correction based on GSL measurements
     if(calculate_TL_dose){
       TL_Ratio <- NATURAL_TL/REGENERATED_TL
       temp_TL_DE <- (DOSE_MC * TL_Ratio) - CT
       TL_DE <- mean(temp_TL_DE)
       TL_DE.ERROR <- sd(temp_TL_DE)


     }else{
       TL_DE <- NA
       TL_DE.ERROR <- NA

     }

   ##(6) create final data.frame
   data <- data.frame(
     DE = mean(temp_DE),
     DE_ERROR = sd(temp_DE),
     POSITION,
     INTEGRAL_RATIO,
     TRAVEL_DOSIMETER = NA,
     CT_CORRECTION = cross_talk_correction[1],
     CT_CORRECTION_Q2.5 = cross_talk_correction[2],
     CT_CORRECTION_Q97.5 = cross_talk_correction[3],
     TL_DE = TL_DE,
     TL_DE.ERROR = TL_DE.ERROR,
     row.names = NULL
   )


  ##calculate test parameters
  ##TL_peak_shift
  ##check TL peak positions, if it differes more than the threshold, return a message
  ##can be done better, but should be enough here.
  if(any("TL_peak_shift"%in%names(test_parameters))){
    ##calculate value
    TP_TL_peak_shift.value <- abs((object[[2]][which.max(object[[2]][,2]),1] -
             object[[4]][which.max(object[[4]][,2]),1]))


    ##compare
    TP_TL_peak_shift.status <-  TP_TL_peak_shift.value > test_parameters$TL_peak_shift

    ##return warning
    if(TP_TL_peak_shift.status)
      warning("TL peak shift detected for aliquot position ",POSITION, "! Check curves!", call. = FALSE)

    ##set data.frame
    TP_TL_peak_shift <- data.frame(
      CRITERIA = "TL_peak_shift",
      THRESHOLD = test_parameters$TL_peak_shift,
      VALUE = TP_TL_peak_shift.value,
      STATUS = TP_TL_peak_shift.status,
      stringsAsFactors = FALSE)

  }else{
    TP_TL_peak_shift <- data.frame(stringsAsFactors = FALSE)

  }

  ##stimulation_power
  if(any("stimulation_power"%in%names(test_parameters))){

     ##get curves ids holding the information on the stimulation power
     temp_curves_OSL <- get_RLum(object_raw, recordType = "OSL", curveType = "measured")
     temp_curves_OSL <- lapply(temp_curves_OSL, function(o){
       if("stimulator"%in%names(o@info)){
         if(grepl(o@info$stimulator, pattern = "LED", fixed = TRUE)){
           return(o)
         }
       }
      return(NULL)
     })

     ##remove NULL
     temp_curves_OSL <- temp_curves_OSL[!sapply(temp_curves_OSL, is.null)]

     ##check whether something is left
     if(length(temp_curves_OSL) < 2){
       TP_stimulation_power.value <- NA
       TP_stimulation_power.status <- FALSE

     }else{
       ##calculate sum of the power
       TP_stimulation_power.value <-  vapply(temp_curves_OSL, function(x){
         sum(x@data[,2])

       }, numeric(1))

       ##estimate a theoretical value based on the first value ... it does not
       ##matter which value is correct or not
       TP_stimulation_power.value <- abs(1 -
         sum(TP_stimulation_power.value)/(TP_stimulation_power.value[1] * length(TP_stimulation_power.value)))

       TP_stimulation_power.status <- TP_stimulation_power.value > test_parameters$stimulation_power

       if(TP_stimulation_power.status)
         warning("Stimulation power was not stable for ALQ ",POSITION, "! Results are likely to be wrong!", call. = FALSE)

     }

     ##remove object
     rm(temp_curves_OSL)

     ##set data.frame
     TP_stimulation_power <- data.frame(
       CRITERIA = "stimulation_power",
       THRESHOLD = test_parameters$stimulation_power,
       VALUE = TP_stimulation_power.value,
       STATUS = TP_stimulation_power.status,
       stringsAsFactors = FALSE)

   }else{
     TP_stimulation_power <- data.frame(stringsAsFactors = FALSE)

   }

   ##compile all test parameter df
   df_test_parameters <- rbind(
     TP_TL_peak_shift,
     TP_stimulation_power)


  # Terminal output -----------------------------------------------------------------------------
  if(verbose){
    cat(" [analyse_Al2O3_Measurement()] #",POSITION, " ", "DE: ",
               round(data$DE, 2), " \u00B1 ", round(data$DE_ERROR,2), "\n", sep = "")

  }


  # Plotting ------------------------------------------------------------------------------------
    ##enable or disable plot ... we cannot put the condition higher, because we here
    ##calculate something we are going to need later
    if (plot) {

      ##get plot settings
      par.default <- par()$mfrow
      on.exit(par(mfrow = par.default))

      ##settings
      plot_settings <- list(
        main = c(paste("ALQ POS:", POSITION, "| OSL"), paste("ALQ POS:", POSITION, "| TL")),
        norm = TRUE,
        mtext = ""
      )


     ##modify on request
     plot_settings <- modifyList(x = plot_settings, val = list(...),)

     ##plot curves
     par(mfrow = c(1,2))

     plot_RLum(
       object,
       plot.single = TRUE,
       combine = TRUE,
       mtext = list(paste0("DE: ", round(data$DE,2), " \u00b1 ", round(data$DE_ERROR,2)), ""),
       xlab = list("Simulation [s]", "Temperature [\u00B0C]"),
       legend.text = list(list("#1 NAT", "#3 REG", "#5 BG"), list("#2 NAT", "#4 REG")),
       legend.pos = list("topright", "topleft"),
       main = as.list(plot_settings$main),
       norm = plot_settings$norm
     )



    }

  # Output --------------------------------------------------------------------------------------
  UID <- create_UID()

  output <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = cbind(data, UID),
      data_table = cbind(temp_df, UID),
      test_parameters = cbind(df_test_parameters, UID)
      ),
    info = list(
      call = sys.call()
    )
  )

}
