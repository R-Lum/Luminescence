#' Al2O3:C Passive Dosimeter Measurement Analysis
#'
#' The function provides the analysis routines for measurements on a
#' FI lexsyg SMART reader using Al2O3:C pellets according to Kreutzer et al., XXXX
#'
#' **Working with a travel dosimeter**
#' ##ADD INFORMATION ON HOW IT WORKS WITH THE TRAVEL DOSIMETERS
#'
#' **Test parameters**
#'
#' `TL_peak_shift` [numeric] (default: `15`):
#'
#' Checks whether the TL peak shift is bigger > 15 K, indicating a problem with the
#' thermal contact of the pellet
#'
#'
#' @param object [RLum.Analysis-class] **(required)**:
#' measurement input
#'
#' @param signal_integral [numeric] (*optional*):
#' signal integral, used for the signal and the background.
#' If nothing is provided the full range is used
#'
#' @param dose_points [numeric] (*with default*):
#' vector with dose points, if dose points are repeated, only the general
#' pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., XXXX
#'
#' @param recordType [character] (*with default*): input curve selection, which is passed to
#' function [get_RLum]. To deactivate the automatic selection set the argument to `NULL`
#'
#' @param irradiation_time_correction [numeric] or [RLum.Results-class] (*optional*):
#' information on the used irradiation time correction obained by another experiements.
#' I a `numeric` is provided it has to be of length two: mean, standard error
#'
#' @param cross_talk_correction [numeric] or [RLum.Results-class] (*optional*):
#' information on the used irradiation time correction obained by another experiements.
#' If a `numeric` vector is provided it has to be of length three:
#' mean, 2.5 \% quantile, 97.5 \% quantile.
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
#' @param ... further arguments that can be passed to the plot output
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
#'  `$data_talbe` \tab `data.frame` \tab full dose and signal table \cr
#' }
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
#' @section Function version: 0.1.5
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso [analyse_Al2O3C_ITC]
#'
#' @references TODO
#'
#' @keywords datagen
#'
#' @examples
#'
#' ## nothing so far TODO ... add tests with example
#'
#' @md
#' @export
analyse_Al2O3C_Measurement <- function(
  object,
  signal_integral = NULL,
  dose_points = c(0,4),
  recordType = c("OSL (UVVIS)", "TL (UVVIS)"),
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
      signal_integral <- rep(list(signal_integral, length = length(object)))

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
      analyse_Al2O3C_Measurement(
        object = object[[x]],
        signal_integral = signal_integral[[x]],
        dose_points = dose_points[[x]],
        irradiation_time_correction = irradiation_time_correction[[x]],
        cross_talk_correction = cross_talk_correction[[x]],
        test_parameters = test_parameters[[x]],
        verbose = verbose,
        plot = plot[x],
        ...

      )


    })

    ##merge results
    results <- merge_RLum(results)

    ##travel dosimeter
    ##check for travel dosimeter and subtract the values so far this is meaningful at all
    if(!is.null(travel_dosimeter)){
      ##check data type
      if(!is(travel_dosimeter, "numeric"))
        stop("[analyse_Al2O3C_Measurement()] Input for `travel_dosimeter` is not numeric!", call. = FALSE)

      ##check whether everything is subtracted from everything ... you never know, users do weird stuff
      if(length(travel_dosimeter) == nrow(results$data))
        try(stop("[analyse_Al2O3C_Measurement()] You specified every position as travel dosimeter, nothing corrected!", call. = FALSE))

      ##check if the position is valid
      if(!any(travel_dosimeter%in%results$data$POSITION))
        try(stop("[analyse_Al2O3C_Measurement()] Invalid position in 'travel_dosimeter', nothing corrected!", call. = FALSE))

      ##correct for the travel dosimeter calculating the weighted mean and the sd (as new error)
      ##if only one value is given just take it
      if(length(travel_dosimeter) == 1){
        correction <- as.numeric(results$data[travel_dosimeter==results$data$POSITION,c(1,2)])

      }else{
        temp.correction <- results$data[results$data$POSITION%in%travel_dosimeter,c(1,2)]
        correction <- c(stats::weighted.mean(temp.correction[,1],temp.correction[,2]), sd(temp.correction[,1]))
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
        cat("\n ...+ travel dosimeter correction applied.\n ...+ results stored in object $data_TDcorrected.\n")

    } ##end travel dosimeter

    ##return results
    return(results)

  }

  # Integretiy check  ---------------------------------------------------------------------------

  ##TODO ... do more, push harder
  ##Add sufficient unit tests

  # Preparation ---------------------------------------------------------------------------------

  ##select curves based on the recordType selection; if not NULL
  if(!is.null(recordType)){
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


        ##grep cross talk correction
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
    TL_peak_shift = 15
  )

  ##modify default values by given input
  if(!is.null(test_parameters)){
    test_parameters <- modifyList(test_parameters.default, test_parameters)

    ##remove NULL elements from list
    test_parameters <- test_parameters[!sapply(test_parameters, is.null)]

  }else{
    test_parameters <- test_parameters.default

  }

  ##calculate needed values
  NATURAL <- sum(object[[1]][signal_integral, 2])
  REGENERATED <- sum(object[[3]][signal_integral, 2])
  BACKGROUND <- sum(object[[5]][signal_integral, 2])

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

   ##(3) combine the two values
   DOSE <- DOSE_MC - CT

   ##(4) signal ratio
   INTEGRAL_RATIO <- temp_df$NET_INTEGRAL[1]/temp_df$NET_INTEGRAL[2]

   ##(5) calculate DE
   temp_DE <- (DOSE * INTEGRAL_RATIO)

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
     TL_PEAK_SHIFT = NA,
     row.names = NULL

   )



  ##check TL peak positions, if it differes more than the threshold, return a message
  ##can be done better, but should be enough here.
  if(any("TL_peak_shift"%in%names(test_parameters))){
    check_curves <-
      abs((object[[2]][which.max(object[[2]][,2]),1] -
             object[[4]][which.max(object[[4]][,2]),1])) > test_parameters$TL_peak_shift

    data[["TL_PEAK_SHIFT"]] <- check_curves

    if(check_curves)
      warning("TL peak shift detected for aliquot position ",POSITION, "! Check curves!", call. = FALSE)

  }


  # Terminal output -----------------------------------------------------------------------------
  if(verbose){
    cat(" [analyse_Al2O3_Measurement()] #",POSITION, " ", "DE: ",
               round(data$DE, 2), " \u00B1 ", round(data$DE_ERROR,2), "\n",sep = "")

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
        main = "Sample Carousel Crosstalk",
        mtext = ""
      )

      ##modify on request
      plot_settings <- modifyList(x = plot_settings, list(...))


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
       main = list(paste("ALQ POS:", POSITION, "| OSL"), paste("ALQ POS:", POSITION, "| TL"))
     )



    }

  # Output --------------------------------------------------------------------------------------
  UID <- create_UID()

  output <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = cbind(data, UID),
      data_table = cbind(temp_df, UID)
      ),
    info = list(
      call = sys.call()
    )
  )

}
