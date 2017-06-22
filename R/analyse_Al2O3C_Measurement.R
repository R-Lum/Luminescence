#'Al2O3:C Passive Dosimeter Measurement Analysis
#'
#'The function provides the analysis routines for measurements on a FI lexsyg SMART reader using
#'Al2O3:C pellets according to Kreutzer et al., 2017
#'
#'@param object [RLum.Analysis-class] **(required)**: measurement input
#'
#'@param signal_integral [numeric] *(optional)*: signal integral, used for the signal
#' and the background. If nothing is provided the full range is used
#'
#'@param dose_points [numeric] *(with default)*: vector with dose points, if dose points
#' are repeated, only the general pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., 2017
#'
#'@param irradiation_time_correction [numeric] or [RLum.Results-class] *(optional)*:
#' information on the used irradiation time correction obained by another experiements. I a `numeric`
#' is provided it has to be of length two: mean, standard error
#'
#'@param cross_talk_correction [numeric] or [RLum.Results-class] *(optional)*:
#' information on the used irradiation time correction obained by another experiements. If a `numeric`
#' vector is provided it has to be of length three: mean, 2.5 \% quantile, 97.5 \% quantile.
#'
#'@param verbose [logical] *(with default)*: enable/disable verbose mode
#'
#'@param plot [logical] *(with default)*: enable/disable plot output
#'
#'@param ... further arguments that can be passed to the plot output
#'
#'@return Function returns results numerically and graphically:\cr
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#' **`RLum.Results`**-object\cr
#'
#' **slot:** **`@data`**\cr
#' \tabular{lll}{
#' **Element** \tab **Type** \tab **Description**\cr
#'  `$data` \tab `data.frame` \tab the estimated equivalent dose \cr
#'  `$data_talbe` \tab `data.frame` \tab full dose and signal table \cr
#' }
#'
#'**slot:** **`@info`**\cr
#'
#' The original function call\cr
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' \itemize{
#'  \item OSL and TL curves, combined on two plots.
#'
#' }
#'
#'
#' @section Function version: 0.1.0
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
#' ##nothing so far TODO ... add tests with example
#'
#' @md
#' @export
analyse_Al2O3C_Measurement <- function(
  object,
  signal_integral = NULL,
  dose_points = c(0,4),
  irradiation_time_correction = NULL,
  cross_talk_correction = NULL,
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


    ##verbose
    ##plot
    results <- lapply(1:length(object), function(x) {
      analyse_Al2O3C_Measurement(
        object = object[[x]],
        signal_integral = signal_integral[[x]],
        dose_points = dose_points[[x]],
        irradiation_time_correction = irradiation_time_correction[[x]],
        cross_talk_correction = cross_talk_correction[[x]],
        verbose = verbose,
        plot = plot,
        ...

      )


    })


    ##merge and plot
    return(merge_RLum(results))

  }

  # Integretiy check  ---------------------------------------------------------------------------

  ##TODO ... do more, push harder
  ##Add sufficient unit tests

  # Preparation ---------------------------------------------------------------------------------

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
  if (is.null(irradiation_time_correction)) {
    irradiation_time_correction <- c(1,0)

  } else{
    if (is(irradiation_time_correction, "RLum.Results")) {
      if (irradiation_time_correction@originator == "analyse_Al2O3C_ITC") {
        irradiation_time_correction <- get_RLum(irradiation_time_correction)

        ##insert case for more than one observation ...
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

  ##calculate needed values
  NATURAL <- sum(object[[1]][signal_integral, 2])
  REGENERATED <- sum(object[[3]][signal_integral, 2])
  BACKGROUND <- sum(object[[5]][signal_integral, 2])

  ##combine into data.frame
  temp_df <- data.frame(
      POSITION = POSITION,
      DOSE = dose_points + irradiation_time_correction[1],
      DOSE_ERROR = dose_points * irradiation_time_correction[2]/irradiation_time_correction[1],
      STEP = c("NATURAL", "REGENERATED"),
      INTEGRAL = c(NATURAL, REGENERATED),
      BACKGROUND = c(BACKGROUND, BACKGROUND),
      NET_INTEGRAL = c(NATURAL - BACKGROUND, REGENERATED - BACKGROUND),
      row.names = NULL
    )

     ##0 dose points should not be biased by the correction ..
     id_zero <- which(dose_points == 0)
     temp_df$DOSE[id_zero] <- 0
     temp_df$DOSE_ERROR[id_zero] <- 0


   ##calculate DE by using the irradiation time correction AND the cross talk correction

   ##(1) sample dose point values with irradiation time corrections (random)
   DOSE_MC <- rnorm(1000, mean = temp_df$DOSE[2], sd = temp_df$DOSE_ERROR[2])

   ##(2) random sampling from cross-irradiation
   CT <- runif(1000, min = cross_talk_correction[2], max = cross_talk_correction[3])

   ##(3) combine the two values
   DOSE <- DOSE_MC - CT

   ##(4) signal ratio
   INTEGRAL_RATIO <- temp_df$NET_INTEGRAL[1]/temp_df$NET_INTEGRAL[2]

   ##(5) calculate DE
   temp_DE <- (DOSE * INTEGRAL_RATIO)

     ##(5) create final data.frame
   data <- data.frame(
     DE = mean(temp_DE),
     DE_ERROR = sd(temp_DE),
     POSITION,
     INTEGRAL_RATIO,
     CT_CORRECTION = cross_talk_correction[1],
     CT_CORRECTION_Q2.5 = cross_talk_correction[2],
     CT_CORRECTION_Q97.5 = cross_talk_correction[3],
     row.names = NULL

   )


  # Terminal output -----------------------------------------------------------------------------
  if(verbose){
    cat(paste0("\n[analyse_Al2O3_Measurement()] #",POSITION, " ", "DE: ",
               round(data$DE, 2), " \u00B1 ", round(data$DE_ERROR,2)))

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
       xlab = list("Simulation [s]", "Temperature [\u00B0C]"),
       mtext = "# - indicates seq. order",
       legend.text = list(list("#1 NAT", "#3 REG", "#5 BG"), list("#2 NAT", "#4 REG")),
       legend.pos = list("topright", "topleft"),
       main = list(paste("POS:", POSITION, "| OSL"), paste("POS:", POSITION, "| TL"))
     )



    }

  # Output --------------------------------------------------------------------------------------
  output <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = data,
      data_table = temp_df
      ),
    info = list(
      call = sys.call()
    )
  )

}
