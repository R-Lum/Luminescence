#'Al2O3:C Reader Cross Talk Analysis
#'
#'The function provides the analysis of cross-talk measurements on a FI lexsyg SMART reader using
#'Al2O3:C pellets
#'
#'@param object \code{\linkS4class{RLum.Analysis}} \bold{(required)}: measurement input
#'
#'@param signal_integral \code{\link{numeric}} (optional): signal integral, used for the signal
#' and the background. If nothing is provided the full range is used
#'
#'@param dose_points \code{\link{numeric}} (with default): vector with dose points, if dose points
#' are repeated, only the general pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., 2017
#'
#'@param irradiation_time_correction \code{\link{numeric}} or \code{\linkS4class{RLum.Results}} (optional):
#' information on the used irradiation time correction obained by another experiements.
#'
#'@param method_control \code{\link{list}} (optional): optional parameters to control the calculation.
#' See details for further explanations
#'
#'@param plot \code{\link{logical}} (with default): enable/disable plot output
#'
#'@param ... further arguments that can be passed to the plot output
#'
#'@return Function returns results numerically and graphically:\cr
#'
#' -----------------------------------\cr
#' [ NUMERICAL OUTPUT ]\cr
#' -----------------------------------\cr
#' \bold{\code{RLum.Reuslts}}-object\cr
#'
#' \bold{slot:} \bold{\code{@data}}\cr
#' \tabular{lll}{
#' \bold{Element} \tab \bold{Type} \tab \bold{Description}\cr
#'  \code{$data} \tab \code{data.frame} \tab summed apparent dose table \cr
#'  \code{$data_full} \tab \code{data.frame} \tab full apparent dose table \cr
#'  \code{$fit} \tab \code{lm} \tab the linear model obtained from fitting \cr
#'  \code{$col.seq} \tab \code{numeric} \tab the used colour vector \cr
#' }
#'
#'\bold{slot:} \bold{\code{@info}}\cr
#'
#' The original function call\cr
#'
#' ------------------------\cr
#' [ PLOT OUTPUT ]\cr
#' ------------------------\cr
#'
#' \itemize{
#'  \item An overview of the obtained apparent dose values
#'
#' }
#'
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\link{analyse_Al2O3C_ITC}}
#'
#' @references TODO
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##nothing so far TODO ... add tests with example
#'
#'@export
analyse_Al2O3C_Measurements <- function(
  object,
  signal_integral = NULL,
  dose_points = c(0,4),
  irradiation_time_correction = NULL,
  cross_talk_correction = NULL,
  travel_dosimeter = NULL,
  method_control = NULL,
  plot = TRUE,
  ...
){

  # Integretiy check  ---------------------------------------------------------------------------

  ##check input object
  if(!all(unlist(lapply(object, function(x){is(x, "RLum.Analysis")})))){
    stop("[analyse_Al2O3C_CrossTalk()] The elements in 'object' are not all of type 'RLum.Analsyis'", call. = FALSE)

  }

  ##TODO ... do more, push harder
  ##Add sufficient unit tests

  # Preparation ---------------------------------------------------------------------------------
  #set method control
  method_control_settings <- list(
    fit.method = "EXP"

  )

  ##modify on request
  if(!is.null(method_control)){
    method_control_settings <- modifyList(x = method_control_settings, val = method_control)

  }


  ##set signal integral
  if(is.null(signal_integral)){
    signal_integral <- c(1:nrow(object[[1]][[1]][]))

  }else{
    ##check whether the input is valid, otherwise make it valid
    if(min(signal_integral) < 1 | max(signal_integral) > nrow(object[[1]][[1]][])){
      signal_integral <- c(1:nrow(object[[1]][[1]][]))
      warning(
        paste0(
          "[analyse_Al2O3C_ITC()] Input for 'signal_integral' corrected to 1:", nrow(object[[1]][[1]][])
        ),
        call. = FALSE
      )
    }

  }

  ##check irradiation time correction
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
          "[analyse_Al2O3C_CrossTalk()] The object provided for the argument 'irradiation_time_correction' was created by an unsupported function!",
          call. = FALSE
        )

      }

    }
  }

  # Calculation ---------------------------------------------------------------------------------
  ##we have two dose points, and one background curve, we do know only the 2nd dose

  ##create signal table list
  signal_table_list <- lapply(1:length(object), function(i) {
    ##calculate all the three signals needed
    BACKGROUND <- sum(object[[i]][[3]][, 2])
    NATURAL <- sum(object[[i]][[1]][, 2])
    REGENERATED <- sum(object[[i]][[2]][, 2])


    temp_df <- data.frame(
      POSITION = get_RLum(object[[i]][[1]], info.object = "position"),
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

    return(temp_df)

  })

  APPARENT_DOSE <- as.data.frame(data.table::rbindlist(lapply(1:length(object), function(x){

    ##run in MC run
    DOSE <- rnorm(1000, mean = signal_table_list[[x]]$DOSE[2], sd = signal_table_list[[x]]$DOSE_ERROR[2])

    ##calculation
    temp <- (DOSE * signal_table_list[[x]]$NET_INTEGRAL[1])/signal_table_list[[x]]$NET_INTEGRAL[2]

    data.frame(
      POSITION = signal_table_list[[x]]$POSITION[1],
      AD = mean(temp),
      AD_ERROR = sd(temp))

  })))

  ##add apparent dose to the information
  signal_table_list <- lapply(1:length(signal_table_list), function(x){
      cbind(signal_table_list[[x]], rep(APPARENT_DOSE[x,2:3], 2))

  })

  ##combine
  data_full <- as.data.frame(data.table::rbindlist(signal_table_list), stringsAsFactors = FALSE)

  # Plotting ------------------------------------------------------------------------------------

    ##get plot settings
    par.default <- par(no.readonly = TRUE)
    on.exit(par(par.default))

    ##settings
    plot_settings <- list(
      main = "Sample Carousel Crosstalk",
      mtext = ""
    )

      ##modify on request
      plot_settings <- modifyList(x = plot_settings, list(...))


    ##pre-calculations for graphical parameters
    n.positions <- length(unique(APPARENT_DOSE$POSITION))
    arc.step <- (2 * pi) / n.positions
    step <- 0

    ##condense data.frame, by calculating the mean for similar positions
    AD_matrix <- t(vapply(sort(unique(APPARENT_DOSE$POSITION)), function(x){
        c(x,mean(APPARENT_DOSE[["AD"]][APPARENT_DOSE[["POSITION"]] == x]),
          sd(APPARENT_DOSE[["AD"]][APPARENT_DOSE[["POSITION"]] == x]))

    }, FUN.VALUE = vector(mode = "numeric", length = 3)))

    ##create colour ramp
    col.seq <- data.frame(POSITION = AD_matrix[order(AD_matrix[,2]),1],
                          COLOUR = plotrix::smoothColors("green", nrow(AD_matrix) - 2, "red"),
                          stringsAsFactors = FALSE)

    col.seq <- col.seq[["COLOUR"]][order(col.seq[["POSITION"]])]

    ##calculate model
    fit <- lm(
      formula = y ~ poly(x, 2, raw=TRUE),
      data = data.frame(y = APPARENT_DOSE$AD[order(APPARENT_DOSE$POSITION)], x = sort(APPARENT_DOSE$POSITION)))

    ##enable or disable plot ... we cannot put the condition higher, because we here
    ##calculate something we are going to need later
    if (plot) {

      ##set layout matrix
      layout(mat = matrix(
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3),
        5,
        5,
        byrow = TRUE
      ))

      ##create empty plot
      par(
        mar = c(1, 1, 1, 1),
        omi = c(1, 1, 1, 1),
        oma = c(0.2, 0.2, 0.2, 0.2),
        cex = 1.1
      )
      shape::emptyplot(c(-1.15, 1.15), main = plot_settings$main, frame.plot = FALSE)

      ##add outher circle
      shape::plotcircle(r = 1.1, col = rgb(0.9, 0.9, 0.9, 1))

      ##add inner octagon
      shape::filledcircle(
        r1 = 0.6,
        mid = c(0, 0),
        lwd = 1,
        lcol = "black",
        col = "white"
      )

      ##add circles
      for (i in 1:n.positions) {
        shape::plotcircle(
          r = 0.05,
          mid = c(cos(step), sin(step)),
          cex = 6,
          pch = 20,
          col = col.seq[i]
        )
        text(x = cos(step) * 0.85,
             y = sin(step) * .85,
             labels = i)
        step <- step + arc.step

      }

      ##add center plot with position
      plot(NA, NA,
           xlim = range(AD_matrix[,1]),
           ylim = range(APPARENT_DOSE[,2]),
           frame.plot = FALSE,
           type = "l")

        ##add points
        points(x = APPARENT_DOSE, pch = 20, col = rgb(0,0,0,0.3))

        ##add linear model
        lines(sort(APPARENT_DOSE$POSITION), predict(fit), col = "red")

      ##add colour legend
      shape::emptyplot(c(-1.2, 1.2), frame.plot = FALSE)
      plotrix::gradient.rect(
        xleft = -0.6,
        ybottom = -1.2,
        xright = 0,
        ytop = 1.2,
        col = plotrix::smoothColors("green", 40, "red"),
        gradient = "y",
        border = NA
      )

      ##add scale text
      text(
        x = -0.3,
        y = 1.2,
        label = "[s]",
        pos = 3,
        cex = 1.1
      )
      text(
        x = 0.4,
        y = 1,
        label = round(max(AD_matrix[, 2]),2),
        pos = 3,
        cex = 1.1
      )
      text(
        x = 0.4,
        y = -1.5,
        label = 0,
        pos = 3,
        cex = 1.1
      )

    }

  # Output --------------------------------------------------------------------------------------
  output <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = data.frame(
        POSITION = AD_matrix[,1],
        AD = AD_matrix[,2],
        AD_ERROR = AD_matrix[,3]
        ),
      data_full = data_full,
      fit = fit,
      col.seq = col.seq
      ),
    info = list(
      call = sys.call()
    )
  )

}

analyse_Al2O3C_Measurements(
  object = temp_CAL,
  irradiation_time_correction =


  )
