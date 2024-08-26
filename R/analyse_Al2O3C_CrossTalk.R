#' @title Al2O3:C Reader Cross Talk Analysis
#'
#' @description The function provides the analysis of cross-talk measurements on a
#' FI lexsyg SMART reader using Al2O3:C chips
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
#' made by Kreutzer et al., 2018
#'
#' @param recordType [character] (*with default*): input curve selection, which is passed to
#' function [get_RLum]. To deactivate the automatic selection set the argument to `NULL`
#'
#' @param irradiation_time_correction [numeric] or [RLum.Results-class] (*optional*):
#' information on the used irradiation time correction obtained by another experiments.
#'
#' @param method_control [list] (*optional*):
#' optional parameters to control the calculation.
#' See details for further explanations
#'
#' @param plot [logical] (*with default*):
#' enable/disable plot output
#'
#' @param ... further arguments that can be passed to the plot output
#'
#' @return
#' Function returns results numerically and graphically:
#'
#'  -----------------------------------\cr
#'  `[ NUMERICAL OUTPUT ]`\cr
#'  -----------------------------------\cr
#'
#'  **`RLum.Results`**-object
#'
#'  **slot:** **`@data`**
#'
#'  \tabular{lll}{
#'   **Element** \tab **Type** \tab **Description**\cr
#'   `$data` \tab `data.frame` \tab summed apparent dose table \cr
#'   `$data_full` \tab `data.frame` \tab full apparent dose table \cr
#'   `$fit` \tab `lm` \tab the linear model obtained from fitting \cr
#'   `$col.seq` \tab `numeric` \tab the used colour vector \cr
#'  }
#'
#' **slot:** **`@info`**
#'
#' The original function call
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' - An overview of the obtained apparent dose values
#'
#' @section Function version: 0.1.3
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [analyse_Al2O3C_ITC]
#'
#' @references
#'
#' Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier, N., 2018. Environmental Dose Rate
#' Determination Using a Passive Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
#' Geochronometria 45, 56-67. doi: 10.1515/geochr-2015-0086
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.Al2O3C, envir = environment())
#'
#' ##run analysis
#' analyse_Al2O3C_CrossTalk(data_CrossTalk)
#'
#' @md
#' @export
analyse_Al2O3C_CrossTalk <- function(
  object,
  signal_integral = NULL,
  dose_points = c(0,4),
  recordType = c("OSL (UVVIS)"),
  irradiation_time_correction = NULL,
  method_control = NULL,
  plot = TRUE,
  ...
){

  # Integrity check  ---------------------------------------------------------------------------

  ##check input object
  if(!all(unlist(lapply(object, function(x){is(x, "RLum.Analysis")})))){
    stop("[analyse_Al2O3C_CrossTalk()] The elements in 'object' are not all of type 'RLum.Analysis'", call. = FALSE)

  }

  ##TODO ... do more, push harder
  ##Accept the entire sequence ... including TL and extract
  ##Add sufficient unit tests

  # Preparation ---------------------------------------------------------------------------------
  ##select curves based on the recordType selection; if not NULL
  if(!is.null(recordType)){
    object <- get_RLum(object, recordType = recordType, drop = FALSE)

  }

  #set method control
  method_control_settings <- list(
    fit.method = "EXP"

  )

  ##modify on request
  if(!is.null(method_control)){
    if (!is.list(method_control)) {
      stop("[analyse_Al2O3C_CrossTalk()] 'method_control' is expected ",
           "to be a list", call. = FALSE)
    }
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
  if (!is.null(irradiation_time_correction)) {

    if (!is.numeric(irradiation_time_correction) &&
        !is(irradiation_time_correction, "RLum.Results")) {
      stop("[analyse_Al2O3C_CrossTalk()] 'irradiation_time_correction' ",
           "is expected to be a numeric value or an RLum.Results object",
           call. = FALSE)
    }

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
    id_zero <- which(dose_points == 0)
    temp_df$DOSE[id_zero] <- 0
    temp_df$DOSE_ERROR[id_zero] <- 0

    return(temp_df)

  })

  APPARENT_DOSE <- as.data.frame(data.table::rbindlist(lapply(1:length(object), function(x){

    ##run in MC run
    if(!is.null(irradiation_time_correction)){
    DOSE <- rnorm(1000, mean = signal_table_list[[x]]$DOSE[2], sd = signal_table_list[[x]]$DOSE_ERROR[2])

    }else{
      DOSE <- signal_table_list[[x]]$DOSE[2]

    }


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
    ## set colours
    col_pal <- grDevices::hcl.colors(100, palette = "RdYlGn", rev = TRUE)

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
    col.seq <- data.frame(
      POSITION = AD_matrix[order(AD_matrix[,2]),1],
      COLOUR = col_pal[seq(1,100, length.out = nrow(AD_matrix))],
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
      graphics::rect(
        xleft = rep(-0.6, 100),
        ybottom = seq(-1.2,1.1,length.out = 100),
        xright = rep(0, 100),
        ytop = seq(-1.1,1.2,length.out = 100),
        col = col_pal,
        lwd = 0,
        border = FALSE
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
