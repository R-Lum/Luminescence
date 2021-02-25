#' Al2O3 Irradiation Time Correction Analysis
#'
#' The function provides a very particular analysis to correct the irradiation
#' time while irradiating Al2O3:C chips in a luminescence reader.
#'
#' Background: Due to their high dose sensitivity Al2O3:C chips are usually
#' irradiated for only a very short duration or under the closed beta-source
#' within a luminescence reader. However, due to its high dose sensitivity, during
#' the movement towards the beta-source, the pellet already receives and non-negligible
#' dose. Based on measurements following a protocol suggested by Kreutzer et al., 2018,
#' a dose response curve is constructed and the intersection (absolute value) with the time axis
#' is taken as real irradiation time.
#'
#' **`method_control`**
#'
#' To keep the generic argument list as clear as possible, arguments to allow a deeper control of the method
#' are all preset with meaningful default parameters and can be
#' handled using the argument `method_control` only, e.g.,
#' `method_control = list(fit.method = "LIN")`. Supported arguments are:
#'
#' \tabular{lll}{
#' **ARGUMENT** \tab **FUNCTION** \tab **DESCRIPTION**\cr
#' `mode` \tab `plot_GrowthCurve` \tab as in [plot_GrowthCurve]; sets the mode used for fitting\cr
#' `fit.method` \tab `plot_GrowthCurve` \tab as in [plot_GrowthCurve]; sets the function applied for fitting\cr
#' }
#'
#' @param object [RLum.Analysis-class] or [list] **(required)**:
#' results obtained from the measurement.
#' Alternatively a list of 'RLum.Analysis' objects can be provided to allow an automatic analysis.
#'
#' @param signal_integral [numeric] (*optional*):
#' signal integral, used for the signal and the background.
#' If nothing is provided the full range is used. Argument can be provided as [list].
#'
#' @param dose_points [numeric] (*with default*):
#' vector with dose points, if dose points are repeated, only the general
#' pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., 2018. Argument can be provided as [list].
#'
#' @param recordType [character] (*with default*): input curve selection, which is passed to
#' function [get_RLum]. To deactivate the automatic selection set the argument to `NULL`
#'
#' @param method_control [list] (*optional*):
#' optional parameters to control the calculation.
#' See details for further explanations
#'
#' @param verbose [logical] (*with default*):
#' enable/disable verbose mode
#'
#' @param plot [logical] (*with default*):
#' enable/disable plot output
#'
#' @param ... further arguments that can be passed to the plot output
#'
#' @return
#' Function returns results numerically and graphically:
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
#'   **Element** \tab **Type** \tab **Description**\cr
#'  `$data` \tab `data.frame` \tab correction value and error \cr
#'  `$table` \tab `data.frame` \tab table used for plotting  \cr
#'  `$table_mean` \tab `data.frame` \tab table used for fitting \cr
#'  `$fit` \tab `lm` or `nls` \tab the fitting as returned by the function [plot_GrowthCurve]
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
#' - A dose response curve with the marked correction values
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [plot_GrowthCurve]
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
#' analyse_Al2O3C_ITC(data_ITC)
#'
#' @md
#' @export
analyse_Al2O3C_ITC <- function(
  object,
  signal_integral = NULL,
  dose_points = c(2,4,8,12,16),
  recordType = c("OSL (UVVIS)"),
  method_control = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
){


  # SELF CALL -----------------------------------------------------------------------------------
  if(is.list(object)){

    ##check whether the list contains only RLum.Analysis objects
    if(!all(unique(sapply(object, class)) == "RLum.Analysis")){
      stop("[analyse_Al2O3C()] All objects in the list provided by 'objects' need to be of type 'RLum.Analysis'",
           call. = FALSE)

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

    ##method_control
    ##verbose
    ##plot


    ##run analysis
    results_full <- lapply(1:length(object), function(x){

      ##run analysis
      results <- try(analyse_Al2O3C_ITC(
        object = object[[x]],
        signal_integral = signal_integral[[x]],
        dose_points = dose_points[[x]],
        method_control = method_control,
        verbose = verbose,
        plot = plot,
        main = ifelse("main"%in% names(list(...)), list(...)$main, paste0("ALQ #",x)),
        ...
      ))

      ##catch error
      if(inherits(results, "try-error")){
        return(NULL)

      }else{
        return(results)

      }

    })

    ##return
    return(merge_RLum(results_full))


  }

  # Integretiy check  ---------------------------------------------------------------------------

  ##check input object
  if(class(object) != "RLum.Analysis"){
    stop("[analyse_Al2O3C_ITC()] 'object' needs to be of type 'RLum.Analysis'", call. = FALSE)

  }

  ##TODO
  ##implement more checks ... if you find some time, somehow, somewhere

  # Preparation ---------------------------------------------------------------------------------

  ##select curves based on the recordType selection; if not NULL
  if(!is.null(recordType)){
    object <- get_RLum(object, recordType = recordType, drop = FALSE)

  }

  #set method control
  method_control_settings <- list(
    mode = "extrapolation",
    fit.method = "EXP"

  )

    ##modify on request
    if(!is.null(method_control)){
      method_control_settings <- modifyList(x = method_control_settings, val = method_control)

    }


  ##dose points enhancement
  ##make sure that the dose_point is enhanced
  dose_points <- rep(dose_points, times = length(object)/2)

  # Calculation ---------------------------------------------------------------------------------

  ##set signal integral
  if(is.null(signal_integral)){
    signal_integral <- c(1:nrow(object[[1]][]))

  }else{
    ##check whether the input is valid, otherwise make it valid
    if(min(signal_integral) < 1 | max(signal_integral) > nrow(object[[1]][])){
      signal_integral <- c(1:nrow(object[[1]][]))
      warning(
        paste0(
          "[analyse_Al2O3C_ITC()] Input for 'signal_integral' corrected to 1:", nrow(object[[1]][])
        ),
        call. = FALSE
      )
    }

  }

  ##calcuate curve sums, assuming the background
  net_SIGNAL <- vapply(1:length(object[seq(1,length(object), by = 2)]), function(x){
    temp_signal <- sum(object[seq(1,length(object), by = 2)][[x]][,2])
    temp_background <- sum(object[seq(2,length(object), by = 2)][[x]][,2])
    return(temp_signal - temp_background)

  }, FUN.VALUE = vector(mode = "numeric", length = 1))

  ##create data.frames

    ##single points
    df <- data.frame(
      DOSE = dose_points,
      net_SIGNAL = net_SIGNAL,
      net_SIGNAL.ERROR = 0,
      net_SIGNAL_NORM = net_SIGNAL/max(net_SIGNAL),
      net_SIGNAL_NORM.ERROR = 0
    )

    ##take mean
    ##make data frame for all curves for MC runs
    df_mean <- as.data.frame(data.table::rbindlist(lapply(unique(df$DOSE), function(x){
      data.frame(
        DOSE = x,
        net_SIGNAL = mean(df[df$DOSE == x, "net_SIGNAL"]),
        net_SIGNAL.ERROR = sd(df[df$DOSE == x, "net_SIGNAL"]),
        net_SIGNAL_NORM = mean(df[df$DOSE == x, "net_SIGNAL_NORM"]),
        net_SIGNAL_NORM.ERROR = sd(df[df$DOSE == x, "net_SIGNAL_NORM"])
      )
    })))


  ##calculate GC
  GC <- plot_GrowthCurve(
    sample = df_mean,
    mode = method_control_settings$mode,
    output.plotExtended = FALSE,
    output.plot = FALSE,
    fit.method = method_control_settings$fit.method,
    verbose = FALSE
  )


  ##output
  if(verbose){
    cat("\n[analyse_Al2O3C_ITC()]\n")
    cat(paste0("\n Used fit:\t\t",method_control_settings$fit.method))
    cat(paste0("\n Time correction value:\t", round(GC$De$De,3), " \u00B1 ", GC$De$De.Error))
    cat("\n\n")

  }


  # Plotting ------------------------------------------------------------------------------------

  if(plot){

    ##set plot settings
    plot_settings <- list(
      xlab = "Dose [s]",
      ylab = "Integrated net GSL [a.u.]",
      main = "Irradiation Time Correction",
      xlim = c(-5, max(df$DOSE)),
      ylim = c(0,max(df$net_SIGNAL)),
      legend.pos = "right",
      legend.text = "dose points",
      mtext = ""

    )

    ##modfiy list on request
    plot_settings <- modifyList(x = plot_settings, val = list(...))

    ##make plot area
    plot(NA, NA,
         xlim = plot_settings$xlim,
         ylim = plot_settings$ylim,
         xlab = plot_settings$xlab,
         ylab = plot_settings$ylab,
         main = plot_settings$main
    )

    ##add zero lines
    abline(v = 0)
    abline(h = 0)

    ##add dose points
    points(x = df$DOSE, y = df$net_SIGNAL)

    ##add dose response curve
    x <- seq(min(plot_settings$xlim), max(plot_settings$xlim), length.out = 100)
    lines(
      x = x,
      y = eval(GC$Formula)
    )

    ##show offset
    x <- 0
    lines(x = c(-GC$De[1], -GC$De[1]), y = c(eval(GC$Formula), 0), lty = 2, col = "red")
    shape::Arrows(
      x0 = 0,
      y0 = eval(GC$Formula),
      x1 = as.numeric(-GC$De[1]),
      y1 = eval(GC$Formula),
      arr.type = "triangle",
      arr.adj = -0.5,
      col = 'red',
      cex = par()$cex
    )

    ##add text
    text(
      x = -GC$De[1] / 2,
      y = eval(GC$Formula),
      pos = 3,
      labels = paste(round(GC$De[1],3), "\u00B1", GC$De[2]),
      col = 'red',
      cex = 0.8
    )

    ##add 2nd x-axis
    axis(
      side = 1,
      at = axTicks(side = 1),
      labels = paste0("(",(axTicks(side = 1) + round(as.numeric(GC$De[1]),2)), ")"),
      line = 1,
      col.axis = "red",
      lwd.ticks = 0,
      lwd = 0,
      cex.axis = 0.9
    )

    ##add legend
    legend(
      plot_settings$legend.pos,
      bty = "n",
      pch = 1,
      legend = plot_settings$legend.text
    )

    ##add mtext
    mtext(side = 3, text = plot_settings$mtext)

  }

  # Output --------------------------------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      data = data.frame(
        VALUE = as.numeric(GC$De$De),
        VALUE_ERROR = as.numeric(sd(GC$De.MC))
      ),
      table = df,
      table_mean = df_mean,
      fit = GC$Fit
    ),
    info = list(call = sys.call())
  ))

}
