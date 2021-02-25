#' Create De(t) plot
#'
#' Plots the equivalent dose (De) in dependency of the chosen signal integral
#' (cf. Bailey et al., 2003). The function is simply passing several arguments
#' to the function [plot] and the used analysis functions and runs it in a loop.
#' Example: `legend.pos` for legend position, `legend` for legend text.
#'
#' **method**
#'
#' The original method presented by Bailey et al., 2003 shifted the signal integrals and slightly
#' extended them accounting for changes in the counting statistics. Example: `c(1:3, 3:5, 5:7)`.
#' However, here also another method is provided allowing to expand the signal integral by
#' consecutively expanding the integral by its chosen length. Example: `c(1:3, 1:5, 1:7)`
#'
#' Note that in both cases the integral limits are overlap. The finally applied limits are part
#' of the function output.
#'
#' **analyse_function.control**
#'
#' The argument `analyse_function.control` currently supports the following arguments
#' `sequence.structure`, `dose.points`, `mtext.outer`, `fit.method`, `fit.force_through_origin`, `plot`, `plot.single`
#'
#' @param object [RLum.Analysis-class] (**required**):
#' input object containing data for analysis
#'
#' @param signal.integral.min [integer] (**required**):
#' lower bound of the signal integral.
#'
#' @param signal.integral.max [integer] (**required**):
#' upper bound of the signal integral.
#'
#' @param background.integral.min [integer] (**required**):
#' lower bound of the background integral.
#'
#' @param background.integral.max [integer] (**required**):
#' upper bound of the background integral.
#'
#' @param method [character] (*with default*):
#' method applied for constructing the De(t) plot.
#' - `shift` (*the default*): the chosen signal integral is shifted the shine down curve,
#' - `expansion`: the chosen signal integral is expanded each time by its length
#'
#' @param signal_integral.seq [numeric] (*optional*):
#' argument to provide an own signal integral sequence for constructing the De(t) plot
#'
#' @param analyse_function [character] (*with default*):
#' name of the analyse function to be called. Supported functions are:
#' `'analyse_SAR.CWOSL'`, `'analyse_pIRIRSequence'`
#'
#' @param analyse_function.control [list] (*optional*):
#' selected arguments to be passed to the supported analyse functions
#' (`'analyse_SAR.CWOSL'`, `'analyse_pIRIRSequence'`)
#'
#' @param n.channels [integer] (*optional*):
#' number of channels used for the De(t) plot. If nothing is provided all
#' De-values are calculated and plotted until the start of the background
#' integral.
#'
#' @param show_ShineDownCurve [logical] (*with default*):
#' enables or disables shine down curve in the plot output
#'
#' @param respect_RC.Status [logical] (*with default*):
#'  remove De-values with 'FAILED' RC.Status from the plot
#'  (cf. [analyse_SAR.CWOSL] and [analyse_pIRIRSequence])
#'
#' @param verbose [logical] (*with default*):
#' enables or disables terminal feedback
#'
#' @param ... further arguments and graphical parameters passed to
#' [plot.default], [analyse_SAR.CWOSL] and [analyse_pIRIRSequence] (see details for further information).
#' Plot control parameters are: `ylim`, `xlim`, `ylab`, `xlab`, `main`, `pch`, `mtext`, `cex`, `legend`,
#' `legend.text`, `legend.pos`
#'
#' @return
#' A plot and an [RLum.Results-class] object with the produced De values
#'
#' `@data`:
#'
#' \tabular{lll}{
#' **Object** \tab **Type** \tab **Description**\cr
#' De.values \tab `data.frame` \tab table with De values \cr
#' signal_integral.seq \tab `numeric` \tab integral sequence used for the calculation
#' }
#'
#' `@info`:
#'
#' \tabular{lll}{
#' **Object** \tab **Type** \tab **Description**\cr
#' call \tab `call` \tab the original function call
#' }
#'
#'
#' @note
#' The entire analysis is based on the used analysis functions, namely
#' [analyse_SAR.CWOSL] and [analyse_pIRIRSequence]. However, the integrity
#' checks of this function are not that thoughtful as in these functions itself.
#' It means, that every sequence should be checked carefully before running long
#' calculations using several hundreds of channels.
#'
#' @section Function version: 0.1.3
#'
#' @author Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @references
#' Bailey, R.M., Singarayer, J.S., Ward, S., Stokes, S., 2003. Identification of partial resetting
#' using De as a function of illumination time. Radiation Measurements 37, 511-518.
#' doi:10.1016/S1350-4487(03)00063-5
#'
#' @seealso [plot], [analyse_SAR.CWOSL], [analyse_pIRIRSequence]
#'
#' @examples
#'
#' \dontrun{
#' ##load data
#' ##ExampleData.BINfileData contains two BINfileData objects
#' ##CWOSL.SAR.Data and TL.SAR.Data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##transform the values from the first position in a RLum.Analysis object
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
#' plot_DetPlot(object,
#'              signal.integral.min = 1,
#'              signal.integral.max = 3,
#'              background.integral.min = 900,
#'              background.integral.max = 1000,
#'              n.channels = 5,
#' )
#' }
#'
#' @md
#' @export
plot_DetPlot <- function(
  object,
  signal.integral.min,
  signal.integral.max,
  background.integral.min,
  background.integral.max,
  method = "shift",
  signal_integral.seq = NULL,
  analyse_function = "analyse_SAR.CWOSL",
  analyse_function.control = list(),
  n.channels = NULL,
  show_ShineDownCurve = TRUE,
  respect_RC.Status = FALSE,
  verbose = TRUE,
  ...
) {


# Integrity Tests -----------------------------------------------------------------------------
  ##get structure
  object.structure <- structure_RLum(object)


# Set parameters ------------------------------------------------------------------------------
  ##set n.channels
  if(is.null(n.channels)){
    n.channels <- ceiling(
      (background.integral.min - 1 - signal.integral.max) / (signal.integral.max - signal.integral.min)
    )

  }

  analyse_function.settings <- list(
     sequence.structure = c("TL", "IR50", "pIRIR225"),
     dose.points = NULL,
     mtext.outer = "",
     fit.method = "EXP",
     fit.force_through_origin = FALSE,
     plot = FALSE,
     plot.single = FALSE
  )

  analyse_function.settings <- modifyList(analyse_function.settings, analyse_function.control)


# Analyse -------------------------------------------------------------------------------------
  ##set integral sequence
  if (is.null(signal_integral.seq)) {
    if(signal.integral.min == signal.integral.max){
      signal_integral.seq <- signal.integral.min:(background.integral.min - 1)


    }else{
      signal_integral.seq <-
        seq(signal.integral.min,
            background.integral.min - 1,
            by = signal.integral.max - signal.integral.min)

    }

  }


  if(analyse_function  == "analyse_SAR.CWOSL"){
    results <- merge_RLum(lapply(1:n.channels, function(x){
      analyse_SAR.CWOSL(
        object = object,
        signal.integral.min = if(method == "shift"){signal_integral.seq[x]}else{signal_integral.seq[1]},
        signal.integral.max =  signal_integral.seq[x+1],
        background.integral.min = background.integral.min,
        background.integral.max = background.integral.max,
        dose.points = analyse_function.settings$dose.points,
        mtext.outer = analyse_function.settings$mtext.outer,
        fit.force_through_origin = analyse_function.settings$fit.force_through_origin,
        fit.method = analyse_function.settings$fit.method,
        plot = analyse_function.settings$plot,
        plot.single = analyse_function.settings$plot.single,
        verbose = verbose
      )

    }))


  }
  else if(analyse_function  == "analyse_pIRIRSequence"){

    results <- merge_RLum(lapply(1:n.channels, function(x){
      analyse_pIRIRSequence(
        object = object,
        signal.integral.min = if(method == "shift"){signal_integral.seq[x]}else{signal_integral.seq[1]},
        signal.integral.max = signal_integral.seq[x+1],
        background.integral.min = background.integral.min,
        background.integral.max = background.integral.max,
        dose.points = analyse_function.settings$dose.points,
        mtext.outer = analyse_function.settings$mtext.outer,
        plot = analyse_function.settings$plot,
        plot.single = analyse_function.settings$plot.single,
        sequence.structure = analyse_function.settings$sequence.structure,
        verbose = verbose

      )

    }))



  }
  else{
   stop("[plot_DetPlot()] 'analyse_function' unknown!", call. = FALSE)

  }


# Plot ----------------------------------------------------------------------------------------

  ##get De results
  if(analyse_function == "analyse_pIRIRSequence"){
    pIRIR_signals <- unique(get_RLum(results)$Signal)

  }else{
    pIRIR_signals <- NA

  }

  ##run this in a loop to account for pIRIR data
  df_final <- lapply(1:length(pIRIR_signals), function(i){

    ##get data.frame
    df <- get_RLum(results)

    ##further limit
    if(!is.na(pIRIR_signals[1])){
      df <- df[df$Signal == pIRIR_signals[i],]

    }

    ##add shine down curve, which is by definition the first IRSL/OSL curve
    ##and normalise on the highest De value
    OSL_curve <-
      as(get_RLum(object, recordType = "SL")[[i]], "matrix")

    ##limit to what we see
    OSL_curve <- OSL_curve[1:signal_integral.seq[n.channels + 1],]

    m <-
      ((min(df$De - df$De.Error, na.rm = TRUE)) - (max(df$De, na.rm = TRUE) + max(df$De.Error, na.rm = TRUE))) / (min(OSL_curve[, 2], na.rm = TRUE) - max(OSL_curve[, 2], na.rm = TRUE))
    n <- (max(df$De, na.rm = TRUE) + max(df$De.Error, na.rm = TRUE)) - m * max(OSL_curve[, 2])

    OSL_curve[, 2] <- m * OSL_curve[, 2] + n
    rm(n, m)

    ##set plot settings
    plot.settings <- list(
      ylim = c(min(df$De - df$De.Error, na.rm = TRUE),
               (max(df$De, na.rm = TRUE) + max(df$De.Error, na.rm = TRUE))),
      xlim = c(min(OSL_curve[, 1]), max(OSL_curve[, 1])),
      ylab = expression(paste(D[e] / s, " and ", L[n]/(a.u.))),
      xlab = "Stimulation time [s]",
      main = "De(t) plot",
      pch = 1,
      mtext = ifelse(is.na(pIRIR_signals[1]), "", paste0("Signal: ",pIRIR_signals[i])),
      cex = 1,
      legend = TRUE,
      legend.text = c(expression(L[n]-signal), expression(D[e])),
      legend.pos = "bottomleft"
    )
    plot.settings <- modifyList(plot.settings, list(...))

    ##general settings
    par(cex = plot.settings$cex)

    ##open plot area
    plot(
      NA,
      NA,
      xlim = plot.settings$xlim,
      ylim = plot.settings$ylim,
      xlab = plot.settings$xlab,
      ylab = plot.settings$ylab,
      main = plot.settings$main
    )

    if (show_ShineDownCurve) {
      lines(OSL_curve, type = "b", pch = 20)
    }

    ##set x-axis
    df_x <-
      OSL_curve[seq(signal.integral.max, signal_integral.seq[n.channels+1], length.out = nrow(df)),1]

    #combine everything to allow excluding unwanted values
    df_final <- cbind(df, df_x)

    if (respect_RC.Status) {
      df_final <- df_final[df_final$RC.Status != "FAILED", ]

    }


    ##TodDo:color failed points red
    ##plot points and error bars
    points(df_final[, c("df_x", "De")], pch = plot.settings$pch)
    segments(
      x0 = df_final$df_x,
      y0 = df_final$De + df_final$De.Error,
      x1 = df_final$df_x,
      y1 = df_final$De - df_final$De.Error
    )

    ##set mtext
    mtext(side = 3, plot.settings$mtext)

    ##legend
    if(plot.settings$legend){
      legend(
        plot.settings$legend.pos,
        legend = plot.settings$legend.text,
        pch = c(plot.settings$pch, 20),
        bty = "n"
      )
    }

    ##set return
    return(df_final)

  })

  ##merge results
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      De.values = as.data.frame(data.table::rbindlist(df_final)),
      signal_integral.seq = signal_integral.seq
      ),
    info = list(call = sys.call())
  ))

}
