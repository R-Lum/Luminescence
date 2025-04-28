#' @title Create De(t) plot
#'
#' @description Plots the equivalent dose (\eqn{D_e}) in dependency of the chosen signal integral
#' (cf. Bailey et al., 2003). The function is simply passing several arguments
#' to the function [plot] and the used analysis functions and runs it in a loop.
#' Example: `legend.pos` for legend position, `legend` for legend text.
#'
#' @details
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
#' The argument `analyse_function.control` currently supports the following arguments:
#' `sequence.structure`, `dose.points`, `mtext.outer`, `fit.method`,
#' `fit.force_through_origin`, `plot`, `plot_singlePanels`
#'
#' @param object [RLum.Analysis-class] (**required**): input object containing data for analysis
#' Can be provided as a [list] of such objects.
#'
#' @param signal.integral.min [integer] (**required**):
#' lower bound of the signal integral.
#'
#' @param signal.integral.max [integer] (**required**):
#' upper bound of the signal integral. Must be strictly greater than
#' `signal.integral.min`.
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
#' [analyse_SAR.CWOSL], [analyse_pIRIRSequence]
#'
#' @param analyse_function.control [list] (*optional*):
#' selected arguments to be passed to the supported analyse functions
#' ([analyse_SAR.CWOSL], [analyse_pIRIRSequence]). The arguments must be provided
#' as named [list], e.g., `list(dose.points = c(0,10,20,30,0,10)` will set the
#' regeneration dose points.
#'
#' @param n.channels [integer] (*optional*):
#' number of channels used for the De(t) plot. If nothing is provided all
#' De-values are calculated and plotted until the start of the background
#' integral.
#'
#' @param show_ShineDownCurve [logical] (*with default*):
#' enable/disable shine down curve in the plot output.
#'
#' @param respect_RC.Status [logical] (*with default*):
#' remove De values with 'FAILED' RC.Status from the plot (cf. [analyse_SAR.CWOSL]
#' and [analyse_pIRIRSequence]).
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param multicore [logical] (*with default*) : enable/disable multi core
#' calculation if `object` is a [list] of [RLum.Analysis-class] objects. Can be an
#' [integer] specifying the number of cores
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#' Disabling the plot is useful in cases where the output need to be processed
#' differently.
#'
#' @param ... further arguments and graphical parameters passed to
#' [plot.default], [analyse_SAR.CWOSL] and [analyse_pIRIRSequence] (see details for further information).
#' Plot control parameters are: `ylim`, `xlim`, `ylab`, `xlab`, `main`, `pch`, `mtext`, `cex`, `legend`,
#' `legend.text`, `legend.pos`
#'
#' @return
#' A plot and an [RLum.Results-class] object with the produced \eqn{D_e} values
#'
#' `@data`:
#'
#' \tabular{lll}{
#' **Object** \tab **Type** \tab **Description**\cr
#' `De.values` \tab `data.frame` \tab table with De values \cr
#' `signal_integral.seq` \tab `numeric` \tab integral sequence used for the calculation
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
#' @section Function version: 0.1.8
#'
#' @author Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl University of Heidelberg (Germany)
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
#' plot_DetPlot(
#'   object,
#'   signal.integral.min = 1,
#'   signal.integral.max = 3,
#'   background.integral.min = 900,
#'   background.integral.max = 1000,
#'   n.channels = 5)
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
  multicore = TRUE,
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("plot_DetPlot")
  on.exit(.unset_function_name(), add = TRUE)

# SELF CALL ---------------------------------------------------------------
  if(inherits(object, "list")) {
   ## remove all RLum.Analysis objects
   object <- .rm_nonRLum(x = object, class = "RLum.Analysis")

   ## get parameters to be passed on
   f_def <- sys.call(sys.parent(n = -1))
   args_default <- as.list(f_def)[-(1:2)]

    ## detect cores
    .validate_class(multicore, c("logical", "numeric", "integer"))
    .validate_length(multicore, 1)
    cores <- if (is.logical(multicore) && multicore) {
               parallel::detectCores() # nocov
             } else {
               max(multicore, 1)
             }

    ## function that calls plot_DetPlot() on each element of the list
    nested.fun <- function(x) {
      do.call(plot_DetPlot, c(list(object = x), args_default))
    }

    if (cores > 1) {
      ## run in parallel
      cl <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      if (verbose)
        message("\n[plot_DetPlot()] Running multicore session using ", cores,
                " cores ...")
      res_list <- parallel::parLapply(cl = cl, X = object, fun = nested.fun)
    } else {
      ## run in serial
      res_list <- lapply(object, nested.fun)
    }

    return(merge_RLum(res_list))
  }

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "RLum.Analysis")
  .validate_not_empty(object)

  ##get structure
  object.structure <- structure_RLum(object)

  ## signal.integral
  .validate_positive_scalar(signal.integral.min, int = TRUE)
  .validate_positive_scalar(signal.integral.max, int = TRUE)
  if (signal.integral.min >= signal.integral.max) {
    .throw_error("'signal.integral.max' must be greater than 'signal.integral.min'")
  }

  ## background.integral
  .validate_positive_scalar(background.integral.min, int = TRUE)
  .validate_positive_scalar(background.integral.min, int = TRUE)

  ## analyse_function
  analyse_function <- .validate_args(analyse_function,
                                     c("analyse_SAR.CWOSL", "analyse_pIRIRSequence"))

  ## deprecated argument
  if ("plot.single" %in% names(list(...))) {
    plot_singlePanels <- list(...)$plot.single
    .throw_warning("'plot.single' is deprecated, use 'plot_singlePanels' ",
                   "instead")
  }

# Set parameters ------------------------------------------------------------------------------
  ##set n.channels
  if(is.null(n.channels)){
    n.channels <- ceiling(
      (background.integral.min - 1 - signal.integral.max) / (signal.integral.max - signal.integral.min)
    )
    if (verbose) {
      message("'n.channels' not specified, set to ", n.channels)
    }
  }

  analyse_function.settings <- list(
     sequence.structure = c("TL", "IR50", "pIRIR225"),
     dose.points = NULL,
     mtext.outer = "",
     fit.method = "EXP",
     fit.force_through_origin = FALSE,
     trim_channels = FALSE,
     plot = FALSE,
     plot_singlePanels = FALSE
  )

  analyse_function.settings <- modifyList(analyse_function.settings, analyse_function.control)

# Analyse -------------------------------------------------------------------------------------
  ##set integral sequence
  if (is.null(signal_integral.seq)) {
    signal_integral.seq <- seq(signal.integral.min,
                               background.integral.min - 1,
                               by = signal.integral.max - signal.integral.min)
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
        trim_channels = analyse_function.settings$trim_channels,
        plot = analyse_function.settings$plot,
        plot_singlePanels = analyse_function.settings$plot_singlePanels,
        verbose = verbose
      )
    }))

  }
  else if(analyse_function  == "analyse_pIRIRSequence"){
    result.temp.list <- lapply(1:n.channels, function(x) {
      analyse_pIRIRSequence(
        object = object,
        signal.integral.min = if(method == "shift"){signal_integral.seq[x]}else{signal_integral.seq[1]},
        signal.integral.max = signal_integral.seq[x+1],
        background.integral.min = background.integral.min,
        background.integral.max = background.integral.max,
        dose.points = analyse_function.settings$dose.points,
        mtext.outer = analyse_function.settings$mtext.outer,
        plot = analyse_function.settings$plot,
        plot_singlePanels = analyse_function.settings$plot_singlePanels,
        sequence.structure = analyse_function.settings$sequence.structure,
        verbose = verbose
      )
    })

    ## as the analyse_pIRIRSequence() may fail, we see how many results
    ## we've actually managed to produce
    num.valid.results <- sum(!sapply(result.temp.list, is.null))
    if (num.valid.results == 0) {
      .throw_error("No valid results produced")
    }
    if (num.valid.results == 1) {
      results <- result.temp.list
    } else {
      results <- merge_RLum(result.temp.list)
    }
    rm(result.temp.list)
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
      if(!is.na(pIRIR_signals[1]))
        df <- df[df$Signal == pIRIR_signals[i],]

      ##add shine down curve, which is by definition the first IRSL/OSL curve
      ##and normalise on the highest De value
      OSL_curve <-
        as(get_RLum(object, recordType = "SL")[[i]], "matrix")

      ##limit to what we see
      OSL_curve <- OSL_curve[1:signal_integral.seq[n.channels + 1],]

      m <-
        ((min(df$De - df$De.Error, na.rm = TRUE)) -
           (max(df$De, na.rm = TRUE) +
              max(df$De.Error, na.rm = TRUE))) /
        (min(OSL_curve[, 2], na.rm = TRUE) -
           max(OSL_curve[, 2], na.rm = TRUE))
      n <- (max(df$De, na.rm = TRUE) +
              max(df$De.Error, na.rm = TRUE)) - m * max(OSL_curve[, 2])

      OSL_curve[, 2] <- m * OSL_curve[, 2] + n
      rm(n, m)

        ##set plot setting
        plot.settings <- modifyList(list(
          ylim = c(
            min(df$De - df$De.Error, na.rm = TRUE),
            (max(df$De, na.rm = TRUE) + max(df$De.Error, na.rm = TRUE))),
          xlim = c(min(OSL_curve[, 1]), max(OSL_curve[, 1])),
          ylab = if(show_ShineDownCurve[1])
                  expression(paste(D[e], " [s] and ", L[n], " [a.u.]"))
                else
                  expression(paste(D[e], " [s]")),
          xlab = "Stimulation time [s]",
          main = "De(t) plot",
          pch = 1,
          mtext = ifelse(is.na(pIRIR_signals[1]), "", paste0("Signal: ",pIRIR_signals[i])),
          cex = 1,
          legend = if(show_ShineDownCurve[1]) TRUE else FALSE,
          legend.text = if(show_ShineDownCurve[1])
                          c(expression(D[e]), expression(L[n]-signal))
                        else
                          expression(D[e]),
          legend.pos = "bottomleft"
        ), list(...))

        ##set x-axis
        df_x <-
          OSL_curve[seq(signal.integral.max, signal_integral.seq[n.channels+1], length.out = nrow(df)),1]

        #combine everything to allow excluding unwanted values
        df_final <- cbind(df, df_x)

        if (respect_RC.Status)
          df_final <- df_final[df_final$RC.Status != "FAILED", ]

       if(plot[1]) {
        ##general settings
        old_par <- par(no.readonly = TRUE)
        par(cex = plot.settings$cex)
        on.exit(par(old_par), add = TRUE)

        ##open plot area
        plot(
          NA,
          NA,
          xlim = plot.settings$xlim,
          ylim = if(any(is.infinite(plot.settings$ylim))) c(-1,1) else plot.settings$ylim ,
          xlab = plot.settings$xlab,
          ylab = plot.settings$ylab,
          main = plot.settings$main
        )

        if (show_ShineDownCurve)
          lines(OSL_curve, type = "b", pch = 20)

        ##ToDo:color failed points red
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
      } ## end plot
      ##set return
      return(df_final)
    })


# Return ------------------------------------------------------------------
  ##merge results
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      De.values = as.data.frame(data.table::rbindlist(df_final)),
      signal_integral.seq = signal_integral.seq
      ),
    info = list(
      call = sys.call())
  ))
}
