#'Al2O3 Irradiation Time Correction Analysis
#'
#'The function provides a very particular analysis to correct the irradiation time while irradiating Al2O3:C
#'pellets in a luminescence reader.
#'
#'Background: Due to their high dose sensitivity Al2O3:C pellets are usually irradiated for only a very short duration or
#'under the closed beta-source within a luminescence reader. However, due to its high dose sensitivity, the movement
#'itself below the source induces an apparent luminescence signal, which can be translated to an irradiation time.
#'Based on measurements following a protocol suggested by Kreutzer et al., 2017, a dose response curve is constructed
#'and the intersection with the time axis is taken as real irradiation time.
#'
#' @param object \code{\linkS4class{RLum.Analysis}} \bold{(required)}: measurement input
#'
#' @param signal_integral \code{\link{numeric}} (optional): signal integral, used for the signal
#' and the background. If nothing is provided the full range is used
#'
#' @param dose_points \code{\link{numeric}} (with default): vector with dose points, if dose points
#' are repeated, only the general pattern needs to be provided. Default values follow the suggestions
#' made by Kreutzer et al., 2017
#'
#' @param method_control \code{\link{list}} (optional): optional parameters to control the calculation.
#' See details for further explanations
#'
#' @param verbose \code{\link{logical}} (with default): enable/disable verbose mode
#'
#' @param plot \code{\link{logical}} (with default): enable/disable plot output
#'
#' @param ... further aruments that can be passed to the plot output
#'
#' @return Function returns results numerically and graphically:\cr
#'
#' -----------------------------------\cr
#' [ NUMERICAL OUTPUT ]\cr
#' -----------------------------------\cr
#' \bold{\code{RLum.Reuslts}}-object\cr
#'
#' \bold{slot:} \bold{\code{@data}}\cr
#' \tabular{lll}{
#' \bold{Element} \tab \bold{Type} \tab \bold{Description}\cr
#'  \code{$data} \tab \code{data.frame} \tab correction value and error \cr
#'  \code{$table} \tab \code{data.frame} \tab table used for plotting  \cr
#'  \code{$table_mean} \tab \code{data.frame} \tab table used for fitting
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
#'  \item A dose response curve with the marked correction values
#'
#' }
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\link{plot_GrowthCurve}}
#'
#' @references TODO
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##nothing so far TODO ... add tests with example
#'
#' @export
analyse_Al2O3_ITC <- function(
  object,
  signal_integral = NULL,
  dose_points = c(2,4,8,12,16),
  method_control = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
){

  # Integretiy check  ---------------------------------------------------------------------------

  ##check input object
  if(class(object) != "RLum.Analysis"){
    stop("[analyse_Al2O3C_ITC()] 'object' needs to be of type 'RLum.Analsyis'", call. = FALSE)

  }

  ##check whether the curves are similar
  if(length(unique(names(object))) > 1){
    stop("[analyse_Al2O3C_ITC()] Input 'recordType' is not similar for all RLum.Data.Curve-objects!", call. = FALSE)

  }

  ##TODO
  ##implement more, currently we have stick with this here

  # Preparation ---------------------------------------------------------------------------------

  #set method control
  method_control_settings <- list(
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
      net_SIGNAL.ERROR = 0
    )

    ##take mean
    ##make data frame for all curves for MC runs
    df_mean <- as.data.frame(data.table::rbindlist(lapply(unique(df$DOSE), function(x){
      data.frame(
        DOSE = x,
        net_SIGNAL = mean(df[df$DOSE == x, "net_SIGNAL"]),
        net_SIGNAL.Error = sd(df[df$DOSE == x, "net_SIGNAL"])
      )
    })))


  ##calculate GC
  GC <- plot_GrowthCurve(
    sample = df_mean,
    mode = "additive",
    output.plotExtended = FALSE,
    output.plot = FALSE,
    fit.method = method_control_settings$fit.method,
    verbose = FALSE
  )


  ##output
  if(verbose){
    cat("\n[analyse_Al2O3C_ITC()]\n")
    cat(paste0("\n Used fit:\t\t",method_control_settings$fit.method))
    cat(paste0("\n Time correction value:\t", GC$De$De, " \u00B1 ", GC$De$De.Error))
    cat("\n\n")

  }


  # Plotting ------------------------------------------------------------------------------------

  if(plot){

    ##set plot settings
    plot_settings <- list(
      xlab = "Dose [s]",
      ylab = "Integraed net GSL [a.u.]",
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
      col = 'red'
    )

    ##add text
    text(
      x = -GC$De[1] / 2,
      y = eval(GC$Formula),
      pos = 3,
      labels = paste(GC$De[1], "\u00B1", GC$De[2]),
      col = 'red',
      cex = 0.8
    )

    ##add 2nd x-axis
    axis(
      side = 1,
      at = axTicks(side = 1),
      labels = paste0("(",(axTicks(side = 1) + as.numeric(GC$De[1])), ")"),
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
        VALUE_ERROR = as.numeric(GC$De$De.Error)
      ),
      table = df,
      table_mean = df_mean
    ),
    info = list(call = sys.call())
  ))

}
