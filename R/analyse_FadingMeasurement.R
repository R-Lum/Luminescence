#' Analyse fading measurements and returns the fading rate per decade (g-value)
#'
#' The function analysis fading measurements and returns a fading rate including an error estimation.
#' The function is not limited to standard fading measurements, as can be seen, e.g., Huntely and
#' Lamothe 2001.
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}): input object with the
#' measurement data. Alternatively a \code{\link{list}} containing \code{\linkS4class{RLum.Analysis}}
#' objects can be provided
#'
#' @param structure \code{\link{character}} (with default): sets the structure of the measurement
#' data. Allowed are \code{'Lx'} or \code{c('Lx','Tx')}. Other input is ignored
#'
#' @param signal.integral \code{\link{vector}} (\bold{required}): vector with the
#' limits for the signal integral
#'
#' @param background.integral \code{\link{vector}} (\bold{required}): vector with the
#' bounds for the background integral
#'
#' @param n.MC \code{\link{integer}} (with default): number for Monte Carlo runs for the error
#' estimation
#'
#' @param verbose \code{\link{logical}} (with default): enables/disables verbose mode
#'
#' @param plot \code{\link{logical}} (with default): enables/disables plot output
#'
#' @param plot.single \code{\link{logical}} (with default): enables/disables single plot
#' mode, i.e. one plot window per plot
#'
#' @param \dots (optional) further arguments that can be passed to internally used functions (see details)
#'
#' @return An RLum.Results objects is returned ... TODO
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @note \bold{This function has BETA status and should not be used for publication work!}
#'
#' @keywords datagen
#'
#' @references -
#'
#' @seealso \code{\link{calc_OSLLxTxRatio}}, \code{\link{read_BIN2R}}, \code{\link{read_XSYG2R}},
#' \code{\link{extract_IrradiationTimes}}
#'
#' @examples
#'
#' ##nothing so far
#'
#' @return
analyse_FadingMeasurement <- function(
  object,
  structure = c("Lx", "Tx"),
  signal.integral,
  background.integral,
  n.MC = 100,
  verbose = TRUE,
  plot = TRUE,
  plot.single = FALSE,
  ...
){

  # Integrity Tests -----------------------------------------------------------------------------
  if (is(object, "list")) {
    if (!unique(sapply(object, class)) == "RLum.Analysis") {
      stop(
        "[analyse_FadingMeasurement()] 'object' expects an 'RLum.Analysis' object or a 'list' of such objects!"
      )

    }

  } else if (class(object) == "RLum.Analyis") {
    object <- list(object)

  } else{
    stop(
      "[analyse_FadingMeasurement()] 'object' needs to be of type 'RLum.Analysis' or a 'list' of such objects!"
    )

  }


  # Prepare data --------------------------------------------------------------------------------

  ##support read_XSYG2R()
  if(length(unique(unlist(lapply(object, slot, name = "originator")))) == 1 &&
     unique(unlist(lapply(object, slot, name = "originator"))) == "read_XSYG2R"){

  irradiation_times <- extract_IrradiationTimes(object = object)

  ##reduce irradiation times ... extract curve data
  TIMESINCEIRR <- unlist(lapply(irradiation_times, function(x) {

    ##get time since irradiation
    temp_TIMESINCEIRR <-
      x$irr.times[["TIMESINCEIRR"]][!grepl(pattern = "irradiation",
                                           x = x$irr.times[["STEP"]],
                                           fixed = TRUE)]

    ##substract half irradiation time
    temp_IRR_TIME <-
      x$irr.times[["IRR_TIME"]][!grepl(pattern = "irradiation",
                                           x = x$irr.times[["STEP"]],
                                           fixed = TRUE)]

    return(temp_TIMESINCEIRR - temp_IRR_TIME/2)

  }))

  ##clean object by removing the irradiation step ... and yes, we drop!
  object_clean <- unlist(get_RLum(object, curveType = "measured"))

  ##support read_BIN2R()
  }else if (length(unique(unlist(lapply(object, slot, name = "originator")))) == 1 &&
            unique(unlist(lapply(object, slot, name = "originator"))) == "read_BIN2R"){
    try(stop("[analyse_FadingMeasurement()] Analysing data imported from a BIN-file is currently not supported!", call. = FALSE))
    return(NULL)

  ##not support
  }else{
    try(stop("[analyse_FadingMeasurement()] Unknown or unsupported originator!", call. = FALSE))
    return(NULL)

  }

  # Calculation ---------------------------------------------------------------------------------

  ##calculate Lx/Tx or ... just Lx, it depends on the patttern
  if(length(structure) == 2){
    Lx_data <- object_clean[seq(1,length(object_clean), by = 2)]
    Tx_data <- object_clean[seq(2,length(object_clean), by = 2)]


  }else if(length(structure) == 1){
    Lx_data <- object_clean
    Tx_data <- NULL

  }else{
    try(stop("[analyse_FadingMeasurement()] I have no idea what your structure means!", call. = FALSE))
    return(NULL)

  }

  ##calculate Lx/Tx table
  LxTx_table <- merge_RLum(lapply(1:length(Lx_data), function(x) {
    calc_OSLLxTxRatio(
      Lx.data = Lx_data[[x]],
      Tx.data = Tx_data[[x]],
      signal.integral = signal.integral,
      background.integral = background.integral,
      signal.integral.Tx = list(...)$signal.integral.Tx,
      background.integral.Tx = list(...)$background.integral.Tx,
      sigmab = list(...)$sigmab,
      sig0 = if(
        is.null(list(...)$sig0)){
        formals(calc_OSLLxTxRatio)$sig0
      }else{
        list(...)$sig0
      },
      background.count.distribution = if(
        is.null(list(...)$background.count.distribution)){
        formals(calc_OSLLxTxRatio)$background.count.distribution
      }else{
        list(...)$background.count.distribution
      }
    )

  }))$LxTx.table

  ##normalise data to prompt measurement (this gives as room)
  tc <- min(TIMESINCEIRR)

  ##normalise
  if(length(structure) == 2){
    LxTx_NORM <-
      LxTx_table[["LxTx"]] / LxTx_table[["LxTx"]][which(TIMESINCEIRR== tc)]
    LxTx_NORM.ERROR <-
      LxTx_table[["LxTx.Error"]] / LxTx_table[["LxTx"]][which(TIMESINCEIRR == tc)]


  }else{
    LxTx_NORM <-
      LxTx_table[["Net_LnLx"]] / LxTx_table[["Net_LnLx"]][which(TIMESINCEIRR== tc)]
    LxTx_NORM.ERROR <-
      LxTx_table[["Net_LnLx.Error"]] / LxTx_table[["Net_LnLx"]][which(TIMESINCEIRR == tc)]

  }


  ##normalise time since irradtion
  TIMESINCEIRR_NORM <- TIMESINCEIRR/tc


  ##add dose and time since irradiation
  LxTx_table <-
    cbind(
      LxTx_table,
      TIMESINCEIRR = TIMESINCEIRR,
      TIMESINCEIRR_NORM = TIMESINCEIRR_NORM,
      TIMESINCEIRR_NORM.LOG = log10(TIMESINCEIRR_NORM),
      LxTx_NORM = LxTx_NORM,
      LxTx_NORM.ERROR = LxTx_NORM.ERROR
    )


  # Fitting -------------------------------------------------------------------------------------
  ##we need to fit the data to get the g_value

  ##sample for monte carlo runs
  MC_matrix <- cbind(LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                     matrix(rnorm(
                       n = n.MC * nrow(LxTx_table),
                       mean = LxTx_table[["LxTx_NORM"]],
                       sd = LxTx_table[["LxTx_NORM.ERROR"]]
                     ),
                     ncol = n.MC))



  ##apply the fit
  fit_matrix <- vapply(X = 2:(n.MC+1), FUN = function(x){

    ##fit
    lm(y~x, data = data.frame(
      x = MC_matrix[,1],
      y = MC_matrix[,x]))$coefficients


  }, FUN.VALUE = vector("numeric", length = 2))


  ##calculate g-values from matrix
  g_value.MC <- abs(fit_matrix[2, ]) * 1 / fit_matrix[1, ] * 100


  ##for plotting
  fit <-
     lm(y ~ x, data = data.frame(x = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                 y = LxTx_table[["LxTx_NORM"]]))

  ##for predicting
  fit_predict <-
    lm(y ~ x, data = data.frame(y = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                x = LxTx_table[["LxTx_NORM"]]))

  ##calculate final g_value
  g_value_fit <- abs(fit$coefficient[2]) * 1 / fit$coefficient[1] * 100

  ##construct output data.frame
  g_value <- data.frame(
    FIT =  g_value_fit,
    MEAN = mean(g_value.MC),
    SD = sd(g_value.MC),
    Q_0.025 = quantile(x = g_value.MC, probs = 0.025),
    Q_0.16 = quantile(x = g_value.MC, probs = 0.16),
    Q_0.84 = quantile(x = g_value.MC, probs = 0.84),
    Q_0.975 = quantile(x = g_value.MC, probs = 0.975)

  )


  # Approximation -------------------------------------------------------------------------------
  T_0.5.interpolated <- approx(x = LxTx_table[["LxTx_NORM"]],
         y = LxTx_table[["TIMESINCEIRR_NORM"]],
         xout = 0.5)

  T_0.5.predict <- stats::predict.lm(fit_predict,newdata = data.frame(x = 0.5), interval = "predict")

  T_0.5 <- data.frame(
    T_0.5_INTERPOLATED = T_0.5.interpolated$y,
    T_0.5_PREDICTED =  (10^T_0.5.predict[,1])*tc,
    T_0.5_PREDICTED.LOWER =  (10^T_0.5.predict[,2])*tc,
    T_0.5_PREDICTED.UPPER =  (10^T_0.5.predict[,2])*tc

  )

  # Plotting ------------------------------------------------------------------------------------
  if(plot) {
    if (!plot.single) {
      par.default <- par()$mfrow
      on.exit(par(mfrow = par.default))
      par(mfrow = c(2, 2))

    }

    ##get package
    col <- get("col", pos = .LuminescenceEnv)

    ##set some plot settings
    plot_settings <- list(
      xlab = "Stimulation time [s]",
      log = "",
      mtext = ""

    )

    ##modify on request
    plot_settings <- modifyList(x = plot_settings, val = list(...))

    ##get unique irradiation times ... for plotting
    irradiation_times.unique <- unique(TIMESINCEIRR)

    ##limit to max 5
    if(length(irradiation_times.unique) >= 5){
      irradiation_times.unique <-
        irradiation_times.unique[seq(1, length(irradiation_times.unique),
                                     length.out = 5)]

    }


    if(length(structure) == 2){
      plot_RLum(
        set_RLum(class = "RLum.Analysis", records = object_clean[seq(1, length(object_clean), by = 2)]),
        combine = TRUE,
        col = c(col[1:5],rep(rgb(0,0,0,0.3), length(TIMESINCEIRR) - 5)),
        plot.single = TRUE,
        legend.text = c(paste(irradiation_times.unique, "s"), "others"),
        legend.col = c(col[1:length(irradiation_times.unique)], rgb(0,0,0,0.3)),
        xlab = plot_settings$xlab,
        log = plot_settings$log,
        main = expression(paste(L[x], " - curves")),
        mtext = plot_settings$mtext
      )

      plot_RLum(
        set_RLum(class = "RLum.Analysis", records = object_clean[seq(2, length(object_clean), by = 2)]),
        combine = TRUE,
        col = c(col[1:5],rep(rgb(0,0,0,0.3), length(TIMESINCEIRR) - 5)),
        plot.single = TRUE,
        legend.text = c(paste(irradiation_times.unique, "s"), "others"),
        legend.col = c(col[1:length(irradiation_times.unique)], rgb(0,0,0,0.3)),
        xlab = plot_settings$xlab,
        log = plot_settings$log,
        main = expression(paste(T[x], " - curves")),
        mtext = plot_settings$mtext
      )



    }else{
      plot_RLum(
        set_RLum(class = "RLum.Analysis", records = object_clean),
        combine = TRUE,
        col = c(col[1:5],rep(rgb(0,0,0,0.3), length(TIMESINCEIRR) - 5)),
        plot.single = TRUE,
        legend.text = c(paste(irradiation_times.unique, "s"), "others"),
        legend.col = c(col[1:length(irradiation_times.unique)], rgb(0,0,0,0.3)),
        xlab = plot_settings$xlab,
        log = plot_settings$log,
        main = expression(paste(L[x], " - curves")),
        mtext = plot_settings$mtext
      )


      ##empty Tx plot
      plot(NA,NA, xlim = c(0,1), ylim = c(0,1), xlab = "",
           ylab = "",
           axes = FALSE)
      text(x = 0.5, y = 0.5, labels = expression(paste("No ", T[x], " curves detected")))



    }

    ##(2) Fading plot
    plot(
      NA,
      NA,
      ylab = "Normalised intensity [a.u.]",
      xaxt = "n",
      xlab = "Time since irradition [s]",
      sub = expression(paste("[", log[10](t / t[c]), "]")),
      ylim = c(0.1, 1.1),
      xlim = range(LxTx_table[["TIMESINCEIRR_NORM.LOG"]]),
      main = "Signal Fading"
    )

    ##add axis
    axis(
      side = 1,
      at = axTicks(side = 1),
      labels = round((10 ^ (axTicks(
        side = 1
      )) * tc), digits = 0)
    )

    mtext(side = 3, paste0(
      "g-value: ",
      round(g_value$FIT, digits = 2),
      " \u00b1 ",
      round(g_value$SD, digits = 2),
      " (%/decade) | tc = ", tc
    ), cex = par()$cex * 0.9)

    ##add curves
    x <- NA
    for (i in 1:n.MC) {
      curve(fit_matrix[2,i] * x + fit_matrix[1,i], col = rgb(0,0.2,0.4,0.2), add = TRUE)

    }

    ##add master curve in red
    curve(fit$coefficient[2] * x + fit$coefficient[1], col = "red", add = TRUE, lwd = 1.5)


    ##addpoints
    points(x = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
           y = LxTx_table[["LxTx_NORM"]],
           pch = 21,
           bg = "grey")

    ##error bars
    segments(
      x0 = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
      x1 = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
      y0 = LxTx_table[["LxTx_NORM"]] + LxTx_table[["LxTx_NORM.ERROR"]],
      y1 = LxTx_table[["LxTx_NORM"]] - LxTx_table[["LxTx_NORM.ERROR"]],
      col = "grey"

    )

    plot(density(g_value.MC),
         main = "Density: g-values (%/decade)")
    rug(x = g_value.MC)
    abline(v = c(g_value[["Q_0.16"]],g_value[["Q_0.84"]]),  lty = 2, col = "darkgreen")
    abline(v = c(g_value[["Q_0.025"]],g_value[["Q_0.975"]]),  lty = 2, col = "red")
    legend(
      "topleft",
      legend = c("HPD - 68 %", "HPD - 95 %"),
      lty = 2,
      col = c("darkgreen", "red"),
      bty = "n"
    )



  }

  # Terminal ------------------------------------------------------------------------------------
  if(verbose){

    cat("\n[analyse_FadingMeasurement()]\n")
    cat(paste0("\n n.MC:\t",n.MC))
    cat(paste0("\n tc:\t",tc))
    cat("\n---------------------------------------------------")
    cat(paste0("\nT_0.5 interpolated:\t",T_0.5$T_0.5_INTERPOLATED))
    cat(paste0("\nT_0.5 predicted:\t",round(T_0.5$T_0.5_PREDICTED, digits = 2)))
    cat(paste0("\ng-value:\t\t", round(g_value$FIT, digits = 2), " \u00b1 ", round(g_value$SD, digits = 2),
      " (%/decade)"))
    cat("\n---------------------------------------------------")




  }

  # Return --------------------------------------------------------------------------------------
  return(
    set_RLum(class = "RLum.Results",
             data = list(
               g_value = g_value,
               T_0.5 =   T_0.5,
               tc = tc,
               fit = fit,
               LxTx_table = LxTx_table,
               irr.times = irradiation_times
             ),
             info = list(call = sys.call()))


  )

}
