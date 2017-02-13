#' Analyse fading measurements and returns the fading rate per decade (g-value)
#'
#' The function analysis fading measurements and returns a fading rate including an error estimation.
#' The function is not limited to standard fading measurements, as can be seen, e.g., Huntley and
#' Lamothe 2001. Additionally, the density of recombination centres (rho') is estimated after
#' Kars et al. 2008.
#'
#' All provided output corresponds to the \eqn{tc} value obtained by this analysis. Additionally
#' in the output object the g-value normalised to 2-days is provided. The output of this function
#' can be passed to the function \code{\link{calc_FadingCorr}}.\cr
#'
#' \bold{Fitting and error estimation}\cr
#'
#' For the fitting the function \code{\link[stats]{lm}} is used without applying weights. For the
#' error estimation all input values, except tc, as the precision can be consdiered as sufficiently
#' high enough with regard to the underlying problem, are sampled assuming a normal distribution
#' for each value with the value as the mean and the provided uncertainty as standard deviation. \cr
#'
#' \bold{Density of recombination centres}
#'
#' The density of recombination centres, expressed by the dimensionless variable rho', is estimated
#' by fitting equation 5 in Kars et al. 2008 to the data. For the fitting the function
#' \code{\link[stats]{nls}} is used without applying weights. For the error estimation the same
#' procedure as for the g-value is applied (see above).
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}): input object with the
#' measurement data. Alternatively, a \code{\link{list}} containing \code{\linkS4class{RLum.Analysis}}
#' objects or a \code{\link{data.frame}} with three columns
#' (x = LxTx, y = LxTx error, z = time since irradiation) can be provided.
#' Can also be a wide table, i.e. a \code{\link{data.frame}} with a number of colums divisible by 3
#' and where each triplet has the before mentioned column structure.
#'
#' @param structure \code{\link{character}} (with default): sets the structure of the measurement
#' data. Allowed are \code{'Lx'} or \code{c('Lx','Tx')}. Other input is ignored
#'
#' @param signal.integral \code{\link{vector}} (\bold{required}): vector with the
#' limits for the signal integral. Not required if a \code{data.frame} with LxTx values are
#' provided.
#'
#' @param background.integral \code{\link{vector}} (\bold{required}): vector with the
#' bounds for the background integral. Not required if a \code{data.frame} with LxTx values are
#' provided.
#'
#' @param t_star \code{\link{character}} (with default): method for calculating the time elasped
#' since irradiaton. Options are: \code{'half'}, which is \eqn{t_star := t_1 + (t_2 - t_1)/2} (Auclair et al., 2003)
#' and \code{'end'}, which takes the time between irradiation and the measurement step. Default is \code{'half'}
#'
#' @param n.MC \code{\link{integer}} (with default): number for Monte Carlo runs for the error
#' estimation
#'
#' @param verbose \code{\link{logical}} (with default): enables/disables verbose mode
#'
#' @param plot \code{\link{logical}} (with default): enables/disables plot output
#'
#' @param plot.single \code{\link{logical}} (with default): enables/disables single plot
#' mode, i.e. one plot window per plot. Alternatively a vector specifying the plot to be drawn, e.g.,
#' \code{plot.single = c(3,4)} draws only the last two plots
#'
#' @param \dots (optional) further arguments that can be passed to internally used functions (see details)
#'
#' @return An \code{\linkS4class{RLum.Results}} object is returned:
#'
#' Slot: \bold{@data}\cr
#'
#' \tabular{lll}{
#' \bold{OBJECT} \tab \code{TYPE} \tab \code{COMMENT}\cr
#' \code{fading_results} \tab \code{data.frame} \tab results of the fading measurement in a table \cr
#' \code{fit} \tab \code{lm} \tab object returned by the used linear fitting function \code{\link[stats]{lm}}\cr
#' \code{rho_prime} \tab \code{data.frame} \tab results of rho' estimation after Kars et al. 2008 \cr
#' \code{LxTx_table} \tab \code{data.frame} \tab Lx/Tx table, if curve data had been provided \cr
#' \code{irr.times} \tab \code{integer} \tab vector with the irradiation times in seconds \cr
#' }
#'
#' Slot: \bold{@info}\cr
#'
#' \tabular{lll}{
#' \bold{OBJECT} \tab \code{TYPE} \tab \code{COMMENT}\cr
#' \code{call} \tab \code{call} \tab the original function call\cr
#'
#' }
#'
#'
#' @section Function version: 0.1.5
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @note \bold{This function has BETA status and should not be used for publication work!}
#'
#' @keywords datagen
#'
#' @references
#'
#' Auclair, M., Lamothe, M., Huot, S., 2003. Measurement of anomalous fading for feldpsar IRSL using
#' SAR. Radiation Measurements 37, 487-492. doi:10.1016/S1350-4487(03)00018-0
#'
#' Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading in K-feldspars and the measurement
#' and correction for it in optical dating. Canadian Journal of Earth Sciences 38,
#' 1093-1106. doi:10.1139/cjes-38-7-1093
#'
#' Kars, R.H., Wallinga, J., Cohen, K.M., 2008. A new approach towards anomalous fading correction for feldspar
#' IRSL dating-tests on samples in field saturation. Radiation Measurements 43, 786-790. doi:10.1016/j.radmeas.2008.01.021
#'
#' @seealso \code{\link{calc_OSLLxTxRatio}}, \code{\link{read_BIN2R}}, \code{\link{read_XSYG2R}},
#' \code{\link{extract_IrradiationTimes}}
#'
#' @examples
#'
#' ## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
#' data("ExampleData.Fading", envir = environment())
#'
#' ##(1) get fading measurement data (here a three column data.frame)
#' fading_data <- ExampleData.Fading$fading.data$IR50
#'
#' ##(2) run analysis
#' g_value <- analyse_FadingMeasurement(
#' fading_data,
#' plot = TRUE,
#' verbose = TRUE,
#' n.MC = 10)
#'
#' ##(3) this can be further used in the function
#' ## to correct the age according to Huntley & Lamothe, 2001
#' results <- calc_FadingCorr(
#' age.faded = c(100,2),
#' g_value = g_value,
#' n.MC = 10)
#'
#'
#' @export
analyse_FadingMeasurement <- function(
  object,
  structure = c("Lx", "Tx"),
  signal.integral,
  background.integral,
  t_star = 'half',
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

  } else if (class(object) == "RLum.Analysis") {
    object <- list(object)

  } else if(class(object) == "data.frame"){
    if (ncol(object) %% 3 != 0) {
      stop("[analyse_FadingMeasurement()] 'object': if you provide a data.frame as input, the number of columns must be a multiple of 3.")
    } else {
      object <- do.call(rbind,
                        lapply(seq(1, ncol(object), 3), function(col) {
                          setNames(object[ , col:c(col+2)], c("LxTx", "LxTxError", "timeSinceIrr"))
                          })
                        )
      object <- object[complete.cases(object), ]
    }

    ##set table and object
    LxTx_table <- data.frame(LxTx = object[[1]], LxTx.Error = object[[2]])
    TIMESINCEIRR <- object[[3]]
    irradiation_times <- TIMESINCEIRR
    object <- NULL


  }else{
    stop(
      "[analyse_FadingMeasurement()] 'object' needs to be of type 'RLum.Analysis' or a 'list' of such objects!"
    )

  }


  # Prepare data --------------------------------------------------------------------------------
  if(!is.null(object)){

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

        ##in accordance with Auclair et al., 2003, p. 488
        ##but here we have no t1 ... this needs to be calculated
        ##set variables
        t1 <- temp_TIMESINCEIRR
        t2 <- temp_TIMESINCEIRR + temp_IRR_TIME

        if(t_star == "half"){
          ##calculate t_star
          t_star <- t1 + (t2 - t1)/2

        }else if (t_star == "end"){
          ##set t_start as t_1 (so after the end of irradiation)
          t_star <- t1

        }else{
          stop("[analyse_FadingMeasurement()] Invalid value for t_star.")

        }

        return(t_star)

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

    ##calculate Lx/Tx or ... just Lx, it depends on the patttern ... set IRR_TIME
    if(length(structure) == 2){
      Lx_data <- object_clean[seq(1,length(object_clean), by = 2)]
      Tx_data <- object_clean[seq(2,length(object_clean), by = 2)]

      ##we need only every 2nd irradiation time, the one from the Tx should be the same ... all the time
      TIMESINCEIRR <- TIMESINCEIRR[seq(1,length(TIMESINCEIRR), by =2)]


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

  }


  ##create unique identifier
  uid <- .create_UID()

  ##normalise data to prompt measurement
  tc <- min(TIMESINCEIRR)[1]

  ##normalise
  if(length(structure) == 2 | is.null(object)){
    LxTx_NORM <-
      LxTx_table[["LxTx"]] / LxTx_table[["LxTx"]][which(TIMESINCEIRR== tc)[1]]
    LxTx_NORM.ERROR <-
      LxTx_table[["LxTx.Error"]] / LxTx_table[["LxTx"]][which(TIMESINCEIRR == tc)[1]]


  }else{
    LxTx_NORM <-
      LxTx_table[["Net_LnLx"]] / LxTx_table[["Net_LnLx"]][which(TIMESINCEIRR== tc)[1]]
    LxTx_NORM.ERROR <-
       LxTx_table[["Net_LnLx.Error"]] / LxTx_table[["Net_LnLx"]][which(TIMESINCEIRR == tc)[1]]

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
      LxTx_NORM.ERROR = LxTx_NORM.ERROR,
      UID = uid
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
    stats::lm(y~x, data = data.frame(
      x = MC_matrix[,1],
      y = MC_matrix[,x]))$coefficients


  }, FUN.VALUE = vector("numeric", length = 2))


  ##calculate g-values from matrix
  g_value.MC <- abs(fit_matrix[2, ]) * 1 / fit_matrix[1, ] * 100

  ##calculate rho prime (Kars et al. 2008; proposed by Georgina King)

  ##s value after Huntley (2006) J. Phys. D.
  Hs <- 3e15

  ##sample for monte carlo runs
  MC_matrix_rhop <-  matrix(rnorm(
    n = n.MC * nrow(LxTx_table),
    mean = LxTx_table[["LxTx_NORM"]],
    sd = LxTx_table[["LxTx_NORM.ERROR"]]
  ), ncol = n.MC)

  ## calculate rho prime for all MC samples
  fit_vector_rhop <- apply(MC_matrix_rhop, MARGIN = 2, FUN = function(x) {
    tryCatch({
      coef(minpack.lm::nlsLM(x ~ c * exp(-rhop * (log(1.8 * Hs * LxTx_table$TIMESINCEIRR))^3),
                             start = list(c = x[1], rhop = 10^-5.5)))[["rhop"]]
    },
    error = function(e) {
      return(NA)
    })
  })

  ## discard all NA values produced in MC runs
  fit_vector_rhop <- fit_vector_rhop[!is.na(fit_vector_rhop)]

  ## calculate mean and standard deviation of rho prime (in log10 space)
  rhoPrime <- data.frame(
    MEAN = mean(fit_vector_rhop),
    SD = sd(fit_vector_rhop),
    Q_0.025 = quantile(x = fit_vector_rhop, probs = 0.025),
    Q_0.16 = quantile(x = fit_vector_rhop, probs = 0.16),
    Q_0.84 = quantile(x = fit_vector_rhop, probs = 0.84),
    Q_0.975 = quantile(x = fit_vector_rhop, probs = 0.975),
    row.names = NULL
  )

  ##for plotting
  fit <-
    stats::lm(y ~ x,
              data = data.frame(x = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                y = LxTx_table[["LxTx_NORM"]]))


  fit_power <- stats::lm(y ~ I(x^3) + I(x^2) + I(x) ,
                         data = data.frame(x = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                           y = LxTx_table[["LxTx_NORM"]]))


  ##for predicting
  fit_predict <-
    stats::lm(y ~ x, data = data.frame(y = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                       x = LxTx_table[["LxTx_NORM"]]))

  ##calculate final g_value
  ##the 2nd term corrects for the (potential) offset from one
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

  ##normalise the g-value to 2-days using the equation provided by Sebastien Huot via e-mail
  ##this means the data is extended
  k0 <- g_value[,c("FIT", "SD")] / 100 / log(10)
  k1 <- k0 / (1 - k0 * log(172800/tc))
  g_value_2days <-  100 * k1 * log(10)
  names(g_value_2days) <- c("G_VALUE_2DAYS", "G_VALUE_2DAYS.ERROR")

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
    if (!plot.single[1]) {
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


    if (!is.null(object)) {
      if (length(structure) == 2) {

        if (is(plot.single, "logical") ||
            (is(plot.single, "numeric") & 1 %in% plot.single)) {
          plot_RLum(
            set_RLum(class = "RLum.Analysis", records = object_clean[seq(1, length(object_clean), by = 2)]),
            combine = TRUE,
            col = c(col[1:5], rep(
              rgb(0, 0, 0, 0.3), length(TIMESINCEIRR) - 5
            )),
            plot.single = TRUE,
            legend.text = c(paste(irradiation_times.unique, "s"), "others"),
            legend.col = c(col[1:length(irradiation_times.unique)], rgb(0, 0, 0, 0.3)),
            xlab = plot_settings$xlab,
            log = plot_settings$log,
            legend.pos = "outside",
            main = expression(paste(L[x], " - curves")),
            mtext = plot_settings$mtext
          )

          ##add integration limits
          abline(
            v = range(signal.integral) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
            lty = 2,
            col = "green"
          )
          abline(
            v = range(background.integral) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
            lty = 2,
            col = "red"
          )
        }

        if (is(plot.single, "logical") ||
            (is(plot.single, "numeric") & 2 %in% plot.single)) {
          plot_RLum(
            set_RLum(class = "RLum.Analysis", records = object_clean[seq(2, length(object_clean), by = 2)]),
            combine = TRUE,
            col = c(col[1:5], rep(
              rgb(0, 0, 0, 0.3), length(TIMESINCEIRR) - 5
            )),
            plot.single = TRUE,
            legend.text = c(paste(irradiation_times.unique, "s"), "others"),
            legend.col = c(col[1:length(irradiation_times.unique)], rgb(0, 0, 0, 0.3)),
            xlab = plot_settings$xlab,
            log = plot_settings$log,
            legend.pos = "outside",
            main = expression(paste(T[x], " - curves")),
            mtext = plot_settings$mtext
          )

          if (is.null(list(...)$signal.integral.Tx)) {
            ##add integration limits
            abline(
              v = range(signal.integral) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
              lty = 2,
              col = "green"
            )
            abline(
              v = range(background.integral) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
              lty = 2,
              col = "red"
            )

          } else{
            ##add integration limits
            abline(
              v = range(list(...)$signal.integral.Tx) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
              lty = 2,
              col = "green"
            )
            abline(
              v = range(list(...)$background.integral.Tx) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
              lty = 2,
              col = "red"
            )

          }


        }

      } else{
        if (is(plot.single, "logical") ||
            (is(plot.single, "numeric") & 1 %in% plot.single)) {
          plot_RLum(
            set_RLum(class = "RLum.Analysis", records = object_clean),
            combine = TRUE,
            col = c(col[1:5], rep(
              rgb(0, 0, 0, 0.3), length(TIMESINCEIRR) - 5
            )),
            plot.single = TRUE,
            legend.text = c(paste(irradiation_times.unique, "s"), "others"),
            legend.col = c(col[1:length(irradiation_times.unique)], rgb(0, 0, 0, 0.3)),
            legend.pos = "outside",
            xlab = plot_settings$xlab,
            log = plot_settings$log,
            main = expression(paste(L[x], " - curves")),
            mtext = plot_settings$mtext
          )

          ##add integration limits
          abline(
            v = range(signal.integral) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
            lty = 2,
            col = "green"
          )
          abline(
            v = range(background.integral) * max(as.matrix(object_clean[[1]][, 1])) / nrow(as.matrix(object_clean[[1]])),
            lty = 2,
            col = "red"
          )

        }

        ##empty Tx plot
        if (is(plot.single, "logical") ||
            (is(plot.single, "numeric") & 2 %in% plot.single)) {
          plot(
            NA,
            NA,
            xlim = c(0, 1),
            ylim = c(0, 1),
            xlab = "",
            ylab = "",
            axes = FALSE
          )
          text(x = 0.5,
               y = 0.5,
               labels = expression(paste("No ", T[x], " curves detected")))

        }

      }

    }else{
      if (is(plot.single, "logical") ||
          (is(plot.single, "numeric") & 1 %in% plot.single)) {
        ##empty Lx plot
        plot(
          NA,
          NA,
          xlim = c(0, 1),
          ylim = c(0, 1),
          xlab = "",
          ylab = "",
          axes = FALSE
        )
        text(x = 0.5,
             y = 0.5,
             labels = expression(paste("No ", L[x], " curves detected")))

      }

      if (is(plot.single, "logical") ||
          (is(plot.single, "numeric") & 2 %in% plot.single)) {
        ##empty Tx plot
        plot(
          NA,
          NA,
          xlim = c(0, 1),
          ylim = c(0, 1),
          xlab = "",
          ylab = "",
          axes = FALSE
        )
        text(x = 0.5,
             y = 0.5,
             labels = expression(paste("No ", T[x], " curves detected")))


      }
    }

    ##(2) Fading plot
    if (is(plot.single, "logical") ||
        (is(plot.single, "numeric") & 3 %in% plot.single)) {
      plot(
        NA,
        NA,
        ylab = "Normalised intensity [a.u.]",
        xaxt = "n",
        xlab = "Time since irradition [s]",
        sub = expression(paste("[", log[10](t / t[c]), "]")),
        ylim = if (max(LxTx_table[["LxTx_NORM"]]) > 1.1) {
          c(0.1, max(LxTx_table[["LxTx_NORM"]]) + max(LxTx_table[["LxTx_NORM.ERROR"]]))
        } else{
          c(0.1, 1.1)
        },
        xlim = range(LxTx_table[["TIMESINCEIRR_NORM.LOG"]]),
        main = "Signal Fading"
      )

      ##add axis
      axis(side = 1,
           at = axTicks(side = 1),
           labels = suppressWarnings(format((10 ^ (axTicks(side = 1)) * tc),
                                            digits = 0,
                                            decimal.mark = "",
                                            scientific = TRUE
           )))

      mtext(
        side = 3,
        paste0(
          "g-value: ",
          round(g_value$FIT, digits = 2),
          " \u00b1 ",
          round(g_value$SD, digits = 2),
          " (%/decade) | tc = ",
          format(tc, digits = 4, scientific = TRUE)
        ),
        cex = par()$cex * 0.9
      )

      ##add curves
      x <- NA
      for (i in 1:n.MC) {
        curve(fit_matrix[2, i] * x + fit_matrix[1, i],
              col = rgb(0, 0.2, 0.4, 0.2),
              add = TRUE)

      }

      ##add master curve in red
      curve(
        fit$coefficient[2] * x + fit$coefficient[1],
        col = "red",
        add = TRUE,
        lwd = 1.5
      )

      ##add power law curve
      curve(
        x ^ 3 * fit_power$coefficient[2] + x ^ 2 * fit_power$coefficient[3] + x * fit_power$coefficient[4] + fit_power$coefficient[1],
        add = TRUE,
        col = "blue",
        lty = 2
      )

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

      ##add legend
      legend(
        "bottom",
        legend = c("fit", "fit MC", "trend"),
        col = c("red", "grey", "blue"),
        lty = c(1, 1, 2),
        bty = "n",
        horiz = TRUE
      )
    }

    if (is(plot.single, "logical") ||
        (is(plot.single, "numeric") & 4 %in% plot.single)) {
      plot(density(g_value.MC),
           main = "Density: g-values (%/decade)")
      rug(x = g_value.MC)
      abline(v = c(g_value[["Q_0.16"]], g_value[["Q_0.84"]]),
             lty = 2,
             col = "darkgreen")
      abline(v = c(g_value[["Q_0.025"]], g_value[["Q_0.975"]]),
             lty = 2,
             col = "red")
      legend(
        "topleft",
        legend = c("HPD - 68 %", "HPD - 95 %"),
        lty = 2,
        col = c("darkgreen", "red"),
        bty = "n"
      )


    }

  }

  # Terminal ------------------------------------------------------------------------------------
  if (verbose){

    cat("\n[analyse_FadingMeasurement()]\n")
    cat(paste0("\n n.MC:\t",n.MC))
    cat(paste0("\n tc:\t",format(tc, digits = 4, scientific = TRUE), " s"))
    cat("\n---------------------------------------------------")
    cat(paste0("\nT_0.5 interpolated:\t",T_0.5$T_0.5_INTERPOLATED))
    cat(paste0("\nT_0.5 predicted:\t",format(T_0.5$T_0.5_PREDICTED, digits = 2, scientific = TRUE)))
    cat(paste0("\ng-value:\t\t", round(g_value$FIT, digits = 2), " \u00b1 ", round(g_value$SD, digits = 2),
               " (%/decade)"))
    cat(paste0("\ng-value (norm. 2 days):\t", round(g_value_2days[1], digits = 2), " \u00b1 ", round(g_value_2days[2], digits = 2),
               " (%/decade)"))
    cat("\n---------------------------------------------------")
    cat(paste0("\nrho':\t\t\t", format(rhoPrime$MEAN, digits = 3), " \u00b1 ", format(rhoPrime$SD, digits = 3)))
    cat(paste0("\nlog10(rho'):\t\t", round(log10(rhoPrime$MEAN), 2), " \u00b1 ", round(rhoPrime$SD /  (rhoPrime$MEAN * log(10, base = exp(1))), 2)))
    cat("\n---------------------------------------------------")

  }

  # Return --------------------------------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      fading_results = cbind(
        g_value,
        TC = tc,
        G_VALUE_2DAYS = g_value_2days[1],
        G_VALUE_2DAYS.ERROR = g_value_2days[2],
        T_0.5,
        UID = uid
      ),
      fit = fit,
      rho_prime = rhoPrime,
      LxTx_table = LxTx_table,
      irr.times = irradiation_times
    ),
    info = list(call = sys.call())
  ))

}
