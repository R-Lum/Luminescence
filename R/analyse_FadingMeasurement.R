#' @title Analyse fading measurements and returns the fading rate per decade (g-value)
#'
#' @description
#' The function analysis fading measurements and returns a fading rate including an error estimation.
#' The function is not limited to standard fading measurements, as can be seen, e.g., Huntley and
#' Lamothe (2001). Additionally, the density of recombination centres (rho') is estimated after
#' Kars et al. (2008).
#'
#' @details
#' All provided output corresponds to the \eqn{tc} value obtained by this analysis. Additionally
#' in the output object the g-value normalised to 2-days is provided. The output of this function
#' can be passed to the function [calc_FadingCorr].
#'
#' **Fitting and error estimation**
#'
#' For the fitting the function [stats::lm] is used without applying weights. For the
#' error estimation all input values, except `tc`, as the precision can be considered as sufficiently
#' high enough with regard to the underlying problem, are sampled assuming a normal distribution
#' for each value with the value as the mean and the provided uncertainty as standard deviation.
#'
#' **The options for `t_star`**
#'
#' \itemize{
#'  \item{`t_star = "half"` (the default)}{ The calculation follows the simplified
#'  version in Auclair et al. (2003), which reads
#'  \deqn{t_{star} := t_1 + (t_2 - t_1)/2}}
#'  \item{`t_star = "half_complex"`}{ This option applies the complex function shown in Auclair et al. (2003),
#'  which is derived from Aitken (1985) appendix F, equations 9 and 11.
#'  It reads \deqn{t_{star} = t0 * 10^[(t_2 log(t_2/t_0) - t_1 log(t_1/t_0) - 0.43(t_2 - t_1))/(t_2 - t_1)]}}
#'  where 0.43 = \eqn{1/ln(10)}. t0, which is an arbitrary constant, is set to 1.
#'  Please note that the equation in Auclair et al. (2003) is incorrect
#'  insofar that it reads \eqn{10exp(...)}, where the base should be 10 and not the Euler's number.
#'  Here we use the correct version (base 10).
#'  \item{`t_star = "end"`}{ This option uses the simplest possible form for `t_star` which is the time since
#'  irradiation without taking into account any addition parameter and it equals t1 in Auclair et al. (2003)}
#'  \item{`t_star = <function>`}{ This last option allows you to provide an R function object that works on t1 and
#'  gives you all possible freedom. For instance, you may want to define the following
#'  function `fun <- function(x) {x^2}`, this would square all values of t1, because internally
#'  it calls `fun(t1)`. The name of the function does not matter.}
#' }
#'
#' **Density of recombination centres**
#'
#' The density of recombination centres, expressed by the dimensionless variable rho', is estimated
#' by fitting equation 5 in Kars et al. 2008 to the data. For the fitting the function
#' [stats::nls] is used without applying weights. For the error estimation the same
#' procedure as for the g-value is applied (see above).
#'
#' **Multiple aliquots & Lx/Tx normalisation**
#'
#' Be aware that this function will always normalise all `Lx/Tx` values by the `Lx/Tx` value of the
#' prompt measurement of the first aliquot. This implicitly assumes that there are no systematic
#' inter-aliquot variations in `Lx/Tx` values. If deemed necessary to normalise the `Lx/Tx` values
#' of each aliquot by its individual prompt measurement please do so **before** running
#' [analyse_FadingMeasurement] and provide the already normalised values for `object` instead.
#'
#' @param object [RLum.Analysis-class] (**required**):
#' input object with the measurement data. Alternatively, a [list] containing [RLum.Analysis-class]
#' objects or a [data.frame] with three columns
#' (x = LxTx, y = LxTx error, z = time since irradiation) can be provided.
#' Can also be a wide table, i.e. a [data.frame] with a number of columns divisible by 3
#' and where each triplet has the before mentioned column structure.
#'
#' **Please note: The input object should solely consists of the curve needed for the data analysis, i.e.
#' only IRSL curves representing Lx (and Tx)**
#'
#' If data from multiple aliquots are provided please **see the details below** with regard to
#' Lx/Tx normalisation. **The function assumes that all your measurements are related to
#' one (comparable) sample. If you to treat independent samples, you have use this function
#' in a loop.**
#'
#' @param structure [character] (*with default*):
#' sets the structure of the measurement data. Allowed are `'Lx'` or `c('Lx','Tx')`.
#' Other input is ignored
#'
#' @param signal.integral [vector] (**required**): vector with channels for the signal integral
#' (e.g., `c(1:10)`). Not required if a `data.frame` with `LxTx` values is provided.
#'
#' @param background.integral [vector] (**required**): vector with channels for the background integral
#' (e.g., `c(90:100)`). Not required if a `data.frame` with `LxTx` values is provided.
#'
#' @param t_star [character] (*with default*):
#' method for calculating the time elapsed since irradiation if input is **not** a `data.frame`.
#' Options are: `'half'` (the default), `'half_complex`, which uses the long equation in Auclair et al. 2003, and
#' and `'end'`, which takes the time between irradiation and the measurement step.
#' Alternatively, `t_star` can be a function with one parameter which works on `t1`.
#' For more information see details. \cr
#'
#' *`t_star` has no effect if the input is a [data.frame], because this input comes
#' without irradiation times.*
#'
#' @param n.MC [integer] (*with default*):
#' number for Monte Carlo runs for the error estimation
#'
#' @param verbose [logical] (*with default*):
#' enables/disables verbose mode
#'
#' @param plot [logical] (*with default*):
#' enables/disables plot output
#'
#' @param plot.single [logical] (*with default*):
#' enables/disables single plot mode, i.e. one plot window per plot.
#' Alternatively a vector specifying the plot to be drawn, e.g.,
#' `plot.single = c(3,4)` draws only the last two plots
#'
#' @param ... (*optional*) further arguments that can be passed to internally used functions. Supported arguments:
#' `xlab`, `log`, `mtext` and `xlim` for the two first curve plots, and `ylim` for the fading
#' curve plot. For further plot customization please use the numerical output of the functions for
#' own plots.
#'
#' @return
#' An [RLum.Results-class] object is returned:
#'
#' Slot: **@data**
#'
#' \tabular{lll}{
#'  **OBJECT** \tab **TYPE** \tab **COMMENT**\cr
#' `fading_results` \tab `data.frame` \tab results of the fading measurement in a table \cr
#' `fit` \tab `lm` \tab object returned by the used linear fitting function [stats::lm]\cr
#' `rho_prime` \tab `data.frame` \tab results of rho' estimation after Kars et al. (2008) \cr
#' `LxTx_table` \tab `data.frame` \tab Lx/Tx table, if curve data had been provided \cr
#' `irr.times` \tab `integer` \tab vector with the irradiation times in seconds \cr
#' }
#'
#' Slot: **@info**
#'
#' \tabular{lll}{
#' **OBJECT** \tab `TYPE` \tab `COMMENT`\cr
#' `call` \tab `call` \tab the original function call\cr
#' }
#'
#' @section Function version: 0.1.21
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @keywords datagen
#'
#' @references
#'
#' Aitken, M.J., 1985. Thermoluminescence dating, Studies in archaeological science.
#' Academic Press, London, Orlando.
#'
#' Auclair, M., Lamothe, M., Huot, S., 2003. Measurement of anomalous fading for feldspar IRSL using
#' SAR. Radiation Measurements 37, 487-492. \doi{10.1016/S1350-4487(03)00018-0}
#'
#' Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading in K-feldspars and the measurement
#' and correction for it in optical dating. Canadian Journal of Earth Sciences 38,
#' 1093-1106. doi: `10.1139/cjes-38-7-1093`
#'
#' Kars, R.H., Wallinga, J., Cohen, K.M., 2008. A new approach towards anomalous
#' fading correction for feldspar  IRSL dating-tests on samples in field saturation.
#' Radiation Measurements 43, 786-790. \doi{10.1016/j.radmeas.2008.01.021}
#'
#' @seealso [calc_OSLLxTxRatio], [read_BIN2R], [read_XSYG2R],
#' [extract_IrradiationTimes], [calc_FadingCorr]
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
#' @md
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
    if (any(sapply(object, class) != "RLum.Analysis")) {
      ##warning
      warning(paste("[analyse_FadingMeasurement()]",
                    length(which(sapply(object, class) != "RLum.Analysis")), "non-supported records removed!"), call. = FALSE)

      ##remove unwanted stuff
      object[sapply(object, class) != "RLum.Analysis"] <- NULL

      ##check whether this is empty now
      if(length(object) == 0)
      stop(
        "[analyse_FadingMeasurement()] 'object' expects an 'RLum.Analysis' object or a
        'list' of such objects!", call. = FALSE
      )

    }

  } else if (inherits(object, "RLum.Analysis")) {
    object <- list(object)

  } else if(inherits(object,"data.frame")){
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
      "[analyse_FadingMeasurement()] 'object' needs to be of type 'RLum.Analysis' or a 'list' of such objects!", call. = FALSE
    )

  }


  # Prepare data --------------------------------------------------------------------------------
  if(!is.null(object)){

    ##support read_XSYG2R()
    if(length(unique(unlist(lapply(object, slot, name = "originator")))) == 1 &&
       unique(unlist(lapply(object, slot, name = "originator"))) == "read_XSYG2R"){

      ## extract irradiation times
      irradiation_times <- extract_IrradiationTimes(object)

      ## get TIMESINCEIRR
      TIMESINCEIRR <- unlist(lapply(irradiation_times, function(x) {
        x@data$irr.times[["TIMESINCEIRR"]][!grepl(pattern = "irradiation",
                                             x = x@data$irr.times[["STEP"]],
                                             fixed = TRUE)]
      }))

      ## get irradiation times
      irradiation_times <- unlist(lapply(irradiation_times, function(x) {
          x@data$irr.times[["IRR_TIME"]][!grepl(pattern = "irradiation",
                                           x = x@data$irr.times[["STEP"]],
                                           fixed = TRUE)]

      }))


      ##clean object by removing the irradiation step ... and yes, we drop!
      object_clean <- unlist(get_RLum(object, curveType = "measured"))

      ##support read_BIN2R()
    }else if (length(unique(unlist(lapply(object, slot, name = "originator")))) == 1 &&
              unique(unlist(lapply(object, slot, name = "originator"))) %in% c("read_BIN2R","Risoe.BINfileData2RLum.Analysis")){

      ##assign object, unlist and drop it
      object_clean <- unlist(get_RLum(object))

      ##set TIMESINCEIRR vector
      TIMESINCEIRR <- vapply(object_clean, function(o){
        o@info$TIMESINCEIRR

      }, numeric(1))


      ##check whether we have negative irradiation times, sort out such values
      if(any(TIMESINCEIRR < 0)){
        #count affected records
        rm_records <- length(which(TIMESINCEIRR < 0))

        ##now we have a problem and we first have to make sure that we understand
        ##the data structure and remove also the corresponding values
        if(all(structure == c("Lx", "Tx"))){
          rm_id <- matrix(TIMESINCEIRR, ncol = 2, byrow = TRUE)
          rm_id[apply(rm_id < 0, MARGIN = 1, any),] <- NA
          rm_id <- which(is.na(as.numeric(t(rm_id))))
          object_clean[rm_id] <- NULL
          TIMESINCEIRR <- TIMESINCEIRR[-rm_id]
          rm_records <- length(rm_id)
          rm(rm_id)

        }else{
          object_clean[TIMESINCEIRR < 0] <- NULL
          TIMESINCEIRR <- TIMESINCEIRR[!TIMESINCEIRR < 0]

        }

        ##return warning
        warning(
          paste0("[analyse_FadingMeasurement()] ",
                 rm_records, " records 'time since irradiation' value removed from the dataset!"),
                call. = FALSE)
        rm(rm_records)

      }

      ##set irradiation times
      irradiation_times <- vapply(object_clean, function(o){
        o@info$IRR_TIME

      }, numeric(1))

      ##not support
    }else{
      try(stop("[analyse_FadingMeasurement()] Unknown or unsupported originator!", call. = FALSE))
      return(NULL)

    }

    ##correct irradiation time for t_star
    ##in accordance with Auclair et al., 2003, p. 488
    ##but here we have no t1 ... this needs to be calculated
    ##set variables
    t1 <- TIMESINCEIRR
    t2 <- TIMESINCEIRR + irradiation_times

    ## set t_star ----
    if(is(t_star, "function")){
      t_star <- t_star(t1)

    } else {
      if(t_star == "half"){
        ##calculate t_star using the simplified equation in Auclair et al. (2003)
        t_star <- t1 + (t2 - t1)/2

      } else if(t_star == "half_complex"){
        # calculate t_star after the full equation Auclair et al. (2003)
        # t0 is an arbitrary constant, we are setting that to 1
        t0 <- 1
        t_star <- t0 * 10^((t2 * log10(t2/t0) - t1 * log10(t1/t0) - (t2 - t1) * log10(exp(1))) /
                     (t2 - t1))


      }else if (t_star == "end"){
        ##set t_start as t_1 (so after the end of irradiation)
        t_star <- t1

      }else{
        stop("[analyse_FadingMeasurement()] Invalid input for t_star.", call. = FALSE)

      }
    }

    ##overwrite TIMESINCEIRR
    TIMESINCEIRR <- t_star
    rm(t_star)

    # Calculation ---------------------------------------------------------------------------------

    ##calculate Lx/Tx or ... just Lx, it depends on the pattern ... set IRR_TIME
    if(length(structure) == 2){
      Lx_data <- object_clean[seq(1,length(object_clean), by = 2)]
      Tx_data <- object_clean[seq(2,length(object_clean), by = 2)]

      ##we need only every 2nd irradiation time, the one from the Tx should be the same ... all the time
      TIMESINCEIRR <- TIMESINCEIRR[seq(1,length(TIMESINCEIRR), by = 2)]


    }else if(length(structure) == 1){
      Lx_data <- object_clean
      Tx_data <- NULL

    }else{
      try(stop("[analyse_FadingMeasurement()] I have no idea what your structure means!", call. = FALSE))
      return(NULL)

    }

    ##calculate Lx/Tx table
    LxTx_table <- merge_RLum(.warningCatcher(lapply(1:length(Lx_data), function(x) {
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

    })))$LxTx.table

  }

  ##create unique identifier
  uid <- create_UID()

  ##normalise data to prompt measurement
  tc <- min(TIMESINCEIRR)[1]

  ##remove NA values in LxTx table
  if(any(is.infinite(LxTx_table[["LxTx"]]))){
    rm_id <- which(is.infinite(LxTx_table[["LxTx"]]))
    LxTx_table <- LxTx_table[-rm_id,]
    TIMESINCEIRR <- TIMESINCEIRR[-rm_id]
    rm(rm_id)

  }


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
  ##prevent that n.MC can became smaller than 2
  n.MC <- max(c(n.MC[1],2))

  ##we need to fit the data to get the g_value

  ##sample for monte carlo runs
  MC_matrix <- suppressWarnings(cbind(LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                     matrix(rnorm(
                       n = n.MC * nrow(LxTx_table),
                       mean = LxTx_table[["LxTx_NORM"]],
                       sd = abs(LxTx_table[["LxTx_NORM.ERROR"]])
                     ),
                     ncol = n.MC)))

  ##apply the fit
  fit_matrix <- vapply(X = 2:(n.MC+1), FUN = function(x){
    ##fit
    fit <- try(stats::lm(y~x, data = data.frame(
      x = MC_matrix[,1],
      y = MC_matrix[,x]))$coefficients, silent = TRUE)

    if(inherits(fit, "try-error")){
      return(c(NA_real_, NA_real_))

    }else{
      return(fit)

    }

  }, FUN.VALUE = vector("numeric", length = 2))

  ##calculate g-values from matrix
  g_value.MC <- -fit_matrix[2, ] * 1 / fit_matrix[1, ] * 100

  ##calculate rho prime (Kars et al. 2008; proposed by Georgina E. King)

  ##s value after Huntley (2006) J. Phys. D.
  Hs <- 3e15

  ##sample for monte carlo runs
  MC_matrix_rhop <-  suppressWarnings(matrix(rnorm(
    n = n.MC * nrow(LxTx_table),
    mean = LxTx_table[["LxTx_NORM"]],
    sd = abs(LxTx_table[["LxTx_NORM.ERROR"]])
  ), ncol = n.MC))


  ## calculate rho prime for all MC samples
  fit_vector_rhop <- suppressWarnings(apply(MC_matrix_rhop, MARGIN = 2, FUN = function(x) {
    tryCatch({
      coef(minpack.lm::nlsLM(x ~ c * exp(-rhop * (log(1.8 * Hs * LxTx_table$TIMESINCEIRR))^3),
                             start = list(c = x[1], rhop = 10^-5.5)))[["rhop"]]
    },
    error = function(e) {
      return(NA)
    })
  }))

  ## discard all NA values produced in MC runs
  fit_vector_rhop <- fit_vector_rhop[!is.na(fit_vector_rhop)]

  ## calculate mean and standard deviation of rho prime (in log10 space)
  rhoPrime <- data.frame(
    MEAN = mean(fit_vector_rhop),
    SD = sd(fit_vector_rhop),
    Q_0.025 = quantile(x = fit_vector_rhop, probs = 0.025, na.rm = TRUE),
    Q_0.16 = quantile(x = fit_vector_rhop, probs = 0.16, na.rm = TRUE),
    Q_0.84 = quantile(x = fit_vector_rhop, probs = 0.84, na.rm = TRUE),
    Q_0.975 = quantile(x = fit_vector_rhop, probs = 0.975, na.rm = TRUE),
    row.names = NULL
  )


  ## calc g-value -----
  fit <-
    try(stats::lm(y ~ x,
              data = data.frame(x = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                y = LxTx_table[["LxTx_NORM"]])), silent = TRUE)


  fit_power <- try(stats::lm(y ~ I(x^3) + I(x^2) + I(x) ,
                         data = data.frame(x = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                           y = LxTx_table[["LxTx_NORM"]])), silent = TRUE)


  ##for predicting
  fit_predict <-
    try(stats::lm(y ~ x, data = data.frame(y = LxTx_table[["TIMESINCEIRR_NORM.LOG"]],
                                       x = LxTx_table[["LxTx_NORM"]])), silent = TRUE)

  ##calculate final g_value
  ##the 2nd term corrects for the (potential) offset from one
  if(inherits(fit, "try-error")){
    g_value_fit <- NA

  }else{
    g_value_fit <- -fit$coefficient[2] * 1 / fit$coefficient[1] * 100

  }

  ##construct output data.frame
  g_value <- data.frame(
    FIT =  g_value_fit,
    MEAN = mean(g_value.MC),
    SD = sd(g_value.MC),
    Q_0.025 = quantile(x = g_value.MC, probs = 0.025, na.rm = TRUE),
    Q_0.16 = quantile(x = g_value.MC, probs = 0.16, na.rm = TRUE),
    Q_0.84 = quantile(x = g_value.MC, probs = 0.84, na.rm = TRUE),
    Q_0.975 = quantile(x = g_value.MC, probs = 0.975, na.rm = TRUE)
  )

  ##normalise the g-value to 2-days using the equation provided by Sébastien Huot via e-mail
  ##this means the data is extended
  ## calc g2-value days ----
  k0 <- g_value[,c("FIT", "SD")] / 100 / log(10)
  k1 <- k0 / (1 - k0 * log(172800/tc))
  g_value_2days <-  100 * k1 * log(10)
  names(g_value_2days) <- c("G_VALUE_2DAYS", "G_VALUE_2DAYS.ERROR")

  # Approximation -------------------------------------------------------------------------------
  T_0.5.interpolated <- try(approx(x = LxTx_table[["LxTx_NORM"]],
                               y = LxTx_table[["TIMESINCEIRR_NORM"]],
                               ties = mean,
                               xout = 0.5), silent = TRUE)

  if(inherits(T_0.5.interpolated, 'try-error')){
    T_0.5.predict <- NULL
    T_0.5.interpolated <- NULL

  }else{
    T_0.5.predict <- stats::predict.lm(fit_predict,newdata = data.frame(x = 0.5),
                                       interval = "predict")

  }


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
      ylim = NULL,
      xlim = NULL,
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

    ## plot Lx-curves -----
    if (!is.null(object)) {
      if (length(structure) == 2) {

        if (is(plot.single, "logical") ||
            (is(plot.single, "numeric") & 1 %in% plot.single)) {
          plot_RLum(
            set_RLum(class = "RLum.Analysis",
                     records = object_clean[seq(1, length(object_clean), by = 2)]),
            combine = TRUE,
            col = c(col[1:5], rep(
              rgb(0, 0, 0, 0.3), abs(length(TIMESINCEIRR) - 5)
            )),
            records_max = 10,
            plot.single = TRUE,
            legend.text = c(paste(round(irradiation_times.unique, 1), "s")),
            xlab = plot_settings$xlab,
            xlim = plot_settings$xlim,
            log = plot_settings$log,
            legend.pos = "outside",
            main = expression(paste(L[x], " - curves")),
            mtext = plot_settings$mtext
          )

          ##add integration limits
          abline(v = c(
            object_clean[[1]][range(signal.integral), 1],
            object_clean[[1]][range(background.integral), 1]),
            lty = c(2,2,2,2),
            col = c("green", "green", "red", "red"))

        }

        # plot Tx-curves ----
        if (is(plot.single, "logical") ||
            (is(plot.single, "numeric") & 2 %in% plot.single)) {
          plot_RLum(
            set_RLum(class = "RLum.Analysis",
                     records = object_clean[seq(2, length(object_clean), by = 2)]),
            combine = TRUE,
            records_max = 10,
            plot.single = TRUE,
            legend.text = paste(round(irradiation_times.unique, 1), "s"),
            xlab = plot_settings$xlab,
            log = plot_settings$log,
            legend.pos = "outside",
            main = expression(paste(T[x], " - curves")),
            mtext = plot_settings$mtext
          )

          if (is.null(list(...)$signal.integral.Tx)) {
            ##add integration limits
            abline(v = c(
              object_clean[[1]][range(signal.integral), 1],
              object_clean[[1]][range(background.integral), 1]),
              lty = c(2,2,2,2),
              col = c("green", "green", "red", "red"))

          } else{
            ##add integration limits
            abline(
              v = range(list(...)$signal.integral.Tx) *
                max(as.matrix(object_clean[[1]][, 1])) /
                nrow(as.matrix(object_clean[[1]])),
              lty = 2,
              col = "green"
            )
            abline(
              v = range(list(...)$background.integral.Tx) *
                max(as.matrix(object_clean[[1]][, 1])) /
                nrow(as.matrix(object_clean[[1]])),
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
            records_max = 10,
            plot.single = TRUE,
            legend.text = c(paste(round(irradiation_times.unique, 1), "s")),
            legend.pos = "outside",
            xlab = plot_settings$xlab,
            log = plot_settings$log,
            main = expression(paste(L[x], " - curves")),
            mtext = plot_settings$mtext
          )

          ##add integration limits
          abline(
            v = range(signal.integral) * max(as.matrix(object_clean[[1]][, 1])) /
              nrow(as.matrix(object_clean[[1]])),
            lty = 2,
            col = "green"
          )
          abline(
            v = range(background.integral) * max(as.matrix(object_clean[[1]][, 1])) /
              nrow(as.matrix(object_clean[[1]])),
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

    ## plot fading ----
    if (is(plot.single, "logical") ||
        (is(plot.single, "numeric") & 3 %in% plot.single)) {

      if(all(is.na(LxTx_table[["LxTx_NORM"]]))){
          shape::emptyplot()
          text(x = .5, y = .5, labels = "All NA values!")

      }else{
        plot(
          NA,
          NA,
          ylab = "Norm. intensity",
          xaxt = "n",
          xlab = "Time since irradition [s]",
          sub = expression(paste("[", log[10](t / t[c]), "]")),
          ylim = if(is.null(plot_settings$ylim)){
            if (max(LxTx_table[["LxTx_NORM"]]) > 1.1) {
              c(0.1, max(LxTx_table[["LxTx_NORM"]]) + max(LxTx_table[["LxTx_NORM.ERROR"]]))
            } else {
              c(0.1, 1.1)
            }
          } else {
            plot_settings$ylim

          },
          xlim = range(LxTx_table[["TIMESINCEIRR_NORM.LOG"]], na.rm = TRUE),
          main = "Signal Fading"
        )

        ##add axis (with an additional formatting to provide a nice log10-axis)
        ##https://stackoverflow.com/questions/6897243/labelling-logarithmic-scale-display-in-r
        x_axis_lab <- seq(0:nchar(floor(max(LxTx_table[["TIMESINCEIRR"]]))))
        x_axis_ticks <- log10((10^x_axis_lab)/tc)

        ## if we have less then two values to show, we fall back to the
        ## old data representation.
        if (length(x_axis_ticks[x_axis_ticks > 0]) > 2) {
          axis(
            side = 1,
            at = x_axis_ticks,
            labels = sapply(x_axis_lab, function(i)
              as.expression(bquote(10 ^ .(i))))
          )
          ##lower axis
          axis(
            side = 1,
            at = x_axis_ticks,
            labels = paste0("[",round(x_axis_ticks,1),"]"),
            cex.axis = 0.7,
            tick = FALSE,
            line = 0.75)

        } else {
          axis(
            side = 1,
            at = axTicks(side = 1),
            labels = suppressWarnings(format((10 ^ (axTicks(side = 1)) * tc),
                                                digits = 1,
                                                decimal.mark = "",
                                                scientific = TRUE)))

          ##lower axis
          axis(
            side = 1,
            at = axTicks(1),
            labels = axTicks(1),
            cex.axis = 0.7,
            tick = FALSE,
            line = 0.75)

        }

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

        ##add MC error polygon
        x_range <- range(LxTx_table[["TIMESINCEIRR_NORM.LOG"]], na.rm = TRUE)
          x <- seq(x_range[1], x_range[2], length.out = 50)
          m <- matrixStats::rowRanges(vapply(1:n.MC, function(i){
            fit_matrix[2, i] * x + fit_matrix[1, i]

          }, numeric(length(x))))
          polygon(
            x = c(x, rev(x)),
            y = c(m[, 2], rev(m[, 1])),
            col = rgb(0, 0, 0, 0.2),
            border = NA
          )

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
      }#end if a
    }#

    if (is(plot.single, "logical") ||
        (is(plot.single, "numeric") & 4 %in% plot.single)) {

      if(all(is.na(g_value.MC))){
        shape::emptyplot()
        text(x = .5, y = .5, labels = "All NA values!")

      }else{
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
    cat(paste0("\nlog10(rho'):\t\t", suppressWarnings(round(log10(rhoPrime$MEAN), 2)), " \u00b1 ", round(rhoPrime$SD /  (rhoPrime$MEAN * log(10, base = exp(1))), 2)))
    cat("\n---------------------------------------------------\n")

  }

  # Return --------------------------------------------------------------------------------------

  ##set data.frame
  if(all(is.na(g_value))){
    fading_results <- data.frame(
      FIT = NA,
      MEAN = NA,
      SD = NA,
      Q_0.025 = NA,
      Q_0.16 = NA,
      Q_0.84 = NA,
      Q_0.975 = NA,
      TC = NA,
      G_VALUE_2DAYS = NA,
      G_VALUE_2DAYS.ERROR = NA,
      T_0.5_INTERPOLATED = NA,
      T_0.5_PREDICTED = NA,
      T_0.5_PREDICTED.LOWER = NA,
      T_0.5_PREDICTED.UPPER =  NA,
      UID = uid,
      stringsAsFactors = FALSE
    )

  }else{
    fading_results <- data.frame(
      g_value,
      TC = tc,
      G_VALUE_2DAYS = g_value_2days[1],
      G_VALUE_2DAYS.ERROR = g_value_2days[2],
      T_0.5,
      UID = uid,
      stringsAsFactors = FALSE
    )

  }

  ##return
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      fading_results = fading_results,
      fit = fit,
      rho_prime = rhoPrime,
      LxTx_table = LxTx_table,
      irr.times = irradiation_times
    ),
    info = list(call = sys.call())
  ))

}
