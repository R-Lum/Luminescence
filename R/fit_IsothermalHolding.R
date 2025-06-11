#' @title Fit Isothermal Holding Data
#'
#' @description ##TODO
#'
#' @details ##TODO
#'
#' @param data [character] or [data.frame] (**required**): file path or data
#' frame with 5 columns named "SAMPLE", "TEMP", "TIME", "LxTx", "LxTx_ERROR".
#'
#' @param ITL_model [character] (*with default*): model to be fitted, either
#' `"GOK"` or `"BTS"`.
#'
#' @param rhop [numeric] or [RLum.Results-class] (*with default*): a vector
#' of rho prime values (one for each sample) or an [RLum.Results-class] object
#' produced by [analyse_FadingMeasurement]
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#' @param txtProgressBar [logical] (*with default*): enable/disable the
#' progress bar. Ignored if `verbose = FALSE`.
#'
#' @param trace [logical] (*with default*): enable/disable tracing during
#' the nls fitting ([minpack.lm::nlsLM]).
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the `plot` function.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Svenja Riedesel, DTU Risø (Denmark)\cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @keywords datagen internal
#'
#' @return
#' The function returns an [RLum.Results-class] object and an *optional* plot.
#' The object returned contains the following elements:
#'
#' \item{fit}{[list] with the fitted models}
#' \item{coefs}{[data.frame] containing the fitted coefficients for the models}
#' \item{data}{[data.frame] containing the data used in the fitting process}
#'
#' @seealso [analyse_ThermochronometryData], [analyse_FadingMeasurement]
#'
#' @references
#' Li, B., Li, S.H., The effect of band-tail states on the
#' thermal stability of the infrared stimulated luminescence
#' from K-feldspar, Journal of Luminescence 136 (2013) 5–10.
#' doi: 10.1016/j.jlumin.2012.08.043
#'
#' @examples
#' # example code ##TODO
#'
#' @noRd
fit_IsothermalHolding <- function(
    data,
    ITL_model = 'GOK',
    rhop,
    plot = TRUE,
    verbose = TRUE,
    txtProgressBar = TRUE,
    trace = FALSE,
    ...
) {
  .set_function_name("fit_IsothermalHolding")
  on.exit(.unset_function_name(), add = TRUE)

  ## TODOs
  ## - other functions for fitting need to be implemented
  ## - uncertainties are not yet considered for the fitting, because they are not
  ##   part of the input data.
  ## - documentation needs to be completed
  ## - so far non confidence intervals on the fit ... discussion needed
  ## - the rhop value has uncertainties, which are not yet considered

  .validate_class(data, c("character", "RLum.Results", "data.frame"))
  ITL_model <- .validate_args(ITL_model, c("GOK", "BTS"))
  .validate_class(rhop, c("numeric", "RLum.Results"))
  .validate_logical_scalar(plot)
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(txtProgressBar)

  if (inherits(data[1], "character")) {
    records_ITL <- .import_ThermochronometryData(file = data, output_type = "RLum.Results")@data$ITL

  } else if (inherits(data, "RLum.Results")) {
    if (data@originator != ".import_ThermochronometryData") {
      .throw_error("'data' has unsupported originator (expected: ",
                   "'.import_ThermochronometryData', found: '",
                   data@originator, "')")
    }
    records_ITL <- data@data$ITL

  } else if (inherits(data, "data.frame")) {
    records_ITL <- data
  }

  if (!all(colnames(records_ITL) %in%
           c("SAMPLE", "TEMP", "TIME", "LxTx", "LxTx_ERROR"))) {
    .throw_error("'data' has the wrong column headers, please check the manual")
  }

  ## never show the progress bar if not verbose
  if (!verbose) {
    txtProgressBar <- FALSE
  }

  ###### --- Extract data from RLum.Results for ITL fitting --- #####
  ## get unique sample names; we will use this to filter the data
  sample_id <- unique(records_ITL[["SAMPLE"]])

  ## extract data.frames for each sample with all information
  df_raw_list <- lapply(sample_id, function(x) records_ITL[records_ITL$SAMPLE == x, ])

  ## allow to control how many random values for the s parameter should be
  ## generated when fitting the BTS model
  num_s_values_bts <- list(...)$num_s_values_bts
  if (!is.null(num_s_values_bts)) {
    .validate_positive_scalar(num_s_values_bts, int = TRUE)
  } else {
    num_s_values_bts <- 1000
  }

  if (ITL_model == "GOK")
    num_s_values_bts <- 1

  ## initialise the progress bar
  if (txtProgressBar) {
    num.models <- sum(sapply(df_raw_list, function(x) length(unique(x$TEMP))))
    pb <- txtProgressBar(min = 0, max = num.models * num_s_values_bts,
                         char = "=", style = 3)
  }

  ###### --- Perform ITL fitting --- #####
  # Define variables --------------------------------------------------------
  kB <- .const$kB  # Boltzmann constant (eV/K)
  DeltaE <- 1.5 # upper limit of integration (in eV), see Li&Li (2013), p.6

  ## get the rhop value from the fading measurement analysis if available
  if (inherits(rhop, "RLum.Results") && rhop@originator == "analyse_FadingMeasurement")
    rhop <- rhop@data$rho_prime[[1]]

  ## Define formulas to fit -------------------------------------------------
  ##
  ## We define each model as a function that describes the right-hand side
  ## of the formula. This allows us to use the `$value` term in the BTS model
  ## to extract the solution of the integral, which would otherwise be
  ## incorrectly interpreted by nlsLM().

  f_GOK <- function(A, b, Et, s10, isoT, x) {
    T_K <- isoT + .const$C2K
    A * exp(-rhop * log(1.8 * 3e15 * (250 + x))^3) *
      (1 - (1 - b) * 10^s10 * exp(-Et / (kB * T_K)) * x)^(1 / (1 - b))
  }
  f_BTS <- function(A, Eu, Et, s10, isoT, x) {
    T_K <- isoT + .const$C2K
    ## call C++ calculation part; which is a lot faster than
    ## repeated calls to stats::integrate
    ## an even faster implementation would use pure C++ for everyting,
    ## however, then we would use minpack.lm::nls.lm() instead ... perhaps
    ## in the future
    f_BTS_cpp_part(
      x, A, Eu, s10, Et, kB, T_K, DeltaE, rhop)

  }

  ## switch the models
  start <- switch(
    ITL_model,
    'GOK' = list(A = 1, b  = 1, Et = 1, s10 = 5),
    'BTS' = list(A = 1, Eu = 0.1, Et = 2))

  lower <- switch(
    ITL_model,
    'GOK' = c(0, 0,   0, 0),
    'BTS' = c(1, 0.3, 1))

  upper <- switch(
    ITL_model,
    'GOK' = c(20, Inf, 3, 20),
    'BTS' = c(20, 0.5, 3))

  ## Fitting ----------------------------------------------------------------
  ## we have a double loop situation: we have a list with n samples, and
  ## each sample has n temperature steps
  num.fits <- 0
  fitted.coefs <- NULL
  fit_list <- lapply(df_raw_list, function(s){

    ## extract temperatures
    isoT <- unique(s$TEMP)

    ## run the fitting at each temperature
    tmp <- lapply(isoT, function(isoT) {

      ## extract data to fit
      tmp_fitdata <- s[s$TEMP == isoT,]
      df <- data.frame(x = tmp_fitdata$TIME,
                       y = tmp_fitdata$LxTx)

      if (ITL_model == "GOK") {
        fit <- try({
          minpack.lm::nlsLM(
              formula = y ~ f_GOK(A, b,  Et, s10, isoT, x),
              data = df,
              start = start,
              lower = lower,
              upper = upper,
              control = list(
                  maxiter = 500
              ),
              trace = trace)
        }, silent = TRUE)

        coefs <- if (!inherits(fit, "try-error")) {
                   coef(fit)
                 } else {
                   stats::setNames(rep(NA_real_, length(start)), names(start))
                 }
        fitted.coefs <<- rbind(fitted.coefs,
                               data.frame(SAMPLE = unique(s$SAMPLE),
                                          TEMP = isoT,
                                          t(coefs)))

        ## update the progress bar
        num.fits <<- num.fits + 1  # uses <<- as we are in a nested lapply()
        if (txtProgressBar) {
          setTxtProgressBar(pb, num.fits)
        }

      } else if (ITL_model == "BTS") {
        ## run fitting with different start parameters for s10
        all.s10 <- rnorm(num_s_values_bts, mean = 10, sd = 1.5)
        fit <- lapply(1:length(all.s10), function(idx) {
          s10 <- all.s10[idx]
          t <- try(minpack.lm::nlsLM(
                       formula = y ~ f_BTS(A, Eu, Et, s10, isoT, x),
                       data = df,
                       start = start,
                       lower = lower,
                       upper = upper,
                       control = list(
                           maxiter = 500
                       ), trace = FALSE),
                   silent = TRUE)

          ## update the progress bar
          num.fits <<- num.fits + 1  # uses <<- as we are in a nested lapply()
          if (txtProgressBar) {
            setTxtProgressBar(pb, num.fits)
          }

          if (inherits(t, "try-error"))
            return(NULL)
          return(t)
        })

        ## pick the one with the best fit after removing those that didn't fit
        fit <- .rm_NULL_elements(fit)
        fit <- fit[[which.min(vapply(fit, stats::deviance, numeric(1)))]]
        s10 <- environment(fit$m$predict)$env$s10
        fitted.coefs <<- rbind(fitted.coefs,
                               data.frame(SAMPLE = unique(s$SAMPLE),
                                          TEMP = isoT,
                                          t(coef(fit)), s10))
      }

      if (inherits(fit, "try-error"))
        fit <- NA

      ## return fit
      return(fit)
    })

    ## add temperature as name to list
    names(tmp) <- isoT
    return(tmp)
  })

  ## add sample names
  names(fit_list) <- sample_id

  if (txtProgressBar) {
    close(pb)
  }

# Plotting ----------------------------------------------------------------
  if (plot) {
    ## define plot settings
    plot_settings <- modifyList(
      x = list(
        xlim = range(vapply(df_raw_list, function(x) range(x$TIME), numeric(2))),
        ylim = range(vapply(df_raw_list, function(x) {
          max_LxTx <- suppressWarnings(max(x$LxTx_ERROR, na.rm = TRUE))
          range(x$LxTx, na.rm = TRUE) + if (is.infinite(max_LxTx)) 0 else max_LxTx}, numeric(2))),
        log = "x",
        xlab = "Isothermal holding time [s]",
        ylab = expression(paste("Norm. lumin. [", L[x]/T[x], "]")),
        pch = 21,
        col =  grDevices::palette("Okabe-Ito"),
        col.border = "black",
        main = sample_id,
        mtext = paste("Fitted with the", ITL_model, "model"),
        cex = 1.0,
        mfrow = if (length(unique(sample_id)) > 1) c(min(c(2, ceiling(length(sample_id)/2))),2) else NULL,
        legend = TRUE,
        legend.pos = "bottomleft",
        legend.cex = 0.6
      ),
      val = list(...)
    )

    ## get x vector for the prediction later; we use the same
    ## value for all provided data
    x <- c(1:1e+03,seq(1e+03,plot_settings$xlim[2], length.out = 10000))

    ## par settings (the check for mfrow ensures that it works in the analysis function)
    if (!is.null(plot_settings$mfrow)) {
      par_old <- par(no.readonly = TRUE)
      on.exit(par(par_old))
      par(cex = plot_settings$cex, mfrow = plot_settings$mfrow)
    }

    ## now we have to loop over the samples
    for (i in seq_along(sample_id)) {
      ## open plot area
      plot(NA,NA,
        xlim = plot_settings$xlim,
        ylim = plot_settings$ylim,
        log = plot_settings$log,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        main = rep(plot_settings$main, length.out = length(sample_id))[i])

      ## add plot subtitle
      mtext(side = 3, plot_settings$mtext, cex = 0.7 * plot_settings$cex)

      ## add fitted curves
      isoT <- unique(df_raw_list[[i]][["TEMP"]])
      for (c in seq_along(isoT)) {
        ## only plot the fitted lines if the fit had worked
        if (inherits(fit_list[[i]][[c]], "nls")) {
          y <- predict(fit_list[[i]][[c]], newdata = data.frame(x = x), interval = "confidence", se.fit = TRUE)
          lines(
            x = x,
            y = y,
            col = rep(plot_settings$col[c], length.out = length(isoT)))
        }

        ## plot the points (don't use matplot because this would assume
        ## the same length for the time vector)
        df_pts <- df_raw_list[[i]][df_raw_list[[i]][["TEMP"]] == isoT[c], ]
        points(
          x = df_pts[["TIME"]],
          y = df_pts[["LxTx"]],
          pch = plot_settings$pch,
          bg = plot_settings$col[c],
          col = plot_settings$col.border)

        ## add error bars (if NA nothing is plotted by R default)
        segments(
          x0 = df_pts[["TIME"]],
          x1 = df_pts[["TIME"]],
          y0 = df_pts[["LxTx"]] - df_pts[["LxTx_ERROR"]],
          y1 = df_pts[["LxTx"]] + df_pts[["LxTx_ERROR"]],
          col = plot_settings$col[c])
      }

      ## add legend
      if (plot_settings$legend[1]) {
       legend(
         plot_settings$legend.pos,
         legend = paste0(isoT, "\u00b0C"),
         bty = "n",
         pch = 20,
         lty = 1,
         col = plot_settings$col,
         cex = plot_settings$legend.cex)
      }
    }## plot loop

  }## plot condition

  # Return results ----------------------------------------------------------
  output <- set_RLum(
    class = "RLum.Results",
    data = list(
     fit = fit_list,
     coefs = fitted.coefs,
     data = records_ITL),
    info = list(
      call = sys.call())
    )

  return(output)
}
