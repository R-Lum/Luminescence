#' @title Fit Isothermal Holding Data
#'
#' @description ##TODO
#'
#' @details ##TODO
#'
#' @param data [character] or [list] (**required**): input data with a table or file path
#'
#' @param ITL_model [character] (*with default*): ITL data to be fitted
#'
#' @param rhop [numeric] or [RLum.Results-class] (*with default*): rhop prime values (one for each sample) or
#' [RLum.Results-class] object produced by [analyse_FadingMeasurement]
#'
#' @param plot [logical] (*with default*): enable/disable plot
#'
#' @param ... further parameters to be passed to the plot output
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Svenja Riedesel, DTU Risø (Denmark)
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @keywords datagen
#'
#' @return
#' An [RLum.Results-class] object is returned: ##TODO
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
#' # example code ##TOD
#'
#'
#' @md
#' @export
fit_IsothermalHolding <- function(
    data,
    ITL_model = 'GOK',
    rhop,
    plot = TRUE,
    ...
) {
  .set_function_name("fit_IsothermalHolding")
  on.exit(.unset_function_name(), add = TRUE)

  ## TODOs
  ## - other functions for fitting need to be implemented
  ## - fitting is not really stable, eventually better start parameter estimation required
  ## - uncertainties are not yet considered for the fitting, because they are not
  ##   part of the input data.
  ## - documentation needs to be completed
  ## - so far non confidence intervals on the fit ... discussion needed
  ## - the rhop value has uncertainties, which are not yet considered

  .validate_class(data, c("character", "RLum.Results", "data.frame"))
  ITL_model <- .validate_args(ITL_model, c("GOK", "BTS"))

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

  ###### --- Extract data from RLum.Results for ITL fitting --- #####
  ## get unique sample names; we will use this to filter the data
  sample_id <- unique(records_ITL[["SAMPLE"]])

  ## extract data.frames for each sample with all information
  df_raw_list <- lapply(sample_id, function(x) records_ITL[records_ITL$SAMPLE == x, ])

  ###### --- Perform ITL fitting --- #####
  # Define variables --------------------------------------------------------
  kB <- 8.6173303e-05  # Boltzmann's constant

  ## get the rhop value from the fading measurement analysis if available,
  ## otherwise take the input and recycle it for the number of samples
  if (inherits(rhop, "RLum.Results") && rhop@originator == "analyse_FadingMeasurement")
    rhop <- rhop@data$rho_prime[[1]]
  else
    rhop <- rep(rhop, length.out = length(sample_id))

  # Define formulas to fit --------------------------------------------------
  ## silence note from R CMD check
  A <- Eu <- Et <- s <- NULL

  f_GOK <- 'y ~ A * exp(-rhop * log(1.8 * 3e15 * (250 + x))^3) * (1 - (1 - b) * s * exp(-E / (kB * (isoT + 273.15))) * x)^(1 / (1 - b))'
  f_BTSPre <- function(Eb) A*exp(-Eb/Eu)*exp(-s*t*exp(-(Et-Eb)/(kB*(isoT+273.15))))
  f_BTS <- 'y ~ exp(-rhop * log(1.8 * 3e15 * (250 + x))^3)*integrate(F_BTSPre,0,DeltaE)'

  ## switch the models
  FUN <- switch(
    ITL_model,
    'GOK' = f_GOK,
    'BTS' = f_BTS)

  start <- switch(
    ITL_model,
    'GOK' = list(A = 1, b = 1, E = 1, s = 1e+5),
    'BTS' = list(A = 1, Eb = 1, Eu = 1, s = 1e+5, t = 1))

  lower <- switch(
    ITL_model,
    'GOK' = c(0,0,0,0),
    'BTS' = c(0,0,0,0))

  upper <- switch(
    ITL_model,
    'GOK' = c(Inf,Inf,3,1e+20),
    'BTS' = c(Inf,Inf,3,1e+20,Inf))


  ## Fitting ----------------------------------------------------------------

  ## add the correct rhop value to the formula
  FUN <- gsub(pattern = "rhop", replacement = rhop, x = FUN, fixed = TRUE)

  ## we have a double loop situation: we have a list with n samples, and
  ## each sample has n temperature steps
  fit_list <- lapply(df_raw_list, function(s){
    ## extract temperatures
    isoT <- unique(s$TEMP)

    ## run the fitting
    tmp <- lapply(isoT, function(isoT){
      ## add the correct isoT to the formula based on the settings
      FUN <- gsub(pattern = "isoT", replacement = isoT, x = FUN, fixed = TRUE)

      ## extract data to fit
      tmp_fitdata <- s[s$TEMP == isoT,]

      ## run fitting with different start parameters
      fit <- try({
        minpack.lm::nlsLM(
          formula = as.formula(FUN),
          data =  data.frame(x = tmp_fitdata$TIME, y = tmp_fitdata$LxTx),
          start = start,
          lower = lower,
          upper = upper,
          control = list(
          maxiter = 500
          ),
          trace = FALSE)
      }, silent = TRUE)

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
      ## get unique times
      isoT <- unique(df_raw_list[[i]][["TEMP"]])

      ## open plot area
      plot(NA,NA,
        xlim = plot_settings$xlim,
        ylim = plot_settings$ylim,
        log = plot_settings$log,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        main = rep(plot_settings$main, length.out = length(sample_id))[i])

      ## add fitted curves
      for (c in seq_along(isoT)) {
        ## only plot the fitted lines if the fit had worked
        if (inherits(fit_list[[i]][[c]], "nls")) {
          y <- predict(fit_list[[i]][[c]], newdata = data.frame(x = x), interval = "confidence", se.fit = TRUE)
          lines(
            x = x,
            y = y,
            col = rep(plot_settings$col[c], length.out = length(isoT)))
        }
      }

      ## plot the points (don't use matplot because this would assume the same length
      ## for the time vector
      for (p in seq_along(isoT)) {
        df_pts <- df_raw_list[[i]][df_raw_list[[i]][["TEMP"]] == isoT[p],]
        ## data points
        points(
          x = df_pts[["TIME"]],
          y = df_pts[["LxTx"]],
          pch = plot_settings$pch,
          bg = plot_settings$col[p],
          col = plot_settings$col.border)

        ## add error bars (if NA nothing is plotted by R default)
        segments(
          x0 = df_pts[["TIME"]],
          x1 = df_pts[["TIME"]],
          y0 = df_pts[["LxTx"]] - df_pts[["LxTx_ERROR"]],
          y1 = df_pts[["LxTx"]] + df_pts[["LxTx_ERROR"]],
          col = plot_settings$col[p])
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

    ## add plot subtitle
    mtext(side = 3, plot_settings$mtext, cex = 0.7 * plot_settings$cex)

  }## plot condition

  # Return results ----------------------------------------------------------
  output <- set_RLum(
    class = "RLum.Results",
    data = list(
     fit = fit_list,
     data = records_ITL),
    info = list(
      call = sys.call())
    )

  return(output)
}
