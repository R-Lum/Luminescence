#'@title Luminescence Emission Spectra Deconvolution
#'
#'@description Luminescence spectra deconvolution on [RLum.Data.Spectrum-class] and [matrix] objects
#'on an **energy scale**. The function is optimised for emission spectra typically
#'obtained in the context of TL, OSL and RF measurements detected between 200 and 1000 nm.
#'The function is not prepared to deconvolve TL curves (counts against temperature;
#'no wavelength scale). If you are interested in such analysis, please check, e.g.,
#'the package `'tgcd'`.
#'
#'@details
#'
#'**Used equation**
#'
#'The emission spectra (on an energy scale) can be best described as the sum of multiple
#'Gaussian components:
#'
#''\deqn{
#'y = \Sigma  Ci * 1/(\sigma_{i} * \sqrt(2 * \pi)) * exp(-1/2 * ((x - \mu_{i})/\sigma_{i}))^2)
#'}
#'
#'with the parameters \eqn{\sigma} (peak width) and \eqn{\mu} (peak centre) and \eqn{C}
#'(scaling factor).
#'
#'
#'**Start parameter estimation and fitting algorithm**
#'
#'The spectrum deconvolution consists of the following steps:
#'
#'1. Peak finding \cr
#'2. Start parameter estimation \cr
#'3. Fitting via [minpack.lm::nls.lm]\cr
#'
#'The peak finding is realised by an approach (re-)suggested by Petr Pikal via the R-help
#'mailing list (`https://stat.ethz.ch/pipermail/r-help/2005-November/thread.html`) in November 2005.
#'This goes back to even earlier discussion in 2001 based on Prof Brian Ripley's idea.
#'It smartly uses the functions [stats::embed] and [max.col] to identify peaks positions.
#'For the use in this context, the algorithm has been further modified to scale on the
#'input data resolution (cf. source code).\cr
#'
#'The start parameter estimation uses random sampling from a range of meaningful parameters
#'and repeats the fitting until 1000 successful fits have been produced or the set `max.runs` value
#'is exceeded.
#'
#'Currently the best fit is the one with the lowest number for squared residuals, but
#'other parameters are returned as well. If a series of curves needs to be analysed,
#'it is recommended to make few trial runs, then fix the number of components and
#'run  at least 10,000 iterations (parameter `method_control = list(max.runs = 10000)`).
#'
#'**Supported `method_control` settings**
#'
#'\tabular{llll}{
#' **Parameter** \tab **Type** \tab **Default** \tab **Description**\cr
#' `max.runs` \tab [integer] \tab `10000` \tab maximum allowed search iterations, if exceed
#' the searching stops \cr
#' `graining` \tab [numeric] \tab `15` \tab gives control over how coarse or fine the spectrum is split into search intervals for the peak finding algorithm \cr
#' `trace` \tab [logical] \tab `FALSE` \tab enables/disables the tracing of the minimisation routine
#'}
#'
#'@param object [RLum.Data.Spectrum-class], [matrix] (**required**): input
#'object. Please note that an energy spectrum is expected
#'
#'@param frame [numeric] (*optional*): defines the frame to be analysed
#'
#'@param start_parameters [numeric] (*optional*): allows to provide own start parameters for a
#'semi-automated procedure. Parameters need to be provided in eV. Every value provided replaces a
#'value from the automated peak finding algorithm (in ascending order).
#'
#'@param n_components [numeric] (*optional*): allows a number of the aimed number of
#'components. However, it defines rather a maximum than than a minimum. Can be combined with
#'other parameters.
#'
#'@param input_scale [character] (*optional*): defines whether your x-values define wavelength or
#'energy values. For the analysis an energy scale is expected, allowed values are `'wavelength'` and
#'`'energy'`. If nothing (`NULL`) is defined, the function tries to understand the input
#'automatically.
#'
#'@param sub_negative [numeric] (*with default*): substitute negative values in the input object
#'by the number provided here (default: `0`). Can be set to `NULL`, i.e. negative values are kept.
#'
#'@param method_control [list] (*optional*): options to control the fit method, see details
#'
#'@param verbose [logical] (*with default*): enable/disable verbose mode
#'
#'@param plot [logical] (*with default*): enable/disable plot output
#'
#'@param ... further arguments to be passed to control the plot output
#'(supported: `main`, `xlab`, `ylab`, `xlim`, `ylim`, `log`, `mtext`, `legend` (`TRUE` or `FALSE`),
#'`legend.text`, `legend.pos`)
#'
#'@return
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$data` \tab `matrix` \tab the final fit matrix \cr
#'  `$fit` \tab `nls` \tab the fit object returned by [minpack.lm::nls.lm] \cr
#'  `$fit_info` \tab `list` \tab a few additional parameters that can be used to asses the quality
#'  of the fit
#' }
#'
#'
#'**slot:** **`@info`**
#'
#' The original function call
#'
#' ---------------------------------\cr
#' `[ TERMINAL OUTPUT ]`   \cr
#' ---------------------------------\cr
#'
#' The terminal output provides brief information on the
#' deconvolution process and the obtained results.
#' Terminal output is only shown of the argument `verbose = TRUE`.
#'
#' ---------------------------\cr
#' `[ PLOT OUTPUT ]`      \cr
#' ---------------------------\cr
#'
#' The function returns a plot showing the raw signal with the
#' detected components. If the fitting failed, a basic plot is returned
#' showing the raw data and indicating the peaks detected for the start
#' parameter estimation. The grey band in the residual plot indicates the
#' 10% deviation from 0 (means no residual).
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@seealso [RLum.Data.Spectrum-class], [RLum.Results-class], [plot_RLum],
#'[convert_Wavelength2Energy], [minpack.lm::nls.lm]
#'
#'@keywords datagen
#'
#'@examples
#'
#'##load example data
#'data(ExampleData.XSYG, envir = environment())
#'
#'##subtract background
#'TL.Spectrum@@data <- TL.Spectrum@@data[] - TL.Spectrum@@data[,15]
#'
#'results <- fit_EmissionSpectra(
#'  object = TL.Spectrum,
#'  frame = 5,
#'  method_control = list(max.runs = 10)
#' )
#'
#' ##deconvolution of a TL spectrum
#' \dontrun{
#'
#' ##load example data
#'
#' ##replace 0 values
#' results <- fit_EmissionSpectra(
#'  object = TL.Spectrum,
#'  frame = 5, main = "TL spectrum"
#' )
#'
#'}
#'
#'@md
#'@export
fit_EmissionSpectra <- function(
  object,
  frame = NULL,
  n_components = NULL,
  start_parameters = NULL,
  sub_negative = 0,
  input_scale = NULL,
  method_control = list(),
  verbose = TRUE,
  plot = TRUE,
  ...
){


  ##TODO: Find a way to get a significant number of components
  ## This function works only on a list of matrices, so what ever we do here, we have to
  ## create a list of data treat, frame controls the number of frames analysed

  ##input RLum.Data.Spectrum ... make list either way
  if(class(object)[1] == "RLum.Data.Spectrum")
    object <- list(object)

  ##stop, mixed input is not allowed
  if(class(object)[1] == "list" && length(unique(sapply(object, class))) != 1)
     stop("[fit_EmissionSpectra()] List elements of different class detected!", call. = FALSE)

  ##deal with RLum.Data.Spectrum lists
  if(class(object)[1] == "list" && all(sapply(object, class) == "RLum.Data.Spectrum")){
    temp <- lapply(object, function(o){
      ##get x-axis
      x <- as.numeric(rownames(o@data))
      rownames(o@data) <- NULL

      ##set frame
      if(is.null(frame)){
        frame <- 1:ncol(o@data)

      }else{
        if(max(frame) > ncol(o@data)|| min(frame) < 1){
          stop(
            paste0(
              "[fit_EmissionSpectra()] 'frame' invalid. Allowed range min: 1 and max: ",ncol(o@data)),
            call. = FALSE)

        }

      }

      ##get frame
      temp_frame <- lapply(frame, function(f) cbind(x, o@data[,f]))
      names(temp_frame) <- paste0("Frame: ", frame)
      return(temp_frame)

    })

    ##set object name
    names(temp) <- paste0("ALQ: ", 1:length(temp), " ")

    ##unlist, now we have what we want
    object <- unlist(temp, use.names = TRUE, recursive = FALSE)
    names(object) <- gsub(" .", names(object), replacement = " | ", fixed = TRUE)
    rm(temp)

  }

  ##handle a single matrix that may have different columns
  if(class(object)[1] == "matrix" && ncol(object) > 2){
    rownames(object) <- NULL

    ##set frame
    if(is.null(frame)){
      frame <- 1:(ncol(object) - 1)

    }else{
      if(max(frame) > (ncol(object)-1) || min(frame) < 1){
        stop(
          paste0(
            "[fit_EmissionSpectra()] 'frame' invalid. Allowed range min: 1 and max: ", ncol(object)-1),"!",
          call. = FALSE)

      }

    }

    temp <- lapply(frame +1 , function(x) cbind(object[,1],object[,x]))
    names(temp) <- paste0("Frame: ",frame)
    object <- temp
    rm(temp)
  }

  ##now treat different lists, the aim is to have a list of 2-column matrices
  ##we have two types of lists,
  # Self-call -----------------------------------------------------------------------------------
  if(class(object)[1] == "list"){
    ##get argument list
    args_list <- list(...)

    ##recycle arguments
    if(!"mtext" %in% names(args_list)){
      mtext <- names(object)

    }else{
      mtext <- as.list(rep(args_list$mtext, length(object)))
      args_list$mtext <- NULL

    }

    ##run over the list
    results <- lapply(1:length(object), function(o){
      do.call(fit_EmissionSpectra, args = c(
        list(
          object = object[[o]],
          start_parameters = start_parameters,
          n_components = n_components,
          sub_negative = sub_negative,
          method_control = method_control,
          frame = frame,
          mtext = mtext[[o]]),
        verbose = verbose,
        plot = plot,
        args_list)
      )

    })

    ##merge output and return
    return(merge_RLum(results))

  }


  # Start main core -----------------------------------------------------------------------------
  ##backstop, from here we allow only a matrix
  if(class(object)[1] != "matrix")
    stop("[fit_EmissionSpectra()] Input not supported, please read the manual!",call. = FALSE)

  ##extract matrix for everything below
  m <- object[,1:2]

  ##replace all negative values
  if(!is.null(sub_negative))
    m[m[,2] < 0,2] <- sub_negative

  ##output
  if(verbose){
    cat("\n[fit_EmissionSpectra()]\n")
    cat("\n>> Treating dataset >>",frame,"<<\n")
  }

  ##check the scale
  if(is.null(input_scale)){
    ##values above 30 are unlikely, means its likely that we have a wavelength scale
    if(max(m[,1]) > 30){
      if(verbose) cat(">> Wavelength scale detected ...\n")
      m <- convert_Wavelength2Energy(m, order = TRUE)
      if(verbose) cat(">> Wavelength to energy scale conversion ... \t[OK]\n")

    }

  }else if(input_scale == "wavelength"){
    m <- convert_Wavelength2Energy(m, order = TRUE)
    if(verbose) cat(">> Wavelength to energy scale conversion ... \t[OK]\n")

  }

  # set data.frame ------------------------------------------------------------------------------
  df <- data.frame(x = m[,1], y = m[,2]/max(m[,2])) ##normalise values, it is just easier

  # Settings ------------------------------------------------------------------------------------
  ##create peak finding function ... this helps to get good start parameters
  ##https://grokbase.com/t/r/r-help/05bqza71c4/r-finding-peaks-in-a-simple-dataset-with-r
  ##https://stat.ethz.ch/pipermail/r-help/2005-November/thread.html
  ##author: Petr Pikal in 2004; with modifications by Sebastian Kreutzer
  .peaks <- function(x, span, size = nrow(m)) {
    z <- stats::embed(x, span)
    s <- span %/% sample(1:4, size = 1)
    ##the part `ceiling(...)` scales the entire algorithm
    v <- max.col(z, ties.method = "first") == ceiling(10^(3 - log10(nrow(m)))) + s
    result <- c(rep(FALSE, s), v)
    which(result[1:(length(result) - s)])

  }

  ##set fit function
  x <- 0 #cheat R check routine
  fit_forumla <- function(n.components){
    sigma <- paste0("sigma.",1:n.components)
    mu <- paste0("mu.",1:n.components)
    C <- paste0("C.",1:n.components)
    as.formula(
      paste0("y ~ ",
             paste(C," * 1/(",sigma," * sqrt(2 * pi)) * exp(-0.5 * ((x - ",mu,")/",sigma,")^2)",
                   collapse = " + ")))

  }

# Fitting -------------------------------------------------------------------------------------
  ## method_control --------
  method_control <- modifyList(x = list(
    max.runs = 10000,
    graining = 15,
    trace = FALSE
  ), val = method_control)

  ##initialise objects
  success_counter <- 0
  run <- 0
  fit <- list()
  mu <- C <- sigma <- NA
  R2 <- SSR <- SST <- R2adj <- NA

  ## (WHILE LOOP START) -------
  ##run iterations
  while(success_counter < 1000 && run < method_control$max.runs){
    ##try to find start parameters
    ##check graining parameter
    if(method_control$graining >= nrow(m))
      stop(paste0(
        "[fit_EmissionSpectra()] method_control$graining cannot be larger than available channels (",
        nrow(m) ,")!"),
           call. = FALSE)

    ##identify peaks
    id_peaks <- .peaks(m[,2], sample(method_control$graining[1]:(nrow(m) - 1), 1))

      ##make sure that we do not end up in an endless loop
      if(length(id_peaks) == 0){
        if (verbose) cat("\r>> Searching components ... \t\t\t[-]")
        run <- run + 1
        next()
      }

    ## set start parameters for fitting --------
    mu <- m[id_peaks,1]

    if(!is.null(start_parameters))
      mu <- c(sort(start_parameters), mu[-c(1:length(start_parameters))])

    ## limit the number of components
    if(!is.null(n_components[1]))
      mu <- mu[seq(1,length(mu), length.out = n_components[1])]

    sigma <- rep(sample(0.01:10,1),length(mu))
    C <- rep(max(df[[2]])/2, length(mu))

    names(mu) <- paste0("mu.", 1:length(mu))
    names(sigma) <- paste0("sigma.", 1:length(mu))
    names(C) <- paste0("C.", 1:length(mu))

    ##run fitting using the Levenberg-Marquardt algorithm
    fit_try <- try(minpack.lm::nlsLM(
      formula = fit_forumla(n.components = length(mu)),
      data = df,
      start = c(sigma, mu, C),
      trace = method_control$trace,
      lower = rep(0, 3 * length(mu)),
      upper = c(
        rep(1000, length(mu)),
        rep(max(df[[1]]), length(mu)),
        rep(max(df[[2]]), length(mu))),
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), silent = TRUE)

    ##handle output
    if (class(fit_try)[1] != "try-error") {
      success_counter <- success_counter + 1
      fit[[success_counter]] <- fit_try
      if (verbose) cat("\r>> Searching components ... \t\t\t[/]")
    } else{
      if (verbose) cat("\r>> Searching components ... \t\t\t[\\]")

    }

    ##update run counter
    run <- run + 1

  }


  ## handle the output
  if(length(fit) == 0){
    if (verbose) cat("\r>> Searching components ... \t\t\t[FAILED]")

  }else{
    if (verbose) cat("\r>> Searching components ... \t\t\t[OK]")

  }

  ##Extract best fit values
  ##TODO ... should be improved, its works, but maybe there are better solutions
  if (length(fit) != 0) {
    ##obtain the fit with the best fit
    best_fit <- vapply(fit, function(x) sum(residuals(x) ^ 2), numeric(1))
    fit <- fit[[which.min(best_fit)]]

    ## more parameters
    SSR <- min(best_fit)
    SST <- sum((df[[2]] - mean(df[[2]]))^2)
    R2 <- 1 - SSR/SST
    R2adj <- ((1 - R2) * (nrow(df) - 1)) /
      (nrow(df) - length(coef(fit)) - 1)


  }else{
    fit <- NA

  }

  # Extract values ------------------------------------------------------------------------------
  ##extract components
  if(!is.na(fit[1]) && is(fit, "nls")){
    ##extract values we need only
    m_coef <- summary(fit)$coefficients
    m_coef <- matrix(
      data = c(
        as.numeric(m_coef[grepl(pattern = "mu", x = rownames(m_coef), fixed = TRUE),1:2]),
        as.numeric(m_coef[grepl(pattern = "sigma", x = rownames(m_coef), fixed = TRUE),1:2]),
        as.numeric(m_coef[grepl(pattern = "C", x = rownames(m_coef), fixed = TRUE),1:2])
      ),
      ncol = 6
    )

    ##set colnames
    colnames(m_coef) <- c("mu", "SE(mu)", "sigma", "SE(sigma)", "C", "SE(C)")

    ##order by sigma
    m_coef <- m_coef[order(m_coef[,1]),, drop = FALSE]

    ##extract single values, we need this later
    mu <- m_coef[,"mu"]
    sigma <- m_coef[,"sigma"]
    C <- m_coef[,"C"]

  }else{
    m_coef <- NA

  }

  # Terminal output -----------------------------------------------------------------------------
  if(verbose && !is.na(m_coef[1])){
    cat(paste0("\n\n>> Fitting results (",length(mu), " component model):\n"))
    cat("-------------------------------------------------------------------------\n")
    print(m_coef)
    cat("-------------------------------------------------------------------------")
    cat(paste0("\nSE: standard error | SSR: ", format(min(best_fit), scientific=TRUE, digits = 4),
               "| R^2: ", round(R2,3), " | R^2_adj: ", round(R2adj,4)))
    cat("\n(use the output in $fit for a more detailed analysis)\n\n")

  }

  # Plotting ------------------------------------------------------------------------------------
  if(plot){
    ##get colour values
    col <- get("col", pos = .LuminescenceEnv)[-1]

    ##plot settings
    plot_settings <- modifyList(x = list(
      xlab = "Energy [eV]",
      ylab = "Luminescence [a.u.]",
      main = "Emission Spectrum Deconvolution",
      xlim = range(df[[1]]),
      ylim = range(df[[2]]),
      log = "",
      mtext = "",
      legend = TRUE,
      legend.pos = "topright",
      legend.text = c("sum", paste0("c",1:length(mu),": ", round(mu,2), " eV"))

    ), val = list(...))

    if(!is.na(fit[1]) && class(fit)[1] != "try-error"){
    ##make sure that the screen closes if something is wrong
    on.exit(close.screen(n = c(1,2)))

    ##set split screen settings
    split.screen(rbind(
      c(0.1,1,0.32, 0.98),
      c(0.1,1,0.1, 0.315)))

    ##SCREEN 1 ----------
    screen(1)
    par(mar = c(0, 4, 3, 4))
    plot(
      df,
      pch = 20,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      xlim = plot_settings$xlim,
      ylim = plot_settings$ylim,
      main = plot_settings$main,
      col = rgb(0, 0, 0, .6),
      xaxt = "n",
      yaxt = "n",
      log = plot_settings$log
    )

    ## add axis normalised
    axis(side = 2,
         at = axTicks(side = 2),
         labels = c(axTicks(2)))

    ## add axis with real count vales
    axis(
      side = 2,
      axTicks(side = 2)[-1],
      labels = format(
        max(m[, 2]) * axTicks(side = 2)[-1],
        digit = 1,
        scientific = TRUE
      ),
      line = 0.8,
      cex.axis = 0.7,
      tick = FALSE
    )

    ##plot sum curve
    lines(x = df[[1]], y = predict(fit), col = col[1], lwd = 1.5)

    ##add mtext
    mtext(side = 3, text = plot_settings$mtext)

    ##add components
    for(i in 1:length(mu)){
      curve(
         (C[i] * 1 / (sigma[i] * sqrt(2 * pi)) * exp(-0.5 * ((x - mu[i])/sigma[i])^2)),
         add = TRUE,
         col = col[i + 1]
       )

    }

    ##add legend
    if(plot_settings$legend){
      legend(
        plot_settings$legend.pos,
        legend = plot_settings$legend.text,
        lwd = 1,
        col = col[1:(length(mu) + 2)],
        bty = "n"
      )
    }

    ## SCREEN 2 -----
    screen(2)
    par(mar = c(4, 4, 0, 4))
    plot(NA, NA,
      ylim = range(residuals(fit)),
      xlab = plot_settings$xlab,
      type = "b",
      pch = 20,
      yaxt = "n",
      xlim = plot_settings$xlim,
      ylab = "",
      col = rgb(0,0,0,.6),
      log = ifelse(grepl(plot_settings$log[1], pattern = "x", fixed = TRUE), "x", "")
    )

    ## add one axis label
    axis(side = 2, at = 0, labels = 0)

    ## add ± 5 line
    polygon(x = c(df[[1]], rev(df[[1]])),
            y = c(df[[2]] * 1.1 - df[[2]], rev(df[[2]] * 0.9 - df[[2]])),
            border = FALSE, col = rgb(0.8,0.8,0.8))

    ## add points
    points(df[[1]],residuals(fit), pch = 20, col = rgb(0,0,0,0.3))

    ## add zero line
    abline(h = 0, lty = 2)

    ##add wavelength axis
    h <- 4.135667662e-15 #eV * s
    c <- 299792458e+09 #nm/s
    axis(
      side = 1,
      labels = paste0("(",round((h * c) / axTicks(side = 3), 0), "nm)"),
      at = axTicks(side = 3),
      cex.axis = .7,
      line = .8,
      tick = FALSE
    )

  }else{

    ##provide control plot
    plot(df, main = "fit_EmissionSpectra() - control plot")

    ##abline
    abline(v = mu, lty = 2)

    ##add information
    mtext(side = 3, text = "(dashed lines indicate identified peaks)")

    ##add components
    for(i in 1:length(mu)){
      curve(
        (C[i] * 1 / (sigma[i] * sqrt(2 * pi)) * exp(-0.5 * ((x - mu[i])/sigma[i])^2)),
        add = TRUE,
        col = i
      )
    }

   }
  }##if plot

  # Output --------------------------------------------------------------------------------------
  results <- set_RLum(
    class = "RLum.Results",
    data = list(data = m_coef,
                fit = fit,
                fit_info = list(
                  SSR = SSR,
                  SST = SST,
                  R2 = R2,
                  R2adj = R2adj)
                ),
    info = list(call = sys.call())
  )

  ##return
  return(results)

}
