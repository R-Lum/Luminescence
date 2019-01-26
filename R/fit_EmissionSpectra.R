#'@title Emission Spectra Deconvoluation
#'
#'@description Luminescence spectra signal deconvolution on [RLum.Data.Spectrum-class] objects
#'
#'@details ##TODO add details
#'
#'##TODO add tests
#'
#'@param object [RLum.Data.Spectrum-class], [matrix] (**required**): input
#'object. Please note that an energy spectrum is expected
#'
#'@param frame [numeric] (*optional*): defines the frame to be analysed
#'
#'@param method_settings [list] (*optional*): options to control the fit method, see details
#'
#'@param verbose [logical] (*with default*): enable/disable verbose mode
#'
#'@param plot [logical] (*with default*): enable/disable plot output
#'
#'@param ... further arguments to be passed to control the plot output (e.g., `main`, `xlab`, `ylab`)
#'
#'
#'@return Returns an [RLum.Results-class] object ##TODO
#'
#'@note Beta version, not uet recommended for productive usage
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#'@seealso [RLum.Data.Spectrum-class], [RLum.Results-class], [plot_RLum], [convert_Wavelength2Energy]
#'
#'@keywords datagen
#'
#'@references ##TODO
#'
#'@examples
#'
#'##deconvolution of a TL spectrum
#'##TODO should be modified ... the bg substraction is odd, also the object conversion
#'\dontrun{
#'
#' ##load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##subtract background
#' m <- TL.Spectrum@data
#' m <- m - m[, 15]
#' m <- cbind(as.numeric(rownames(m)), m[,5])
#' m <- convert_Wavelength2Energy(m, order = TRUE)
#' m[m[,2] < 0 ,2] <- 0
#' results <- fit_EmissionSpectra(m, main = "TL spectrum")
#'
#'}
#'
#'@md
#'@export
fit_EmissionSpectra <- function(
  object,
  frame = NULL,
  method_settings = list(),
  verbose = TRUE,
  plot = TRUE,
  ...
){


  ## This function works only on a list of matricies, so what ever we do here, we have to
  ## create a list of data treat, frame controls the number of frames analysed

  ##input RLum.Data.Spectrum
  if(class(object) == "RLum.Data.Spectrum")
    object <- list(object)

  ##stop, mixed input is not allowed
  if(class(object) == "list" && length(unique(sapply(object, class))) != 1)
    stop("[fit_EmissionSpectra()] List elements of different class detected!", call. = FALSE)


  ##deal with RLum.Data.Spectrum lists
  if(class(object) == "list" && all(sapply(object, class) == "RLum.Data.Spectrum")){
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
              "[fit_EmissionSpectra()] 'frame' invalid.Allowed range min: 1 and max:",ncol(o@data)),
            call. = FALSE)

        }

      }

      ##get frame
      temp_frame <- lapply(frame, function(f) cbind(x, o@data[,f]))
      names(temp_frame) <- paste0("Frame: ", frame)
      return(temp_frame)

    })

    ##set object name
    names(temp) <- paste0("ALQ: ", 1:length(temp))

    ##unlist, now we have what we want
    object <- unlist(temp, use.names = TRUE, recursive = FALSE)
    rm(temp)

  }

  ##handle a single matrix that may have different columns
  if(class(object) == "matrix" && ncol(object) > 2){
    rownames(object) <- NULL


    ##set frame
    if(is.null(frame)){
      frame <- 1:(ncol(object) -1)

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

  ##now treat different lists, the aim is to have a list of 2-column matricies
  ##we have two types of lists,
  # Self-call -----------------------------------------------------------------------------------
  if(class(object) == "list"){

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
      fit_EmissionSpectra(
        object = object[[o]],
        frame = names(object)[o],
        method_settings = method_settings,
        mtext = mtext[[o]],
        ... = args_list

      )

    })

    ##merge output and return
    return(merge_RLum(results))

  }


  ##backstop, from here we allow only a matrix
  if(class(object) != "matrix")
    stop("[fit_EmissionSpectra()] Input not supported, please read the manual!",call. = FALSE)

  m <- object[,1:2]

  # set data.frame ------------------------------------------------------------------------------
  df <- data.frame(x = m[,1], y = m[,2])

  # Settings ------------------------------------------------------------------------------------
  ##create peak finding function ... this helps to get good start parameters
  ##https://grokbase.com/t/r/r-help/05bqza71c4/r-finding-peaks-in-a-simple-dataset-with-r
  ##author: Petr Pikal in 2004; with modifications by Sebastian Kreutzer
  .peaks <- function(x, span, size = nrow(m)) {
    z <- stats::embed(x, span)
    s <- span %/% 2
    ##the part `ceiling(...)` scales the entire algorithm
    v <- max.col(z, ties.method = "first") == ceiling(10^(3 - log10(nrow(m)))) + s
    result <- c(rep(FALSE, s), v)
    result <- result[1:(length(result) - s)]
    which(result)
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

  #set method parameters
  method_settings <- modifyList(x = list(
    max.runs = 1000,
    trace = FALSE

  ), val = method_settings)


  ##initialse objects
  success_counter <- 0
  run <- 0
  fit <- list()
  mu <- NA
  C <- NA
  sigma <- NA

  ##output
  if(verbose) cat("\n[fit_EmissionSpectra()]\n\n")

  ## ++++++++++++++++++++++++++++ (LOOP) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
  ##run iterations
  while(success_counter < 100 && run < method_settings$max.runs){

    ##try to find start parameters
    ##identify peaks
    id_peaks <- .peaks(m[,2], sample(25:(nrow(m) - 1), 1))

      ##make sure that we do not end up in an endless loop
      if(length(id_peaks) == 0){
        if (verbose) cat("\r>> Searching components in frame", frame, "... [-]")
        run <- run + 1
        next()
      }

    ##set start parameters for fitting
    ##TODO: maybe we allow manual start parameters, but better would be a semi-automated solution
    mu <- m[id_peaks,1]
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
      trace = method_settings$trace,
      lower = rep(0, 3 * length(mu)),
      upper = c(
        rep(1000, length(mu)),
        rep(max(df[[1]]), length(mu)),
        rep(max(df[[2]]), length(mu))),
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), silent = TRUE)

    ##handle output
    if (class(fit_try) != "try-error") {
      success_counter <- success_counter + 1
      fit[[success_counter]] <- fit_try
      if (verbose) cat("\r>> Searching components in frame", frame, "... [/]")
    } else{
      if (verbose) cat("\r>> Searching components in frame", frame, "... [\\]")

    }

    ##update run counter
    run <- run + 1

  }
  ## ++++++++++++++++++++++++++++ (LOOP) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++##

  ## handle the output
  if(length(fit) == 0){
    if (verbose) cat("\r>> Searching components in frame", frame, "... [FAILED]")

  }else{
    if (verbose) cat("\r>> Searching components in frame", frame, "... [DONE]")

  }

  ##Extract best fit values
  ##TODO ... should be improved, its works, but maybe there are better solutions
  if (length(fit) != 0) {
    ##obtain the fit with the best fit
    best_fit <- vapply(fit, function(x) sum(residuals(x) ^ 2), numeric(1))
    fit <- fit[[which.min(best_fit)]]

  }else{
    fit <- NA

  }

  # Extract values ------------------------------------------------------------------------------
  ##extract components
  if(!is.na(fit) && class(fit) == "nls"){
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
  if(verbose && !is.na(m_coef)){
    cat(paste0("\n>> Fitting results (",length(mu), " component model):\n"))
    cat("-------------------------------------------------------------------------\n")
    print(m_coef)
    cat("-------------------------------------------------------------------------")
    cat(paste0("\nSE: standard error | SSR: ",min(best_fit)))
    cat("\n(use output in $fit for a more detailed analysis)\n\n")

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
      legend.text = c("sum", paste0("c",1:length(mu),": ", round(mu,2), " keV"))

    ), val = list(...))


    if(!is.na(fit) && class(fit) != "try-error"){
    ##make sure that the screen closes if something is wrong
    on.exit(close.screen(all.screens = TRUE))

    ##set split screen settings
    split.screen(rbind(
      c(0.1,1,0.32, 0.98),
      c(0.1,1,0.1, 0.315)))

    ##SCREEN 1 ========================
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
      log = plot_settings$log
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

    ##SCREEN 2 ========================
    screen(2)
    par(mar = c(4, 4, 0, 4))
    plot(
      x = df[[1]],
      y = residuals(fit),
      xlab = plot_settings$xlab,
      type = "b",
      pch = 20,
      yaxt = "n",
      xlim = plot_settings$xlim,
      ylab = "\u03B5",
      col = rgb(0,0,0,.6),
      log = ifelse(grepl(plot_settings$log[1], pattern = "x", fixed = TRUE), "x", "")
    )

    ##add wavelength axis
    h <- 4.135667662e-15 #eV * s
    c <- 299792458e+09 #nm/s
    axis(
      side = 1,
      labels = paste("(",round((h * c) / axTicks(side = 3), 0), "nm)"),
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
                fit = fit),
    info = list(call = sys.call())
  )

  ##return
  return(results)

}
