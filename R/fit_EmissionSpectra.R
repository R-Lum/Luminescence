#'@title Emission Spectra Deconvoluation
#'
#'@description Luminescence spectra signal deconvolution on [RLum.Data.Spectrum-class] objects
#'
#'@details ##TODO add details
#'
#'##TODO add tests
#'
#'@param object [RLum.Data.Spectrum-class], [data.frame], [matrix] (**required**): input
#'object. Please note that an energy spectrum is expected
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
  verbose = TRUE,
  plot = TRUE,
  ...
){


  # Extract matrix ------------------------------------------------------------------------------
  ##TODO if an RLum.Data.Spectrum object is used, we should iterate, but definition over all
  ##frames

  ##From this point on, we assume that the first column of the matrix holds the energy information
  ##TODO support data.frame
  m <- object

  ##create data.frame that will be needed below
  df <- data.frame(x = m[,1], y = m[,2])

  # Settings ------------------------------------------------------------------------------------
  ##creat peak find function ... this helps to get good start parameters
  ##https://grokbase.com/t/r/r-help/05bqza71c4/r-finding-peaks-in-a-simple-dataset-with-r
  ##author: Petr Pikal in 2004 with modifications by Sebastian Kreutzer
  .peaks <- function(x, span) {
    z <- stats::embed(x, span)
    s <- span %/% 2
    v <- max.col(z, ties.method = "first") == 1 + s
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
  ##set parameters
  success_counter <- 0
  run <- 0
  fit <- list()

  ##output
  if(verbose){
    cat("\n[fit_EmissionSpectra()]\n")
    cat("\nSearching components ... ")

  }

  ##run iterations
  while(success_counter < 50 && run < 1000){

    ##try to find start parameters
    ##identify peaks
    id_peaks <- .peaks(m[, 2], sample(25:150, 1))

      ##prevent break
      if(length(id_peaks) == 0) next()

    mu <- m[id_peaks,1]
    sigma <- rep(1,length(mu))
    C <- rep(max(df[[2]])/2, length(mu))

    names(mu) <- paste0("mu.", 1:length(mu))
    names(sigma) <- paste0("sigma.", 1:length(mu))
    names(C) <- paste0("C.", 1:length(mu))

    ##run fitting using the Levenberg-Marquardt algorithm
    fit_try <- try(minpack.lm::nlsLM(
      formula = fit_forumla(n.components = length(mu)),
      data = df,
      start = c(sigma, mu, C),
      trace = FALSE,
      lower = rep(0, 3 * length(mu)),
      upper = c(
        rep(1000, length(mu)),
        rep(max(df[[1]]), length(mu)),
        rep(max(df[[2]]), length(mu))),
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), silent = TRUE)

    if (class(fit_try) != "try-error") {
      success_counter <- success_counter + 1
      fit[[success_counter]] <- fit_try
      if (verbose) cat("+")
    } else{
      if (verbose) cat("-")

    }

    ##update run counter
    run <- run + 1

  }

  ##Extract best fit values
  ##TODO ... should be improved, not very good
  if (length(fit) != 0) {
    ##obtain the fit with the best fit
    best_fit <- vapply(fit, function(x) {
      sum(residuals(x) ^ 2)

    }, numeric(1))

    fit <- fit[[which.min(best_fit)]]
  }else{
    fit <- NA

  }

  # Extract values ------------------------------------------------------------------------------
  ##extract components
  if(class(fit) == "nls"){
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
    cat("\n\nFitting results \n")
    cat("--------------------------------------------------------------------\n")
    print(m_coef)
    cat("--------------------------------------------------------------------")
    cat("\n (SE: standard error | SSR: ",min(best_fit),")")

  }

  # Plotting ------------------------------------------------------------------------------------
  if(plot){

    ##plot settings
    plot_settings <- modifyList(x = list(
      xlab = "Energy [eV]",
      ylab = "Luminescence [a.u.]",
      main = "",
      xlim = range(df[[1]]),
      ylim = range(df[[2]])

    ), val = list(...))

    if(class(fit) != "try-error"){
    ##make sure that the screen closes if something is wrong
    on.exit(close.screen(all.screens = TRUE))

    ##set split screen settings
    split.screen(rbind(
      c(0.1,1,0.32, 0.98),
      c(0.1,1,0.1, 0.32)))

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
      col = rgb(0, 0, 0, .6)
    )

    ##add wavelength axis
    h <- 4.135667662e-15 #eV * s
    c <- 299792458e+09 #nm/s
    axis(
      side = 3,
      labels = paste(round((h * c) / axTicks(side = 3), 0), "nm"),
      at = axTicks(side = 3),
      cex.axis = .8,
      line = -.9,
      tick = FALSE
    )

    ##plot sum curve
    lines(x = df[[1]], y = predict(fit), col = "red", lwd = 1.5)

    ##add components
    for(i in 1:length(mu)){
      curve(
         (C[i] * 1 / (sigma[i] * sqrt(2 * pi)) * exp(-0.5 * ((x - mu[i])/sigma[i])^2)),
         add = TRUE,
         col = i + 2
       )

    }

    ##add legend
    legend(
      "topright",
      legend = c("sum", paste0("c",1:length(mu),": ", round(mu,2), " keV")),
      lwd = 1,
      col = 2:(length(mu) + 2),
      bty = "n"
    )

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
      col = rgb(0,0,0,.6)
    )

  }else{
    plot(df)

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

  return(results)

}
