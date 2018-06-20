#' Fitting OSL Lifetime Components
#'
#' @details
#' The fitting is optimised to fit the off-time flank of POSL measurements.
#'
#' **method_control** options
#'
#' TODO
#'
#' @param object [RLum.Data.Curve-class], [data.frame] or [matrix] **(required)**:
#' Input object containing the data to be analysed. All objects can be provided also as list for an automated
#' processing
#'
#' @param signal_range [numeric] (*optional*): allows to set a channel range, by default all channels are used, e.g.
#' `signal_range = c(2,100)` considers only channels 2 to 100 and `signal_range = c(2)` considers only channels
#' from channel 2 onwards.
#'
#' @param n.components [numeric] (*optional*): Fix the number of components. If set the algorithm will try
#' to fit the number of predefined components. If nothing is set, the algorithm will try to find the best number
#' of components.
#'
#' @param method_control [list] (*optonal*): Named to allow a more fine control of the fitting process. See details
#' for allowed options.
#'
#' @param plot [logical] (*with default*): Enable/disable plot output
#'
#' @param verbose [logical] (*with default*): Enable/disable terminal feedback
#'
#' @param ... parameters passed to [plot.default] to control the plot output. Please note that not
#' all parameters are supported.
#'
#' @return
#'
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
#'  `$start_matrix` \tab `matrix` \tab the start matrix used for the fitting \cr
#'  `$fit` \tab `nls` \tab the fit object returned by [minpack.lm::nls.lm] \cr
#' }
#'
#'
#'**slot:** **`@info`**
#'
#' The original function call
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' A plot showing the original data and the fit so far possible
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France),
#' Christoph Schmidt, University of Bayreuth (Germany)
#'
#' @seealso [minpack.lm::nls.lm], [DEoptim::DEoptim]
#'
#' @references
#' Bluszcz, A., Adamiec, G., 2006. Application of differential evolution to fitting OSL decay curves.
#' Radiation Measurements 41, 886-891. doi:10.1016/j.radmeas.2006.05.016\cr
#'
#' **Further reading**
#'
#' Storn, R., Price, K., 1997. Differential Evolution – A Simple and Efficient Heuristic for Global Optimization over Continuous Spaces.
#' Journal of Global Optimization 11, 341–359.
#'
#'@examples
#'
#'print("nothing here so far")
#'
#'@md
#'@export
fit_OSLLifeTimes <- function(
  object,
  signal_range = NULL,
  n.components = NULL,
  method_control = list(),
  plot = TRUE,
  verbose = TRUE,
  ...
  ){


# Self-call -----------------------------------------------------------------------------------
if(class(object) == "list" || class(object) == "RLum.Analysis"){
  ##allow RLum.Analysis objects
  if(all(vapply(object, function(x){
    class(x) == "RLum.Analysis"}, logical(1)))){
    object <- lapply(object, function(x){x@records})
    object <- .unlist_RLum(object)

  }

  ##expand parameters
  ##n.components
  if(!is.null(n.components))
    n.components <- as.list(rep(n.components, length(object)))

  ##run function
  temp_results <- lapply(1:length(object), function(x){
      temp <- try(fit_OSLLifeTimes(
        object = object[[x]],
        signal_range = signal_range,
        n.components = n.components[[x]],
        method_control = method_control,
        plot = plot,
        verbose = verbose,
        ... = list(...)
     ), silent = FALSE)

     if(class(temp) == "try-error"){
       return(NULL)

     }else{
       return(temp)

     }

  })

  ##combine results and return
  results <- merge_RLum(temp_results)
  results@originator <- "fit_OSLLifeTimes"
  return(results)

}

# Input integrity tests ------------------------------------------------------------------
  if(class(object) == "RLum.Data.Curve"){
   if(!grepl(pattern = "POSL", x = object@recordType, fixed = TRUE))
     stop(paste0("[fit_OSLLifeTime()] recordType ",object@recordType, "not supported for input object!"),
          call. = FALSE)

    df <- as.data.frame(object@data)

  }else if(class(object) == "data.frame"){
    df <- object[,1:2]

  } else if(class(object) == "matrix"){
    df <- as.data.frame(object[,1:2])

  }else{
    try(stop(paste0("[fit_OSLLifeTime()] Class '",class(object), "' not supported as input, NULL returned!"),
             call. = FALSE))
    return(NULL)

  }

  ##rename columns for data.frame
  colnames(df) <- c("x","y")

  ##signal_range
  if(!is.null(signal_range)){
    if(class(signal_range) != "numeric")
      stop("[fit_OSLLifeTimes()] Argument 'signal_range' must by of type numeric!", call. = FALSE)

    ##check lengths
    if(length(signal_range) == 1)
      signal_range <- c(signal_range, nrow(df))

    if(length(signal_range) > 2)
      warning("[fit_OSLLifeTimes()] 'signal_range' has more than two elements, take only the first two!", call. = FALSE)

    if(signal_range[2] > nrow(df)){
      warning("[fit_OSLLifeTimes()] 'signal_range' > number of channels, reset to maximum!", call. = FALSE)
      signal_range[2] <- nrow(df)
    }

    if(signal_range[1] > signal_range[2]){
      warning("[fit_OSLLifeTimes()] 'signal_range' first element > last element, reset to default", call. = FALSE)
      signal_range <- c(1, nrow(df))
    }

    ##set range
    df_raw <- df
    df <- df[signal_range[1]:signal_range[2],]

  }

# Fitting -------------------------------------------------------------------------------------

  ##(0) CONTROL +++++++++++++++++++++++++++++++++++++++++++++++++++++
  method_control_setting <- list(
    seed = NULL,
    DEoptim.trace = FALSE,
    DEoptim.itermax = 1000

  )

  ##udpate list if the user did something
  method_control_setting <- modifyList(x = method_control_setting ,val = method_control)

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(A) SETTINGS
  ##
  ##
  ##(1) >> set fitting function for minpack.lm
  x <- 0
  fit_forumla <- function(n.components, tp){
    A <- paste0("A.",1:n.components)
    tau <- paste0("tau.",1:n.components)
    as.formula(paste0("y ~ ", paste(A," * exp(- x/(",tau," + ", tp, "))", collapse = " + ")))

  }
  ##
  ##
  ##(2) create formula for differential evolution run
  fn_constructor <- function(m){
    ##get length of x-vector
    x_len <- 1:(2 * m)

    ##generate term
    term <- vapply(seq(1,length(x_len), by = 2), function(i){
      paste0("(x[", i, "] * exp(-t/(x[", i + 1, "] + tp)))")

    },character(1))

    ##parse
    term <- paste(term, collapse = " + ")

    ##combine
    term <- paste0("sum(1 * ((n/c) - (",term,"))^2)")

    ##parse
    parse(text = eval(term))

  }
  ##
  ##
  ##(3) initialse objects
  chi_squared <- c(NA, NA)
  F <- c(Inf, Inf)
  start <- NULL

  if(is.null(n.components)){
    m <- 1

  }else{
    m <- n.components

  }
  ##
  ##
  ##(4) set seed
  if(!is.null(method_control_setting$seed))
    set.seed(method_control_setting$seed)

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(B) RUN DIFFERENTIAL EVOLUTION TO DETERMINE NUMBER OF COMPONENTS
  ##
  if(verbose){
    cat("\n[fit_OSLLifeTime()]\n")
    cat("\n(1) Start parameter and component adapation")
    cat("\n---------------------(start adaption)------------------------------------")

  }

  while(F[2] > qf(0.95, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2) & F[1] >= F[2]){
    ##set F
    F[1] <- F[2]

    ##construct formula outside of the loop; needs to be done here, otherwise the performance
    ##goes down
    formula_string <- fn_constructor(m)

    ##set fn
    fn <- function(x, tp = 0.1, n = df[[2]], c = df[[1]][2] - df[[1]][1], t = df[[1]], term = formula_string){
      eval(formula_string)
    }

    ##set start parameters
    if(!is.null(start))
      start_parameters <- start$optim$bestmem

    ##run differential evolution
    start <- DEoptim::DEoptim(
      fn = fn,
      lower = c(rep(0, 2 * m)),
      upper = rep(c(10 * sum(df[[2]]), 10000), m),
      control = DEoptim::DEoptim.control(
        trace = method_control_setting$DEoptim.trace,
        itermax = method_control_setting$DEoptim.itermax,
        c = .5,
        strategy = 2,
        parallelType = 0
      )
    )

    ##set chi^2 value and calculate F for the 2nd run
    chi_squared[2] <- start$optim$bestval
    if(!is.na(chi_squared[1])){
      F[2] <- (abs(diff(chi_squared))/2) /
        (chi_squared[2]/(length(df[[2]]) - 2 * m  - 2))

    }

    ##terminal feedback
    if(verbose){
      cat("\n>> + adaption for",m, "comp.", ": ", round(F[2],2), "(calc.) <> ",
          round(qf(0.95, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2), 2), "(ref.)")

      if(F[2] > qf(0.95, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2) & F[1] >= F[2]){
        cat(" >> [add comp.]")

      }else{
        cat(" >> [stop]\n")
        cat("---------------------(end adaption)--------------------------------------\n\n")

      }

    }

    ##break here if n.components was set others than NULL, in such case we force the number
    if(!is.null(n.components)){
      cat(" >> [forced stop]\n")
      cat("---------------------(end adaption)--------------------------------------\n\n")
      start_parameters <- start$optim$bestmem
      break()

    }

    ##update objects
    chi_squared[1] <- chi_squared[2]
    m <- m + 1

  }

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(C) RUN LM-FITTING
  ##
  if(is.null(n.components))
    m <- m - 2

  A <- start_parameters[seq(1,length(start_parameters), by = 2)]
  tau <- start_parameters[seq(2,length(start_parameters), by = 2)]
  names(A) <- paste0("A.", 1:(m))
  names(tau) <- paste0("tau.", 1:(m))

  ##add terminal feedback
  if(verbose){
    cat(">> Applied component matrix\n")
    start_matrix <- matrix(data = c(A,tau), ncol = 2)
    colnames(start_matrix) <- c("A", "tau")
    rownames(start_matrix) <- paste0("Comp.", 1:(m))
    print(start_matrix)
    cat("\n\n")

  }

  ##set tp
  tp <-  0.1

  fit <- try(minpack.lm::nlsLM(
    formula = fit_forumla(n.components = m, tp = tp),
    data = df,
    start = c(A, tau),
    upper = c(rep(sum(df[[2]]), length(A)), rep(Inf,length(tau))),
    lower = c(rep(0,2*length(A))),
    na.action = "na.exclude",
    trace = FALSE,
    control = minpack.lm::nls.lm.control(maxiter = 500)
  ), silent = TRUE)


# Post-processing -----------------------------------------------------------------------------

  if (class(fit) != 'try-error') {
    ##extract coefficients
    A <- coef(fit)[1:(m)]
    tau <- coef(fit)[(m + 1):(2 * m)]

    ##order coef
    o <- order(tau)
    tau <- tau[o]
    A <- A[o]

    ##summary matrix
    summary_matrix <- summary(fit)$coefficients

    ##order matrix by tau ... this is a little bit tricky
    summary_matrix <- summary_matrix[c(o,o + length(A)),]
    rownames(summary_matrix) <- rownames(summary_matrix)[c(o,o + length(A))]


  }else{
    m <- 1
    A <- NA
    tau <- NA
    summary_matrix <- NA

  }

# Terminal output -----------------------------------------------------------------------------
if(verbose){

  if (class(fit) != 'try-error') {
    cat("(2) Fitting results\n")
    cat("-------------------------------------------------------------------------\n")
    print(summary_matrix)
    cat("-------------------------------------------------------------------------\n")

  }else{
    cat("(2) Fitting results (sorted by ascending tau)\n")
    cat("-------------------------------------------------------------------------\n")
    try(stop("The fitting was not sucessful, consider to try again!", call. = FALSE))
    cat("-------------------------------------------------------------------------\n")

  }

}



# Plotting ------------------------------------------------------------------------------------
if(plot) {

  ##set plot default settings
  plot_settings <- list(
    main = "OSL Lifetimes",
    xlab = "Time [a.u.]",
    ylab = "POSL [a.u.]",
    log = "",
    xlim = c(0,max(df[[1]])),
    ylim = c(0,max(df[[2]])),
    col = get("col", pos = Luminescence:::.LuminescenceEnv)[-1],
    lty = rep(1, (m + 1)),
    legend.pos = "topright",
    legend.text = c("sum", paste0("comp. ", 1:m))

  )

    ##modify settings on request
    plot_settings <- modifyList(x = plot_settings, val = list(...))

    ##catch log scale
    if(grepl(pattern = "x", plot_settings$log, fixed = TRUE)){
      if(plot_settings$xlim[1] == 0){
        plot_settings$xlim[1] <- 1
        warning("[fit_OSLLifeTime()] log-scale requires x-values > 0, set to 1!", immediate. = TRUE, call. = FALSE)
      }
    }

    if(grepl(pattern = "y", plot_settings$log, fixed = TRUE)){
      if(plot_settings$ylim[1] == 0){
        plot_settings$ylim[1] <- 1
        warning("[fit_OSLLifeTime()] log-scale requires y-values > 0, set to 1!", immediate. = TRUE, call. = FALSE)
      }
    }


  ##plot if the fitting was a sucess
  if (class(fit) != 'try-error') {

    ##make sure that the screen closes if something is wrong
    on.exit(close.screen(n = c(1:2), all.screens = FALSE))

    split.screen(rbind(
      c(0.1,1,0.32, 0.98),
      c(0.1,1,0.1, 0.32)))

    screen(1)
    par(mar = c(0, 4, 3, 4))
    plot(df,
         xaxt = "n",
         xlab = "",
         ylab =  plot_settings$ylab,
         col = rgb(0,0,0,0.8),
         ylim = plot_settings$ylim,
         xlim = plot_settings$xlim,
         log = plot_settings$log,
         main = plot_settings$main
         )


    ##+ add some curve
    lines(
      df$x,
      fitted(fit),
      col = plot_settings$col[1],
      lwd = 1.3,
      lty = plot_settings$lty[1]
     )

    ##+ add components
    for(i in 1:m) {
      if (length(plot_settings$lty) < 2)
        plot_settings$lty <- rep(plot_settings$lty, 1 + m)

      if (length(plot_settings$col) < 2)
        plot_settings$col <- rep(plot_settings$col, 1 + m)

      curve(
        A[i] * exp(-x / (tau[i] + tp)),
        add = TRUE,
        col = plot_settings$col[i + 1],
        lty = plot_settings$lty[i + 1]
      )

    }

    ##+ add legend
    legend(
      plot_settings$legend.pos,
      legend = plot_settings$legend.text,
      lty = plot_settings$lty,
      col = plot_settings$col[c(1, 2:(m + 2))],
      bty = "n"
    )


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
      log = if(plot_settings$log == "x"){"x"}else{""},
      ylab = "\u03B5"
    )

  }else{
      plot(
        df,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        col = rgb(0, 0, 0, 0.8),
        main = plot_settings$main,
        xlim = plot_settings$xlim,
        ylim = plot_settings$ylim,
        log = plot_settings$log
      )

  }

}#if plot


# Return --------------------------------------------------------------------------------------

  ##create return object
  return(
    set_RLum(
      class = "RLum.Results",
      data = list(
        data = summary_matrix,
        start_matrix = start_matrix,
        fit = fit
      ),
      info = list(
        call = sys.call()
      )
    )
  )

}

temp <- read_XSYG2R("~/R/Personen/Christoph_Schmidt/20180619/2018-03-17_L1_SP_BSL_FB2A_proto_3.xsyg", fastForward = TRUE) %>%
  get_RLum(recordType = "UVVIS", drop = FALSE)

fit_OSLLifeTimes(temp[[3]])
