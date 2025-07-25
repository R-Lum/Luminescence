#' Fitting and Deconvolution of OSL Lifetime Components
#'
#' @details
#' The function intends to provide an easy access to pulsed optically stimulated luminescence (POSL) data,
#' in order determine signal lifetimes. The fitting is currently optimised to work with the off-time flank of POSL measurements
#' only. For the signal deconvolution, a differential evolution optimisation is combined with nonlinear least-square fitting
#' following the approach by Bluszcz & Adamiec (2006).
#'
#' **Component deconvolution algorithm**
#'
#' The component deconvolution consists of two steps:
#'
#' (1) Adaptation phase
#'
#' In the adaptation phase the function tries to figure out the optimal and statistically justified
#' number of signal components following roughly the approach suggested by Bluszcz & Adamiec (2006). In
#' contrast to their work, for the optimisation by differential evolution here the package `'DEoptim'` is used.
#'
#' The function to be optimized has the form:
#'
#' \deqn{\chi^2 = \sum(w * (n_i/c - \sum(A_i * exp(-x/(tau_i + t_p))))^2)}
#'
#' with \eqn{w = 1} for unweighted regression analysis (`method_control = list(weights = FALSE)`) or
#' \eqn{w = c^2/n_i} for weighted regression analysis. The default values is `TRUE`.
#'
#' \deqn{F = (\Delta\chi^2 / 2) / (\chi^2/(N - 2*m - 2))}
#'
#' (2) Final fitting
#'
#' **`method_control`**
#'
#' \tabular{lll}{
#' **Parameter** \tab **Type** \tab **Description**\cr
#' `p` \tab [numeric] \tab controls the probability for the F statistic reference values. For a significance level of 5 % a value of 0.95 (the default) should be added, for 1 %, a value of 0.99 is sufficient: 1 > p > 0 (cf. [stats::FDist])\cr
#' `seed` \tab [numeric] \tab set the seed for the random number generator, provide a value here to get reproducible results \cr
#' `DEoptim.trace` \tab [logical] \tab enable/disable the tracing of the differential evolution (cf. [DEoptim::DEoptim.control]) \cr
#' `DEoptim.itermax` \tab [logical] \tab control the number of the allowed generations (cf. [DEoptim::DEoptim.control]) \cr
#' `weights` \tab [logical] \tab enable/disable the weighting for the start parameter estimation and fitting (see equations above).
#' The default values is `TRUE` \cr
#' `nlsLM.trace` \tab [logical] \tab enable/disable trace mode for the nls fitting ([minpack.lm::nlsLM]), can be used to identify convergence problems, default is `FALSE` \cr
#' `nlsLM.upper` \tab [logical] \tab enable/disable upper parameter boundary, default is `TRUE` \cr
#' `nlsLM.lower` \tab [logical] \tab enable/disable lower parameter boundary, default is `TRUE`
#' }
#'
#' @param object [RLum.Data.Curve-class], [RLum.Analysis-class], [data.frame] or [matrix] (**required**):
#' Input object containing the data to be analysed. All objects can be provided also as list for an automated
#' processing. Please note: `NA` values are automatically removed and the dataset should comprise at least 5 data points (possibly more if `n.components` is
#' set to a value greater than 1)
#'
#' @param tp [numeric] (*with default*): option to account for the stimulation pulse width. For off-time measurements
#' the default value is 0. `tp` has the same unit as the measurement data, e.g., µs. Please set this parameter
#' carefully, if it all, otherwise you may heavily bias your fit results.
#'
#' @param signal_range [numeric] (*optional*): allows to set a channel range, by default all channels are used, e.g.
#' `signal_range = c(2,100)` considers only channels 2 to 100 and `signal_range = c(2)` considers only channels
#' from channel 2 onwards.
#'
#' @param n.components [numeric] (*optional*): Fix the number of components. If set the algorithm will try
#' to fit the number of predefined components. If nothing is set, the algorithm will try to find the best number
#' of components.
#'
#' @param method_control [list] (*optional*): Named to allow a more fine control of the fitting process. See details
#' for allowed options.
#'
#' @param plot [logical] (*with default*): Enable/disable the plot output.
#'
#' @param plot_simple [logical] (*with default*): Enable/disable the reduced
#' plot output. If `TRUE`, no
#' residual plot is shown, however, plot output can be combined using the standard R layout options,
#' such as `par(mfrow = c(2,2))`.
#'
#' @param verbose [logical] (*with default*): Enable/disable output to the
#' terminal.
#'
#' @param ... parameters passed to [plot.default] to control the plot output, supported are:
#' `main`, `xlab`, `ylab`, `log`, `xlim`, `ylim`, `col`, `lty`, `legend.pos`, `legend.text`. If the input
#' object is of type [RLum.Analysis-class] this arguments can be provided as a [list].
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
#'  `$total_counts` \tab `integer` \tab Photon count sum \cr
#'  `$fit` \tab `nls` \tab the fit object returned by [minpack.lm::nls.lm] \cr
#' }
#'
#'
#'**slot:** **`@info`**
#'
#' The original function call
#'
#' ------------------------\cr
#' `[ TERMINAL OUTPUT ]`\cr
#' ------------------------\cr
#'
#' Terminal output is only shown of the argument `verbose = TRUE`.
#'
#' *(1) Start parameter and component adaption*\cr
#' Trave of the parameter adaptation process
#'
#' *(2) Fitting results (sorted by ascending tau)*\cr
#' The fitting results sorted by ascending tau value. Please note
#' that if you access the `nls` fitting object, the values are not sorted.
#'
#' *(3) Further information*\cr
#' - The photon count sum
#' - Durbin-Watson residual statistic to assess whether the residuals are correlated, ideally
#' the residuals should be not correlated at all. Rough measures are: \cr
#' D = 0: the residuals are systematically correlated \cr
#' D = 2: the residuals are randomly distributed \cr
#' D = 4: the residuals are systematically anti-correlated\cr
#'
#' You should be suspicious if D differs largely from 2.
#'
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' A plot showing the original data and the fit so far possible. The lower plot shows the
#' residuals of the fit.
#'
#' @section Function version: 0.1.5
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University,
#' Christoph Schmidt, University of Bayreuth (Germany)
#'
#' @seealso [minpack.lm::nls.lm], [DEoptim::DEoptim]
#'
#' @references
#' Bluszcz, A., Adamiec, G., 2006. Application of differential evolution to fitting OSL decay curves.
#' Radiation Measurements 41, 886-891. \doi{10.1016/j.radmeas.2006.05.016}\cr
#'
#' Durbin, J., Watson, G.S., 1950. Testing for Serial Correlation in Least Squares Regression: I.
#' Biometrika 37, 409-21. doi:10.2307/2332391
#'
#' **Further reading**
#'
#' Hughes, I., Hase, T., 2010. Measurements and Their Uncertainties. Oxford University Press.
#'
#' Storn, R., Price, K., 1997. Differential Evolution –
#' A Simple and Efficient Heuristic for Global Optimization over Continuous Spaces.
#' Journal of Global Optimization 11, 341–359.
#'
#'@examples
#'
#'##load example data
#'data(ExampleData.TR_OSL, envir = environment())
#'
#'##fit lifetimes (short run)
#'fit_OSLLifeTimes(
#'  object = ExampleData.TR_OSL,
#'  n.components = 1)
#'
#'##long example
#'\dontrun{
#'fit_OSLLifeTimes(
#' object = ExampleData.TR_OSL)
#' }
#'
#'@export
fit_OSLLifeTimes <- function(
  object,
  tp = 0,
  signal_range = NULL,
  n.components = NULL,
  method_control = list(),
  plot = TRUE,
  plot_simple = FALSE,
  verbose = TRUE,
  ...
) {
  .set_function_name("fit_OSLLifeTimes")
  on.exit(.unset_function_name(), add = TRUE)

# Self-call -----------------------------------------------------------------------------------
if(inherits(object, "list") || inherits(object, "RLum.Analysis")){
  .validate_not_empty(object)

  ##allow RLum.Analysis objects
  if(all(vapply(object, function(x){
    inherits(x, "RLum.Analysis")}, logical(1)))){
    object <- lapply(object, function(x){x@records})
    object <- .unlist_RLum(object)
  }

  ## expand input arguments
  rep.length <- length(object)

  if (!is.null(n.components))
    n.components <- .listify(n.components, rep.length)
  tp <- .listify(tp, rep.length)

  ## names of extra arguments
  arg_names <- names(list(...))

  ##pretreat some of the ... settings to avoid
  ## expand all arguments
  arg_list <- NULL
  if(!is.null(arg_names)){
    arg_list <- lapply(arg_names , function(x){
      unlist(rep(list(...)[[x]], length.out = length(object)))
    })

    ## make sure we organise this list (not nice but it works)
    arg_list <- lapply(1:length(object), function(x){
      args <- lapply(arg_list, function(y) y[[x]])
      names(args) <- arg_names
      args
    })
  }

  ##run function
  temp_results <- lapply(1:length(object), function(x){
    temp <- try(do.call(what = fit_OSLLifeTimes,
        c(list(
         object = object[[x]],
         tp = tp[[x]],
         signal_range = signal_range,
         n.components = n.components[[x]],
         method_control = method_control,
         plot = plot,
         plot_simple = plot_simple,
         verbose = verbose
         ),
         arg_list[[x]])
     ), outFile = stdout()) # redirect error messages so they can be silenced

     if(inherits(temp, "try-error")){
       return(NULL)
     }
    return(temp)
  })

  ##combine results and return
  results <- merge_RLum(temp_results)

  if(!is.null(results))
    results@originator <- "fit_OSLLifeTimes"

  ##return
  return(results)
}

  ## Integrity checks -------------------------------------------------------

  is.valid <- .validate_class(object,
                              c("RLum.Data.Curve", "data.frame", "matrix"),
                              extra = "a 'list' of such objects",
                              throw.error = FALSE)
  if (!is.valid)
    return(NULL)

  .validate_not_empty(object)
  if(inherits(object, "RLum.Data.Curve")){
   if(!grepl(pattern = "POSL", x = object@recordType, fixed = TRUE))
     .throw_error("recordType '", object@recordType,
                  "' not supported for input object")

    object <- as.data.frame(object@data)

  } else if(inherits(object, "matrix")){
    object <- as.data.frame(object)
  }

  if (ncol(object) < 2) {
    .throw_error("'object' should have at least two columns")
  }
  df <- object[, 1:2]

  ##remove NA values, whatever it is worth for
  if (anyNA(df)) {
    df <- na.exclude(df)
    .throw_warning("NA values detected and removed from dataset")
  }

  ##rename columns for data.frame
  colnames(df) <- c("x","y")

  #check for 0 data in dataset ... we opt for hard stop
  if(any(df[[2]] == 0)){
    .throw_warning("The dataset contains 0, a value of 0.1 ",
                   "has been added to your count values")
    df[[2]] <- df[[2]] + 0.1
  }

  ##save original data for later
  df_raw <- df

  ##signal_range
  if(!is.null(signal_range)){
    .validate_class(signal_range, "numeric")

    ## format the extremes of the signal range
    reset_msg <- function(sr) {
      sprintf("'signal_range' reset to c(%d, %d)", sr[1], sr[2])
    }

    ##check lengths
    if(length(signal_range) == 1)
      signal_range <- c(signal_range, nrow(df))
    else if (length(signal_range) > 2)
      .throw_warning("'signal_range' has more than 2 elements, ",
                     "only the first 2 will be used, ", reset_msg(signal_range))

    if (any(signal_range < 1)) {
      signal_range <- pmax(signal_range, 1)
      .throw_warning("'signal_range' accepts only positive values, ",
                     reset_msg(signal_range))
    }

    if(signal_range[2] > nrow(df)){
      signal_range[2] <- nrow(df)
      .throw_warning("The last element of 'signal_range' exceeds the number ",
                     "of channels, ", reset_msg(signal_range))
    }

    if(signal_range[1] > signal_range[2]){
      signal_range <- c(1, nrow(df))
      .throw_warning("The first element of 'signal_range' exceeds the last ",
                     "element, ", reset_msg(signal_range))
    }

    ##set range
    df <- df[signal_range[1]:signal_range[2],]
  }

  ## number of components requested
  m <- max(.validate_positive_scalar(n.components, int = TRUE, null.ok = TRUE), 1)

  ## ensure that we have a minimum of data points available: the minimum
  ## is computed so that the degrees of freedom for the F distribution is
  ## positive (see `qf()` in the `while` loop further down at (B))
  min.num.signals <- 2 * m + 2 + 1
  if (nrow(df) < min.num.signals) {
    .throw_message("For ", m, " components the dataset must have at least ",
                   min.num.signals, " signal points, NULL returned")
    return(NULL)
  }


# Fitting -------------------------------------------------------------------------------------

  ##(0) CONTROL +++++++++++++++++++++++++++++++++++++++++++++++++++++
  method_control_setting <- list(
    p = 0.95,
    seed = NULL,
    DEoptim.trace = FALSE,
    DEoptim.itermax = 1000,
    weights = TRUE,
    nlsLM.trace = FALSE,
    nlsLM.upper = TRUE,
    nlsLM.lower = TRUE
  )

  ##udpate list if the user did something
  method_control_setting <- modifyList(x = method_control_setting, val = method_control)

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(A) SETTINGS
  ##
  ##
  ##(1) >> set fitting function for minpack.lm
  x <- 0 #cheat R check routine
  fit_formula <- function(n.components, tp) {
    comps <- 1:n.components
    stats::reformulate(paste0("A.", comps, " * exp(-x / (tau.", comps,
                              " + ", tp, "))", collapse = " + "),
                       response = "y")
  }
  ##
  ##
  ##(2) create formula for differential evolution run
  fn_constructor <- function(m) {
    ##generate term
    idx <- seq(1, 2 * m, by = 2)
    term <- paste0("(x[", idx, "] * exp(-t / (x[", idx + 1, "] + tp)))",
                   collapse = " + ")

    ##set weight (should be given as character)
    w <- if (method_control_setting$weights) "c^2 / n" else "1"

    ##combine
    term <- paste0("sum(",w," * ((n/c) - (",term,"))^2)")

    ##parse ... if we do it here, we boost the speed of the evaluation
    parse(text = eval(term))
  }
  ##
  ##
  ##(3) initialise objects
  chi_squared <- c(NA, NA)
  F <- c(Inf, Inf)
  start <- NULL

  ##
  ##
  ##(4) set seed
  if(!is.null(method_control_setting$seed))
    set.seed(method_control_setting$seed)

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(B) RUN DIFFERENTIAL EVOLUTION TO DETERMINE NUMBER OF COMPONENTS
  ##prevent collateral damage, so we want a data.frame that has at least 10 rows
  if(verbose){
      cat("\n[fit_OSLLifeTime()]\n")
      cat("\n(1) Start parameter and component adapation")
      cat("\n---------------------(start adaption)------------------------------------")
    }

    while(!is.na(suppressWarnings(stats::qf(method_control_setting$p, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2))) && (
          F[2] > stats::qf(method_control_setting$p, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2) & F[1] >= F[2])){

      ##set F
      F[1] <- F[2]

      ##construct formula outside of the loop; needs to be done here, otherwise the performance
      ##goes down
      formula_string <- fn_constructor(m)

      ##set fn
      set_tp <- tp
      set_c <- diff(c(0,df[[1]]))
      set_t <- df[[1]]
      set_n <- df[[2]]

      ##set function
      ##Personal reminder:
      ##Why this function is not written in C++ ... because it adds basically nothing
      ##in terms of speed ~ 10 µs faster, but needed to be compiled and thus cannot be changed
      ##directly in the code
      fn <- function(x, tp = set_tp, n = set_n, c = set_c, t = set_t, term = formula_string){
         eval(formula_string)
      }

      ##set start parameters
      if(!is.null(start))
        start_parameters <- start$optim$bestmem

      ##run differential evolution
      start <- try(DEoptim::DEoptim(
        fn = fn,
        lower = rep(0, 2 * m),
        upper = rep(c(10 * sum(df[[2]]), 10000), m),
        control = DEoptim::DEoptim.control(
           trace = method_control_setting$DEoptim.trace,
           itermax = method_control_setting$DEoptim.itermax,
           c = .5,
           strategy = 2,
           parallelType = 0 #Does it make sense to use parallel processing here: no, it does not scale well
         )
      ), silent = TRUE)

      if (inherits(start, "try-error")) {
        .throw_error("Failed to optimize the function, check the input data")
      }

      ##set chi^2 value and calculate F for the 2nd run
      chi_squared[2] <- start$optim$bestval
      if(!is.na(chi_squared[1])){
        F[2] <- (abs(diff(chi_squared))/2) /
          (chi_squared[2]/(nrow(df) - 2 * m  - 2))
      }

      ##terminal feedback
      if(verbose){
        cat("\n>> + adaption for",m, "comp.", ": ", round(F[2],2), "(calc.) <> ",
            round(stats::qf(method_control_setting$p, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2), 2), "(ref.)")

        if(F[2] > stats::qf(method_control_setting$p, df1 = 2, df2 = length(df[[2]]) - 2 * m - 2) & F[1] >= F[2]){
          cat(" >> [add comp.]")

        }else{
          cat(" >> [stop]\n")
          cat("---------------------(end adaption)--------------------------------------\n\n")
        }
      }

      ##break here if n.components was set others than NULL, in such case we force the number
      if(!is.null(n.components)){
        if(verbose){
         cat(" >> [forced stop]\n")
         cat("---------------------(end adaption)--------------------------------------\n\n")
        }

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
    ##reduce m by 2, why 2?
    ##  - the last component violated the F statistic, so was obviously not the best call
    ##  - the loop adds every time another component
    if(is.null(n.components)){
      ## this covers the extreme case that the process stops after the first run
      if(m == 2){
        start_parameters <- start$optim$bestmem
      }
      m <- max(m - 2, 1)
    }

    A <- start_parameters[seq(1,length(start_parameters), by = 2)]
    tau <- start_parameters[seq(2,length(start_parameters), by = 2)]
    names(A) <- paste0("A.", 1:(m))
    names(tau) <- paste0("tau.", 1:(m))

    ##create start_matrix
    start_matrix <- matrix(data = c(A,tau), ncol = 2)
    colnames(start_matrix) <- c("A", "tau")
    rownames(start_matrix) <- paste0("Comp.", 1:(m))

    ##add terminal feedback
    if(verbose){
      cat("\n>> Applied component matrix\n")
      print(start_matrix)
      cat("\n\n")
    }

    ##run fitting using the Levenberg-Marquardt algorithm
    fit <- try(minpack.lm::nlsLM(
      formula = fit_formula(n.components = m, tp = tp),
      data = df,
      start = c(A, tau),
      upper = if(method_control_setting$nlsLM.upper){
        c(rep(sum(df[[2]]), length(A)), rep(Inf,length(tau)))
       }else{
         NULL
       },
      lower = if(method_control_setting$nlsLM.lower){
         rep(0, 2 * length(A))
       }else{
         NULL
       },
      na.action = "na.exclude",
      weights = if(method_control_setting$weights){
        set_c^2/df[,2]
      }else{
        rep(1,nrow(df))
      },
      trace = method_control_setting$nlsLM.trace,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ), outFile = stdout()) # redirect error messages so they can be silenced

# Post-processing -----------------------------------------------------------------------------

  A <- NA
  tau <- NA
  summary_matrix <- NA
  D <- NA
  if (!inherits(fit, 'try-error')) {
    ##extract coefficients
    A <- coef(fit)[1:(m)]
    tau <- coef(fit)[(m + 1):(2 * m)]

    ##order coef
    o <- order(tau)
    tau <- tau[o]
    A <- A[o]

    ##summary matrix
    summary_matrix <- summary(fit)$coefficients

    ##return warning if one parameter is negative, this can happen if the user let the boundaries
    ##free float
    if(any(summary_matrix[,1]<0))
      .throw_warning("At least one parameter is negative, ",
                     "please check carefully your results")

    ##order matrix by tau, but keep the rownames
    temp_rownames <- rownames(summary_matrix)
    summary_matrix <- summary_matrix[c(o,o + length(A)),]
    rownames(summary_matrix) <- temp_rownames
    rm(temp_rownames)

    ##calculate Durbin-Watson statistic
    R <- residuals(fit)
    D <- round(sum((R - c(0,R[-length(R)]))^2) / sum(R^2),2)
    rm(R)
  } else {
    m <- 1
  }

# Terminal output -----------------------------------------------------------------------------
if(verbose){

  if (!inherits(fit, 'try-error')) {
    cat("(2) Fitting results (sorted by ascending tau)\n")
    cat("-------------------------------------------------------------------------\n")
    print(summary_matrix)
    cat("-------------------------------------------------------------------------\n")

  }else{
    .throw_message("The fitting was not successful, consider trying again")
  }

  cat("\n(3) Further information\n")
  cat("-------------------------------------------------------------------------\n")
  cat("Photon count sum: ", sum(df[[2]]),"\n")
  cat("Durbin-Watson residual statistic: ", D,"")

  string <- NA
  if(!is.na(D)){
    string <- c("\u005b",rep(" ",(D * 10)/4),"\u003c\u003e",rep(" ",10 - (D * 10)/4),"\u005d\n")
  }
  cat(paste(string, collapse = ""))
  cat("\n")
}

# Plotting ------------------------------------------------------------------------------------
if(plot) {

  ##set plot default settings
  plot_settings <- list(
    main = "OSL Lifetimes",
    xlab = "Time [a.u.]",
    ylab = "POSL [a.u.]",
    log = "",
    xlim = c(0,max(df_raw[[1]])),
    ylim = c(0,max(df_raw[[2]])),
    col = get("col", pos = .LuminescenceEnv)[-1],
    lty = rep(1, (m + 1)),
    legend.pos = "topright",
    legend.text = c("sum", paste0("comp. ", 1:m))
  )

    ##modify settings on request
    plot_settings <- modifyList(x = plot_settings, val = list(...))

    ##catch log scale
    if (is.list(plot_settings$log))
      plot_settings$log <- unlist(plot_settings$log)
    if(grepl(pattern = "x", plot_settings$log, fixed = TRUE)){
      if(plot_settings$xlim[1] == 0){
        plot_settings$xlim[1] <- max(min(df_raw[[1]]), 1e-4)
        .throw_warning("log-scale requires x-values > 0, set min xlim to ",
                       round(plot_settings$xlim[1], 4))
      }
    }

    if(grepl(pattern = "y", plot_settings$log, fixed = TRUE)){
      if(plot_settings$ylim[1] == 0){
        plot_settings$ylim[1] <- max(min(df_raw[[2]]), 1e-04)
        .throw_warning("log-scale requires y-values > 0, set min ylim to ",
                       round(plot_settings$ylim[1], 4))
      }
    }

  ##plot if the fitting was a success
  if (!inherits(fit, 'try-error')) {

    if(!plot_simple){
      ##make sure that the screen closes if something is wrong
      on.exit(graphics::close.screen(all.screens = TRUE), add = TRUE)

      graphics::split.screen(rbind(
        c(0.1,1,0.32, 0.98),
        c(0.1,1,0.1, 0.32)))

      graphics::screen(1)
      par(mar = c(0, 4, 3, 4))
    }

    plot(NA,NA,
         xaxt = if(plot_simple) "s" else "n",
         xlab = if(plot_simple) plot_settings$xlab else "",
         ylab = plot_settings$ylab,
         ylim = plot_settings$ylim,
         xlim = plot_settings$xlim,
         log = plot_settings$log,
         main = plot_settings$main
         )

    ##add used points
    points(df, col = rgb(0,0,0,0.8))

    ##add not used points df_raw (this solution avoids overplotting)
    if(nrow(df) != nrow(df_raw))
      points(df_raw[!df_raw[[1]]%in%df[[1]],], col = "grey")

    ##+ add some curve
    lines(
      df$x,
      stats::fitted(fit),
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

    if(!plot_simple){
      graphics::screen(2)
      par(mar = c(4, 4, 0, 4))
      plot(
        x = df[[1]],
        y = residuals(fit),
        xlab = plot_settings$xlab,
        type = "b",
        pch = 20,
        xlim = plot_settings$xlim,
        log = if(plot_settings$log == "x"){"x"}else{""},
        ylab = "Resid."
      )
    }

  }else{
      ## fitting failed
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
  set_RLum(
      class = "RLum.Results",
      data = list(
        data = summary_matrix,
        start_matrix = start_matrix,
        total_counts = sum(df[[2]]),
        fit = fit
      ),
      info = list(
        call = sys.call()
      )
  )
}
