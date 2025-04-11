#' @title Fitting Thermal Quenching Data
#'
#' @description Applying a nls-fitting to thermal quenching data.
#'
#' @details
#'
#' **Used equation**\cr
#'
#' The equation used for the fitting is
#'
#' \deqn{y = A / (1 + C * exp(-W / (kB * x))) + c}
#'
#' *W* is the energy depth in eV, *C* is a dimensionless constant, *A* and *c*
#' are used to adjust the curve for the given signal, *kB* is the Boltzmann
#' constant in eV/K and *x* is the absolute temperature in K.
#'
#' **Error estimation**\cr
#'
#' The error estimation is done by varying the input parameters using the
#' given uncertainties in a Monte Carlo simulation. Errors are assumed to
#' follow a normal distribution.
#'
#' **`start_param`** \cr
#'
#' The function allows the injection of starting values for the parameters to
#' be optimised via the `start_param` argument. The parameters must be provided
#' as a named list.
#' Examples: `start_param = list(A = 1, C = 1e+5, W = 0.5, c = 0)`
#'
#' **`method_control`** \cr
#'
#' The following arguments can be provided via `method_control`. Please note that arguments provided
#' via `method_control` are not further tested, i.e., if the function crashes your input was probably
#' wrong.
#'
#' \tabular{lll}{
#' **ARGUMENT** \tab **TYPE** \tab **DESCRIPTION**\cr
#' `upper` \tab named [vector] \tab sets upper fitting boundaries, if provided boundaries for all arguments
#' are required, e.g., `c(A = 0, C = 0, W = 0, c = 0)` \cr
#' `lower` \tab names [vector] \tab set lower fitting boundaries (see `upper` for details) \cr
#' `trace`   \tab [logical] \tab enable/disable progression trace for [minpack.lm::nlsLM]\cr
#'  `weights` \tab [numeric] \tab option to provide own weights for the fitting, the length of this
#'  vector needs to be equal to the number for rows of the input `data.frame`. If set to `NULL` no weights
#'  are applied. The weights are defined by the third column of the input `data.frame`.
#' }
#'
#' @param data [data.frame] (**required**): input data with three columns, the first column contains
#' temperature values in deg. C, columns 2 and 3 the dependent values with its error
#'
#' @param start_param [list] (*optional*): option to provide the start
#' parameters for the fitting, see details
#'
#' @param method_control [list] (*optional*): further options to fine tune
#' the fitting, see details for
#' further information
#'
#' @param n.MC [integer] (*with default*): number of Monte Carlo runs for the
#' error estimation. If `n.MC` is `NULL` or 1, the error estimation is skipped.
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param ... further arguments that can be passed to control the plotting, support are `main`, `pch`,
#' `col_fit`, `col_points`, `lty`, `lwd`, `xlab`, `ylab`, `xlim`, `ylim`, `xaxt`
#'
#' @return
#'
#' The function returns numerical output and an (*optional*) plot.
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' `[.. $data : data.frame]`\cr
#'
#' A table with all fitting parameters and the number of Monte Carlo runs used
#' for the error estimation (this may be smaller that `n.MC`).
#'
#' `[.. $fit : nls object]` \cr
#'
#'  The nls [stats::nls] object returned by the function [minpack.lm::nlsLM]. This object
#'  can be further passed to other functions supporting an nls object (cf. details section
#'  in [stats::nls])
#'
#' **slot:** **`@info`**
#'
#' `[.. $call : call]`\cr
#'
#' The original function call.
#'
#' -----------------------------------\cr
#' `[ GAPHICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' Plotted are temperature against the signal and their uncertainties.
#' The fit is shown as dashed-line (can be modified). Please note that for the fitting the absolute
#' temperature values are used but are re-calculated to deg. C for the plot.
#'
#' @section Function version: 0.2
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#'
#' Wintle, A.G., 1975. Thermal Quenching of Thermoluminescence in Quartz. Geophys. J. R. astr. Soc. 41, 107–113.
#'
#' @seealso [minpack.lm::nlsLM]
#'
#' @examples
#'
#' ##create short example dataset
#' data <- data.frame(
#'   T = c(25, 40, 50, 60, 70, 80, 90, 100, 110),
#'   V = c(0.06, 0.058, 0.052, 0.051, 0.041, 0.034, 0.035, 0.033, 0.032),
#'   V_X = c(0.012, 0.009, 0.008, 0.008, 0.007, 0.006, 0.005, 0.005, 0.004))
#'
#' ##fit
#' fit_ThermalQuenching(
#'  data = data,
#'  n.MC = NULL)
#'
#' @md
#' @export
fit_ThermalQuenching <- function(
  data,
  start_param = list(),
  method_control = list(),
  n.MC = 100,
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("fit_ThermalQuenching")
  on.exit(.unset_function_name(), add = TRUE)

  # Self-call -----------------------------------------------------------------------------------
  if(inherits(data, "list")){

    ##get arguments
    args <- as.list(match.call())
    args[[1]] <- NULL
    args$data <- NULL

    ##run function
    results_list <- lapply(data, function(x){
       do.call(fit_ThermalQuenching, c(list(data = x),args))
    })

    ##combine and return
    return(merge_RLum(results_list))
  }


  ## Integrity checks -------------------------------------------------------

  .validate_class(data, "data.frame",
                  extra = "a 'list' of such objects")

  if(nrow(data) < 1 || ncol(data) < 3)
    .throw_error("'data' is empty or has fewer than three columns")

  if (ncol(data) > 3) {
    .throw_warning("'data' has more than 3 columns, taking only the first three")
    data <- data[, 1:3]
  }

  if (anyNA(data)) {
    .throw_warning("NA values in 'data' automatically removed")
    data <- na.exclude(data)
  }

  .validate_class(start_param, "list")
  .validate_class(method_control, "list")
  .validate_positive_scalar(n.MC, int = TRUE, null.ok = TRUE)
  if (is.null(n.MC))
    n.MC <- 1

  ## Prepare data -----------------------------------------------------------
  ##
  ## Set formula for quenching according to Wintle 1973.
  ## Here we add a constant, otherwise the fit will not really work.
  ##
  ## The actual implementation adopts a slight reformulation, which is
  ## mathematically equivalent to the one presented in the documentation
  ## above, but is numerically more stable:
  ##
  ## y = A / (1 + C * exp(-W / (kB * x))) + c
  ##   = A / (1 + exp(log(C)) * exp(-W / (kB * x))) + c
  ##   = A / (1 + exp(log(C) - W / (kB * x))) + c
  ##   = A / (1 + exp(C' - W / (kB * x))) + c
  ##
  ## For backward compatibility, we keep using the name C for the logged
  ## parameter, so that the user will keep seeing values in the original scale,
  ## although internally we operate in log-space for C. When we show the
  ## final results, we'll have to exponentiate the fitted parameter.
  ##
  kB <- .const$kB
  f <- y ~ A / (1 + exp(C - W / (kB * x))) + c

  ##set translate values in data.frame to absolute temperature
  data_raw <- data
  data[[1]] <- data[[1]] + .const$C2K

  ##start parameter
  start_param <- modifyList(x = list(
    A = max(data[[2]]),
    C = log(max(data[[1]]) * 1e5),
    W = 0.5,
    c = 0),
    val = start_param)

  ##method control
  method_control <- modifyList(
    x = list(
     lower = c(A = 0, C = 0, W = 0, c = -Inf),
     upper = c(A = 10 * start_param$A,
               C = log(max(data[[1]]) * 1e12),
               W = 10, c = 10 * start_param$A),
     trace = FALSE,
     weights = data[[3]]
    ),
    val = method_control)

  weights <- method_control$weights
  if (is.null(weights))
    weights <- rep(1, nrow(data))

  # Fitting -------------------------------------------------------------------------------------
  ##guine fitting
  fit <- try(minpack.lm::nlsLM(
    formula = f,
    data = data.frame(x = data[[1]], y = data[[2]]),
    weights = weights,
    control = list(
      maxiter = 500,
      maxfev = 1000,
      trace = method_control$trace),
    start = start_param,
    lower = method_control$lower,
    upper = method_control$upper
  ), silent = TRUE)

  ##only continue if the first fitting worked out
  if (inherits(fit, "try-error")) {
    .throw_message("Fitting failed, NULL returned")
    return(NULL)
  }

    ##Prepare MC runs for the fitting
    x_MC <- data[[1]]
    y_MC <- matrix(
      data = data[[2]] + rnorm(n.MC * length(x_MC)) * data[[3]],
      nrow = length(x_MC),
      ncol = n.MC)
    y_MC[y_MC < 0] <- 0

  ## run fitting
  fit_MC <- lapply(1:n.MC, function(x) {
      temp <- try(minpack.lm::nlsLM(
        formula = f,
        data = data.frame(x = x_MC, y = y_MC[,x]),
        weights = weights,
        control = list(
          maxiter = 500,
          maxfev = 1000
        ),
        start = start_param,
        lower = method_control$lower,
        upper = method_control$upper
      ), silent = TRUE)

      ##return value
      if(inherits(temp, 'try-error')) {
        return(NULL) # nocov
      } else{
        temp
      }
  })

  ## remove NULL (the fit was not successful)
  fit_MC <- .rm_NULL_elements(fit_MC)
  n.MC <- length(fit_MC)

  ## Extract values ---------------------------------------------------------

   ##(1) - extract parameters from main fit
   fit_coef <- coef(fit)
   A <- fit_coef[["A"]]
   C <- fit_coef[["C"]]
   W <- fit_coef[["W"]]
   c <- fit_coef[["c"]]

   ##(2) - extract values from MC run
   fit_coef_MC_full <- vapply(X = fit_MC, FUN = coef, FUN.VALUE = numeric(4))
   fit_coef_MC <- round(matrixStats::rowSds(fit_coef_MC_full),3)

  ## (3) - unlog the C parameter before computing the standard deviation
  fit_coef_MC[2] <- signif(sd(exp(fit_coef_MC_full[2, ])), 4)

   A_MC_X <- fit_coef_MC[1]
   C_MC_X <- fit_coef_MC[2]
   W_MC_X <- fit_coef_MC[3]
   c_MC_X <- fit_coef_MC[4]

  ## Terminal output --------------------------------------------------------
  if (verbose) {

    cat("\n[fit_ThermalQuenching()]\n\n")
    cat(" A =", A, "\u00b1", A_MC_X, "\n")
    cat(" C =", exp(C), "\u00b1", C_MC_X, "\n")
    cat(" W =", W, "\u00b1", W_MC_X, "eV\n")
    cat(" c =", c, "\u00b1", c_MC_X, "\n")
    cat(" --------------------------------\n")
  }

  ## Plotting ---------------------------------------------------------------
  if(plot) {
    ##plot settings
    plot_settings <- list(
      xlim = range(data[[1]]),
      ylim = c(min(data[[2]]) - data[[3]][which.min(data[[2]])],
               max(data[[2]]) + data[[3]][which.max(data[[2]])]),
      pch = 1,
      xaxt = "n",
      xlab = "Temperature [\u00b0C]",
      ylab = "Dependent [a.u.]",
      main = "Thermal quenching",
      lty = 2,
      col_points = "black",
      col_fit = "red",
      lwd = 1.3,
      mtext = if(n.MC == 1) "" else paste0("n.MC = ", n.MC)
    )

    ##overwrite settings
    plot_settings <- modifyList(x = plot_settings, val = list(...))

    ##create plot window
    plot(
      x = NA,
      y = NA,
      xlim = plot_settings$xlim,
      ylim = plot_settings$ylim,
      xaxt = plot_settings$xaxt,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      main = plot_settings$main
    )

    ##add axis with correct temperature
    if(!is.null(plot_settings$xaxt) && plot_settings$xaxt == "n"){
      at <- pretty(round(axTicks(side = 1) - .const$C2K))
      axis(side = 1, at = at + .const$C2K, labels = at)
    }

    ## add MC curves
    if (n.MC > 1) {
      for(i in 1:n.MC){
        A <- fit_coef_MC_full[1,i]
        C <- fit_coef_MC_full[2,i]
        W <- fit_coef_MC_full[3,i]
        c <- fit_coef_MC_full[4,i]
        x <- 0
        curve(A / (1 + exp(C - W / (kB * x))) + c,
              col = rgb(0, 0, 0, .1), add = TRUE)
      }
    }

    ##add points and uncertainties
    points(data[, 1:2],
           pch = plot_settings$pch,
           lwd = 2,
           col = plot_settings$col_points)
    segments(x0 = data[[1]], x1 = data[[1]],
             y0 = data[[2]] + data[[3]],
             y1 = data[[2]] - data[[3]],
             col = plot_settings$col_points
             )

    ##add central fit
    A <- fit_coef[["A"]]
    C <- fit_coef[["C"]]
    W <- fit_coef[["W"]]
    c <- fit_coef[["c"]]

    x <- 0
    curve(A / (1 + exp(C - W / (kB * x))) + c,
          lty = plot_settings$lty,
          lwd = plot_settings$lwd,
          col = plot_settings$col_fit,
          add = TRUE)

    ##add mtext
    mtext(side = 3, text = plot_settings$mtext)
  }

  ## Return -----------------------------------------------------------------
  output_df <- data.frame(
    A = A,
    A_X = A_MC_X,
    C = exp(C),
    C_X = C_MC_X,
    W = W,
    W_X = W_MC_X,
    c = c,
    c_X = c_MC_X,
    n.MC = n.MC
  )

  output <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = output_df,
      fit = fit
    ),
    info = list(
      call = sys.call()
    )
  )

  return(output)
}
