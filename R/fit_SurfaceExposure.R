#' @title Nonlinear Least Squares Fit for OSL surface exposure data
#'
#' @description
#' This function determines the (weighted) least-squares estimates of the
#' parameters of either equation 1 in *Sohbati et al. (2012a)* or equation 12 in
#' *Sohbati et al. (2012b)* for a given OSL surface exposure data set (**BETA**).
#'
#' @details
#' **Weighted fitting**
#'
#' If `weights = TRUE` the function will use the inverse square of the error (\eqn{1/\sigma^2})
#' as weights during fitting using [minpack.lm::nlsLM]. Naturally, for this to
#' take effect individual errors must be provided in the third column of the
#' `data.frame` for `data`. Weighted fitting is **not** supported if `data`
#' is a list of multiple `data.frame`s, i.e., it is not available for global
#' fitting.
#'
#' **Dose rate**
#' If any of the arguments `Ddot` or `D0` is at its default value (`NULL`),
#' this function will fit equation 1 in Sohbati et al. (2012a) to the data. If
#' the effect of dose rate (i.e., signal saturation) needs to be considered,
#' numeric values for the dose rate (`Ddot`) (in Gy/ka) and the characteristic
#' saturation dose (`D0`) (in Gy) must be provided. The function will then fit
#' equation 12 in Sohbati et al. (2012b) to the data.
#'
#' **NOTE**: Currently, this function does **not** consider the variability
#' of the dose rate with sample depth (`x`)! In the original equation the dose
#' rate `D` is an arbitrary function of `x` (term `D(x)`), but here `D` is assumed
#' constant.
#'
#' **Global fitting**
#' If `data` is [list] of multiple `data.frame`s, each representing a separate
#' sample, the function automatically performs a global fit to the data. This
#' may be useful to better constrain the parameters `sigmaphi` or `mu` and
#' **requires** that known ages for each sample is provided
#' (e.g., `age = c(100, 1000)` if `data` is a list with two samples).
#'
#'
#' @param data [data.frame] or [list] (**required**):
#' Measured OSL surface exposure data with the following structure:
#'
#' ```
#'                                (optional)
#'      | depth (a.u.)| intensity | error |
#'      |     [ ,1]   |    [ ,2]  | [ ,3] |
#'      |-------------|-----------|-------|
#' [1, ]|    ~~~~     |    ~~~~   | ~~~~  |
#' [2, ]|    ~~~~     |    ~~~~   | ~~~~  |
#'  ... |     ...     |     ...   |  ...  |
#' [x, ]|    ~~~~     |    ~~~~   | ~~~~  |
#'
#' ```
#'
#' Alternatively, a [list] of `data.frames` can be provided, where each
#' `data.frame` has the same structure as shown above, with the exception that
#' they must **not** include the optional error column. Providing a [list] as
#' input automatically activates the global fitting procedure (see details).
#'
#' @param sigmaphi [numeric] (*optional*):
#' A numeric value for `sigmaphi`, i.e. the charge detrapping rate.
#' Example: `sigmaphi = 5e-10`
#'
#' @param mu [numeric] (*optional*):
#' A numeric value for mu, i.e. the light attenuation coefficient.
#' Example: `mu = 0.9`
#'
#' @param age [numeric] (*optional*):
#' The age (a) of the sample, if known. If `data` is a [list] of *x* samples,
#' then `age` must be a numeric vector of length *x*.
#' Example: `age = 10000`, or `age = c(1e4, 1e5, 1e6)`.
#'
#' @param Ddot [numeric] (*optional*):
#' A numeric value for the environmental dose rate (Gy/ka). For this argument
#' to be considered a value for `D0` must also be provided; otherwise it will be
#' ignored.
#'
#' @param D0 [numeric] (*optional*):
#' A numeric value for the characteristic saturation dose (Gy). For this argument
#' to be considered a value for `Ddot` must also be provided; otherwise it will be
#' ignored.
#'
#' @param weights [logical] (*optional*):
#' If `TRUE` the fit will be weighted by the inverse square of the error.
#' Requires `data` to be a [data.frame] with three columns.
#'
#' @param plot [logical] (*optional*):
#' enable/disable the plot output.
#'
#' @param legend [logical] (*optional*):
#' Show or hide the equation inside the plot.
#'
#' @param error_bars [logical] (*optional*):
#' Show or hide error bars (only applies if errors were provided).
#'
#' @param coord_flip [logical] (*optional*):
#' Flip the coordinate system.
#'
#' @param ... Further parameters passed to [plot].
#' Custom parameters include:
#' - `verbose` ([logical]): show or hide console output
#' - `line_col`: Colour of the fitted line
#' - `line_lty`: Type of the fitted line (see `lty` in `?par`)
#' - `line_lwd`: Line width of the fitted line (see `lwd` in `?par`)
#'
#' @return
#'
#' Function returns results numerically and graphically:
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
#'  `$summary` \tab `data.frame` \tab summary of the fitting results \cr
#'  `$data` \tab `data.frame` \tab the original input data \cr
#'  `$fit` \tab `nls` \tab the fitting object produced by [minpack.lm::nlsLM] \cr
#'  `$args` \tab `character` \tab arguments of the call \cr
#'  `$call` \tab `call` \tab the original function call \cr
#' }
#'
#'**slot:** **`@info`**
#'
#' Currently unused.
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' A scatter plot of the provided depth-intensity OSL surface exposure data
#' with the fitted model.
#'
#' @section Function version: 0.1.0
#'
#' @note
#' **This function has BETA status. If possible, results should be**
#' **cross-checked.**
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @seealso  [ExampleData.SurfaceExposure], [minpack.lm::nlsLM]
#'
#' @references
#'
#' Sohbati, R., Murray, A.S., Chapot, M.S., Jain, M., Pederson, J., 2012a.
#' Optically stimulated luminescence (OSL) as a chronometer for surface exposure
#' dating. Journal of Geophysical Research 117, B09202. doi:
#' \doi{10.1029/2012JB009383}
#'
#' Sohbati, R., Jain, M., Murray, A.S., 2012b. Surface exposure dating of
#' non-terrestrial bodies using optically stimulated luminescence: A new method.
#' Icarus 221, 160-166.
#'
#' @keywords datagen
#'
#' @examples
#'
#' ## Load example data
#' data("ExampleData.SurfaceExposure")
#'
#' ## Example 1 - Single sample
#' # Known parameters: 10000 a, mu = 0.9, sigmaphi = 5e-10
#' sample_1 <- ExampleData.SurfaceExposure$sample_1
#' head(sample_1)
#' results <- fit_SurfaceExposure(
#'  data = sample_1,
#'  mu = 0.9,
#'  sigmaphi = 5e-10)
#' get_RLum(results)
#'
#'
#' ## Example 2 - Single sample and considering dose rate
#' # Known parameters: 10000 a, mu = 0.9, sigmaphi = 5e-10,
#' # dose rate = 2.5 Gy/ka, D0 = 40 Gy
#' sample_2 <- ExampleData.SurfaceExposure$sample_2
#' head(sample_2)
#' results <- fit_SurfaceExposure(
#'  data = sample_2,
#'  mu = 0.9,
#'  sigmaphi = 5e-10,
#'  Ddot = 2.5,
#'  D0 = 40)
#' get_RLum(results)
#'
#' ## Example 3 - Multiple samples (global fit) to better constrain 'mu'
#' # Known parameters: ages = 1e3, 1e4, 1e5, 1e6 a, mu = 0.9, sigmaphi = 5e-10
#' set_1 <- ExampleData.SurfaceExposure$set_1
#' str(set_1, max.level = 2)
#' results <- fit_SurfaceExposure(
#'   data = set_1,
#'   age = c(1e3, 1e4, 1e5, 1e6),
#'   sigmaphi = 5e-10)
#' get_RLum(results)
#'
#'
#' ## Example 4 - Multiple samples (global fit) and considering dose rate
#' # Known parameters: ages = 1e2, 1e3, 1e4, 1e5, 1e6 a, mu = 0.9, sigmaphi = 5e-10,
#' # dose rate = 1.0 Ga/ka, D0 = 40 Gy
#' set_2 <- ExampleData.SurfaceExposure$set_2
#' str(set_2, max.level = 2)
#' results <- fit_SurfaceExposure(
#'  data = set_2,
#'  age = c(1e2, 1e3, 1e4, 1e5, 1e6),
#'  sigmaphi = 5e-10,
#'  Ddot = 1,
#'  D0 = 40)
#'get_RLum(results)
#'
#' @md
#' @export
fit_SurfaceExposure <- function(
    data,
    sigmaphi = NULL,
    mu = NULL,
    age = NULL,
    Ddot = NULL,
    D0 = NULL,
    weights = FALSE,
    plot = TRUE,
    legend = TRUE,
    error_bars = TRUE,
    coord_flip = FALSE,
    ...
) {
  .set_function_name("fit_SurfaceExposure")
  on.exit(.unset_function_name(), add = TRUE)

  ## SETTINGS ----
  settings <- list(
    verbose = TRUE,
    info = list()
  )
  settings <- modifyList(settings, list(...))

  ## Integrity checks -------------------------------------------------------

  .validate_not_empty(data)

  ## Data type validation
  if (inherits(data, "RLum.Results"))
    data <- get_RLum(data, "data")

  if (inherits(data, "matrix"))
    data <- as.data.frame(data)

  if (inherits(data, "data.table"))
    data <- as.data.frame(data)

  ## For global fitting of multiple data sets 'data' must be a list
  if (inherits(data, "list")) {

    # Global fitting requires and equal amount of ages to be provided
    if (length(data) != length(age))
      .throw_error("If 'data' is a list of data sets for global fitting, ",
                   "'age' must be of the same length.")

    # TODO: Support weighted fitting for global fit
    if (weights) {
      if (settings$verbose)
        .throw_warning("'weights' is not supported when multiple data sets ",
                       "are provided for global fitting")
      weights <- FALSE
    }

    # collapse list into a data.frame with a $group column to distinguish
    # between individual samples
    data_list <- data

    for (i in seq_along(data))
      data[[i]]$group <- LETTERS[[i]]

    data <- do.call(rbind, data)
    data$group <- as.factor(data$group)
    global_fit <- TRUE
  } else {
    # ignore 'global_fit' if 'data' is a data.frame
    global_fit <- FALSE
  }

  # Exit if data type is invalid
  .validate_class(data, "data.frame")
  if (ncol(data) < 2) {
    .throw_error("'data' should have at least two columns")
  }

  # Check which parameters have been provided
  if (!is.null(age) && anyNA(age)) age <- NULL
  if (!is.null(sigmaphi) && anyNA(sigmaphi)) sigmaphi <- NULL
  if (!is.null(mu) && anyNA(mu)) mu <- NULL

  ## Weighting options (only available for global fitting)
  if (ncol(data) >= 3 && weights && !global_fit)
    wi <- (1 / data[ ,3]^2) / sum(1 / data[ ,3]^2)
  else
    wi <- rep(1, times = nrow(data))

  ## remove rows with NA
  if (anyNA(data)) {
    data <- data[stats::complete.cases(data), ]
    if (settings$verbose)
      message("[fit_SurfaceExposure()] NA values in 'data' were removed")
  }

  ## extract errors into separate variable
  if (ncol(data) >= 3 && !global_fit)
    error <- data[ ,3]
  else
    error <- NULL

  ## Take only the first to columns (depth, signal)
  if (ncol(data) > 2 && !global_fit)
    data <- data[ ,1:2]

  ## Data preprocessing ----

  # set column names
  if (!global_fit)
    colnames(data) <- c("x", "y")
  else
    colnames(data) <- c("x", "y", "group")

  ## FITTING ----

  ## Functions
  # w/o dose rate
  fun <- y ~ exp(-sigmaphi * age * 365.25*24*3600 * exp(-mu * x))
  fun_global <- y ~ exp(-sigmaphi * age[group] * 365.25*24*3600 * exp(-mu * x))

  # w/ dose rate (Sohbati et al. 2012, eq 12)
  if (!is.null(Ddot))
    Ddot <- Ddot / 1000 / 365.25 / 24 / 60 / 60

  fun_w_dr <- y ~ (sigmaphi * exp(-mu * x) * exp(-(age * 365.25*24*3600) * (sigmaphi * exp(-mu * x) + Ddot/D0)) + Ddot/D0) /
                         (sigmaphi * exp(-mu * x) + Ddot/D0)
  fun_global_w_dr <- y ~ (sigmaphi * exp(-mu * x) * exp(-(age[group] * 365.25*24*3600) * (sigmaphi * exp(-mu * x) + Ddot/D0)) + Ddot/D0) /
                                (sigmaphi * exp(-mu * x) + Ddot/D0)

  ## start parameter
  start <- list(sigmaphi = if (is.null(sigmaphi)) 5.890e-09 else NULL,
                mu = if (is.null(mu)) 1 else NULL,
                age = if (is.null(age)) 2 else NULL)

  start <- .rm_NULL_elements(start)

  ## fitting boundaries
  lower <- list(sigmaphi = if (is.null(sigmaphi)) -Inf else NULL,
                mu = if (is.null(mu)) 0 else NULL,
                age = if (is.null(age)) 0 else NULL)
  upper <- list(sigmaphi = if (is.null(sigmaphi)) Inf else NULL,
                mu = if (is.null(mu)) Inf else NULL,
                age = if (is.null(age)) Inf else NULL)

  ## Decision tree which of the functions to use
  if (!is.null(Ddot) && !is.null(D0)) {
    if (global_fit)
      use_fun <- fun_global_w_dr
    else
      use_fun <- fun_w_dr
  } else {
    if (global_fit)
      use_fun <- fun_global
    else
      use_fun <- fun
  }

  # (un)constrained fitting
  fit <- tryCatch({
    suppressWarnings(minpack.lm::nlsLM(formula = use_fun,
                      data = data,
                      start = start,
                      lower = unlist(lower),
                      upper = unlist(upper),
                      weights = wi))
  },
  error = function(e) { e }
  )

  # return NULL if fitting failed
  if (!inherits(fit, "simpleError") && !inherits(try(summary(fit), silent = TRUE), "try-error")) {
    # Extract coefficients
    coef <- as.data.frame(coef(summary(fit)))
  } else {
    if (settings$verbose)
      .throw_message("Unable to fit the data. ",
              "Original error from minpack.lm::nlsLM(): ", fit$message)

    # Fill with NA values
    coef <- data.frame(
      "Estimate" = rep(NA, 3),
      "Std. Error" = rep(NA, 3),
      row.names = c("age", "sigmaphi", "mu"), check.names = FALSE
    )
  }

  ## RESULTS ----
  summary <- data.frame(
    age = if (is.null(age)) coef["age", "Estimate"] else age,
    age_error = coef["age", "Std. Error"],
    sigmaphi = if (is.null(sigmaphi)) coef["sigmaphi", "Estimate"] else sigmaphi,
    sigmaphi_error = coef["sigmaphi", "Std. Error"],
    mu = if (is.null(mu)) coef["mu", "Estimate"] else mu,
    mu_error = coef["mu", "Std. Error"]
  )

  ## Create RLum.Results object
  results <- set_RLum(class = "RLum.Results",
                      originator = "fit_SurfaceExposure",
                      data = list(summary = summary,
                                  data = data,
                                  fit = fit,
                                  args = as.list(sys.call()[-1]),
                                  call = sys.call()),
                      info = settings$info
  )

  ## PLOT ----
  if (plot) {

    # remove $group column for easier data handling
    if (global_fit)
      data$group <- NULL

    # re-order x,y columns
    if (coord_flip)
      data <- data.frame(data$y, data$x)

    # set default plot settings
    plot_settings <- list(
      x = data,
      main = "",
      pch = 21,
      col = "black",
      bg = "red",
      xlab = if (!coord_flip) "Depth (mm)" else "OSL intensity (Ln/Tn)",
      ylab = if (!coord_flip) "OSL intensity (Ln/Tn)" else "Depth (mm)",
      cex = 1.0,
      lty = 1,
      lwd = 1,
      log = "",
      ylim = if (!coord_flip) range(pretty(data[ ,2])) else rev(range(pretty(data[ ,2]))),
      xlim = range(pretty(data[ ,1]))
    )

    # override default settings with valid arguments in ...
    plot_settings <- modifyList(plot_settings, list(...))
    valid_settings <- c(names(par()), formalArgs("title"), formalArgs("plot.default"), "cex")
    plot_settings <- plot_settings[names(plot_settings) %in% valid_settings]

    # set global plot settings
    par(cex = plot_settings$cex)

    if (grepl("y", plot_settings$log)) {
      plot_settings$ylim[1] <- 0.01
      pos.idx <- which(data[, 2] > 0)
      error <- error[pos.idx]
      plot_settings$x <- data[pos.idx, ]
    }

    ## create main plot
    do.call("plot", modifyList(plot_settings, list(x = NA)))


    ## add data points
    if (!global_fit) {
      points(data, type = "p", pch = plot_settings$pch, bg = plot_settings$bg, col = plot_settings$col)
    } else {
      Map(function(d, i) {
        points(d, type = "p", pch = plot_settings$pch, bg = i, col = plot_settings$col)
      }, split(results$data, results$data$group), 1:length(unique(results$data$group)))
    }

    ## add fitted curve
    if (!inherits(fit, "error") && !inherits(fit, "simpleError")) {

      if (coord_flip) {
        oldx <- data[ ,2]
      } else {
        oldx <- data[ ,1]
      }

      newx <- seq(range(oldx)[1], range(oldx)[2], length.out = 10000)
      newy <- suppressWarnings(predict(fit, newdata = list(x = newx)))

      if (coord_flip) {
        tmp <- newx
        newx <- newy
        newy <- tmp
      }

      if (!global_fit) {
        points(newx, newy,
               type = "l",
               col = ifelse("line_col" %in% names(list(...)), list(...)$line_col, "blue"),
               lty = ifelse("line_lty" %in% names(list(...)), list(...)$line_lty, 1),
               lwd = ifelse("line_lwd" %in% names(list(...)), list(...)$line_lwd, 1))
      } else {
        for (i in 1:length(data_list)) {
          seg <- seq(i * 101 - 100, 10000, nrow(data))
          points(newx[seg], newy[seg],
                 type = "l",
                 col = ifelse("line_col" %in% names(list(...)), list(...)$line_col, i),
                 lty = ifelse("line_lty" %in% names(list(...)), list(...)$line_lty, 1),
                 lwd = ifelse("line_lwd" %in% names(list(...)), list(...)$line_lwd, 1))
        }
      }

    } else {
      legend("center", legend = "Unable to fit the data!\t\t")
    }

    # add error bars (if weighted fit)
    if (!is.null(error) && error_bars) {
      segments(plot_settings$x[ ,1], plot_settings$x[ ,2] - error,
               plot_settings$x[ ,1], plot_settings$x[ ,2] + error)

    }

    # add formula
    if (legend && !inherits(fit, "simpleError")) {
      formula_text <- paste0("y = ", as.character(fit$m$formula())[3])

      if (!is.null(age)) {
        if (!global_fit) {
          formula_text <- gsub("age", age, formula_text)
        } else {
          formula_text <- gsub("age", paste0("[", paste(age, collapse = "|"), "]"), formula_text)
          formula_text <- gsub("\\[group\\]", "", formula_text)
        }
      }
      if (!is.null(sigmaphi))
        formula_text <- gsub("sigmaphi", sigmaphi, formula_text)
      if (!is.null(mu))
        formula_text <- gsub("mu", mu, formula_text)

      legend(ifelse(coord_flip, "bottomleft", "bottomright"), legend = formula_text, cex = 0.8, bty = "n")
    }
  }

  ## CONSOLE ----
  if (settings$verbose) {
    cat("\n [fit_SurfaceExposure()] \n\n")

    if (!global_fit) {
      ## STANDARD OUTPUT
      cat(" Estimated paramater(s):\n",
          "-----------------------\n")
      if (is.null(age))
        cat(paste0(" age (a):\t", signif(results$summary$age, 3), " \u00B1 ",
                   signif(results$summary$age_error, 3), "\n"))
      if (is.null(sigmaphi))
        cat(paste0(" sigmaphi:\t", signif(results$summary$sigmaphi, 3), " \u00B1 ",
                   signif(results$summary$sigmaphi_error, 3), "\n"))
      if (is.null(mu))
        cat(paste0(" mu:\t\t", signif(results$summary$mu, 3), " \u00B1 ",
                   signif(results$summary$mu_error, 3), "\n"))
      cat("\n")
    } else {
      ## GLOBAL FIT OUTPUT
      cat(" Shared estimated paramater(s):\n",
          "-----------------------\n")
      if (is.null(sigmaphi))
        cat(paste0(" sigmaphi:\t", signif(unique(results$summary$sigmaphi), 3), " \u00B1 ",
                   signif(unique(results$summary$sigmaphi_error), 3), "\n"))
      if (is.null(mu))
        cat(paste0(" mu:\t\t", signif(unique(results$summary$mu), 3), " \u00B1 ",
                   signif(unique(results$summary$mu_error), 3), "\n"))
      cat("\n")
    }

    ## STANDARD OUTPUT
    cat(" Fixed parameters(s):\n",
        "--------------------\n")
    if (!is.null(age))
      cat(paste0(" age (a):\t", .collapse(age, quote = FALSE), "\n"))
    if (!is.null(sigmaphi))
      cat(paste0(" sigmaphi:\t", sigmaphi, "\n"))
    if (!is.null(mu))
      cat(paste0(" mu:\t\t", mu, "\n"))
    cat("\n")

    if (!is.null(age)) {
      message(paste0("To apply the estimated parameters to a sample of unknown age run:\n\n",
                     "fit_SurfaceExposure(data = ", capture.output(results$args[[1]]),
                     ", sigmaphi = ", signif(unique(results$summary$sigmaphi), 3),
                     ", mu = ", signif(unique(results$summary$mu), 3),
                     ")\n\n"))
    }
  }

  ## EXIT ----
  return(results)
}
