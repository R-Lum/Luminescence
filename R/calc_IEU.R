#' Apply the internal-external-uncertainty (IEU) model after Thomsen et al.
#' (2007) to a given De distribution
#'
#' Function to calculate the IEU De for a De data set.
#'
#' This function uses the equations of Thomsen et al. (2007). The parameters a
#' and b are estimated from dose-recovery experiments.
#'
#' @param data [RLum.Results-class] or [data.frame] (**required**):
#' for [data.frame]: two columns with De `(data[,1])` and
#' De error `(values[,2])`
#'
#' @param a [numeric] (**required**):
#' slope
#'
#' @param b [numeric] (**required**):
#' intercept
#'
#' @param interval [numeric] (**required**):
#' fixed interval (e.g. 5 Gy) used for iteration of `Dbar`, from the mean to
#' Lowest.De used to create Graph.IEU `[Dbar.Fixed vs Z]`
#'
#' @param decimal.point [numeric] (*with default*):
#' number of decimal points for rounding calculations (e.g. 2)
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... further arguments (`trace, verbose`).
#'
#' @return
#' Returns a plot (*optional*) and terminal output. In addition an
#' [RLum.Results-class] object is returned containing the
#' following elements:
#'
#' \item{.$summary}{[data.frame] summary of all relevant model results.}
#' \item{.$data}{[data.frame] original input data}
#' \item{.$args}{[list] used arguments}
#' \item{.$call}{[call] the function call}
#' \item{.$tables}{[list] a list of data frames containing all calculation tables}
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Rachel Smedley, Geography & Earth Sciences, Aberystwyth University (United Kingdom) \cr
#' Based on an excel spreadsheet and accompanying macro written by Kristina Thomsen. \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [plot], [calc_CommonDose], [calc_CentralDose], [calc_FiniteMixture],
#' [calc_FuchsLang2001], [calc_MinDose]
#'
#' @references
#' Smedley, R.K., 2015. A new R function for the Internal External Uncertainty (IEU) model.
#' Ancient TL 33, 16-21.
#'
#' Thomsen, K.J., Murray, A.S., Boetter-Jensen, L. & Kinahan, J.,
#' 2007. Determination of burial dose in incompletely bleached fluvial samples
#' using single grains of quartz. Radiation Measurements 42, 370-379.
#'
#' @examples
#'
#' ## load data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## apply the IEU model
#' ieu <- calc_IEU(ExampleData.DeValues$CA1, a = 0.2, b = 1.9, interval = 1)
#'
#' @md
#' @export
calc_IEU <- function(
  data,
  a,
  b,
  interval,
  decimal.point = 2,
  plot = TRUE,
  ...
) {
  .set_function_name("calc_IEU")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("data.frame", "RLum.Results"))
  if (inherits(data, "RLum.Results")) {
    data <- get_RLum(data)
  }

  ## check that we actually have data
  if (length(data) == 0 || nrow(data) == 0) {
    .throw_error("'data' contains no data")
  }
  if (ncol(data) < 2) {
    .throw_error("'data' should have at least two columns")
  }
  data <- data[, 1:2]
  colnames(data) <- c("De", "De.Error")

  .validate_class(a, "numeric")
  .validate_class(b, "numeric")
  .validate_class(interval, "numeric")

  ##==========================================================================##
  ## ... ARGUMENTS
  ##==========================================================================##
  extraArgs <- list(...)

  ## console output
  verbose <- TRUE
  if ("verbose" %in% names(extraArgs)) {
    verbose <- extraArgs$verbose
  }

  ## trace calculations
  trace <- FALSE
  if ("trace" %in% names(extraArgs)) {
    trace <- extraArgs$trace
  }
  # TODO: main, xlab, ylab, xlim, ylim, pch, col


  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##

  main.calculation <- function(data, Dbar, round.z = FALSE,
                               plot = FALSE, trace = FALSE) {
    N <- nrow(data)
    De <- data$De
    De.Total.Error <- sqrt(data$De.Error^2 + (a * Dbar + b)^2)
    Inv.De.Total.Error.Sq <- 1 / De.Total.Error^2
    Z.top <- cumsum(Inv.De.Total.Error.Sq * De)
    Z.bot <- cumsum(Inv.De.Total.Error.Sq)
    Z <- Z.top / Z.bot
    if (round.z)
      Z <- round(Z, decimal.point)

    EXT.top <- NULL
    for (j in 1:N) {
      x <- (De[1:j] - Z[j])^2 * Inv.De.Total.Error.Sq[1:j]
      EXT.top <- c(EXT.top, sum(x))
    }
    EXT.bot <- (1:N - 1) * Z.bot
    EXT <- EXT.top / EXT.bot
    INT <- 1 / Z.bot
    R <- sqrt(INT / EXT)
    R.Error <- (2 * (1:N - 1))^(-0.5)
    Rank.number <- 1:N
    Table.IEU <- data.table(Rank.number = Rank.number,
                            De = De, De.Error = De.Total.Error,
                            Z, EXT.top, EXT, INT, R, R.Error)

    ## to reduce the number of plots and increase performance,
    ## intermediate calculations are plotted only when `trace = TRUE`
    if (plot && trace) {
      do.plot(Table.IEU$Z, Table.IEU$R, Table.IEU$R.Error)
    }

    Max <- Table.IEU[R >= 1, suppressWarnings(max(Rank.number, na.rm = TRUE))]
    if (is.infinite(Max)) {
      Max <- 1
    }
    Above <- Table.IEU[Max]
    Below <- Table.IEU[Max + 1]
    Slope <- (Above$R - Below$R) / (Above$Z - Below$Z)
    Intercept <- Above$R - (Slope * Above$Z)
    IEU.De <- round((1 - Intercept) / Slope, decimal.point)
    IEU.Error <- max(sqrt(Above$EXT), sqrt(Below$EXT))
    IEU.Error <- round(IEU.Error, decimal.point)
    n <- Max + 1

    if (trace) {
      message(sprintf("[Iteration of Dbar] \n Dbar: %.4f \n IEU.De: %.4f \n IEU.Error: %.4f \n n: %i \n R: %.4f \n",
                      Dbar, IEU.De, IEU.Error, n, Below$R))
    }

    Dbar.Fixed <- Dbar - interval
    return(list(Dbar.Mean = c(1, Dbar, Dbar.Fixed, IEU.De, IEU.Error,
                              n, Below$R, a, b),
                Table.IEU = Table.IEU))
  }

  do.plot <- function(x.vals, y.vals, y.errs,
                      xlab = "Z [Gy]",
                      ylab = expression(paste("R = [", alpha["in"], "/", alpha["ex"],"]")),
                      abline.vals = c(1, 0),
                      asp = NA) {
    ymin <- min((y.vals - y.errs)[-1], na.rm = TRUE)
    ymax <- max((y.vals + y.errs)[-1], na.rm = TRUE)

    plot(x.vals, y.vals, type = "b", xlab = xlab, ylab = ylab,
         ylim = c(min(ymin, 0), ymax),
         asp = asp)

    graphics::arrows(x.vals, y.vals + y.errs, x.vals, y.vals - y.errs,
                    col = 1, angle = 90, length = 0.05, code = 3)

    abline(abline.vals[1], abline.vals[2], untf = FALSE, lty = 3)
  }

  ## ------------------------------------------------------------------------
  ## Start of the algorithm

  # (a) Calculate IEU at fixed intervals of Dbar starting from the Mean and
  # subtracting the interval until Dbar is < Lowest.De; this creates a plot
  data <- data[order(data$De), ]
  Lowest.De <- round(data$De[1], decimal.point)
  Dbar <- round(mean(data$De), decimal.point)

  Dbar.Mean <- main.calculation(data, Dbar)$Dbar.Mean
  Fixed.Iterations <- rbind(data.frame(matrix(nrow = 0, ncol = 9)),
                            Dbar.Mean)
  repeat {
    Dbar.Fixed <- Dbar.Mean[3]
    if (Dbar.Fixed < Lowest.De) {
      break
    }
    Dbar <- Dbar.Fixed
    Dbar.Mean <- main.calculation(data, Dbar)$Dbar.Mean
    Fixed.Iterations <- rbind(Fixed.Iterations, Dbar.Mean)
  }

  colnames(Fixed.Iterations) <- c(FALSE, "Dbar", "Dbar.Fixed", "Zbar",
                                  "Zbar.Error", "n", "Below.R", "a", "b")

  if (plot) {
    do.plot(Fixed.Iterations$Dbar,
            Fixed.Iterations$Zbar,
            Fixed.Iterations$Zbar.Error,
            xlab = "Dbar (Gy)", ylab = "Zbar, weighted mean (Gy)",
            abline.vals = c(0, 1), asp = 1)
  }

  # (b) Calculate Dbar by iteration from [Dbar = Lowest.De] until [IEU.De = Dbar];
  # this calculates the IEU De
  Dbar <- Lowest.De
  res <- main.calculation(data, Dbar)
  Dbar.Mean <- res$Dbar.Mean

  repeat {
    IEU.De <- Dbar.Mean[4]
    if (is.na(IEU.De)) {
      .throw_warning("Numerical error, try changing your 'a' and 'b' values")
      break
    }
    if (IEU.De <= Dbar) {
      break
    }
    Dbar <- IEU.De
    res <- main.calculation(data, Dbar, round.z = TRUE, plot, trace)
    Dbar.Mean <- res$Dbar.Mean
  }

  Dbar <- Dbar.Mean[2]
  IEU.De <- Dbar.Mean[4]
  IEU.Error <- Dbar.Mean[5]
  n <- Dbar.Mean[6]
  Table.IEU <- res$Table.IEU

  # final plot
  if (plot) {
    do.plot(Table.IEU$Z, Table.IEU$R, Table.IEU$R.Error)
  }

  Table.Results <- data.frame(Dbar, IEU.De, IEU.Error, n, a, b)
  colnames(Table.Results) <- c("Dbar", "IEU.De (Gy)", "IEU.Error (Gy)",
                               "Number of De", "a", "b")

  ##==========================================================================##
  ## TERMINAL OUTPUT
  ##==========================================================================##
  if (verbose) {
    message(sprintf(
      "\n [calc_IEU] \n\n Dbar: %.2f \n IEU.De (Gy): %.2f \n IEU.Error (Gy): %.2f Number of De: %.0f \n a: %.4f \n b: %.4f",
      Table.Results[1], Table.Results[2], Table.Results[3],
      Table.Results[4], Table.Results[5], Table.Results[6]))
  }

  ##==========================================================================##
  ## RETURN VALUES
  ##==========================================================================##
  summary <- Table.Results[ ,c(-1, -5, -6)]
  colnames(summary) <- c("de", "de_err", "n")

  call <- sys.call()
  args <- list(a = a, b = b, interval = interval,
               decimal.point = decimal.point, plot = plot)

  newRLumResults.calc_IEU <- set_RLum(
    class = "RLum.Results",
    data = list(summary = summary,
                data = data,
                args = args,
                call = call,
                tables = list(
                  Table.IEUCalculations = as.data.frame(Table.IEU),
                  Table.Fixed.Iteration = Fixed.Iterations,
                  Table.IEUResults = Table.Results
                )))

  invisible(newRLumResults.calc_IEU)
}
