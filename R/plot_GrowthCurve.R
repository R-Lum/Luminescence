#' @title Fit and plot a dose-response curve for luminescence data (Lx/Tx against dose)
#'
#' @description
#'
#' A dose-response curve is produced for luminescence measurements using a
#' regenerative or additive protocol as implemented in [fit_DoseResponseCurve]
#' and [plot_DoseResponseCurve]
#'
#' @param sample [data.frame] (**required**):
#' data frame with columns for `Dose`, `LxTx`, `LxTx.Error` and `TnTx`.
#' The column for the test dose response is optional, but requires `'TnTx'` as
#' column name if used. For exponential fits at least three dose points
#' (including the natural) should be provided. If `fit.method = "OTORX"` you have
#' to provide the test dose in the same unit as the dose in a column called `Test_Dose`.
#' The function searches explicitly for this column name.
#'
#' @param mode [character] (*with default*):
#' selects calculation mode of the function.
#' - `"interpolation"` (default) calculates the De by interpolation,
#' - `"extrapolation"` calculates the equivalent dose by extrapolation (useful for MAAD measurements) and
#' - `"alternate"` calculates no equivalent dose and just fits the data points.
#'
#' Please note that for option `"interpolation"` the first point is considered
#' as natural dose
#'
#' @param fit.method [character] (*with default*):
#' function used for fitting. Possible options are:
#' - `LIN`,
#' - `QDR`,
#' - `EXP`,
#' - `EXP OR LIN`,
#' - `EXP+LIN`,
#' - `EXP+EXP`,
#' - `GOK`,
#' - `OTOR`,
#' - `OTORX`
#'
#' See details in [fit_DoseResponseCurve].
#'
#' @param output.plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param output.plotExtended [logical] (*with default*):
#' If' `TRUE`, 3 plots on one plot area are provided:
#' 1. growth curve,
#' 2. histogram from Monte Carlo error simulation and
#' 3. a test dose response plot.
#'
#' If `FALSE`, just the growth curve will be plotted.
#'
#' @param plot_singlePanels [logical] (*with default*):
#' single plot output (`TRUE/FALSE`) to allow for plotting the results in
#' single plot windows. Requires `plotExtended = TRUE`.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param n.MC [integer] (*with default*):
#' number of MC runs for error calculation.
#'
#' @param ... Further arguments to [fit_DoseResponseCurve] (`fit_weights`,
#' `fit_bounds`, `fit.force_through_origin`, `fit.includingRepeatedRegPoints`,
#' `fit.NumberRegPoints`, `fit.NumberRegPointsReal`, `n.MC`,
#' `txtProgressBar`) and graphical parameters to be passed (supported:
#' `xlim`, `ylim`, `main`, `xlab`, `ylab`).
#'
#' @return
#' Along with a plot (if wanted) the `RLum.Results` object produced by
#' [fit_DoseResponseCurve] is returned.
#'
#' @section Function version: 1.2.2
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Michael Dietze, GFZ Potsdam (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#'
#' Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46.
#'
#' Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li, S.-H., Pagonis, P.,
#' Herman, F., 2015. Radiation-induced growth and isothermal decay of infrared-stimulated luminescence
#' from feldspar. Radiation Measurements 81, 224-231.
#'
#' Pagonis, V., Kitis, G., Chen, R., 2020. A new analytical equation for the dose response of dosimetric materials,
#' based on the Lambert W function. Journal of Luminescence 225, 117333. \doi{10.1016/j.jlumin.2020.117333}
#'
#' @seealso [fit_DoseResponseCurve], [plot_DoseResponseCurve]
#'
#' @examples
#'
#' ##(1) plot growth curve for a dummy dataset
#' data(ExampleData.LxTxData, envir = environment())
#' plot_GrowthCurve(LxTxData)
#'
#' ##(1b) horizontal plot arrangement
#' layout(mat = matrix(c(1,1,2,3), ncol = 2))
#' plot_GrowthCurve(LxTxData, plot_singlePanels = TRUE)
#'
#' ##(2) plot the growth curve with pdf output - uncomment to use
#' ##pdf(file = "~/Desktop/Growth_Curve_Dummy.pdf", paper = "special")
#' plot_GrowthCurve(LxTxData)
#' ##dev.off()
#'
#' ##(3) plot the growth curve with pdf output - uncomment to use, single output
#' ##pdf(file = "~/Desktop/Growth_Curve_Dummy.pdf", paper = "special")
#' temp <- plot_GrowthCurve(LxTxData, plot_singlePanels = TRUE)
#' ##dev.off()
#'
#' ##(4) plot resulting function for given interval x
#' x <- seq(1,10000, by = 100)
#' plot(
#'  x = x,
#'  y = eval(temp$Formula),
#'  type = "l"
#' )
#'
#' ##(5) plot using the 'extrapolation' mode
#' LxTxData[1,2:3] <- c(0.5, 0.001)
#' print(plot_GrowthCurve(LxTxData, mode = "extrapolation"))
#'
#' ##(6) plot using the 'alternate' mode
#' LxTxData[1,2:3] <- c(0.5, 0.001)
#' print(plot_GrowthCurve(LxTxData, mode = "alternate"))
#'
#' @md
#' @export
plot_GrowthCurve <- function(
  sample,
  mode = "interpolation",
  fit.method = "EXP",
  output.plot = TRUE,
  output.plotExtended = TRUE,
  plot_singlePanels = FALSE,
  verbose = TRUE,
  n.MC = 100,
  ...
) {
  .set_function_name("plot_GrowthCurve")
  on.exit(.unset_function_name(), add = TRUE)

  ## deprecated argument
  extraArgs <- list(...)
  if ("output.plotExtended.single" %in% names(extraArgs)) {
    plot_singlePanels <- extraArgs$output.plotExtended.single
    .throw_warning("'output.plotExtended.single' is deprecated, use ",
                   "'plot_singlePanels' instead")
  }
  if ("NumberIterations.MC" %in% names(extraArgs)) {
    n.MC <- extraArgs$NumberIterations.MC
    .throw_warning("'NumberIterations.MC' is deprecated, use ",
                   "'n.MC' instead")
  }

  ## input validation
  .validate_logical_scalar(output.plot)
  .validate_logical_scalar(output.plotExtended)
  .validate_logical_scalar(plot_singlePanels)
  .validate_logical_scalar(verbose)

  ## remaining input validation occurs inside the fitting function
  fit <- fit_DoseResponseCurve(sample, mode, fit.method,
                               verbose = verbose, n.MC = n.MC, ...)

  if (is.null(fit)) {
    if (verbose)
      message("[plot_GrowthCurve()] Fitting failed, no plot possible")
    return(NULL)
  }

  if (output.plot) {
    plot_DoseResponseCurve(fit, plot_extended = output.plotExtended,
                           plot_singlePanels = plot_singlePanels,
                           verbose = verbose, ...)
  }

  invisible(fit)
}
