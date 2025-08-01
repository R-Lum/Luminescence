#' @title Visualise natural/regenerated signal ratios
#'
#' @description
#' This function creates a Natural/Regenerated signal vs. time (NR(t)) plot
#' as shown in Steffen et al. 2009.
#'
#' This function accepts the individual curve data in many different formats. If
#' `data` is a `list`, each element of the list must contain a two
#' column `data.frame` or `matrix` containing the `XY` data of the curves
#' (time and counts). Alternatively, the elements can be objects of class
#' [RLum.Data.Curve-class].
#'
#' Input values can also be provided as a `data.frame` or `matrix` where
#' the first column contains the time values and each following column contains
#' the counts of each curve.
#'
#' @param data [list], [data.frame], [matrix] or [RLum.Analysis-class] (**required**):
#' X,Y data of measured values (time and counts). See details on individual data structure.
#'
#' @param log [character] (*optional*):
#' logarithmic axes (`c("x", "y", "xy")`).
#'
#' @param smooth [character] (*with default*):
#' apply data smoothing. If `"none"` (default), no data smoothing is applied.
#' Use `"rmean"` to calculate the rolling mean, where `k` determines the
#' width of the rolling window (see [data.table::frollmean]). `"spline"`
#' applies a smoothing spline to each curve (see [stats::smooth.spline])
#'
#' @param k [integer] (*with default*):
#' integer width of the rolling window.
#'
#' @param legend [logical] (*with default*):
#' enable/disable the plot legend.
#'
#' @param legend.pos [character] (*with default*):
#' keyword specifying the position of the legend (see [legend]).
#'
#' @param ... further parameters passed to [plot] (also see [par]).
#'
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @seealso [plot]
#'
#' @return Returns a plot and [RLum.Analysis-class] object.
#'
#' @references
#' Steffen, D., Preusser, F., Schlunegger, F., 2009. OSL quartz underestimation due to
#' unstable signal components. Quaternary Geochronology, 4, 353-362.
#'
#' @examples
#'
#' ## load example data
#' data("ExampleData.BINfileData", envir = environment())
#'
#' ## EXAMPLE 1
#'
#' ## convert Risoe.BINfileData object to RLum.Analysis object
#' data <- Risoe.BINfileData2RLum.Analysis(object = CWOSL.SAR.Data, pos = 8, ltype = "OSL")
#'
#' ## extract all OSL curves
#' allCurves <- get_RLum(data)
#'
#' ## keep only the natural and regenerated signal curves
#' pos <- seq(1, 9, 2)
#' curves <- allCurves[pos]
#'
#' ## plot a standard NR(t) plot
#' plot_NRt(curves)
#'
#' ## re-plot with rolling mean data smoothing
#' plot_NRt(curves, smooth = "rmean", k = 10)
#'
#' ## re-plot with a logarithmic x-axis
#' plot_NRt(curves, log = "x", smooth = "rmean", k = 5)
#'
#' ## re-plot with custom axes ranges
#' plot_NRt(curves, smooth = "rmean", k = 5,
#'          xlim = c(0.1, 5), ylim = c(0.4, 1.6),
#'          legend.pos = "bottomleft")
#'
#' ## re-plot with smoothing spline on log scale
#' plot_NRt(curves, smooth = "spline", log = "x",
#'          legend.pos = "top")
#'
#' ## EXAMPLE 2
#'
#' # you may also use this function to check whether all
#' # TD curves follow the same shape (making it a TnTx(t) plot).
#' posTD <- seq(2, 14, 2)
#' curves <- allCurves[posTD]
#'
#' plot_NRt(curves, main = "TnTx(t) Plot",
#'          smooth = "rmean", k = 20,
#'          ylab = "TD natural / TD regenerated",
#'          xlim = c(0, 20), legend = FALSE)
#'
#' ## EXAMPLE 3
#'
#' # extract data from all positions
#' data <- lapply(1:24, FUN = function(pos) {
#'    Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = pos, ltype = "OSL")
#' })
#'
#' # get individual curve data from each aliquot
#' aliquot <- lapply(data, get_RLum)
#'
#' # set graphical parameters
#' par(mfrow = c(2, 2))
#'
#' # create NR(t) plots for all aliquots
#' for (i in 1:length(aliquot)) {
#'    plot_NRt(aliquot[[i]][pos],
#'             main = paste0("Aliquot #", i),
#'             smooth = "rmean", k = 20,
#'             xlim = c(0, 10),
#'             cex = 0.6, legend.pos = "bottomleft")
#' }
#'
#' # reset graphical parameters
#' par(mfrow = c(1, 1))
#'
#'
#' @export
plot_NRt <- function(data, log = FALSE, smooth = c("none", "spline", "rmean"), k = 3,
                     legend = TRUE, legend.pos = "topright", ...) {
  .set_function_name("plot_NRt")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("list", "data.frame", "matrix", "RLum.Analysis"))
  if (inherits(data, "list")) {
    if (length(data) < 2)
      .throw_error("'data' contains only curve data for the natural signal")
    class.data <- sapply(data, function(x) class(x)[1])
    if (all(class.data == "RLum.Data.Curve") || all(class.data == "RLum.Analysis"))
      curves <- lapply(data, get_RLum)
    else if (all(class.data == "data.frame") || all(class.data == "matrix"))
      curves <- data
    else
      .throw_error("'data' doesn't contain the expected type of elements")
  }
  else if (inherits(data, "data.frame") || inherits(data, "matrix")) {
    if (ncol(data) < 3)
      .throw_error("'data' contains only curve data for the natural signal")
    if (is.matrix(data))
      data <- as.data.frame(data)
    curves <- apply(data[2:ncol(data)], MARGIN = 2, function(curve) {
      data.frame(data[ ,1], curve)
    })
  }
  else if (inherits(data, "RLum.Analysis")) {
    RLum.objects <- get_RLum(data, drop = FALSE)
    if (any(sapply(RLum.objects, class) != "RLum.Data.Curve"))
      .throw_error("The provided 'RLum.Analysis' object ",
                   "must exclusively contain 'RLum.Data.Curve' objects")
    curves <- lapply(RLum.objects, get_RLum)
    if (length(curves) < 2)
      .throw_error("'data' contains only curve data for the natural signal")
  }

  smooth <- .validate_args(smooth, c("none", "spline", "rmean"))

  ## BASIC SETTINGS ------
  natural <- curves[[1]]
  regCurves <- curves[2:length(curves)]
  time <- curves[[1]][ ,1]

  if (any(sapply(regCurves, nrow) != nrow(natural))) {
    .throw_error("The time values for the natural signal don't match ",
                 "those for the regenerated signal")
  }

  ## DATA TRANSFORMATION -----

  # calculate ratios
  NR <- lapply(regCurves, natural, FUN = function(reg, nat) {
    ratio <- nat[, 2] / reg[, 2]

    ## avoid infinities and NaNs
    ratio[is.infinite(ratio)] <- NA
    ratio[nat[, 2] == 0] <- 0
    ratio
  })

  # smooth spline
  if (smooth[1] == "spline") {
    NR <- lapply(NR, function(nr) { stats::smooth.spline(nr)$y })
  }
  if (smooth[1] == "rmean") {
    ## here we'd like to use the smoothed values with no fill: as .smoothing()
    ## relies on data.table::frollmean(), the only way to remove the fill
    ## is by setting `fill = NA` (which is already the default) and then
    ## omitting the NA values
    NR <- lapply(NR, function(nr) stats::na.omit(.smoothing(nr, k)))
    time <- stats::na.omit(.smoothing(time, k))
  }

  # normalise data
  NRnorm <- lapply(NR, FUN = function(nr) { nr / nr[1] })


  ## EXTRA ARGUMENTS -----

  # default values
  settings <- list(
    xlim = if (log == "x" || log ==  "xy") c(0.1, max(time)) else c(0, max(time)),
    ylim = range(pretty(c(min(sapply(NRnorm, min, na.rm = TRUE)),
                          max(sapply(NRnorm, max, na.rm = TRUE))))),
    xlab = "Time [s]",
    ylab = "Natural signal / Regenerated signal",
    cex = 1L,
    main = "NR(t) Plot")

  # override defaults with user settings
  settings <- modifyList(settings, list(...))


  ## PLOTTING ----------

  # set graphical parameter
  par(cex = settings$cex)

  # empty plot
  if (is.na(pmatch(log, c("x", "y", "xy"))))
    log <- ""

  do.call(plot, modifyList(list(x = NA, y = NA, log = log, xaxs = "i", yaxs = "i"),
                           settings))

  # horizontal line
  abline(h = 1, lty = 3, col = "grey")

  col <- 1:length(NRnorm)

  # add N/R lines
  mapply(FUN = function(curve, col) {
    points(time, curve, type = "l", col = col)
  }, NRnorm, col)

  # add legend
  if (legend) {
    labels <- paste0("N/R", 1:length(NRnorm))
    ncol <- ifelse(length(NRnorm) > 4, ceiling(length(NRnorm) / 4) , 1)
    legend(legend.pos, legend = labels, col = col, lty = 1, ncol = ncol, cex = 0.8, bty = "n")
  }

  ## RETURN VALUES ----
  obj <- set_RLum("RLum.Analysis", protocol = "UNKNOWN",
                  records = mapply(FUN = function(curve, id) {
                    set_RLum("RLum.Data.Curve",
                             recordType = paste0("N/R", id),
                             curveType = "NRt",
                             data = matrix(c(time, curve), ncol = 2),
                             info = list(
                               data = curves,
                               call = sys.call(-6L),
                               args = as.list(sys.call(-6L)[-1])
                             ))
                  }, NRnorm, seq_len(length(NRnorm)))
  )
  invisible(obj)
}
