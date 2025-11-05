#' @title Function to create a Radial Plot
#'
#' @description A Galbraith's radial plot is produced on a logarithmic or a linear scale.
#'
#' @details Details and the theoretical background of the radial plot are given in the
#' cited literature. This function is based on an S script of Rex Galbraith. To
#' reduce the manual adjustments, the function has been rewritten. Thanks to
#' Rex Galbraith for useful comments on this function. \cr
#' Plotting can be disabled by adding the argument `plot = "FALSE"`, e.g.
#' to return only numeric plot output.
#'
#' Earlier versions of the Radial Plot in this package had the 2-sigma-bar
#' drawn onto the z-axis. However, this might have caused misunderstanding in
#' that the 2-sigma range may also refer to the z-scale, which it does not!
#' Rather it applies only to the x-y-coordinate system (standardised error vs.
#' precision). A spread in doses or ages must be drawn as lines originating at
#' zero precision (x0) and zero standardised estimate (y0). Such a range may be
#' drawn by adding lines to the radial plot ( `line`, `line.col`,
#' `line.label`, cf. examples).
#'
#' A statistic summary, i.e. a collection of statistic measures of
#' centrality and dispersion (and further measures) can be added by specifying
#' one or more of the following keywords:
#' - `"n"` (number of samples),
#' - `"mean"` (mean De value),
#' - `"mean.weighted"` (error-weighted mean),
#' - `"median"` (median of the De values),
#' - `"median.weighted"` (error-weighted median),
#' - `"sdrel"` (relative standard deviation in percent),
#' - `"sdrel.weighted"` (error-weighted relative standard deviation in percent),
#' - `"sdabs"` (absolute standard deviation),
#' - `"sdabs.weighted"` (error-weighted absolute standard deviation),
#' - `"serel"` (relative standard error),
#' - `"serel.weighted"` (error-weighted relative standard error),
#' - `"seabs"` (absolute standard error),
#' - `"seabs.weighted"` (error-weighted absolute standard error),
#' - `"in.2s"` (percent of samples in 2-sigma range),
#' - `"kurtosis"` (kurtosis) and
#' - `"skewness"` (skewness).
#'
#' @param data [data.frame] or [RLum.Results-class] object (**required**):
#' for `data.frame`: either two columns: De (`data[,1]`) and De error
#' (`data[,2]`), or one: De (`values[,1]`). If a single-column data frame
#' is provided, De error is assumed to be 10^-9 for all measurements.
#' To plot several data sets in one plot, the data sets must be provided as
#' `list`, e.g. `list(data.1, data.2)`.
#'
#' @param na.rm [logical] (*with default*):
#' excludes `NA` values from the data set prior to any further operations.
#'
#' @param log.z [logical] (*with default*):
#' Option to display the z-axis in logarithmic scale. Default is `TRUE`.
#'
#' @param central.value [numeric]:
#' User-defined central value, primarily used for horizontal centring
#' of the z-axis.
#'
#' @param centrality [character] or [numeric] (*with default*):
#' measure of centrality, used for automatically centring the plot and drawing
#' the central line. Can either be one out of
#' - `"mean"`,
#' - `"median"`,
#' - `"mean.weighted"` and
#' - `"median.weighted"` or a
#' - numeric value used for the standardisation.
#'
#' @param mtext [character] (*with default*):
#' additional text below the plot title.
#'
#' @param summary [character] (*with default*):
#' add statistic measures of centrality and dispersion to the plot.
#' Can be one or more of several keywords. See details for available keywords.
#'
#' @param summary.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`)
#' for the statistical summary. Alternatively, the keyword `"sub"` may be
#' specified to place the summary below the plot header. However, this latter
#' option is only possible if `mtext` is not used.
#'
#' @param legend [character] vector (*optional*):
#' legend content to be added to the plot.
#'
#' @param legend.pos [numeric] or [character] (*with default*):
#' optional legend position coordinates or keyword (e.g. `"topright"`).
#'
#' @param stats [character] (*optional*):
#' additional labels of statistically important values in the plot. It can be
#' one or more of `"min"`, `"max"` and `"median"`; any other values will be
#' ignored.
#'
#' @param rug [logical]:
#' Option to add a rug to the z-scale, to indicate the location of individual values
#'
#' @param plot.ratio [numeric]:
#' User-defined plot area ratio (i.e. curvature of the z-axis). If omitted,
#' the default value (`4.5/5.5`) is used and modified automatically to optimise
#' the z-axis curvature. The parameter should be decreased when data points
#' are plotted outside the z-axis or when the z-axis gets too elliptic.
#'
#' @param bar.col [character] or [numeric] (*with default*):
#' colour of the bar showing the 2-sigma range around the central
#' value. To disable the bar, use `"none"`. Default is `"grey"`.
#'
#' @param y.ticks [logical]:
#' Option to hide y-axis labels. Useful for data with small scatter.
#'
#' @param grid.col [character] or [numeric] (*with default*):
#' colour of the grid lines (originating at `[0,0]` and stretching to
#' the z-scale). To disable grid lines, use `"none"`. Default is `"grey"`.
#'
#' @param line [numeric]:
#' numeric values of the additional lines to be added.
#'
#' @param line.col [character] or [numeric]:
#' colour of the additional lines.
#'
#' @param line.label [character]:
#' labels for the additional lines.
#'
#' @param output [logical]:
#' Optional output of numerical plot parameters. These can be useful to
#' reproduce similar plots. Default is `FALSE`.
#'
#' @param ... Further plot arguments to pass. `xlab` must be a vector of
#' length 2, specifying the upper and lower x-axes labels.
#'
#' @return Returns a plot object.
#'
#' @section Function version: 0.5.10
#'
#' @author
#' Michael Dietze, GFZ Potsdam (Germany)\cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Based on a rewritten S script of Rex Galbraith, 2010
#'
#' @seealso [plot], [plot_KDE], [plot_Histogram], [plot_AbanicoPlot]
#'
#' @references
#' Galbraith, R.F., 1988. Graphical Display of Estimates Having
#' Differing Standard Errors. Technometrics, 30 (3), 271-281.
#'
#' Galbraith, R.F., 1990. The radial plot: Graphical assessment of spread in
#' ages. International Journal of Radiation Applications and Instrumentation.
#' Part D. Nuclear Tracks and Radiation Measurements, 17 (3), 207-214.
#'
#' Galbraith, R. & Green, P., 1990. Estimating the component ages in a finite
#' mixture. International Journal of Radiation Applications and
#' Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 (3)
#' 197-206.
#'
#' Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
#' track ages. Nuclear Tracks And Radiation Measurements, 21 (4), 459-470.
#'
#' Galbraith, R.F., 1994. Some Applications of Radial Plots. Journal of the
#' American Statistical Association, 89 (428), 1232-1242.
#'
#' Galbraith, R.F., 2010. On plotting OSL equivalent doses. Ancient TL, 28 (1),
#' 1-10.
#'
#' Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent
#' dose and error calculation and display in OSL dating: An overview and some
#' recommendations. Quaternary Geochronology, 11, 1-27.
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#' ExampleData.DeValues <- convert_Second2Gray(
#'   ExampleData.DeValues$BT998, c(0.0438,0.0019))
#'
#' ## plot the example data straightforward
#' plot_RadialPlot(data = ExampleData.DeValues)
#'
#' ## now with linear z-scale
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   log.z = FALSE)
#'
#' ## now with output of the plot parameters
#' plot1 <- plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   log.z = FALSE,
#'   output = TRUE)
#' plot1
#' plot1$zlim
#'
#' ## now with adjusted z-scale limits
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   log.z = FALSE,
#'   xlim = c(0, 5),
#'   zlim = c(100, 200))
#'
#' ## now the two plots with serious but seasonally changing fun
#' #plot_RadialPlot(data = data.3, fun = TRUE)
#'
#' ## now with user-defined central value, in log-scale again
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   central.value = 150)
#'
#' ## now with a rug, indicating individual De values at the z-scale
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   rug = TRUE)
#'
#' ## now with legend, colour, different points and smaller scale
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   legend = "Sample 1",
#'   col = "tomato4",
#'   bar.col = "peachpuff",
#'   pch = "R",
#'   cex = 0.8)
#'
#' ## now without 2-sigma bar, y-axis, grid lines and central value line
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   bar.col = "none",
#'   grid.col = "none",
#'   y.ticks = FALSE,
#'   lwd = 0)
#'
#' ## now with user-defined axes labels
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   xlab = c("Data error (%)", "Data precision"),
#'   ylab = "Scatter",
#'   zlab = "Equivalent dose [Gy]")
#'
#' ## now with minimum, maximum and median value indicated
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   central.value = 150,
#'   stats = c("min", "max", "median"))
#'
#' ## now with a brief statistical summary
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   summary = c("n", "in.2s"))
#'
#' ## now with another statistical summary as subheader
#' plot_RadialPlot(
#'   data = ExampleData.DeValues,
#'   summary = c("mean.weighted", "median"),
#'   summary.pos = "sub")
#'
#' ## now the data set is split into sub-groups, one is manipulated
#' data.1 <- ExampleData.DeValues[1:15,]
#' data.2 <- ExampleData.DeValues[16:25,] * 1.3
#'
#' ## now a common dataset is created from the two subgroups
#' data.3 <- list(data.1, data.2)
#'
#' ## now the two data sets are plotted in one plot
#' plot_RadialPlot(data = data.3)
#'
#' ## now with some graphical modification
#' plot_RadialPlot(
#'   data = data.3,
#'   col = c("darkblue", "darkgreen"),
#'   bar.col = c("lightblue", "lightgreen"),
#'   pch = c(2, 6),
#'   summary = c("n", "in.2s"),
#'   summary.pos = "sub",
#'   legend = c("Sample 1", "Sample 2"))
#'
#' @export
plot_RadialPlot <- function(
  data,
  na.rm = TRUE,
  log.z = TRUE,
  central.value,
  centrality = "mean.weighted",
  mtext = "",
  summary = c("n", "in.2s"),
  summary.pos = "sub",
  legend = NULL,
  legend.pos = "topright",
  stats = "none",
  rug = FALSE,
  plot.ratio,
  bar.col,
  y.ticks = TRUE,
  grid.col,
  line,
  line.col,
  line.label,
  output = FALSE,
  ...
) {
  .set_function_name("plot_RadialPlot")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_not_empty(data)
  .validate_logical_scalar(log.z)
  .validate_class(centrality, c("character", "numeric"))
  if (is.character(centrality)) {
    centrality <- .validate_args(centrality, c("mean", "mean.weighted",
                                               "median", "median.weighted"))
  }

  ## Homogenise input data format
  if (!inherits(data, "list"))
      data <- list(data)

  ## Check input data
  for(i in 1:length(data)) {
    .validate_class(data[[i]], c("data.frame", "RLum.Results"),
                    name = "All elements of 'data'")

    if (inherits(data[[i]], "RLum.Results")) {
        data[[i]] <- get_RLum(data[[i]], "data")
    }

    ## ensure that the dataset it not degenerate
    .validate_not_empty(data[[i]],
                        name = paste0("Input 'data[[", i, "]]'"))

      ## if `data[[i]]` is a single-column data frame, append a second
      ## column with a small non-zero value (10^-9 for consistency with
      ## what `calc_Statistics() does)
      if (ncol(data[[i]]) < 2) {
        data[[i]] <- data.frame(data[[i]], 10^-9)
      } else if (ncol(data[[i]]) > 2) {
        ## keep only the first two columns
        data[[i]] <- data[[i]][, 1:2]
      }
  }

  valid.pos <- c("left", "center", "right", "topleft", "top", "topright",
                 "bottomleft", "bottom", "bottomright")
  .validate_class(summary, "character")
  if (is.numeric(summary.pos)) {
    .validate_length(summary.pos, 2)
  }
  else {
    summary.pos <- .validate_args(summary.pos, c("sub", valid.pos))
  }
  .validate_class(legend, "character", null.ok = TRUE)
  if (is.numeric(legend.pos)) {
    .validate_length(legend.pos, 2)
  } else {
    legend.pos <- .validate_args(legend.pos, valid.pos)
  }

  .validate_class(stats, "character")
  .validate_logical_scalar(rug)

  if (missing(bar.col)) {
    bar.col <- rep("grey80", length(data))
  }
  if (missing(grid.col)) {
    grid.col <- rep("grey70", length(data))
  }

  ## check z-axis log-option for grouped data sets
  if (inherits(data, "list") && length(data) > 1 && !log.z) {
    .throw_warning("'log.z' is set to 'FALSE' altough ",
                   "more than one data set (group) is provided")
  }

  ## optionally, remove NA-values
  if (na.rm) {
    for(i in 1:length(data)) {
      data[[i]] <- na.exclude(data[[i]])
      if (nrow(data[[i]]) == 0)
        .throw_error("After NA removal, nothing is left from data set ", i)
    }
  }

  ## create preliminary global data set
  De.global <- unlist(lapply(data, function(x) x[, 1]))

  ## calculate major preliminary tick values and tick difference
  extraArgs <- list(...)

  ## calculate correction dose to shift non-positive values
  De.add <- 0
  if (log.z && min(De.global) <= 0) {
    if("zlim" %in% names(extraArgs)) {
      De.add <- abs(extraArgs$zlim[1])
    } else {
      ## exclude zeros, as they cause infinities when logged
      De.global.not0 <- De.global[De.global != 0]

      ## estimate delta De to add to all data
      De.add <- min(10^ceiling(log10(abs(De.global.not0))) * 10)

      ## optionally readjust delta De for extreme values
      if (De.add <= abs(min(De.global.not0))) {
        De.add <- De.add * 10
      }
    }

    ## add correction dose to data set and adjust error
    for(i in 1:length(data))
      data[[i]][,1] <- data[[i]][,1] + De.add

    De.global <- De.global + De.add
  }

  ## calculate and append statistical measures --------------------------------

  ## z-values and se based on log-option
  data <- lapply(data, function(x, De.add) {
    cbind(x,
          z = if (log.z) log(x[, 1]) else x[, 1],
          se = if (log.z) x[, 2] / (x[, 1] + De.add) else x[, 2])
  }, De.add = De.add)

  ## calculate central values
  data <- lapply(data, function(x) {
    cbind(x,
          z.central = switch(as.character(centrality[1]),
                             mean = rep(mean(x[, 3], na.rm = TRUE), nrow(x)),
                             median = rep(median(x[, 3], na.rm = TRUE), nrow(x)),
                             mean.weighted = sum(x[, 3] / x[, 4]^2) / sum(1 / x[, 4]^2),
                             median.weighted = rep(.weighted.median(x[, 3], w = x[, 4]), nrow(x)),
                             if (is.numeric(centrality) && length(centrality) >= length(data)) {
                               rep(median(x[, 3], na.rm = TRUE), nrow(x))
                             } else NA)
          )
  })

  if (is.numeric(centrality) && length(centrality) == length(data)) {
    ## compute z.central, as this could not be done in the lapply before
    z.central.raw <- if (log.z) log(centrality + De.add) else centrality + De.add
    lapply(1:length(data), function(x) data[[x]][, 5] <<- rep(z.central.raw[x], nrow(data[[x]])))
  }

  ## calculate precision and standard estimate
  idx <- 0
  data <- lapply(data, function(x) {
    idx <<- idx + 1
    colnames(x) <- c("De", "error", "z", "se", "z.central")
    cbind(x,
          precision = 1 / x[, 4],
          std.estimate = (x[, 3] - x[, 5]) / x[, 4],
          std.estimate.plot = NA, # will be filled in further down
          .id = idx)
  })

  ## generate global data set
  data.global <- if (length(data) > 1) as.data.frame(rbindlist(data)) else data[[1]]

  ## calculate global central value
  z.central.global <- switch(as.character(centrality[1]),
                             mean = mean(data.global[, 3], na.rm = TRUE),
                             median = median(data.global[, 3], na.rm = TRUE),
                             mean.weighted = sum(data.global[, 3] / data.global[, 4]^2) / sum(1 / data.global[, 4]^2),
                             median.weighted = .weighted.median(data.global[, 3], w = data.global[, 4]),
                             if (is.numeric(centrality) && length(centrality) >= length(data)) {
                               mean(data.global[, 3], na.rm = TRUE)
                             } else NA)

  ## optionally adjust central value by user-defined value
  if (!missing(central.value)) {
    # ## adjust central value for De.add
    central.value <- central.value + De.add
    z.central.global <- ifelse(log.z,
                               log(central.value),
                               central.value)
  }

  ## re-calculate standardised estimate for plotting
  for(i in 1:length(data)) {
    data[[i]][,8] <- (data[[i]][,3] - z.central.global) / data[[i]][,4]
  }
  data.global$std.estimate.plot <- unlist(lapply(data, function(x) x[, 8]))

  ## print warning for too small scatter
  if (max(abs(1 / data.global[, 6])) < 0.02) {
    message("Attention, small standardised estimate scatter. ",
            "Toggle off y.ticks?")
  }

  ## read out additional arguments---------------------------------------------

  main <- extraArgs$main %||% expression(paste(D[e], " distribution"))
  sub <- extraArgs$sub %||% ""

  if("xlab" %in% names(extraArgs)) {
    xlab <- extraArgs$xlab
    .validate_length(xlab, 2)
  } else {
    xlab <- c(ifelse(log.z, "Relative standard error [%]", "Standard error"),
              "Precision")
  }

  ylab <- extraArgs$ylab %||% "Standardised estimate"
  zlab <- extraArgs$zlab %||% expression(paste(D[e], " [Gy]"))

  limits.z <- extraArgs$zlim %||% {
    z.span <- (mean(data.global[,1]) * 0.5) / (sd(data.global[,1]) * 100)
    z.span <- min(z.span, 0.89)
    c((0.9 - z.span) * min(data.global[[1]]),
      (1.1 + z.span) * max(data.global[[1]]))
  }
  .validate_length(limits.z, 2, name = "'zlim'")
  limits.z <- sort(limits.z)
  if (log.z && limits.z[1] <= 0) {
    limits.z[1] <- 0.01
  }

  limits.x <- extraArgs$xlim %||% c(0, max(data.global[,6]))
  if(limits.x[1] != 0) {
    limits.x[1] <- 0
    .throw_warning("Lower x-axis limit not set to zero, corrected")
  }

  limits.y <- extraArgs$ylim %||% {
    y.span <- (mean(data.global[,1]) * 10) / (sd(data.global[,1]) * 100)
    y.span <- ifelse(y.span > 1, 0.98, y.span)
    c(-(1 + y.span) * max(abs(data.global[, 7])),
      (0.8 + y.span) * max(abs(data.global[, 7])))
  }

  cex <- extraArgs$cex %||% 1
  lty <- extraArgs$lty %||% rep(2, length(data))
  lwd <- extraArgs$lwd %||% rep(1, length(data))
  pch <- extraArgs$pch %||% rep(1, length(data))
  col <- extraArgs$col %||% 1:length(data)
  .validate_positive_scalar(cex)
  .validate_length(lty, length(data))
  .validate_length(lwd, length(data))
  .validate_length(pch, length(data))
  .validate_length(col, length(data))

  tck <- if("tck" %in% names(extraArgs)) {
    .validate_length(extraArgs$tck, length(data), name = "'tck'")
    extraArgs$tck
  } else {
    NA
  }

  tcl <- extraArgs$tcl %||% -0.5
  show <- extraArgs$show %||% TRUE
  fun <- isTRUE(extraArgs$fun)

  ## define auxiliary plot parameters -----------------------------------------

  ## optionally adjust plot ratio
  if(missing(plot.ratio)) {
    if(log.z) {
      plot.ratio <- 1 /  (1 * ((max(data.global[,6]) - min(data.global[,6])) /
        (max(data.global[,7]) - min(data.global[,7]))))
    } else {
      plot.ratio <- 4.5 / 5.5
    }
  }

  ##limit plot ratio
  plot.ratio <- min(c(1e+06, plot.ratio))

  ## calculate conversion factor for plot coordinates
  f <- (max(data.global[,6]) - min(data.global[,6])) /
       (max(data.global[,7]) - min(data.global[,7])) * plot.ratio

  ## calculate major and minor z-tick values
  tick.values.major <- signif(pretty(limits.z, n = 5))
  tick.values.minor <- signif(pretty(limits.z, n = 25), 3)
  tick.values.major <- tick.values.major[between(tick.values.major,
                                                 min(tick.values.minor),
                                                 max(tick.values.minor))]
  tick.values.minor <- tick.values.minor[between(tick.values.minor,
                                                 limits.z[1], limits.z[2])]

  ## add ticks corresponding to the extremes of the z-axis but only if they
  ## fall outside of the major ticks
  tick.add <- signif(limits.z[!between(limits.z,
                                       min(tick.values.major),
                                       max(tick.values.major))])
  tick.values.major <- c(tick.add, tick.values.major)

  user.limits <- limits.z

  if (log.z) {
    tick.values.major[tick.values.major == 0] <- 1
    tick.values.minor[tick.values.minor == 0] <- 1
    tick.values.major <- log(tick.values.major)
    tick.values.minor <- log(tick.values.minor)
    user.limits <- log(user.limits)
  }

  ## calculate z-axis radius
  r.x <- limits.x[2] / max(data.global[,6]) + 0.05
  r <- max(sqrt((data.global[,6])^2+(data.global[,7] * f)^2)) * r.x

  ## compute the coordinates for points on the z-axis
  x.coord <- function(x) r / sqrt(1 + f^2 * (x - z.central.global)^2)
  y.coord <- function(x, x.coord) (x - z.central.global) * x.coord

  ## calculate major z-tick coordinates
  tick.x1.major <- x.coord(tick.values.major)
  tick.y1.major <- y.coord(tick.values.major, tick.x1.major)
  tick.x2.major <- (1 + 0.015 * cex) * tick.x1.major
  tick.y2.major <- y.coord(tick.values.major, tick.x2.major)
  ticks.major <- cbind(tick.x1.major, tick.x2.major,
                       tick.y1.major, tick.y2.major)

  ## calculate minor z-tick coordinates
  tick.x1.minor <- x.coord(tick.values.minor)
  tick.y1.minor <- y.coord(tick.values.minor, tick.x1.minor)
  tick.x2.minor <- (1 + 0.007 * cex) * tick.x1.minor
  tick.y2.minor <- y.coord(tick.values.minor, tick.x2.minor)
  ticks.minor <- cbind(tick.x1.minor,
                       tick.x2.minor,
                       tick.y1.minor,
                       tick.y2.minor)

  ## calculate z-label positions
  label.x <- 1.03 * x.coord(tick.values.major)
  label.y <- y.coord(tick.values.major, label.x)

  ## create z-axis labels
  label.z.text <- if (log.z)
                    signif(exp(tick.values.major), 3)
                  else
                    signif(tick.values.major, 3)

  ## to avoid overprinting we remove the z-axis labels at the extremes if
  ## they are too close to a major tick label (#1013)
  rm.idx <- NULL
  for (idx in seq_along(tick.add)) {
    dist <- sqrt((label.x[-idx] - label.x[idx])^2 +
                 (label.y[-idx] - label.y[idx])^2)
    if (any(dist < 1))
      rm.idx <- c(rm.idx, idx)
  }
  if (!is.null(rm.idx)) {
    label.x <- label.x[-rm.idx]
    label.y <- label.y[-rm.idx]
    label.z.text <- label.z.text[-rm.idx]
  }

  ## subtract De.add from label values
  label.z.text <- label.z.text - De.add

  labels <- cbind(label.x, label.y, label.z.text)

  ## calculate coordinates for 2-sigma-polygon overlay
  polygons <- matrix(nrow = length(data), ncol = 8)

  for(i in 1:length(data)) {
    polygons[i,1:4] <- c(limits.x[1],
                         limits.x[1],
                         max(data.global[,6]),
                         max(data.global[,6]))
    polygons[i,5:8] <- c(-2,
                         2,
                         (data[[i]][1,5] - z.central.global) *
                           polygons[i,3] + 2,
                         (data[[i]][1,5] - z.central.global) *
                           polygons[i,4] - 2)
  }

  ## calculate node coordinates for semi-circle
  num.values <- 500
  ellipse.values <- seq(
    from = min(c(tick.values.major, tick.values.minor, user.limits[1])),
    to = max(c(tick.values.major,tick.values.minor, user.limits[2])),
    length.out = num.values)
  ellipse.x <- x.coord(ellipse.values)
  ellipse.y <- y.coord(ellipse.values, ellipse.x)
  ellipse <- cbind(ellipse.x, ellipse.y)
  ellipse.lims <- rbind(ellipse[c(1, num.values), 1],
                        ellipse[c(1, num.values), 2])

  ## check if z-axis overlaps with 2s-polygon
  polygon_y_max <- max(polygons[,7])
  polygon_y_min <- min(polygons[,7])

  z_2s_upper <- ellipse.x[abs(ellipse.y - polygon_y_max) ==
                            min(abs(ellipse.y - polygon_y_max))]

  z_2s_lower <- ellipse.x[abs(ellipse.y - polygon_y_min) ==
                            min(abs(ellipse.y - polygon_y_min))]

  if ((max(polygons[, 3]) >= z_2s_upper || max(polygons[, 3]) >= z_2s_lower) &&
      bar.col[1] != "none") {
    .throw_warning("z-scale touches 2s-polygon, decrease plot ratio")
  }

  ## calculate statistical labels
  stats.data <- matrix(nrow = 3, ncol = 3)
  data.stats <- as.numeric(data.global[,1])

  if ("min" %in% stats) {
    stats.data[1, 3] <- data.stats[data.stats == min(data.stats)][1]
    stats.data[1, 1] <- data.global[data.stats == stats.data[1, 3], 6][1]
    stats.data[1, 2] <- data.global[data.stats == stats.data[1, 3], 8][1]
  }

  if ("max" %in% stats) {
    stats.data[2, 3] <- data.stats[data.stats == max(data.stats)][1]
    stats.data[2, 1] <- data.global[data.stats == stats.data[2, 3], 6][1]
    stats.data[2, 2] <- data.global[data.stats == stats.data[2, 3], 8][1]
  }

  if ("median" %in% stats) {
    stats.data[3, 3] <- data.stats[data.stats == quantile(data.stats, 0.5, type = 3)]
    stats.data[3, 1] <- data.global[data.stats == stats.data[3, 3], 6][1]
    stats.data[3, 2] <- data.global[data.stats == stats.data[3, 3], 8][1]
  }

  ## recalculate axes limits if necessary
  limits.z.x <- range(ellipse[,1])
  limits.z.y <- range(ellipse[,2])
  if(!("ylim" %in% names(extraArgs))) {
    if(limits.z.y[1] < 0.66 * limits.y[1]) {
      limits.y[1] <- 1.8 * limits.z.y[1]
    }
    if(limits.z.y[2] > 0.77 * limits.y[2]) {
      limits.y[2] <- 1.3 * limits.z.y[2]
    }
  }
  if (!("xlim" %in% names(extraArgs)) && limits.z.x[2] > 1.1 * limits.x[2]) {
      limits.x[2] <- limits.z.x[2]
  }

  ## calculate and paste statistical summary
  De.stats <- matrix(nrow = length(data), ncol = 18)
  colnames(De.stats) <- c("n", "mean", "mean.weighted", "median", "median.weighted",
    "kde.max", "sd.abs", "sd.rel", "se.abs", "se.rel", "q25", "q75", "skewness",
    "kurtosis", "sd.abs.weighted", "sd.rel.weighted", "se.abs.weighted",
    "se.rel.weighted")

  for(i in 1:length(data)) {
    data_to_stats <- data[[i]][,1:2]

    ## remove added De
    if(log.z) data_to_stats$De <- data_to_stats$De - De.add

    statistics <- calc_Statistics(data = data_to_stats)
    De.stats[i,1] <- statistics$weighted$n
    De.stats[i,2] <- statistics$unweighted$mean
    De.stats[i,3] <- statistics$weighted$mean
    De.stats[i,4] <- statistics$unweighted$median
    De.stats[i,5] <- statistics$weighted$median
    De.stats[i,7] <- statistics$unweighted$sd.abs
    De.stats[i,8] <- statistics$unweighted$sd.rel
    De.stats[i,9] <- statistics$unweighted$se.abs
    De.stats[i,10] <- statistics$weighted$se.rel
    De.stats[i,11] <- quantile(data[[i]][,1], 0.25)
    De.stats[i,12] <- quantile(data[[i]][,1], 0.75)
    De.stats[i,13] <- statistics$unweighted$skewness
    De.stats[i,14] <- statistics$unweighted$kurtosis
    De.stats[i,15] <- statistics$weighted$sd.abs
    De.stats[i,16] <- statistics$weighted$sd.rel
    De.stats[i,17] <- statistics$weighted$se.abs
    De.stats[i,18] <- statistics$weighted$se.rel

    ## kdemax - here a little doubled as it appears below again
    De.density <- try(density(x = data[[i]][,1],
                              kernel = "gaussian",
                              from = limits.z[1],
                              to = limits.z[2]),
                      silent = TRUE)

    De.stats[i,6] <- NA
    if(!inherits(De.density, "try-error")) {
      De.stats[i,6] <- De.density$x[which.max(De.density$y)]
    }
  }

  ## helper to generate an element of the statistical summary
  .summary_line <- function(keyword, summary, val, label = keyword,
                            percent = FALSE, sep = FALSE, digits = 2) {
    ifelse(keyword %in% summary,
           paste0(label, " = ", round(val, digits),
                  if (percent) " %" else NULL, if (sep) " | " else "\n"),
           "")
  }

  ## initialize list with a dummy element, it will be removed afterwards
  label.text <- list(NA)

  is.sub <- summary.pos[1] == "sub"
  stops <- NULL
  for (i in 1:length(data)) {
    if (!is.sub)
      stops <- strrep("\n", (i - 1) * length(summary))

    summary.text <- character(0)
    for (j in 1:length(summary)) {
      summary.text <-
        c(summary.text,
          .summary_line("n", summary[j], De.stats[i, 1], sep = is.sub),
          .summary_line("mean", summary[j], De.stats[i, 2], sep = is.sub),
          .summary_line("mean.weighted", summary[j], De.stats[i, 3], sep = is.sub,
                        label = "weighted mean"),
          .summary_line("median", summary[j], De.stats[i, 4], sep = is.sub),
          .summary_line("median.weighted", summary[j], De.stats[i, 5], sep = is.sub,
                        label = "weighted median"),
          .summary_line("kdemax", summary[j], De.stats[i, 6], sep = is.sub),
          .summary_line("sdabs", summary[j], De.stats[i, 7], sep = is.sub,
                        label = "sd"),
          .summary_line("sdrel", summary[j], De.stats[i, 8], sep = is.sub,
                        label = "rel. sd", percent = TRUE),
          .summary_line("seabs", summary[j], De.stats[i, 9], sep = is.sub,
                        label = "se"),
          .summary_line("serel", summary[j], De.stats[i, 10], sep = is.sub,
                        label = "rel. se", percent = TRUE),
          .summary_line("skewness", summary[j], De.stats[i, 13], sep = is.sub),
          .summary_line("kurtosis", summary[j], De.stats[i, 14], sep = is.sub),
          .summary_line("in.2s", summary[j],
                        sum(data[[i]][,7] > -2 & data[[i]][,7] < 2) /
                        nrow(data[[i]]) * 100, sep = is.sub,
                        label = "in 2 sigma", percent = TRUE, digits = 1),
          .summary_line("sdabs.weighted", summary[j], De.stats[i, 15], sep = is.sub,
                        label = "abs. weighted sd"),
          .summary_line("sdrel.weighted", summary[j], De.stats[i, 16], sep = is.sub,
                        label = "rel. weighted sd"),
          .summary_line("seabs.weighted", summary[j], De.stats[i, 17], sep = is.sub,
                        label = "abs. weighted se"),
          .summary_line("serel.weighted", summary[j], De.stats[i, 18], sep = is.sub,
                        label = "rel. weighted se"))
    }
    label.text[[length(label.text) + 1]] <- paste0(
        if (is.sub ) "" else stops,
        paste(summary.text, collapse = ""),
        stops)
  }

  ## remove dummy list element
  label.text[[1]] <- NULL

  ## remove outer vertical lines from string
  if (is.sub) {
    for (i in seq_along(label.text)) {
      label.text[[i]] <- substr(x = label.text[[i]],
                                start = 1,
                                stop = nchar(label.text[[i]]) - 3)
    }
  }

  ## convert keywords into summary and legend placement coordinates
  coords <- .get_keyword_coordinates(summary.pos, limits.x, limits.y)
  summary.pos <- coords$pos
  summary.adj <- coords$adj

  ## calculate line coordinates and further parameters
  if(!missing(line)) {
    #line = line + De.add

    if (log.z) line <- log(line)

    line.coords <- NULL
    for(i in 1:length(line)) {
      line.x <- c(limits.x[1], x.coord(line[i]))
      line.y <- c(0, y.coord(line[i], line.x[2]))
      line.coords[[i]] <- rbind(line.x, line.y)
    }

    if (missing(line.col)) {
      line.col <- seq(from = 1, to = length(line.coords))
    }

    if (missing(line.label)) {
      line.label <- rep("", length(line.coords))
    }
  }

  ## calculate rug coordinates
  if (rug) {
    rug.values <- if (log.z) log(De.global) else De.global
    rug.coords <- NULL
    for(i in 1:length(rug.values)) {
      rug.x <- x.coord(rug.values[i]) * c(0.988, 0.995)
      rug.y <- y.coord(rug.values[i], rug.x)
      rug.coords[[i]] <- rbind(rug.x, rug.y)
    }
  }

  ## Generate plot ------------------------------------------------------------

  ## check if plotting is enabled
  if(show) {

    ## determine number of subheader lines to shift the plot
    if(length(summary) > 0 & summary.pos[1] == "sub") {
      shift.lines <- length(data) + 1
    } else {shift.lines <- 1}

    ## setup plot area
    default <- par(mar = c(4, 4, shift.lines + 1.5, 7),
        xpd = TRUE,
        cex = cex)

    ## reset on exit
    on.exit(par(default), add = TRUE)

    ## create empty plot
    plot(NA,
         xlim = limits.x,
         ylim = limits.y,
         main = "",
         sub = sub,
         xlab = "",
         ylab = "",
         xaxs = "i",
         yaxs = "i",
         frame.plot = FALSE,
         axes = FALSE)

    ## add y-axis label
    mtext(side = 2,
          line = 2.5,
          at = 0,
          adj = 0.5,
          cex = cex,
          text = ylab)

    ## calculate upper x-axis label values
    label.x.upper <- if (log.z) {
      as.character(round(1/axTicks(side = 1)[-1] * 100, 1))
    } else {
      as.character(round(1/axTicks(side = 1)[-1], 1))
    }

    ## optionally, plot 2-sigma-bar
    if(bar.col[1] != "none") {
      for(i in 1:length(data)) {
        polygon(x = polygons[i,1:4],
                y = polygons[i,5:8],
                lty = "blank",
                col = bar.col[i])
      }
    }

    ## optionally, add grid lines
    if(grid.col[1] != "none") {
      for(i in 1:length(tick.x1.major)) {
        lines(x = c(limits.x[1], tick.x1.major[i]),
              y = c(0, tick.y1.major[i]),
              col = grid.col)
      }
    }

    ## optionally, plot central value lines
    if(lwd[1] > 0 & lty[1] > 0) {
      for(i in 1:length(data)) {
        x2 <- x.coord(data[[i]][1, 5])
        y2 <- y.coord(data[[i]][1, 5], x2)
        lines(x = c(limits.x[1], x2),
              y = c(0, y2),
              lty = lty[i],
              lwd = lwd[i],
              col = col[i])
      }
    }

    ## optionally add further lines
    if (!missing(line)) {
      for(i in 1:length(line)) {
        lines(x = line.coords[[i]][1,],
              y = line.coords[[i]][2,],
              col = line.col[i])
        text(x = line.coords[[i]][1,2],
             y = line.coords[[i]][2,2] + par()$cxy[2] * 0.3,
             labels = line.label[i],
             pos = 2,
             col = line.col[i],
             cex = cex * 0.9)
      }
    }

    ## overplot unwanted parts
    polygon(x = c(ellipse[,1], limits.x[2] * 2, limits.x[2] * 2),
            y = c(ellipse[,2], max(ellipse[,2]), min(ellipse[,2])),
            col = "white",
            lty = 0)

    ## add plot title
    title(main = main, line = shift.lines, font = 2)

    ## plot lower x-axis (precision)
    x.axis.ticks <- axTicks(side = 1)
    x.axis.ticks <- x.axis.ticks[c(TRUE, x.axis.ticks <= limits.x[2])]
    x.axis.ticks <- x.axis.ticks[x.axis.ticks <= limits.x[2]]

    ## axis with labels and ticks
    axis(side = 1,
         at = x.axis.ticks,
         lwd = 1,
         xlab = "")

    ## extend axis line to right side of the plot
    lines(x = c(max(x.axis.ticks, na.rm = TRUE), limits.x[2]),
          y = c(limits.y[1], limits.y[1]))

    ## draw closing tick on right hand side
    axis(side = 1, tcl = 0.5, lwd = 0, lwd.ticks = 1, at = limits.x[2],
         labels = FALSE)
    axis(side = 1, tcl = -0.5, lwd = 0, lwd.ticks = 1, at = limits.x[2],
         labels = FALSE)

    ## add upper and lower axis label
    mtext(text = xlab,
          at = (limits.x[1] + limits.x[2]) / 2,
          side = 1,
          line = c(-3.5, 2.5),
          cex = cex)

    ## plot upper x-axis
    axis(side = 1,
         tcl = 0.5,
         lwd = 0,
         lwd.ticks = 1,
         at = x.axis.ticks[-1],
         labels = FALSE)

    ## remove first tick label (infinity)
    label.x.upper <- label.x.upper[1:(length(x.axis.ticks) - 1)]

    ## add tick labels
    axis(side = 1,
         lwd = 0,
         labels = label.x.upper,
         at = x.axis.ticks[-1],
         line = -3)

    ## plot minor z-ticks
    for(i in 1:length(tick.values.minor)) {
      lines(x = c(tick.x1.minor[i], tick.x2.minor[i]),
            y = c(tick.y1.minor[i], tick.y2.minor[i]))
    }

    ## plot major z-ticks
    for(i in 1:length(tick.values.major)) {
      lines(x = c(tick.x1.major[i], tick.x2.major[i]),
            y = c(tick.y1.major[i], tick.y2.major[i]))
    }

    ## plot z-axis
    lines(ellipse)

    ## plot z-values
    text(x = label.x,
         y = label.y,
         label = label.z.text, 0)

    ## plot z-label
    mtext(side = 4,
          at = 0,
          line = 5,
          las = 3,
          adj = 0.5,
          cex = cex,
          text = zlab)

    ## optionally add rug
    if (rug) {
      for(i in 1:length(rug.coords)) {
        lines(x = rug.coords[[i]][1,],
              y = rug.coords[[i]][2,],
              col = col[data.global[i,9]])
      }
    }

    ## plot values
    for(i in 1:length(data)) {
      points(data[[i]][,6][data[[i]][,6] <= limits.x[2]],
             data[[i]][,8][data[[i]][,6] <= limits.x[2]],
             col = col[i],
             pch = pch[i])
    }

    ## optionally add min, max, median sample text
    if(length(stats) > 0) {
      text(x = stats.data[,1],
           y = stats.data[,2],
           labels = round(stats.data[,3], 1),
           pos = 2,
           cex = 0.85)
    }

    ## optionally add legend content
    if (!is.null(legend)) {
      coords <- .get_keyword_coordinates(legend.pos, limits.x, limits.y)
      legend.pos <- coords$pos
      legend.adj <- coords$adj
      legend(x = legend.pos[1],
             y = 0.8 * legend.pos[2],
             xjust = legend.adj[1],
             yjust = legend.adj[2],
             legend = legend,
             pch = pch,
             col = col,
             text.col = col,
             cex = 0.8,
             bty = "n")
    }

    ## plot y-axis
    if (y.ticks) {
      char.height <- par()$cxy[2]
      if (char.height > 4.5 / cex) {
        axis(side = 2, las = 2, at = 0, labels = "\uB1 2", tcl = 0, hadj = 0.5)
      } else {
        axis(side = 2, las = 2, at = c(-2, 0, 2))
      }
    } else {
      axis(side = 2, at = 0)
    }

    ## optionally add subheader text
    mtext(side = 3,
          line = shift.lines - 2,
          text = mtext,
          cex = 0.8 * cex)

    ## add summary content
    for(i in 1:length(data)) {
      if(summary.pos[1] != "sub") {
        text(x = summary.pos[1],
             y = 0.8 * summary.pos[2],
             adj = summary.adj,
             labels = label.text[[i]],
             cex = 0.8 * cex,
             col = col[i])
      } else if (mtext == "") {
          mtext(side = 3,
                line = shift.lines - 1 - i,
                text = label.text[[i]],
                col = col[i],
                cex = 0.8 * cex)
      }
    }

    ##FUN by R Luminescence Team
    if (fun) sTeve() # nocov
  }

  if(output) {
    return(list(data = data,
                data.global = data.global,
                xlim = limits.x,
                ylim = limits.y,
                zlim = limits.z,
                r = r,
                plot.ratio = plot.ratio,
                ticks.major = ticks.major,
                ticks.minor = ticks.minor,
                labels = labels,
                polygons = polygons,
                ellipse.lims = ellipse.lims))
  }
}
