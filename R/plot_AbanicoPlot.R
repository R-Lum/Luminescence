#' @title Function to create an Abanico Plot.
#'
#' @description A plot is produced which allows comprehensive presentation of data precision
#' and its dispersion around a central value as well as illustration of a
#' kernel density estimate, histogram and/or dot plot of the dose values.
#'
#' @details
#' The Abanico Plot is a combination of the classic Radial Plot
#' (`plot_RadialPlot`) and a kernel density estimate plot (e.g
#' `plot_KDE`). It allows straightforward visualisation of data precision,
#' error scatter around a user-defined central value and the combined
#' distribution of the values, on the actual scale of the measured data (e.g.
#' seconds, equivalent dose, years). The principle of the plot is shown in
#' Galbraith & Green (1990). The function authors are thankful for the
#' thought-provoking figure in this article.
#'
#' The semi circle (z-axis) of the classic Radial Plot is bent to a straight
#' line here, which actually is the basis for combining this polar (radial)
#' part of the plot with any other Cartesian visualisation method
#' (KDE, histogram, PDF and so on). Note that the plot allows displaying
#' two measures of distribution. One is the 2-sigma
#' bar, which illustrates the spread in value errors, and the other is the
#' polygon, which stretches over both parts of the Abanico Plot (polar and
#' Cartesian) and illustrates the actual spread in the values themselves.
#'
#' Since the 2-sigma-bar is a polygon, it can be (and is) filled with shaded
#' lines. To change density (lines per inch, default is 15) and angle (default
#' is 45 degrees) of the shading lines, specify these parameters. See
#' `?polygon()` for further help.
#'
#' The Abanico Plot supports other than the weighted mean as measure of
#' centrality. When it is obvious that the data
#' is not (log-)normally distributed, the mean (weighted or not) cannot be a
#' valid measure of centrality and hence central dose. Accordingly, the median
#' and the weighted median can be chosen as well to represent a proper measure
#' of centrality (e.g. `centrality = "median.weighted"`). Also
#' user-defined numeric values (e.g. from the central age model) can be used if
#' this appears appropriate.
#'
#' The proportion of the polar part and the cartesian part of the Abanico Plot
#' can be modified for display reasons (`plot.ratio = 0.75`). By default,
#' the polar part spreads over 75 % and leaves 25 % for the part that
#' shows the KDE graph.
#'
#'
#' A statistic summary, i.e. a collection of statistic measures of
#' centrality and dispersion (and further measures) can be added by specifying
#' one or more of the following keywords:
#'
#' - `"n"` (number of samples)
#' - `"mean"` (mean De value)
#' - `"median"` (median of the De values)
#' - `"sd.rel"` (relative standard deviation in percent)
#' - `"sd.abs"` (absolute standard deviation)
#' - `"se.rel"` (relative standard error)
#' - `"se.abs"` (absolute standard error)
#' - `"in.2s"` (percent of samples in 2-sigma range)
#' - `"kurtosis"` (kurtosis)
#' - `"skewness"` (skewness)
#'
#' **Note** that the input data for the statistic summary is sent to the function
#' `calc_Statistics()` depending on the log-option for the z-scale. If
#' `"log.z = TRUE"`, the summary is based on the logarithms of the input
#' data. If `"log.z = FALSE"` the linearly scaled data is used.
#'
#' **Note** as well, that `"calc_Statistics()"` calculates these statistic
#' measures in three different ways: `unweighted`, `weighted` and
#' `MCM-based` (i.e., based on Monte Carlo Methods). By default, the
#' MCM-based version is used. If you wish to use another method, indicate this
#' with the appropriate keyword using the argument `summary.method`.
#'
#' The optional parameter `layout` allows more sophisticated ways to modify
#' the entire plot. Each element of the plot can be addressed and its properties
#' can be defined. This includes font type, size and decoration, colours and
#' sizes of all plot items. To infer the definition of a specific layout style
#' cf. `get_Layout()` or type e.g., for the layout type `"journal"`
#' `get_Layout("journal")`. A layout type can be modified by the user by
#' assigning new values to the list object.
#'
#' It is possible for the z-scale to specify where ticks are to be drawn
#'  by using the parameter `at`, e.g. `at = seq(80, 200, 20)`, cf. function
#'  documentation of `axis`. Specifying tick positions manually overrides a
#' `zlim`-definition.
#'
#' @param data [data.frame] or [RLum.Results-class] object (**required**):
#' for `data.frame` two columns: De (`data[,1]`) and De error (`data[,2]`).
#'  To plot several data sets in one plot the data sets must be provided as
#'  `list`, e.g. `list(data.1, data.2)`.
#'
#' @param na.rm [logical] (*with default*):
#' exclude NA values from the data set prior to any further operations.
#'
#' @param log.z [logical] (*with default*):
#' Option to display the z-axis in logarithmic scale. Default is `TRUE`.
#'
#' @param z.0 [character] or [numeric] (*with default*):
#' User-defined central value, used for centring of data. One out of `"mean"`,
#' `"mean.weighted"` and `"median"` or a numeric value (not its logarithm).
#' Default is `"mean.weighted"`.
#'
#' @param dispersion [character] (*with default*):
#' measure of dispersion, used for drawing the scatter polygon. One out of
#' - `"qr"` (quartile range, default),
#' - `"pnn"` (symmetric percentile range with `nn` the lower percentile, e.g.
#' `"p05"` indicating the range between 5 and 95 %, or `"p10"` indicating
#' the range between 10 and 90 %), or
#' - `"sd"` (standard deviation) and
#' - `"2sd"` (2 standard deviations),
#'
#' The default is `"qr"`. Note that `"sd"` and `"2sd"` are only meaningful in
#' combination with `"z.0 = 'mean'"` because the unweighted mean is used to
#' centre the polygon.
#'
#' @param plot.ratio [numeric] (*with default*):
#' Relative space, given to the radial versus the cartesian plot part,
#' default is `0.75`.
#'
#' @param rotate [logical] (*with default*):
#' Option to turn the plot by 90 degrees.
#'
#' @param mtext [character] (*with default*):
#' additional text below the plot title.
#'
#' @param summary [character] (*with default*):
#' add statistic measures of centrality and dispersion to the plot.
#' Can be one or more of several keywords. See details for available keywords.
#' Results differ depending on the log-option for the z-scale (see details).
#'
#' @param summary.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`) for the
#' statistical summary. Alternatively, the keyword `"sub"` may be
#' specified to place the summary below the plot header. However, this latter
#' option in only possible if `mtext` is not used.
#'
#' @param summary.method [character] (*with default*):
#' keyword indicating the method used to calculate the statistic summary.
#' One out of
#' - `"unweighted"`,
#' - `"weighted"` and
#' - `"MCM"`.
#'
#' See [calc_Statistics] for details.
#'
#' @param legend [character] vector (*optional*):
#' legend content to be added to the plot.
#'
#' @param legend.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`)
#' for the legend to be plotted.
#'
#' @param stats [character]:
#' additional labels of statistically important values in the plot.
#' One or more out of the following:
#' - `"min"`,
#' - `"max"`,
#' - `"median"`.
#'
#' @param rug [logical] (*with default*):
#' Option to add a rug to the KDE part, to indicate the location of individual values.
#'
#' @param kde [logical] (*with default*):
#' Option to add a KDE plot to the dispersion part, default is `TRUE`.
#'
#' @param hist [logical] (*with default*):
#' Option to add a histogram to the dispersion part. Only meaningful when not
#' more than one data set is plotted.
#'
#' @param dots [logical] (*with default*):
#' Option to add a dot plot to the dispersion part. If number of dots exceeds
#' space in the dispersion part, a square indicates this.
#'
#' @param boxplot [logical] (*with default*):
#' Option to add a boxplot to the dispersion part, default is `FALSE`.
#'
#' @param y.axis [logical] (*with default*): Option to hide standard y-axis
#' labels and show 0 only.
#' Useful for data with small scatter. If you want to suppress the y-axis entirely
#' please use `yaxt == 'n'` (the standard [graphics::par] setting) instead.
#'
#' @param error.bars [logical] (*with default*):
#' Option to show De-errors as error bars on De-points. Useful in combination
#' with `y.axis = FALSE, bar.col = "none"`.
#'
#' @param bar [numeric] (*with default*):
#' option to add one or more dispersion bars (i.e., bar showing the 2-sigma range)
#' centred at the defined values. By default a bar is drawn according to `"z.0"`.
#' To omit the bar set `"bar = FALSE"`.
#'
#' @param bar.col [character] or [numeric] (*with default*):
#' colour of the dispersion bar. Default is `"grey60"`.
#'
#' @param polygon.col [character] or [numeric] (*with default*):
#' colour of the polygon showing the data scatter. Sometimes this
#' polygon may be omitted for clarity. To disable it use `FALSE` or
#' `polygon = FALSE`. Default is `"grey80"`.
#'
#' @param line [numeric]:
#' numeric values of the additional lines to be added.
#'
#' @param line.col [character] or [numeric]:
#' colour of the additional lines.
#'
#' @param line.lty [integer]:
#' line type of additional lines
#'
#' @param line.label [character]:
#' labels for the additional lines.
#'
#' @param grid.col [character] or [numeric] (*with default*):
#' colour of the grid lines (originating at `[0,0]` and stretching to
#' the z-scale). To disable grid lines use `FALSE`. Default is `"grey"`.
#'
#' @param frame [numeric] (*with default*):
#' option to modify the plot frame type. Can be one out of
#' - `0` (no frame),
#' - `1` (frame originates at 0,0 and runs along min/max isochrons),
#' - `2` (frame embraces the 2-sigma bar),
#' - `3` (frame embraces the entire plot as a rectangle).
#'
#' Default is `1`.
#'
#' @param bw [character] (*with default*):
#' bin-width for KDE, choose a numeric value for manual setting.
#'
#' @param interactive [logical] (*with default*):
#' create an interactive abanico plot (requires the `'plotly'` package)
#'
#' @param ... Further plot arguments to pass (see [graphics::plot.default]).
#' Supported are: `main`, `sub`, `ylab`, `xlab`, `zlab`, `zlim`, `ylim`, `cex`,
#' `lty`, `lwd`, `pch`, `col`, `at`, `breaks`. `xlab` must be
#' a vector of length two, specifying the upper and lower x-axis labels.
#'
#' @return
#' Returns a plot object and, optionally, a list with plot calculus data.
#'
#' @section Function version: 0.1.18
#'
#' @author
#' Michael Dietze, GFZ Potsdam (Germany)\cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Inspired by a plot introduced by Galbraith & Green (1990)
#'
#' @seealso [plot_RadialPlot], [plot_KDE], [plot_Histogram], [plot_ViolinPlot]
#'
#' @references
#' Galbraith, R. & Green, P., 1990. Estimating the component ages
#' in a finite mixture. International Journal of Radiation Applications and
#' Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 (3),
#' 197-206.
#'
#' Dietze, M., Kreutzer, S., Burow, C., Fuchs, M.C., Fischer, M., Schmidt, C., 2015.
#' The abanico plot: visualising chronometric data with individual standard errors.
#' Quaternary Geochronology. doi:10.1016/j.quageo.2015.09.003
#'
#' @examples
#'
#' ## load example data and recalculate to Gray
#' data(ExampleData.DeValues, envir = environment())
#' ExampleData.DeValues <- ExampleData.DeValues$CA1
#'
#' ## plot the example data straightforward
#' plot_AbanicoPlot(data = ExampleData.DeValues)
#'
#' ## now with linear z-scale
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  log.z = FALSE)
#'
#' ## now with output of the plot parameters
#' plot1 <- plot_AbanicoPlot(data = ExampleData.DeValues,
#'                           output = TRUE)
#' str(plot1)
#' plot1$zlim
#'
#' ## now with adjusted z-scale limits
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  zlim = c(10, 200))
#'
#' ## now with adjusted x-scale limits
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  xlim = c(0, 20))
#'
#' ## now with rug to indicate individual values in KDE part
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  rug = TRUE)
#'
#' ## now with a smaller bandwidth for the KDE plot
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  bw = 0.04)
#'
#' ## now with a histogram instead of the KDE plot
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  hist = TRUE,
#'                  kde = FALSE)
#'
#' ## now with a KDE plot and histogram with manual number of bins
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  hist = TRUE,
#'                  breaks = 20)
#'
#' ## now with a KDE plot and a dot plot
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  dots = TRUE)
#'
#' ## now with user-defined plot ratio
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  plot.ratio = 0.5)

#' ## now with user-defined central value
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  z.0 = 70)
#'
#' ## now with median as central value
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  z.0 = "median")
#'
#' ## now with the 17-83 percentile range as definition of scatter
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  z.0 = "median",
#'                  dispersion = "p17")
#'
#' ## now with user-defined green line for minimum age model
#' CAM <- calc_CentralDose(ExampleData.DeValues,
#'                         plot = FALSE)
#'
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  line = CAM,
#'                  line.col = "darkgreen",
#'                  line.label = "CAM")
#'
#' ## now create plot with legend, colour, different points and smaller scale
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  legend = "Sample 1",
#'                  col = "tomato4",
#'                  bar.col = "peachpuff",
#'                  pch = "R",
#'                  cex = 0.8)
#'
#' ## now without 2-sigma bar, polygon, grid lines and central value line
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  bar.col = FALSE,
#'                  polygon.col = FALSE,
#'                  grid.col = FALSE,
#'                  y.axis = FALSE,
#'                  lwd = 0)
#'
#' ## now with direct display of De errors, without 2-sigma bar
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  bar.col = FALSE,
#'                  ylab = "",
#'                  y.axis = FALSE,
#'                  error.bars = TRUE)
#'
#' ## now with user-defined axes labels
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  xlab = c("Data error (%)",
#'                           "Data precision"),
#'                  ylab = "Scatter",
#'                  zlab = "Equivalent dose [Gy]")
#'
#' ## now with minimum, maximum and median value indicated
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  stats = c("min", "max", "median"))
#'
#' ## now with a brief statistical summary as subheader
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  summary = c("n", "in.2s"))
#'
#' ## now with another statistical summary
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  summary = c("mean.weighted", "median"),
#'                  summary.pos = "topleft")
#'
#' ## now a plot with two 2-sigma bars for one data set
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  bar = c(30, 100))
#'
#' ## now the data set is split into sub-groups, one is manipulated
#' data.1 <- ExampleData.DeValues[1:30,]
#' data.2 <- ExampleData.DeValues[31:62,] * 1.3
#'
#' ## now a common dataset is created from the two subgroups
#' data.3 <- list(data.1, data.2)
#'
#' ## now the two data sets are plotted in one plot
#' plot_AbanicoPlot(data = data.3)
#'
#' ## now with some graphical modification
#' plot_AbanicoPlot(data = data.3,
#'                  z.0 = "median",
#'                  col = c("steelblue4", "orange4"),
#'                  bar.col = c("steelblue3", "orange3"),
#'                  polygon.col = c("steelblue1", "orange1"),
#'                  pch = c(2, 6),
#'                  angle = c(30, 50),
#'                  summary = c("n", "in.2s", "median"))
#'
#' ## create Abanico plot with predefined layout definition
#' plot_AbanicoPlot(data = ExampleData.DeValues,
#'                  layout = "journal")
#'
#' ## now with predefined layout definition and further modifications
#' plot_AbanicoPlot(
#'  data = data.3,
#'  z.0 = "median",
#'  layout = "journal",
#'  col = c("steelblue4", "orange4"),
#'  bar.col = adjustcolor(c("steelblue3", "orange3"),
#'                          alpha.f = 0.5),
#'  polygon.col = c("steelblue3", "orange3"))
#'
#' ## for further information on layout definitions see documentation
#' ## of function get_Layout()
#'
#' ## now with manually added plot content
#' ## create empty plot with numeric output
#' AP <- plot_AbanicoPlot(data = ExampleData.DeValues,
#'                        pch = NA,
#'                        output = TRUE)
#'
#' ## identify data in 2 sigma range
#' in_2sigma <- AP$data[[1]]$data.in.2s
#'
#' ## restore function-internal plot parameters
#' par(AP$par)
#'
#' ## add points inside 2-sigma range
#' points(x = AP$data[[1]]$precision[in_2sigma],
#'        y = AP$data[[1]]$std.estimate.plot[in_2sigma],
#'        pch = 16)
#'
#' ## add points outside 2-sigma range
#' points(x = AP$data[[1]]$precision[!in_2sigma],
#'        y = AP$data[[1]]$std.estimate.plot[!in_2sigma],
#'        pch = 1)
#'
#' @md
#' @export
plot_AbanicoPlot <- function(
  data,
  na.rm = TRUE,
  log.z = TRUE,
  z.0 = "mean.weighted",
  dispersion = "qr",
  plot.ratio = 0.75,
  rotate = FALSE,
  mtext = "",
  summary = c("n", "in.2s"),
  summary.pos = "sub",
  summary.method = "MCM",
  legend,
  legend.pos,
  stats,
  rug = FALSE,
  kde = TRUE,
  hist = FALSE,
  dots = FALSE,
  boxplot = FALSE,
  y.axis = TRUE,
  error.bars = FALSE,
  bar,
  bar.col,
  polygon.col,
  line,
  line.col,
  line.lty,
  line.label,
  grid.col,
  frame = 1,
  bw = "SJ",
  interactive = FALSE,
  ...
) {
  .set_function_name("plot_AbanicoPlot")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  ## Homogenise input data format
  if(is(data, "list") == FALSE) {
    data <- list(data)
  }

  ## Check input data
  for (i in seq_along(data)) {
    .validate_class(data[[i]], c("data.frame", "RLum.Results"),
                    name = "All elements of 'data'")
    if (inherits(data[[i]], "RLum.Results"))
      data[[i]] <- get_RLum(data[[i]], "data")

      if (ncol(data[[i]]) < 2) {
        .throw_error("Data set (", i, ") has fewer than 2 columns: data ",
                     "without errors cannot be displayed")
      }

      data[[i]] <- data[[i]][,c(1:2)]
  }

  ## optionally, remove NA-values
  if(na.rm == TRUE) {
    for(i in seq_along(data)) {

      n.NA <- sum(!stats::complete.cases(data[[i]]))

      if (n.NA > 0) {
        message("[plot_AbanicoPlot()] Data set (", i, "): ", n.NA,
                " NA value", ifelse (n.NA > 1, "s", ""), " excluded")
        data[[i]] <- na.exclude(data[[i]])
      }
    }
  }

  ##AFTER NA removal, we should check the data set carefully again ...
  ##(1)
  ##check if there is still data left in the entire set
  if(all(sapply(data, nrow) == 0)){
    .throw_message("'data' is empty, nothing plotted")
    return(NULL)
  }
  ##(2)
  ##check for sets with only 1 row or 0 rows at all
  else if(any(sapply(data, nrow) <= 1)){
    ##select problematic sets and remove the entries from the list
    NArm.id <- which(sapply(data, nrow) <= 1)
    data[NArm.id] <- NULL

    .throw_warning("Data sets ", paste(NArm.id, collapse = ", "),
                   " are found to be empty or consisting of only 1 row. Sets removed!")

    rm(NArm.id)

    ##unfortunately, the data set might become now empty at all
    if(length(data) == 0){
      .throw_message("After removing invalid entries, nothing is plotted")
      return(NULL)
    }
  }

  ## check for zero-error values
  for(i in 1:length(data)) {
    if(sum(data[[i]][,2] == 0) > 0) {
      data[[i]] <- data[[i]][data[[i]][,2] > 0,]

      if(nrow(data[[i]]) < 1) {
        .throw_error("Data set contains only values with zero errors")
      }

      .throw_warning("Values with zero errors cannot be displayed and were removed")
    }
  }

  ## check for 0 values in dataset for log
  if (log.z[1]) {
    for(i in 1:length(data)) {
      if(any(data[[i]][[1]] == 0)) {
        .throw_warning("Found zero values in x-column of dataset ", i, ": set log.z = FALSE")
        log.z <- FALSE
      }
    }
  }

  ## plot.ratio must be numeric and positive
  .validate_positive_scalar(plot.ratio)
  .validate_logical_scalar(rotate)
  .validate_logical_scalar(rug)

  if (!is.numeric(z.0)) {
    .validate_class(z.0, "character")
    z.0 <- .validate_args(z.0, c("mean", "mean.weighted", "median"),
                          extra = "a numerical value")
  }

  ## the 'pnn' option need some special treatment
  main.choices <- c("qr", "sd", "2sd")
  extra.choice <-"a percentile of the form 'pnn' (eg. 'p05')"
  if (!dispersion %in% main.choices && !grepl("^p[0-9][0-9]$", dispersion))
    dispersion <- .validate_args(dispersion, main.choices, extra = extra.choice)

  .validate_class(summary, "character")
  .validate_class(summary.pos, c("numeric", "character"))
  if (is.numeric(summary.pos)) {
    .validate_length(summary.pos, 2)
  }
  else {
    .validate_args(summary.pos, c("sub", "left", "center", "right",
                                  "topleft", "top", "topright",
                                  "bottomleft", "bottom", "bottomright"))
  }

  ## save original plot parameters and restore them upon end or stop
  par.default <- par(no.readonly = TRUE)
  cex_old <- par()$cex

  ## this ensures par() is respected for several plots on one page
  if(sum(par()$mfrow) == 2 & sum(par()$mfcol) == 2){
    on.exit(par(par.default), add = TRUE)
  }

  ## check/set layout definitions
  if(!is.null(list(...)$layout))
    layout <- get_Layout(layout = list(...)$layout)
  else
    layout <- get_Layout(layout = "default")

  if(missing(stats))
    stats <- numeric(0)

  if(missing(bar))
    bar <- rep(TRUE, length(data))

  if(missing(bar.col)) {
    bar.fill <- rep(x = rep(x = layout$abanico$colour$bar.fill,
                            length.out = length(data)), length(bar))
    bar.line <- rep(rep(layout$abanico$colour$bar.line,
                        length.out = length(data)), length(bar))
  } else {
    bar.fill <- bar.col
    bar.line <- NA
  }

  if(missing(polygon.col)) {
    polygon.fill <- rep(layout$abanico$colour$poly.fill,
                        length.out = length(data))
    polygon.line <- rep(layout$abanico$colour$poly.line,
                        length.out = length(data))
  } else {
    polygon.fill <- polygon.col
    polygon.line <- NA
  }

  if(missing(grid.col)) {
    grid.major <- layout$abanico$colour$grid.major
    grid.minor <- layout$abanico$colour$grid.minor
  } else {
    if(length(grid.col) == 1) {
      grid.major <- grid.col[1]
      grid.minor <- grid.col[1]
    } else {
      grid.major <- grid.col[1]
      grid.minor <- grid.col[2]
    }
  }

  ## create preliminary global data set
  De.global <- data[[1]][,1]
  if(length(data) > 1) {
    for(i in 2:length(data)) {
      De.global <- c(De.global, data[[i]][,1])
    }
  }

  ## calculate major preliminary tick values and tick difference
  extraArgs <- list(...)
  if ("zlim" %in% names(extraArgs) && !is.null(extraArgs$zlim)) {
    limits.z <- extraArgs$zlim
    .validate_class(limits.z, "numeric", name = "'zlim'")
  } else {
    z.span <- (mean(De.global) * 0.5) / (sd(De.global) * 100)
    z.span <- ifelse(z.span > 1, 0.9, z.span)
    limits.z <- c((ifelse(min(De.global) <= 0, 1.1, 0.9) - z.span) *
                    min(De.global),
                  (1.1 + z.span) * max(De.global))
  }

  if("at" %in% names(extraArgs)) {
    ticks <- extraArgs$at
  } else {
    ticks <- round(pretty(limits.z, n = 5), 3)
  }

  if("breaks" %in% names(extraArgs)) {
    breaks <- extraArgs$breaks
  } else {
    breaks <- "Sturges"
  }

  ## check/set bw-parameter
  for(i in 1:length(data)) {
    bw.test <- try(density(x = data[[i]][,1],
                           bw = bw),
                   silent = TRUE)
    if (inherits(bw.test, "try-error")) {
      bw <- "nrd0"
      .throw_warning("Option for 'bw' not valid, reset to 'nrd0'")
    }
  }

  if ("fun" %in% names(extraArgs)) {
    fun <- list(...)$fun # nocov
  } else {
    fun <- FALSE
  }

  ## check for negative values, stop function, but do not stop
  De.add <- 0
  if(min(De.global) < 0) {
    if("zlim" %in% names(extraArgs)) {
      De.add <- abs(extraArgs$zlim[1])
    } else {
      ## estimate delta De to add to all data
      De.add <-  min(10^ceiling(log10(abs(De.global))) * 10)

      ## optionally readjust delta De for extreme values
      if(De.add <= abs(min(De.global))) {
        De.add <- De.add * 10
      }
    }
  }

  ## optionally add correction dose to data set and adjust error
  if(log.z[1]) {
    for(i in 1:length(data))
      data[[i]][,1] <- data[[i]][,1] + De.add

    De.global <- De.global + De.add

  }

  ## calculate and append statistical measures --------------------------------
  ## z-values based on log-option
  z <- lapply(1:length(data), function(x){
    if(log.z[1]) {
      log(data[[x]][,1])
    } else {
      data[[x]][,1]
    }
  })

  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], z[[x]])
  })
  rm(z)

  ## calculate dispersion based on log-option
  se <- lapply(1:length(data), function(x, De.add){
    if(log.z == TRUE) {
      if(De.add != 0) {
        data[[x]][,2] <- data[[x]][,2] / (data[[x]][,1] + De.add)
      } else {
        data[[x]][,2] / data[[x]][,1]
      }
    } else {
      data[[x]][,2]
    }}, De.add = De.add)

  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], se[[x]])
  })
  rm(se)

  ## calculate initial data statistics
  stats.init <- list()
  for(i in 1:length(data)) {
    stats.init[[i]] <- calc_Statistics(data = data[[i]][,3:4])
  }

  ## calculate central values
  if (z.0 %in% c("mean", "median")) {
    z.central <- lapply(1:length(data), function(x){
      rep(stats.init[[x]]$unweighted[[z.0]],
          length(data[[x]][,3]))})

  } else if(z.0 == "mean.weighted") {
    z.central <- lapply(1:length(data), function(x){
      rep(stats.init[[x]]$weighted$mean,
          length(data[[x]][,3]))})

  } else {
    ## z.0 is numeric
    z.central <- lapply(1:length(data), function(x){
      rep(ifelse(log.z == TRUE,
                 log(z.0),
                 z.0),
          length(data[[x]][,3]))})
  }

  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], z.central[[x]])})
  rm(z.central)

  ## calculate precision
  precision <- lapply(1:length(data), function(x){
    1 / data[[x]][,4]})
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], precision[[x]])})
  rm(precision)

  ## calculate standardised estimate
  std.estimate <- lapply(1:length(data), function(x){
    (data[[x]][,3] - data[[x]][,5]) / data[[x]][,4]})
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], std.estimate[[x]])})

  ## append empty standard estimate for plotting
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], std.estimate[[x]])})
  rm(std.estimate)

  ## append optional weights for KDE curve
  if ("weights" %in% names(extraArgs) && extraArgs$weights == TRUE) {
    wgt <- lapply(1:length(data), function(x) {
      (1 / data[[x]][,2]) / sum(1 / data[[x]][,2]^2)
    })
  } else {
    wgt <- lapply(1:length(data), function(x) {
      rep(1, times = nrow(data[[x]])) / nrow(data[[x]])
    })
  }

  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], wgt[[x]])
  })
  rm(wgt)

  ## generate global data set
  data.global <- cbind(data[[1]],
                       rep(x = 1, times = nrow(data[[1]])))
  colnames(data.global) <- rep("", 10)

  if(length(data) > 1) {
    for(i in 2:length(data)) {
      data.add <- cbind(data[[i]],
                        rep(x = i, times = nrow(data[[i]])))
      colnames(data.add) <- rep("", 10)
      data.global <- rbind(data.global,
                           data.add)
    }
  }

  ## create column names
  colnames(data.global) <- c("De",
                             "error",
                             "z",
                             "se",
                             "z.central",
                             "precision",
                             "std.estimate",
                             "std.estimate.plot",
                             "weights",
                             "data set")

  ## calculate global data statistics
  stats.global <- calc_Statistics(data = data.global[,3:4])

  ## calculate global central value
  if (z.0 %in% c("mean", "median")) {
    z.central.global <- stats.global$unweighted[[z.0]]
  } else  if(z.0 == "mean.weighted") {
    z.central.global <- stats.global$weighted$mean
  } else if(is.numeric(z.0) == TRUE) {
    z.central.global <- ifelse(log.z == TRUE,
                               log(z.0),
                               z.0)
  }

  ## create column names
  for(i in 1:length(data)) {
    colnames(data[[i]]) <- c("De",
                             "error",
                             "z",
                             "se",
                             "z.central",
                             "precision",
                             "std.estimate",
                             "std.estimate.plot",
                             "weights")
  }

  ## re-calculate standardised estimate for plotting
  for(i in 1:length(data)) {
    data[[i]][,8] <- (data[[i]][,3] - z.central.global) / data[[i]][,4]
  }

  data.global.plot <- data[[1]][,8]
  if(length(data) > 1) {
    for(i in 2:length(data)) {
      data.global.plot <- c(data.global.plot, data[[i]][,8])
    }
  }
  data.global[,8] <- data.global.plot

  ## print message for too small scatter
  if(max(abs(1 / data.global[6])) < 0.02) {
    small.sigma <- TRUE
    message("[plot_AbanicoPlot()] Attention, small standardised estimate scatter. Toggle off y.axis?")
  }

  ## read out additional arguments---------------------------------------------
  extraArgs <- list(...)

  main <- if("main" %in% names(extraArgs)) {
    extraArgs$main
  } else {
    expression(paste(D[e], " distribution"))
  }

  sub <- if("sub" %in% names(extraArgs)) {
    extraArgs$sub
  } else {
    ""
  }

  if("xlab" %in% names(extraArgs)) {
    if(length(extraArgs$xlab) != 2) {
      if (length(extraArgs$xlab) == 3) {
        xlab <- c(extraArgs$xlab[1:2], "Density")
      } else {
        .throw_error("'xlab' must have length 2")
      }
    } else {xlab <- c(extraArgs$xlab, "Density")}
  } else {
    xlab <- c(if(log.z == TRUE) {
      "Relative standard error (%)"
    } else {
      "Standard error"
    },
    "Precision",
    "Density")
  }

  ylab <- if("ylab" %in% names(extraArgs)) {
    extraArgs$ylab
  } else {
    "Standardised estimate"
  }

  zlab <- if("zlab" %in% names(extraArgs)) {
    extraArgs$zlab
  } else {
    expression(paste(D[e], " [Gy]"))
  }

  if ("zlim" %in% names(extraArgs) && !is.null(extraArgs$zlim)) {
    limits.z <- extraArgs$zlim
  } else {
    z.span <- (mean(data.global[,1]) * 0.5) / (sd(data.global[,1]) * 100)
    z.span <- ifelse(z.span > 1, 0.9, z.span)
    limits.z <- c((0.9 - z.span) * min(data.global[[1]]),
                  (1.1 + z.span) * max(data.global[[1]]))
  }

  if ("xlim" %in% names(extraArgs) && !is.null(extraArgs$xlim)) {
    limits.x <- extraArgs$xlim
    .validate_class(limits.x, "numeric", name = "'xlim'")
  } else {
    limits.x <- c(0, max(data.global[,6]) * 1.05)
  }

  if(limits.x[1] != 0) {
    .throw_warning("Lower x-axis limit was ", limits.x[1], ", reset to zero")
    limits.x[1] <- 0
  }

  if ("ylim" %in% names(extraArgs) && !is.null(extraArgs$ylim)) {
    limits.y <- extraArgs$ylim
    .validate_class(limits.y, "numeric", name = "'ylim'")
  } else {
    y.span <- (mean(data.global[,1]) * 10) / (sd(data.global[,1]) * 100)
    y.span <- ifelse(y.span > 1, 0.98, y.span)
    limits.y <- c(-(1 + y.span) * max(abs(data.global[,7])),
                  (1 + y.span) * max(abs(data.global[,7])))
  }

  cex <- if("cex" %in% names(extraArgs)) {
    extraArgs$cex
  } else {
    1
  }

  lty <- if("lty" %in% names(extraArgs)) {
    extraArgs$lty
  } else {
    rep(rep(2, length(data)), length(bar))
  }

  lwd <- if("lwd" %in% names(extraArgs)) {
    extraArgs$lwd
  } else {
    rep(rep(1, length(data)), length(bar))
  }

  pch <- if("pch" %in% names(extraArgs)) {
    extraArgs$pch
  } else {
    rep(20, length(data))
  }

  if("col" %in% names(extraArgs)) {
    bar.col <- extraArgs$col
    kde.line <- extraArgs$col
    kde.fill <- NA
    value.dot <- extraArgs$col
    value.bar <- extraArgs$col
    value.rug <- extraArgs$col
    summary.col <- extraArgs$col
    centrality.col <- extraArgs$col
  } else {
    bar.col <- layout$abanico$colour$bar.fill
    if(length(layout$abanico$colour$bar.fill) == 1) {
      bar.col <- 1:length(data)
    }

    kde.line <- layout$abanico$colour$kde.line
    if(length(layout$abanico$colour$kde.line) == 1) {
      kde.line <- 1:length(data)
    }

    kde.fill <- layout$abanico$colour$kde.fill
    if(length(layout$abanico$colour$kde.fill) == 1) {
      kde.fill <- rep(layout$abanico$colour$kde.fill, length(data))
    }

    value.dot <- layout$abanico$colour$value.dot
    if(length(layout$abanico$colour$value.dot) == 1) {
      value.dot <- 1:length(data)
    }

    value.bar <- layout$abanico$colour$value.bar
    if(length(layout$abanico$colour$value.bar) == 1) {
      value.bar <- 1:length(data)
    }

    value.rug <- layout$abanico$colour$value.rug
    if(length(layout$abanico$colour$value.rug) == 1) {
      value.rug <- 1:length(data)
    }

    summary.col <- layout$abanico$colour$summary
    if(length(layout$abanico$colour$summary) == 1) {
      summary.col <- 1:length(data)
    }

    if(length(layout$abanico$colour$centrality) == 1) {
      centrality.col <- rep(x = 1:length(data), times = length(bar))
    } else {
      centrality.col <- rep(x = layout$abanico$colour$centrality,
                            times = length(bar))
    }
  }

  ## update central line colour
  centrality.col <- rep(centrality.col, length(bar))

  ## define auxiliary plot parameters -----------------------------------------
  ## set space between z-axis and baseline of cartesian part
  if(boxplot[1]) {
    lostintranslation <- 1.03

  } else {
    lostintranslation <- 1.03
    plot.ratio <- plot.ratio * 1.05
  }

  ## create empty plot to update plot parameters
  if (!rotate) {
    plot(NA,
         xlim = c(limits.x[1], limits.x[2] * (1 / plot.ratio)),
         ylim = limits.y,
         main = "",
         sub = "",
         xlab = "",
         ylab = "",
         xaxs = "i",
         yaxs = "i",
         frame.plot = FALSE,
         axes = FALSE)
  } else {
    plot(NA,
         xlim = limits.y,
         ylim = c(limits.x[1], limits.x[2] * (1 / plot.ratio)),
         main = "",
         sub = "",
         xlab = "",
         ylab = "",
         xaxs = "i",
         yaxs = "i",
         frame.plot = FALSE,
         axes = FALSE)
  }

  ## calculate conversion factor for plot coordinates
  f <- 0

  ## calculate major and minor z-tick values
  if("at" %in% names(extraArgs)) {
    tick.values.major <- extraArgs$at
    tick.values.minor <- extraArgs$at
  } else {
    tick.values.major <- signif(pretty(limits.z, n = 5), 3)
    tick.values.minor <- signif(pretty(limits.z, n = 25), 3)
  }

  tick.values.major <- tick.values.major[tick.values.major >=
                                           min(tick.values.minor)]
  tick.values.major <- tick.values.major[tick.values.major <=
                                           max(tick.values.minor)]
  tick.values.major <- tick.values.major[tick.values.major >=
                                           limits.z[1]]
  tick.values.major <- tick.values.major[tick.values.major <=
                                           limits.z[2]]
  tick.values.minor <- tick.values.minor[tick.values.minor >=
                                           limits.z[1]]
  tick.values.minor <- tick.values.minor[tick.values.minor <=
                                           limits.z[2]]

  if(log.z[1]) {
    tick.values.major[which(tick.values.major==0)] <- 1
    tick.values.minor[which(tick.values.minor==0)] <- 1

    tick.values.major <- log(tick.values.major)
    tick.values.minor <- log(tick.values.minor)
  }

  ## calculate z-axis radius
  r <- max(sqrt((limits.x[2])^2 + (data.global[,7] * f)^2))

  ## create z-axes labels
  if(log.z[1]) {
    label.z.text <- signif(exp(tick.values.major) - De.add, 3)
  } else {
    label.z.text <- signif(tick.values.major, 3)
  }

  ## calculate node coordinates for semi-circle
  ellipse.values <- c(min(ifelse(log.z == TRUE,
                                 log(limits.z[1]),
                                 limits.z[1]),
                          tick.values.major,
                          tick.values.minor),
                      max(ifelse(log.z == TRUE,
                                 log(limits.z[2]),
                                 limits.z[2]),
                          tick.values.major,
                          tick.values.minor))

  ## correct for unpleasant value
  ellipse.values[ellipse.values == -Inf] <- 0

  ellipse.x <- r / sqrt(1 + f^2 * (ellipse.values - z.central.global)^2)
  ellipse.y <- (ellipse.values - z.central.global) * ellipse.x
  ellipse <- cbind(ellipse.x, ellipse.y)
  if (rotate)
    ellipse <- ellipse[, 2:1]

  ## calculate statistical labels
  stats.data <- matrix(nrow = 3, ncol = 3)
  data.stats <- as.numeric(data.global[,1])

  if("min" %in% stats == TRUE) {
    stats.data[1, 3] <- data.stats[data.stats == min(data.stats)][1]
    stats.data[1, 1] <- data.global[data.stats == stats.data[1, 3], 6][1]
    stats.data[1, 2] <- data.global[data.stats == stats.data[1, 3], 8][1]
  }

  if("max" %in% stats == TRUE) {
    stats.data[2, 3] <- data.stats[data.stats == max(data.stats)][1]
    stats.data[2, 1] <- data.global[data.stats == stats.data[2, 3], 6][1]
    stats.data[2, 2] <- data.global[data.stats == stats.data[2, 3], 8][1]
  }

  if("median" %in% stats == TRUE) {
    stats.data[3, 3] <- data.stats[data.stats == quantile(data.stats, 0.5, type = 3)]
    stats.data[3, 1] <- data.global[data.stats == stats.data[3, 3], 6][1]
    stats.data[3, 2] <- data.global[data.stats == stats.data[3, 3], 8][1]
  }

  ## re-calculate axes limits if necessary
  if(rotate == FALSE) {
    limits.z.x <- range(ellipse[,1])
    limits.z.y <- range(ellipse[,2])
  } else {
    limits.z.x <- range(ellipse[,2])
    limits.z.y <- range(ellipse[,1])
  }

  if(!("ylim" %in% names(extraArgs))) {
    if(limits.z.y[1] < 0.66 * limits.y[1]) {
      limits.y[1] <- 1.8 * limits.z.y[1]
    }
    if(limits.z.y[2] > 0.77 * limits.y[2]) {
      limits.y[2] <- 1.3 * limits.z.y[2]
    }

    if(rotate == TRUE) {
      limits.y <- c(-max(abs(limits.y)), max(abs(limits.y)))
    }

  }
  if(!("xlim" %in% names(extraArgs))) {
    if(limits.z.x[2] > 1.1 * limits.x[2]) {
      limits.x[2] <- limits.z.x[2]
    }
  }

  ## calculate and paste statistical summary
  De.stats <- matrix(nrow = length(data), ncol = 12)
  colnames(De.stats) <- c("n",
                          "mean",
                          "median",
                          "kdemax",
                          "sd.abs",
                          "sd.rel",
                          "se.abs",
                          "se.rel",
                          "q.25",
                          "q.75",
                          "skewness",
                          "kurtosis")

  for(i in 1:length(data)) {
    statistics <- calc_Statistics(data[[i]])[[summary.method]]
    statistics.2 <- calc_Statistics(data[[i]][,3:4])[[summary.method]]

    De.stats[i,1] <- statistics$n
    De.stats[i,2] <- statistics.2$mean
    De.stats[i,3] <- statistics.2$median
    De.stats[i,5] <- statistics$sd.abs
    De.stats[i,6] <- statistics$sd.rel
    De.stats[i,7] <- statistics$se.abs
    De.stats[i,8] <- statistics$se.rel
    De.stats[i,9] <- quantile(data[[i]][,1], 0.25)
    De.stats[i,10] <- quantile(data[[i]][,1], 0.75)
    De.stats[i,11] <- statistics$skewness
    De.stats[i,12] <- statistics$kurtosis

    ## account for log.z-option
    if(log.z[1]) {
      De.stats[i,2:4] <- exp(De.stats[i,2:4]) - De.add
    }

    ## kdemax - here a little doubled as it appears below again
    De.density <- try(density(x = data[[i]][,1],
                              kernel = "gaussian",
                              bw = bw,
                              from = limits.z[1],
                              to = limits.z[2]),
                      silent = TRUE)

    De.stats[i, 4] <- NA
    if (!inherits(De.density, "try-error")) {
      De.stats[i, 4] <- De.density$x[which.max(De.density$y)]
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

  is.sub <- summary.pos[1] == "sub"
  stops <- NULL
  label.text <- list()
  for (i in 1:length(data)) {
    if (!is.sub)
      stops <- paste(rep("\n", (i - 1) * length(summary)), collapse = "")

    summary.text <- character(0)
    for (j in 1:length(summary)) {
      summary.text <-
        c(summary.text,
          .summary_line("n", summary[j], De.stats[i, 1], sep = is.sub),
          .summary_line("mean", summary[j], De.stats[i, 2], sep = is.sub),
          .summary_line("median", summary[j], De.stats[i, 3], sep = is.sub),
          .summary_line("kdemax", summary[j], De.stats[i, 4], sep = is.sub),
          .summary_line("sd.abs", summary[j], De.stats[i, 5], sep = is.sub,
                        label = "abs. sd"),
          .summary_line("sd.rel", summary[j], De.stats[i, 6], sep = is.sub,
                        label = "rel. sd"),
          .summary_line("se.abs", summary[j], De.stats[i, 7], sep = is.sub,
                        label = "se"),
          .summary_line("se.rel", summary[j], De.stats[i, 8], sep = is.sub,
                        label = "rel. se", percent = TRUE),
          .summary_line("skewness", summary[j], De.stats[i, 11], sep = is.sub),
          .summary_line("kurtosis", summary[j], De.stats[i, 12], sep = is.sub),
          .summary_line("in.2s", summary[j],
                        sum(data[[i]][,7] > -2 & data[[i]][,7] < 2) /
                        nrow(data[[i]]) * 100, sep = is.sub,
                        label = "in 2 sigma", percent = TRUE, digits = 1))
    }
    label.text[[i]] <- paste0(
        if (is.sub) "" else stops,
        paste(summary.text, collapse = ""),
        stops)
  }

  ## remove outer vertical lines from string1
  if (is.sub) {
    for (i in seq_along(label.text)) {
      label.text[[i]] <- substr(x = label.text[[i]],
                                start = 1,
                                stop = nchar(label.text[[i]]) - 3)
    }
  }

  if (!rotate) {
    ## convert keywords into summary placement coordinates
    coords <- .get_keyword_coordinates(summary.pos, limits.x, limits.y)

    ## apply some adjustments to the y positioning
    if (summary.pos[1] %in% c("topleft", "top", "topright")) {
        coords$pos[2] <- coords$pos[2] - par()$cxy[2] * 1.0
      } else if (summary.pos[1] %in% c("bottomleft", "bottom", "bottomright")) {
        coords$pos[2] <- coords$pos[2] + par()$cxy[2] * 3.5
    }
    summary.pos <- coords$pos
    summary.adj <- coords$adj

    ## convert keywords into legend placement coordinates
    coords <- .get_keyword_coordinates(legend.pos, limits.x, limits.y)
    legend.pos <- coords$pos
    legend.adj <- coords$adj

  } else {
    ## convert keywords into summary placement coordinates
    ## this time we swap x and y limits as we are rotated, then apply some
    ## adjustments to the x positioning
    coords <- .get_keyword_coordinates(summary.pos, limits.y, limits.x)
    if (summary.pos[1] %in% c("topleft", "left", "bottomleft")) {
      coords$pos[1] <- coords$pos[1] + par()$cxy[1] * 7.5
    }
    summary.pos <- coords$pos
    summary.adj <- coords$adj

    ## convert keywords into legend placement coordinates
    ## this time we swap x and y limits as we are rotated, then apply some
    ## adjustments to the x positioning
    coords <- .get_keyword_coordinates(legend.pos, limits.y, limits.x)
    if (!missing(legend.pos) &&
        legend.pos[1] %in% c("topleft", "left", "bottomleft")) {
      coords$pos[1] <- coords$pos[1] + par()$cxy[1] * 7.5
    }
    legend.pos <- coords$pos
    legend.adj <- coords$adj
  }

  ## define cartesian plot origins
  if(rotate == FALSE) {
    xy.0 <- c(min(ellipse[,1]) * lostintranslation, min(ellipse[,2]))
  } else {
    xy.0 <- c(min(ellipse[,1]), min(ellipse[,2]) * lostintranslation)
  }

  ## calculate coordinates for dispersion polygon overlay
  y.max.x <- 2 * limits.x[2] / max(data.global[6])

  polygons <- matrix(nrow = length(data), ncol = 14)
  for(i in 1:length(data)) {
    if(dispersion == "qr") {
      ci.lower <- quantile(data[[i]][,1], 0.25)
      ci.upper <- quantile(data[[i]][,1], 0.75)
    } else if(grepl(x = dispersion, pattern = "p") == TRUE) {
      ci.plot <- as.numeric(strsplit(x = dispersion,
                                     split = "p")[[1]][2])
      ci.plot <- (100 - ci.plot) / 100
      ci.lower <- quantile(data[[i]][,1], ci.plot)
      ci.upper <- quantile(data[[i]][,1], 1 - ci.plot)
    } else if(dispersion == "sd") {
      if(log.z == TRUE) {
        ci.lower <- exp(mean(log(data[[i]][,1])) - sd(log(data[[i]][,1])))
        ci.upper <- exp(mean(log(data[[i]][,1])) + sd(log(data[[i]][,1])))
      } else {
        ci.lower <- mean(data[[i]][,1]) - sd(data[[i]][,1])
        ci.upper <- mean(data[[i]][,1]) + sd(data[[i]][,1])
      }
    } else if(dispersion == "2sd") {
      if(log.z == TRUE) {
        ci.lower <- exp(mean(log(data[[i]][,1])) - 2 * sd(log(data[[i]][,1])))
        ci.upper <- exp(mean(log(data[[i]][,1])) + 2 * sd(log(data[[i]][,1])))
      } else {
        ci.lower <- mean(data[[i]][,1]) - 2 * sd(data[[i]][,1])
        ci.upper <- mean(data[[i]][,1]) + 2 * sd(data[[i]][,1])
      }
    }

    if(log.z == TRUE) {
      ci.lower[which(ci.lower < 0)] <- 1
      y.lower <- log(ci.lower)
      y.upper <- log(ci.upper)
    } else {
      y.lower <- ci.lower
      y.upper <- ci.upper
    }

    if(rotate == FALSE) {
      polygons[i,1:7] <- c(limits.x[1],
                           limits.x[2],
                           xy.0[1],
                           par()$usr[2],
                           par()$usr[2],
                           xy.0[1],
                           limits.x[2])
      polygons[i,8:14] <- c(0,
                            (y.upper - z.central.global) *
                              limits.x[2],
                            (y.upper - z.central.global) *
                              xy.0[1],
                            (y.upper - z.central.global) *
                              xy.0[1],
                            (y.lower - z.central.global) *
                              xy.0[1],
                            (y.lower - z.central.global) *
                              xy.0[1],
                            (y.lower - z.central.global) *
                              limits.x[2]
      )
    } else {
      y.max <- par()$usr[4]
      polygons[i,1:7] <- c(limits.x[1],
                           limits.x[2],
                           xy.0[2],
                           y.max,
                           y.max,
                           xy.0[2],
                           limits.x[2])
      polygons[i,8:14] <- c(0,
                            (y.upper - z.central.global) *
                              limits.x[2],
                            (y.upper - z.central.global) *
                              xy.0[2],
                            (y.upper - z.central.global) *
                              xy.0[2],
                            (y.lower - z.central.global) *
                              xy.0[2],
                            (y.lower - z.central.global) *
                              xy.0[2],
                            (y.lower - z.central.global) *
                              limits.x[2]
      )
    }
  }

  ## append information about data in confidence interval
  for(i in 1:length(data)) {
    data.in.2s <- rep(x = FALSE, times = nrow(data[[i]]))
    data.in.2s[data[[i]][,8] > -2 & data[[i]][,8] < 2] <- TRUE
    data[[i]] <- cbind(data[[i]], data.in.2s)
  }

  ## calculate coordinates for 2-sigma bar overlay
  if(bar[1] == TRUE) {
    bars <- matrix(nrow = length(data), ncol = 8)

    for(i in 1:length(data)) {
      bars[i,1:4] <- c(limits.x[1],
                       limits.x[1],
                       ifelse("xlim" %in% names(extraArgs),
                              extraArgs$xlim[2] * 0.95,
                              max(data.global$precision)),
                       ifelse("xlim" %in% names(extraArgs),
                              extraArgs$xlim[2] * 0.95,
                              max(data.global$precision)))

      bars[i,5:8] <- c(-2,
                       2,
                       (data[[i]][1,5] - z.central.global) *
                         bars[i,3] + 2,
                       (data[[i]][1,5] - z.central.global) *
                         bars[i,3] - 2)
    }
  } else {
    bars <- matrix(nrow = length(bar), ncol = 8)

    if(is.numeric(bar) == TRUE & log.z == TRUE) {
      bar <- log(bar)
    }

    for(i in 1:length(bar)) {
      bars[i,1:4] <- c(limits.x[1],
                       limits.x[1],
                       ifelse("xlim" %in% names(extraArgs),
                              extraArgs$xlim[2] * 0.95,
                              max(data.global$precision)),
                       ifelse("xlim" %in% names(extraArgs),
                              extraArgs$xlim[2] * 0.95,
                              max(data.global$precision)))

      bars[i,5:8] <- c(-2,
                       2,
                       (bar[i] - z.central.global) *
                         bars[i,3] + 2,
                       (bar[i] - z.central.global) *
                         bars[i,3] - 2)
    }
  }
  if (rotate == TRUE) {
    bars <- matrix(bars[, rev(seq_len(ncol(bars)))], ncol = 8)
  }

  ## calculate error bar coordinates
  if(error.bars == TRUE) {
    arrow.coords <- list()
    for(i in 1:length(data)) {
      arrow.x1 <- data[[i]][,6]
      arrow.x2 <- data[[i]][,6]
      arrow.y1 <- data[[i]][,1] - data[[i]][,2]
      arrow.y2 <- data[[i]][,1] + data[[i]][,2]

      if(log.z == TRUE) {
        arrow.y1 <- log(arrow.y1)
        arrow.y2 <- log(arrow.y2)
      }

      arrow.coords[[i]] <- cbind(
        arrow.x1,
        arrow.x2,
        (arrow.y1 - z.central.global) * arrow.x1,
        (arrow.y2 - z.central.global) * arrow.x1)
    }
  }

  ## calculate KDE
  KDE <- list()
  KDE.bw <- numeric(0)

  for(i in 1:length(data)) {
    KDE.i <- density(x = data[[i]][,3],
                     kernel = "gaussian",
                     bw = bw,
                     from = ellipse.values[1],
                     to = ellipse.values[2],
                     weights = data[[i]]$weights)
    KDE.xy <- cbind(KDE.i$x, KDE.i$y)
    KDE.bw <- c(KDE.bw, KDE.i$bw)
    KDE.xy <- rbind(c(min(KDE.xy[,1]), 0), KDE.xy, c(max(KDE.xy[,1]), 0))
    KDE[[i]] <- cbind(KDE.xy[, 1], KDE.xy[, 2])
  }

  ## calculate mean KDE bandwidth
  KDE.bw <- mean(KDE.bw, na.rm = TRUE)

  ## calculate max KDE value for labelling
  KDE.max.plot <- numeric(length(data))

  for(i in 1:length(data)) {
    KDE.plot <- density(x = data[[i]][,1],
                        kernel = "gaussian",
                        bw = bw,
                        from = limits.z[1],
                        to = limits.z[2])
    KDE.max.plot[i] <- max(KDE.plot$y)
  }
  KDE.max.plot <- max(KDE.max.plot, na.rm = TRUE)

  ## calculate KDE width
  KDE.max <- max(vapply(KDE, function(x) max(x[, 2]), numeric(1)))

  ## optionally adjust KDE width for boxplot option
  if (boxplot) {
    KDE.max <- 1.3 * KDE.max
  }

  ## calculate histogram data without plotting
  hist.data <- list()
  for(i in 1:length(data)) {
    hist.data[[i]] <- hist(data[[i]][,3], plot = FALSE, breaks = breaks)
  }

  ## calculate maximum histogram bar height for normalisation
  hist.max.plot <- max(vapply(hist.data, function(x) max(x$counts, na.rm = TRUE),
                              numeric(1)), na.rm = TRUE)

  ## normalise histogram bar height to KDE dimensions
  for(i in 1:length(data)) {
    hist.data[[i]]$density <- hist.data[[i]]$counts / hist.max.plot *
      KDE.max.plot
  }

  ## calculate boxplot data without plotting
  boxplot.data <- list()
  for(i in 1:length(data)) {
    boxplot.data[[i]] <- graphics::boxplot(data[[i]][, 3], plot = FALSE)
  }

  ## calculate line coordinates and further parameters
  if(missing(line) == FALSE) {
    ## check if line parameters are R.Lum-objects
    for(i in 1:length(line)) {
      if(is.list(line) == TRUE) {
        if(is(line[[i]], "RLum.Results")) {
          line[[i]] <- as.numeric(get_RLum(object = line[[i]],
                                           data.object = "summary")$de)
        }
      } else if(is(object = line, class2 = "RLum.Results")) {
        line <- as.numeric(get_RLum(object = line,
                                    data.object = "summary")$de)
      }
    }

    ## convert list to vector
    if(is.list(line) == TRUE) {
      line <- unlist(line)
    }

    if(log.z == TRUE) {
      line <- log(line)
    }

    if(rotate == FALSE) {
      line.x <- c(limits.x[1], min(ellipse[, 1]), par()$usr[2])
      min.ellipse.x <- min(ellipse[, 1])
    } else {
      line.x <- c(limits.x[1], min(ellipse[, 2]), y.max)
      min.ellipse.x <- min(ellipse[, 2])
    }
    line.coords <- list()
    for(i in 1:length(line)) {
      line.y <- c(0, rep((line[i] - z.central.global) * min.ellipse.x, 2))
      line.coords[[i]] <- rbind(line.x, line.y)
    }

    if(missing(line.col) == TRUE) {
      line.col <- seq_along(line.coords)
    }

    if(missing(line.lty) == TRUE) {
      line.lty <- rep(1, length(line.coords))
    }

    if(missing(line.label) == TRUE) {
      line.label <- rep("", length(line.coords))
    }
  }

  ## calculate rug coordinates
  if (log.z) {
      rug.values <- log(De.global)
  } else {
      rug.values <- De.global
  }

  rug.coords <- list()
  idx <- if (rotate) 2 else 1
  for (i in 1:length(rug.values)) {
    rug.x <- c(xy.0[idx] * (1 - 0.013 * (layout$abanico$dimension$rugl / 100)),
               xy.0[idx])
    rug.y <- rep((rug.values[i] - z.central.global) * min(ellipse[, idx]), 2)
    rug.coords[[i]] <- rbind(rug.x, rug.y)
  }

  ## Generate plot ------------------------------------------------------------
  ##
  ## determine number of subheader lines to shift the plot
  if(length(summary) > 0 & summary.pos[1] == "sub") {
    shift.lines <- (length(data) + 1) * layout$abanico$dimension$summary.line/100
  } else {
    shift.lines <- 1
  }

  ## extract original plot parameters
  bg.original <- par()$bg
  par(bg = layout$abanico$colour$background)

  if (!rotate) {
    ## setup plot area
    par(mar = c(4.5, 4.5, shift.lines + 1.5, 7),
        xpd = TRUE,
        cex = cex)

    dim <- layout$abanico$dimension
    if (dim$figure.width != "auto" || dim$figure.height != "auto") {
      par(mai = dim$margin / 25.4,
          pin = c(dim$figure.width - dim$margin[2] - dim$margin[4],
                  dim$figure.height - dim$margin[1] - dim$margin[3]) / 25.4)
    }

    ## create empty plot
    par(new = TRUE)
    plot(NA,
         xlim = c(limits.x[1], limits.x[2] * (1 / plot.ratio)),
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
    mtext(text = ylab,
          at = mean(x = c(min(ellipse[,2]),
                          max(ellipse[,2])),
                    na.rm = TRUE),
          #        at = 0, ## BUG FROM VERSION 0.4.0, maybe removed in future
          adj = 0.5,
          side = 2,
          line = 3 * layout$abanico$dimension$ylab.line / 100,
          col = layout$abanico$colour$ylab,
          family = layout$abanico$font.type$ylab,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$ylab)[1],
          cex = cex * layout$abanico$font.size$ylab/12)

    ## calculate upper x-axis label values
    label.x.upper <- if(log.z == TRUE) {
      as.character(round(1/axTicks(side = 1)[-1] * 100, 1))
    } else {
      as.character(round(1/axTicks(side = 1)[-1], 1))
    }

    # optionally, plot 2-sigma-bar
    if(bar[1] != FALSE) {
      for(i in 1:length(bar)) {
        polygon(x = bars[i,1:4],
                y = bars[i,5:8],
                col = bar.fill[i],
                border = bar.line[i])
      }
    }

    ## remove unwanted parts
    polygon(x = c(par()$usr[2],
                  par()$usr[2],
                  par()$usr[2] * 2,
                  par()$usr[2] * 2),
            y = c(min(ellipse[,2]) * 2,
                  max(ellipse[,2]) * 2,
                  max(ellipse[,2]) * 2,
                  min(ellipse[,2]) * 2),
            col = bg.original,
            lty = 0)

    ## optionally, plot dispersion polygon
    if(polygon.fill[1] != "none") {
      for(i in 1:length(data)) {
        polygon(x = polygons[i,1:7],
                y = polygons[i,8:14],
                col = polygon.fill[i],
                border = polygon.line[i])
      }
    }

    ## optionally, add minor grid lines
    if(grid.minor != "none") {
      for(i in 1:length(tick.values.minor)) {
        lines(x = c(limits.x[1], min(ellipse[,1])),
              y = c(0, (tick.values.minor[i] - z.central.global) *
                      min(ellipse[,1])),
              col = grid.minor,
              lwd = 1)
      }

      for(i in 1:length(tick.values.minor)) {
        lines(x = c(xy.0[1], par()$usr[2]),
              y = c((tick.values.minor[i] - z.central.global) *
                      min(ellipse[,1]),
                    (tick.values.minor[i] - z.central.global) *
                      min(ellipse[,1])),
              col = grid.minor,
              lwd = 1)
      }
    }

    ## optionally, add major grid lines
    if(grid.major != "none") {
      for(i in 1:length(tick.values.major)) {
        lines(x = c(limits.x[1], min(ellipse[,1])),
              y = c(0, (tick.values.major[i] - z.central.global) *
                      min(ellipse[,1])),
              col = grid.major,
              lwd = 1)
      }
      for(i in 1:length(tick.values.major)) {
        lines(x = c(xy.0[1], par()$usr[2]),
              y = c((tick.values.major[i] - z.central.global) *
                      min(ellipse[,1]),
                    (tick.values.major[i] - z.central.global) *
                      min(ellipse[,1])),
              col = grid.major,
              lwd = 1)
      }
    }

    ## optionally, plot lines for each bar
    if(lwd[1] > 0 & lty[1] > 0 & bar[1] != FALSE & length(data) == 1) {
      if(bar[1] == TRUE & length(bar) == 1) {
        bar[1] <- z.central.global
      }
      for(i in 1:length(bar)) {
        x2 <- r / sqrt(1 + f^2 * (
          bar[i] - z.central.global)^2)
        y2 <- (bar[i] - z.central.global) * x2
        lines(x = c(limits.x[1], x2, xy.0[1], par()$usr[2]),
              y = c(0, y2, y2, y2),
              lty = lty[i],
              lwd = lwd[i],
              col = centrality.col[i])
      }
    } else if(lwd[1] > 0 & lty[1] > 0 & bar[1] != FALSE) {
      for(i in 1:length(data)) {
        z.line <- ifelse(test = is.numeric(bar[i]) == TRUE,
                         yes = bar[i],
                         no = data[[i]][1,5])

        x2 <- r / sqrt(1 + f^2 * (
          z.line - z.central.global)^2)
        y2 <- (z.line - z.central.global) * x2
        lines(x = c(limits.x[1], x2, xy.0[1], par()$usr[2]),
              y = c(0, y2, y2, y2),
              lty = lty[i],
              lwd = lwd[i],
              col = centrality.col[i])
      }
    }

    ## optionally add further lines
    if(!missing(line)) {
      for(i in 1:length(line)) {
        lines(x = line.coords[[i]][1,1:3],
              y = line.coords[[i]][2,1:3],
              col = line.col[i],
              lty = line.lty[i]
              )
        text(x = line.coords[[i]][1,3],
             y = line.coords[[i]][2,3] + par()$cxy[2] * 0.3,
             labels = line.label[i],
             pos = 2,
             col = line.col[i],
             cex = cex * 0.9)
      }
    }

    ## add plot title
    cex.old <- par()$cex
    par(cex = layout$abanico$font.size$main / 12)
    title(main = main,
          family = layout$abanico$font.type$main,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$main)[1],
          col.main = layout$abanico$colour$main,
          line = shift.lines * layout$abanico$dimension$main / 100)
    par(cex = cex.old)

    ## calculate lower x-axis (precision)
    x.axis.ticks <- axTicks(side = 1)
    x.axis.ticks <- x.axis.ticks[c(TRUE, x.axis.ticks <= limits.x[2])]
    x.axis.ticks <- x.axis.ticks[x.axis.ticks <= max(ellipse[,1])]

    ## x-axis with lables and ticks
    axis(side = 1,
         at = x.axis.ticks,
         col = layout$abanico$colour$xtck1,
         col.axis = layout$abanico$colour$xtck1,
         labels = NA,
         tcl = -layout$abanico$dimension$xtcl1 / 200,
         cex = cex)
    axis(side = 1,
         at = x.axis.ticks,
         line = 2 * layout$abanico$dimension$xtck1.line / 100 - 2,
         lwd = 0,
         col = layout$abanico$colour$xtck1,
         family = layout$abanico$font.type$xtck1,
         font = which(c("normal", "bold", "italic", "bold italic") ==
                        layout$abanico$font.deco$xtck1)[1],
         col.axis = layout$abanico$colour$xtck1,
         cex.axis = layout$abanico$font.size$xlab1/12)

    ## extend axis line to right side of the plot
    lines(x = c(max(x.axis.ticks), max(ellipse[,1])),
          y = c(limits.y[1], limits.y[1]),
          col = layout$abanico$colour$xtck1)

    ## draw closing tick on right hand side
    axis(side = 1,
         tcl = -layout$abanico$dimension$xtcl1 / 200,
         lwd = 0,
         lwd.ticks = 1,
         at = limits.x[2],
         labels = FALSE,
         col = layout$abanico$colour$xtck1)

    axis(side = 1,
         tcl = layout$abanico$dimension$xtcl2 / 200,
         lwd = 0,
         lwd.ticks = 1,
         at = limits.x[2],
         labels = FALSE,
         col = layout$abanico$colour$xtck2)

    ## add lower axis label
    mtext(xlab[2],
          at = (limits.x[1] + max(ellipse[,1])) / 2,
          side = 1,
          line = 2.5 * layout$abanico$dimension$xlab1.line / 100,
          col = layout$abanico$colour$xlab1,
          family = layout$abanico$font.type$xlab1,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$xlab1)[1],
          cex = cex * layout$abanico$font.size$xlab1/12)

    ## add upper axis label
    mtext(xlab[1],
          at = (limits.x[1] + max(ellipse[,1])) / 2,
          side = 1,
          line = -3.5 * layout$abanico$dimension$xlab2.line / 100,
          col = layout$abanico$colour$xlab2,
          family = layout$abanico$font.type$xlab2,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$xlab2)[1],
          cex = cex * layout$abanico$font.size$xlab2/12)

    ## plot upper x-axis
    axis(side = 1,
         at = x.axis.ticks[-1],
         col = layout$abanico$colour$xtck2,
         col.axis = layout$abanico$colour$xtck2,
         labels = NA,
         tcl = layout$abanico$dimension$xtcl2 / 200,
         cex = cex)

    ## remove first tick label (infinity)
    label.x.upper <- label.x.upper[1:(length(x.axis.ticks) - 1)]

    axis(side = 1,
         at = x.axis.ticks[-1],
         labels = label.x.upper,
         line = -1 * layout$abanico$dimension$xtck2.line / 100 - 2,
         lwd = 0,
         col = layout$abanico$colour$xtck2,
         family = layout$abanico$font.type$xtck2,
         font = which(c("normal", "bold", "italic", "bold italic") ==
                        layout$abanico$font.deco$xtck2)[1],
         col.axis = layout$abanico$colour$xtck2,
         cex.axis = layout$abanico$font.size$xlab2/12)

    ## plot y-axis
    if(is.null(extraArgs$yaxt) || extraArgs$yaxt != "n"){
      line <- 2 * layout$abanico$dimension$ytck.line / 100 - 2
      family <- layout$abanico$font.type$ytck
      font <- which(c("normal", "bold", "italic", "bold italic") ==
                    layout$abanico$font.deco$ytck)[1]
      col.axis <- layout$abanico$colour$ytck
      cex.axis <- layout$abanico$font.size$ylab/12
      if(y.axis) {
        char.height <- par()$cxy[2]
        tick.space <- grDevices::axisTicks(usr = limits.y, log = FALSE)
        tick.space <- (max(tick.space) - min(tick.space)) / length(tick.space)

        ## this comes into play for panel plots, e.g., par(mfrow = c(4,4))
        if(tick.space < char.height * 1.7) {
          axis(side = 2,
               tcl = -layout$abanico$dimension$ytcl / 200,
               lwd = 1,
               lwd.ticks = 1,
               at = c(-2, 2),
               labels = c("", ""),
               las = 1,
               col = layout$abanico$colour$ytck)
          axis(side = 2,
               at = 0,
               tcl = 0,
               labels = paste("\u00B1", "2"),
               line = line, las = 1,
               family = family, font = font,
               col.axis = col.axis, cex.axis = cex.axis)
        } else {
          axis(side = 2,
               at = seq(-2, 2, by = 2),
               col = layout$abanico$colour$ytck,
               col.axis = layout$abanico$colour$ytck,
               labels = NA,
               las = 1,
               tcl = -layout$abanico$dimension$ytcl / 200,
               cex = cex)
          axis(side = 2,
               at = seq(-2, 2, by = 2),
               col = layout$abanico$colour$ytck,
               line = line, lwd = 0, las = 1,
               family = family, font = font,
               col.axis = col.axis, cex.axis = cex.axis)
        }
      } else {
        axis(side = 2,
             at = 0,
             col = layout$abanico$colour$ytck,
             col.axis = layout$abanico$colour$ytck,
             labels = NA,
             las = 1,
             tcl = -layout$abanico$dimension$ytcl / 200,
             cex = cex)
        axis(side = 2,
             at = 0,
             col = layout$abanico$colour$ytck,
             line = line, lwd = 0, las = 1,
             family = family, font = font,
             col.axis = col.axis, cex.axis = cex.axis)
      }
    }

    ## plot minor z-ticks
    for(i in 1:length(tick.values.minor)) {
      lines(x = c(par()$usr[2],
                  (1 + 0.007 * cex * layout$abanico$dimension$ztcl / 100) *
                    par()$usr[2]),
            y = c((tick.values.minor[i] - z.central.global) *
                    min(ellipse[,1]),
                  (tick.values.minor[i] - z.central.global) *
                    min(ellipse[,1])),
            col = layout$abanico$colour$ztck)
    }

    ## plot major z-ticks
    for(i in 1:length(tick.values.major)) {
      lines(x = c(par()$usr[2],
                  (1 + 0.015 * cex * layout$abanico$dimension$ztcl / 100) *
                    par()$usr[2]),
            y = c((tick.values.major[i] - z.central.global) *
                    min(ellipse[,1]),
                  (tick.values.major[i] - z.central.global) *
                    min(ellipse[,1])),
            col = layout$abanico$colour$ztck)
    }

    ## plot z-axes
    lines(ellipse, col = layout$abanico$colour$border)
    lines(rep(par()$usr[2], nrow(ellipse)), ellipse[,2],
          col = layout$abanico$colour$ztck)


    ## plot z-axis text
    text(x = (1 + 0.04 * cex * layout$abanico$dimension$ztcl / 100) *
           par()$usr[2],
         y = (tick.values.major - z.central.global) * min(ellipse[,1]),
         labels = label.z.text,
         adj = 0,
         family = layout$abanico$font.type$ztck,
         font = which(c("normal", "bold", "italic", "bold italic") ==
                        layout$abanico$font.deco$ztck)[1],
         cex = cex * layout$abanico$font.size$ztck/12)


    ## plot z-label
    mtext(text = zlab,
          at = mean(x = c(min(ellipse[,2]),
                          max(ellipse[,2])),
                    na.rm = TRUE),
          #        at = 0, ## BUG from version 0.4.0, maybe removed in future
          side = 4,
          las = 3,
          adj = 0.5,
          line = 5 * layout$abanico$dimension$zlab.line / 100,
          col = layout$abanico$colour$zlab,
          family = layout$abanico$font.type$zlab,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$zlab)[1],
          cex = cex * layout$abanico$font.size$zlab/12)

    ## plot values and optionally error bars
    if(error.bars == TRUE) {
      for(i in 1:length(data)) {
        graphics::arrows(x0 = arrow.coords[[i]][,1],
               x1 = arrow.coords[[i]][,2],
               y0 = arrow.coords[[i]][,3],
               y1 = arrow.coords[[i]][,4],
               length = 0,
               angle = 90,
               code = 3,
               col = value.bar[i])
      }
    }

    for(i in 1:length(data)) {
      points(data[[i]][,6][data[[i]][,6] <= limits.x[2]],
             data[[i]][,8][data[[i]][,6] <= limits.x[2]],
             col = value.dot[i],
             pch = pch[i],
             cex = layout$abanico$dimension$pch / 100)
    }

    ## optionally add KDE plot
    if(kde == TRUE) {
      KDE.scale <- (par()$usr[2] - xy.0[1]) / (KDE.max * 1.05)

      ## plot KDE lines
      for(i in 1:length(data)) {
        polygon(x = xy.0[1] + KDE[[i]][,2] * KDE.scale,
                y = (KDE[[i]][,1] - z.central.global) * min(ellipse[,1]),
                col = kde.fill[i],
                border = kde.line[i],
                lwd = 1.7)
      }

      ## plot KDE x-axis
      axis(side = 1,
           at = c(xy.0[1], par()$usr[2]),
           col = layout$abanico$colour$xtck3,
           col.axis = layout$abanico$colour$xtck3,
           labels = NA,
           tcl = -layout$abanico$dimension$xtcl3 / 200,
           cex = cex)

      axis(side = 1,
           at = c(xy.0[1], par()$usr[2]),
           labels = as.character(round(c(0, KDE.max.plot), 3)),
           line = 2 * layout$abanico$dimension$xtck3.line / 100 - 2,
           lwd = 0,
           col = layout$abanico$colour$xtck3,
           family = layout$abanico$font.type$xtck3,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$xtck3)[1],
           col.axis = layout$abanico$colour$xtck3,
           cex.axis = layout$abanico$font.size$xtck3/12)

      mtext(text = paste(xlab[3],
                         " (bw ",
                         round(x = KDE.bw,
                               digits = 3),
                         ")",
                         sep = ""),
            at = (xy.0[1] + par()$usr[2]) / 2,
            side = 1,
            line = 2.5 * layout$abanico$dimension$xlab3.line / 100,
            col = layout$abanico$colour$xlab3,
            family = layout$abanico$font.type$xlab3,
            font = which(c("normal", "bold", "italic", "bold italic") ==
                           layout$abanico$font.deco$xlab3)[1],
            cex = cex * layout$abanico$font.size$xlab3/12)
    }

    ## optionally add histogram or dot plot axis
    if(hist == TRUE) {
      axis(side = 1,
           at = c(xy.0[1], par()$usr[2]),
           labels = as.character(c(0, hist.max.plot)),
           line = -1 * layout$abanico$dimension$xtck3.line / 100 - 2,
           lwd = 0,
           col = layout$abanico$colour$xtck3,
           family = layout$abanico$font.type$xtck3,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$xtck3)[1],
           col.axis = layout$abanico$colour$xtck3,
           cex.axis = layout$abanico$font.size$xtck3/12)

      ## add label
      mtext(text = "n",
            at = (xy.0[1] + par()$usr[2]) / 2,
            side = 1,
            line = -3.5 * layout$abanico$dimension$xlab2.line / 100,
            col = layout$abanico$colour$xlab2,
            family = layout$abanico$font.type$xlab2,
            font = which(c("normal", "bold", "italic", "bold italic") ==
                           layout$abanico$font.deco$xlab2)[1],
            cex = cex * layout$abanico$font.size$xlab2/12)

      ## plot ticks
      axis(side = 1,
           at = c(xy.0[1], par()$usr[2]),
           col = layout$abanico$colour$xtck2,
           col.axis = layout$abanico$colour$xtck2,
           labels = NA,
           tcl = layout$abanico$dimension$xtcl2 / 200,
           cex = cex)

      ## calculate scaling factor for histogram bar heights
      hist.scale <- (par()$usr[2] - xy.0[1]) / (KDE.max.plot * 1.05)

      ## draw each bar for each data set
      for(i in 1:length(data)) {
        for(j in 1:length(hist.data[[i]]$density)) {
          ## calculate x-coordinates
          hist.x.i <- c(xy.0[1],
                        xy.0[1],
                        xy.0[1] + hist.data[[i]]$density[j] * hist.scale,
                        xy.0[1] + hist.data[[i]]$density[j] * hist.scale)

          ## calculate y-coordinates
          hist.y.i <- c((hist.data[[i]]$breaks[j] - z.central.global) *
                          min(ellipse[,1]),
                        (hist.data[[i]]$breaks[j + 1] - z.central.global) *
                          min(ellipse[,1]),
                        (hist.data[[i]]$breaks[j + 1] - z.central.global) *
                          min(ellipse[,1]),
                        (hist.data[[i]]$breaks[j] - z.central.global) *
                          min(ellipse[,1]))

          ## remove data out of z-axis range
          hist.y.i <- ifelse(hist.y.i < min(ellipse[,2]),
                             min(ellipse[,2]),
                             hist.y.i)
          hist.y.i <- ifelse(hist.y.i > max(ellipse[,2]),
                             max(ellipse[,2]),
                             hist.y.i)

          ## draw the bars
          polygon(x = hist.x.i,
                  y = hist.y.i,
                  col = kde.fill[i],
                  border = kde.line[i])
        }
      }
    }

    ## optionally add dot plot
    if(dots == TRUE) {
      for(i in 1:length(data)) {
        for(j in 1:length(hist.data[[i]]$counts)) {

          ## calculate scaling factor for histogram bar heights
          dots.distance <- (par()$usr[2] - (xy.0[1] + par()$cxy[1] * 0.4)) / hist.max.plot

          dots.x.i <- seq(from = xy.0[1] + par()$cxy[1] * 0.4,
                          by = dots.distance,
                          length.out = hist.data[[i]]$counts[j])

          dots.y.i <- rep((hist.data[[i]]$mids[j] - z.central.global) *
                            min(ellipse[,1]), length(dots.x.i))

          ## remove data out of z-axis range
          dots.x.i <- dots.x.i[dots.y.i >= min(ellipse[,2]) &
                                 dots.y.i <= max(ellipse[,2])]
          dots.y.i <- dots.y.i[dots.y.i >= min(ellipse[,2]) &
                                 dots.y.i <= max(ellipse[,2])]

          if(max(c(0, dots.x.i), na.rm = TRUE) >= (par()$usr[2] -
                                                   par()$cxy[1] * 0.4)) {
            dots.y.i <- dots.y.i[dots.x.i < (par()$usr[2] - par()$cxy[1] * 0.4)]
            dots.x.i <- dots.x.i[dots.x.i < (par()$usr[2] - par()$cxy[1] * 0.4)]
            pch.dots <- c(rep(20, max(length(dots.x.i) - 1),1), 15)

          } else {
            pch.dots <- rep(20, length(dots.x.i))
          }

          ## plot points
          points(x = dots.x.i,
                 y = dots.y.i,
                 pch = "|",
                 cex = 0.7 * cex,
                 col = kde.line[i])
        }
      }
    }

    ## optionally add box plot
    if(boxplot == TRUE) {

      for(i in 1:length(data)) {

        ## draw median line
        lines(x = c(xy.0[1] + KDE.max * 0.85, xy.0[1] + KDE.max * 0.95),
              y = c((boxplot.data[[i]]$stats[3,1] - z.central.global) *
                      min(ellipse[,1]),
                    (boxplot.data[[i]]$stats[3,1] - z.central.global) *
                      min(ellipse[,1])),
              lwd = 2,
              col = kde.line[i])

        ## draw p25-p75-polygon
        polygon(x = c(xy.0[1] + KDE.max * 0.85,
                      xy.0[1] + KDE.max * 0.85,
                      xy.0[1] + KDE.max * 0.95,
                      xy.0[1] + KDE.max * 0.95),
                y = c((boxplot.data[[i]]$stats[2,1] - z.central.global) *
                        min(ellipse[,1]),
                      (boxplot.data[[i]]$stats[4,1] - z.central.global) *
                        min(ellipse[,1]),
                      (boxplot.data[[i]]$stats[4,1] - z.central.global) *
                        min(ellipse[,1]),
                      (boxplot.data[[i]]$stats[2,1] - z.central.global) *
                        min(ellipse[,1])),
                border = kde.line[i])

        ## draw whiskers
        lines(x = c(xy.0[1] + KDE.max * 0.9,
                    xy.0[1] + KDE.max * 0.9),
              y = c((boxplot.data[[i]]$stats[2,1] - z.central.global) *
                      min(ellipse[,1]),
                    (boxplot.data[[i]]$stats[1,1] - z.central.global) *
                      min(ellipse[,1])),
              col = kde.line[i])

        lines(x = c(xy.0[1] + KDE.max * 0.87,
                    xy.0[1] + KDE.max * 0.93),
              y = rep((boxplot.data[[i]]$stats[1,1] - z.central.global) *
                        min(ellipse[,1]), 2),
              col = kde.line[i])

        lines(x = c(xy.0[1] + KDE.max * 0.9,
                    xy.0[1] + KDE.max * 0.9),
              y = c((boxplot.data[[i]]$stats[4,1] - z.central.global) *
                      min(ellipse[,1]),
                    (boxplot.data[[i]]$stats[5,1] - z.central.global) *
                      min(ellipse[,1])),
              col = kde.line[i])

        lines(x = c(xy.0[1] + KDE.max * 0.87,
                    xy.0[1] + KDE.max * 0.93),
              y = rep((boxplot.data[[i]]$stats[5,1] - z.central.global) *
                        min(ellipse[,1]), 2),
              col = kde.line[i])

        ## draw outlier points
        points(x = rep(xy.0[1] + KDE.max * 0.9,
                       length(boxplot.data[[i]]$out)),
               y = (boxplot.data[[i]]$out - z.central.global) *
                 min(ellipse[,1]),
               cex = cex * 0.8,
               col = kde.line[i])
      }
    }

    ## optionally add stats, i.e. min, max, median sample text
    if(length(stats) > 0) {
      text(x = stats.data[,1],
           y = stats.data[,2],
           pos = 2,
           labels = round(stats.data[,3], 1),
           family = layout$abanico$font.type$stats,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$stats)[1],
           cex = cex * layout$abanico$font.size$stats/12,
           col = layout$abanico$colour$stats)
    }

    ## optionally add rug
    if(rug == TRUE) {
      for(i in 1:length(rug.coords)) {
        lines(x = rug.coords[[i]][1,],
              y = rug.coords[[i]][2,],
              col = value.rug[data.global[i,10]])
      }
    }

    ## plot KDE base line
    lines(x = c(xy.0[1], xy.0[1]),
          y = c(min(ellipse[,2]), max(ellipse[,2])),
          col = layout$abanico$colour$border)

    ## draw border around plot
    if(frame == 1) {
      polygon(x = c(limits.x[1], min(ellipse[,1]), par()$usr[2],
                    par()$usr[2], min(ellipse[,1])),
              y = c(0, max(ellipse[,2]), max(ellipse[,2]),
                    min(ellipse[,2]), min(ellipse[,2])),
              border = layout$abanico$colour$border,
              lwd = 0.8)
    } else if(frame == 2) {
      polygon(x = c(limits.x[1], min(ellipse[,1]), par()$usr[2],
                    par()$usr[2], min(ellipse[,1]), limits.x[1]),
              y = c(2, max(ellipse[,2]), max(ellipse[,2]),
                    min(ellipse[,2]), min(ellipse[,2]), -2),
              border = layout$abanico$colour$border,
              lwd = 0.8)
    } else if(frame == 3) {
      polygon(x = c(limits.x[1], par()$usr[2],
                    par()$usr[2], limits.x[1]),
              y = c(max(ellipse[,2]), max(ellipse[,2]),
                    min(ellipse[,2]), min(ellipse[,2])),
              border = layout$abanico$colour$border,
              lwd = 0.8)
    }

    ## optionally add legend content
    if(!missing(legend)) {
      ## store and change font family
      par.family <- par()$family
      par(family = layout$abanico$font.type$legend)

      legend(x = legend.pos[1],
             y = 0.8 * legend.pos[2],
             xjust = legend.adj[1],
             yjust = legend.adj[2],
             legend = legend,
             pch = pch,
             col = value.dot,
             text.col = value.dot,
             text.font = which(c("normal", "bold", "italic", "bold italic") ==
                                 layout$abanico$font.deco$legend)[1],
             cex = cex * layout$abanico$font.size$legend/12,
             bty = "n")

      ## restore font family
      par(family = par.family)
    }

    ## optionally add subheader text
    mtext(text = mtext,
          side = 3,
          line = (shift.lines - 2) * layout$abanico$dimension$mtext / 100,
          col = layout$abanico$colour$mtext,
          family = layout$abanico$font.type$mtext,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$mtext)[1],
          cex = cex * layout$abanico$font.size$mtext / 12)

    ## add summary content
    for(i in 1:length(data)) {
      if(summary.pos[1] != "sub") {
        text(x = summary.pos[1],
             y = summary.pos[2],
             adj = summary.adj,
             labels = label.text[[i]],
             col = summary.col[i],
             family = layout$abanico$font.type$summary,
             font = which(c("normal", "bold", "italic", "bold italic") ==
                            layout$abanico$font.deco$summary)[1],
             cex = cex * layout$abanico$font.size$summary / 12)
      } else {
        if(mtext == "") {
          mtext(side = 3,
                line = (shift.lines- 1 - i) *
                  layout$abanico$dimension$summary / 100 ,
                text = label.text[[i]],
                col = summary.col[i],
                family = layout$abanico$font.type$summary,
                font = which(c("normal", "bold", "italic", "bold italic") ==
                               layout$abanico$font.deco$summary)[1],
                cex = cex * layout$abanico$font.size$summary / 12)
        }
      }
    }
  } else {
    ## setup plot area
    par(mar = c(4, 4, shift.lines + 5, 4),
        xpd = TRUE,
        cex = cex)

    dim <- layout$abanico$dimension
    if (dim$figure.width != "auto" || dim$figure.height != "auto") {
      par(mai = dim$margin / 25.4,
          pin = c(dim$figure.width - dim$margin[2] - dim$margin[4],
                  dim$figure.height - dim$margin[1] - dim$margin[3]) / 25.4)
    }

    ## create empty plot
    par(new = TRUE)
    plot(NA,
         xlim = limits.y,
         ylim = c(limits.x[1], limits.x[2] * (1 / plot.ratio)),
         main = "",
         sub = sub,
         xlab = "",
         ylab = "",
         xaxs = "i",
         yaxs = "i",
         frame.plot = FALSE,
         axes = FALSE)

    ## add y-axis label
    mtext(text = ylab,
          at = 0,
          adj = 0.5,
          side = 1,
          line = 3 * layout$abanico$dimension$ylab.line / 100,
          col = layout$abanico$colour$ylab,
          family = layout$abanico$font.type$ylab,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$ylab)[1],
          cex = cex * layout$abanico$font.size$ylab/12)

    ## calculate upper x-axis label values
    label.x.upper <- if(log.z == TRUE) {
      as.character(round(1/axTicks(side = 2)[-1] * 100, 1))
    } else {
      as.character(round(1/axTicks(side = 2)[-1], 1))
    }

    # optionally, plot 2-sigma-bar
    if(bar[1] != FALSE) {
      for(i in 1:length(bar)) {
        polygon(x = bars[i,1:4],
                y = bars[i,5:8],
                col = bar.fill[i],
                border = bar.line[i])
      }
    }

    ## remove unwanted parts
    polygon(y = c(par()$usr[2],
                  par()$usr[2],
                  par()$usr[2] * 2,
                  par()$usr[2] * 2),
            x = c(min(ellipse[,2]) * 2,
                  max(ellipse[,2]) * 2,
                  max(ellipse[,2]) * 2,
                  min(ellipse[,2]) * 2),
            col = bg.original,
            lty = 0)

    ## optionally, plot dispersion polygon
    if(polygon.fill[1] != "none") {
      for(i in 1:length(data)) {
        polygon(x = polygons[i,8:14],
                y = polygons[i,1:7],
                col = polygon.fill[i],
                border = polygon.line[i])
      }
    }

    ## optionally, add minor grid lines
    if(grid.minor != "none") {
      for(i in 1:length(tick.values.minor)) {
        lines(y = c(limits.x[1], min(ellipse[,1])),
              x = c(0, (tick.values.minor[i] - z.central.global) * min(ellipse[,1])),
              col = grid.minor,
              lwd = 1)
      }
      for(i in 1:length(tick.values.minor)) {
        lines(y = c(xy.0[2], par()$usr[2]),
              x = c((tick.values.minor[i] - z.central.global) * min(ellipse[,1]),
                    (tick.values.minor[i] - z.central.global) * min(ellipse[,1])),
              col = grid.minor,
              lwd = 1)
      }
    }

    ## optionally, add major grid lines
    if(grid.major != "none") {
      for(i in 1:length(tick.values.major)) {
        lines(y = c(limits.x[1], min(ellipse[,2])),
              x = c(0, (tick.values.major[i] - z.central.global) * min(ellipse[,2])),
              col = grid.major,
              lwd = 1)
      }
      for(i in 1:length(tick.values.major)) {
        lines(y = c(xy.0[2],y.max),
              x = c((tick.values.major[i] - z.central.global) * min(ellipse[,2]),
                    (tick.values.major[i] - z.central.global) * min(ellipse[,2])),
              col = grid.major,
              lwd = 1)
      }
    }

    ## optionally, plot lines for each bar
    if(lwd[1] > 0 & lty[1] > 0 & bar[1] != FALSE & length(data) == 1) {
      if(bar[1] == TRUE & length(bar) == 1) {
        bar[1] <- z.central.global
      }
      for(i in 1:length(bar)) {
        x2 <- r / sqrt(1 + f^2 * (
          bar[i] - z.central.global)^2)
        y2 <- (bar[i] - z.central.global) * x2
        lines(x = c(0, y2, y2, y2),
              y = c(limits.x[1], x2, xy.0[2], par()$usr[4]),
              lty = lty[i],
              lwd = lwd[i],
              col = centrality.col[i])
      }
    }

    ## optionally add further lines
    if(missing(line) == FALSE) {
      for(i in 1:length(line)) {
        lines(y = line.coords[[i]][1,1:3],
              x = line.coords[[i]][2,1:3],
              col = line.col[i],
              lty = line.lty[i]
              )
        text(y = line.coords[[i]][1,3],
             x = line.coords[[i]][2,3] + par()$cxy[2] * 0.3,
             labels = line.label[i],
             pos = 2,
             col = line.col[i],
             cex = cex * 0.9)
      }
    }

    ## add plot title
    cex.old <- par()$cex
    par(cex = layout$abanico$font.size$main / 12)
    title(main = main,
          family = layout$abanico$font.type$main,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$main)[1],
          col.main = layout$abanico$colour$main,
          line = (shift.lines + 3.5) * layout$abanico$dimension$main / 100)
    par(cex = cex.old)

    ## calculate lower x-axis (precision)
    x.axis.ticks <- axTicks(side = 2)
    x.axis.ticks <- x.axis.ticks[c(TRUE, x.axis.ticks <= limits.x[2])]
    x.axis.ticks <- x.axis.ticks[x.axis.ticks <= max(ellipse[,2])]

    ## x-axis with lables and ticks
    axis(side = 2,
         at = x.axis.ticks,
         col = layout$abanico$colour$xtck1,
         col.axis = layout$abanico$colour$xtck1,
         labels = NA,
         tcl = -layout$abanico$dimension$xtcl1 / 200,
         cex = cex)
    axis(side = 2,
         at = x.axis.ticks,
         line = 2 * layout$abanico$dimension$xtck1.line / 100 - 2,
         lwd = 0,
         col = layout$abanico$colour$xtck1,
         family = layout$abanico$font.type$xtck1,
         font = which(c("normal", "bold", "italic", "bold italic") ==
                        layout$abanico$font.deco$xtck1)[1],
         col.axis = layout$abanico$colour$xtck1,
         cex.axis = layout$abanico$font.size$xlab1/12)

    ## extend axis line to right side of the plot
    lines(y = c(max(x.axis.ticks), max(ellipse[,2])),
          x = c(limits.y[1], limits.y[1]),
          col = layout$abanico$colour$xtck1)

    ## draw closing tick on right hand side
    axis(side = 2,
         tcl = -layout$abanico$dimension$xtcl1 / 200,
         lwd = 0,
         lwd.ticks = 1,
         at = limits.x[2],
         labels = FALSE,
         col = layout$abanico$colour$xtck1)

    axis(side = 2,
         tcl = layout$abanico$dimension$xtcl2 / 200,
         lwd = 0,
         lwd.ticks = 1,
         at = limits.x[2],
         labels = FALSE,
         col = layout$abanico$colour$xtck2)

    ## add lower axis label
    mtext(xlab[2],
          at = (limits.x[1] + max(ellipse[,2])) / 2,
          side = 2,
          line = 2.5 * layout$abanico$dimension$xlab1.line / 100,
          col = layout$abanico$colour$xlab1,
          family = layout$abanico$font.type$xlab1,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$xlab1)[1],
          cex = cex * layout$abanico$font.size$xlab1/12)

    ## add upper axis label
    mtext(xlab[1],
          at = (limits.x[1] + max(ellipse[,2])) / 2,
          side = 2,
          line = -3.5 * layout$abanico$dimension$xlab2.line / 100,
          col = layout$abanico$colour$xlab2,
          family = layout$abanico$font.type$xlab2,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$xlab2)[1],
          cex = cex * layout$abanico$font.size$xlab2/12)

    ## plot upper x-axis
    axis(side = 2,
         at = x.axis.ticks[-1],
         col = layout$abanico$colour$xtck2,
         col.axis = layout$abanico$colour$xtck2,
         labels = NA,
         tcl = layout$abanico$dimension$xtcl2 / 200,
         cex = cex)

    ## remove first tick label (infinity)
    label.x.upper <- label.x.upper[1:(length(x.axis.ticks) - 1)]

    axis(side = 2,
         at = x.axis.ticks[-1],
         labels = label.x.upper,
         line = -1 * layout$abanico$dimension$xtck2.line / 100 - 2,
         lwd = 0,
         col = layout$abanico$colour$xtck2,
         family = layout$abanico$font.type$xtck2,
         font = which(c("normal", "bold", "italic", "bold italic") ==
                        layout$abanico$font.deco$xtck2)[1],
         col.axis = layout$abanico$colour$xtck2,
         cex.axis = layout$abanico$font.size$xlab2/12)

    ## plot y-axis
    if(y.axis[1]) {
      char.height <- par()$cxy[2]
      tick.space <- grDevices::axisTicks(usr = limits.y, log = FALSE)
      tick.space <- (max(tick.space) - min(tick.space)) / length(tick.space)
      if(tick.space < char.height * 1.7) {
        axis(side = 1,
             tcl = -layout$abanico$dimension$ytcl / 200,
             lwd = 1,
             lwd.ticks = 1,
             at = c(-2, 2),
             labels = c("", ""),
             las = 1,
             col = layout$abanico$colour$ytck)

        axis(side = 1,
             at = 0,
             tcl = 0,
             line = 2 * layout$abanico$dimension$ytck.line / 100 - 2,
             labels = paste("\u00B1", "2"),
             las = 1,
             family = layout$abanico$font.type$ytck,
             font = which(c("normal", "bold", "italic", "bold italic") ==
                            layout$abanico$font.deco$ytck)[1],
             col.axis = layout$abanico$colour$ytck,
             cex.axis = layout$abanico$font.size$ylab/12)
      } else {
        axis(side = 1,
             at = seq(-2, 2, by = 2),
             col = layout$abanico$colour$ytck,
             col.axis = layout$abanico$colour$ytck,
             labels = NA,
             las = 1,
             tcl = -layout$abanico$dimension$ytcl / 200,
             cex = cex)
        axis(side = 1,
             at = seq(-2, 2, by = 2),
             line = 2 * layout$abanico$dimension$ytck.line / 100 - 2,
             lwd = 0,
             las = 1,
             col = layout$abanico$colour$ytck,
             family = layout$abanico$font.type$ytck,
             font = which(c("normal", "bold", "italic", "bold italic") ==
                            layout$abanico$font.deco$ytck)[1],
             col.axis = layout$abanico$colour$ytck,
             cex.axis = layout$abanico$font.size$ylab/12)
      }
    } else {
      axis(side = 1,
           at = 0,
           col = layout$abanico$colour$ytck,
           col.axis = layout$abanico$colour$ytck,
           labels = NA,
           las = 1,
           tcl = -layout$abanico$dimension$ytcl / 200,
           cex = cex)
      axis(side = 1,
           at = 0,
           line = 2 * layout$abanico$dimension$ytck.line / 100 - 2,
           lwd = 0,
           las = 1,
           col = layout$abanico$colour$ytck,
           family = layout$abanico$font.type$ytck,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$ytck)[1],
           col.axis = layout$abanico$colour$ytck,
           cex.axis = layout$abanico$font.size$ylab/12)
    }

    ## plot minor z-ticks
    for(i in 1:length(tick.values.minor)) {
      lines(y = c(par()$usr[4],
                  (1 + 0.015 * cex * layout$abanico$dimension$ztcl / 100) *
                    y.max),
            x = c((tick.values.minor[i] - z.central.global) *
                    min(ellipse[,2]),
                  (tick.values.minor[i] - z.central.global) *
                    min(ellipse[,2])),
            col = layout$abanico$colour$ztck)
    }

    ## plot major z-ticks
    for(i in 1:length(tick.values.major)) {
      lines(y = c(par()$usr[4],
                  (1 + 0.03 * cex * layout$abanico$dimension$ztcl / 100) *
                    y.max),
            x = c((tick.values.major[i] - z.central.global) *
                    min(ellipse[,2]),
                  (tick.values.major[i] - z.central.global) *
                    min(ellipse[,2])),
            col = layout$abanico$colour$ztck)
    }

    ## plot z-axes
    lines(ellipse, col = layout$abanico$colour$border)
    lines(y = rep(par()$usr[4], nrow(ellipse)),
          x = ellipse[,1],
          col = layout$abanico$colour$ztck)

    ## plot z-axis text
    text(y = (1 + 0.06 * cex * layout$abanico$dimension$ztcl / 100) *
           y.max,
         x = (tick.values.major - z.central.global) * min(ellipse[,2]),
         labels = label.z.text,
         adj = 0.5,
         family = layout$abanico$font.type$ztck,
         font = which(c("normal", "bold", "italic", "bold italic") ==
                        layout$abanico$font.deco$ztck)[1],
         cex = cex * layout$abanico$font.size$ztck/12)

    ## plot z-label
    mtext(text = zlab,
          at = 0,
          side = 3,
          las = 1,
          adj = 0.5,
          line = 2.5 * layout$abanico$dimension$zlab.line / 100,
          col = layout$abanico$colour$zlab,
          family = layout$abanico$font.type$zlab,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$zlab)[1],
          cex = cex * layout$abanico$font.size$zlab/12)

    ## plot values and optionally error bars
    if(error.bars == TRUE) {
      for(i in 1:length(data)) {
        graphics::arrows(y0 = arrow.coords[[i]][,1],
               y1 = arrow.coords[[i]][,2],
               x0 = arrow.coords[[i]][,3],
               x1 = arrow.coords[[i]][,4],
               length = 0,
               angle = 90,
               code = 3,
               col = value.bar[i])
      }
    }

    for(i in 1:length(data)) {
      points(y = data[[i]][,6][data[[i]][,6] <= limits.x[2]],
             x = data[[i]][,8][data[[i]][,6] <= limits.x[2]],
             col = value.dot[i],
             pch = pch[i],
             cex = layout$abanico$dimension$pch / 100)
    }

    ## optionally add KDE plot
    if(kde == TRUE) {

      KDE.scale <- (par()$usr[4] - xy.0[2]) / (KDE.max * 1.05)
      ## plot KDE lines
      for(i in 1:length(data)) {
        polygon(y = xy.0[2] + KDE[[i]][,2] * KDE.scale,
                x = (KDE[[i]][,1] - z.central.global) * min(ellipse[,2]),
                col = kde.fill[i],
                border = kde.line[i],
                lwd = 1.7)
      }

      ## plot KDE x-axis
      axis(side = 2,
           at = c(xy.0[2], y.max),
           col = layout$abanico$colour$xtck3,
           col.axis = layout$abanico$colour$xtck3,
           labels = NA,
           tcl = -layout$abanico$dimension$xtcl3 / 200,
           cex = cex)

      axis(side = 2,
           at = c(xy.0[2], y.max),
           labels = as.character(round(c(0, KDE.max.plot), 3)),
           line = 2 * layout$abanico$dimension$xtck3.line / 100 - 2,
           lwd = 0,
           col = layout$abanico$colour$xtck3,
           family = layout$abanico$font.type$xtck3,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$xtck3)[1],
           col.axis = layout$abanico$colour$xtck3,
           cex.axis = layout$abanico$font.size$xtck3/12)

      mtext(text = paste(xlab[3],
                         " (bw ",
                         round(x = KDE.bw,
                               digits = 3),
                         ")",
                         sep = ""),
            at = (xy.0[2] + y.max) / 2,
            side = 2,
            line = 2.5 * layout$abanico$dimension$xlab3.line / 100,
            col = layout$abanico$colour$xlab3,
            family = layout$abanico$font.type$xlab3,
            font = which(c("normal", "bold", "italic", "bold italic") ==
                           layout$abanico$font.deco$xlab3)[1],
            cex = cex * layout$abanico$font.size$xlab3/12)
    }

    ## optionally add histogram or dot plot axis
    if(hist == TRUE) {
      axis(side = 2,
           at = c(xy.0[2], y.max),
           labels = as.character(c(0, hist.max.plot)),
           line = -1 * layout$abanico$dimension$xtck3.line / 100 - 2,
           lwd = 0,
           col = layout$abanico$colour$xtck3,
           family = layout$abanico$font.type$xtck3,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$xtck3)[1],
           col.axis = layout$abanico$colour$xtck3,
           cex.axis = layout$abanico$font.size$xtck3/12)

      ## add label
      mtext(text = "n",
            at = (xy.0[2] + y.max) / 2,
            side = 2,
            line = -3.5 * layout$abanico$dimension$xlab2.line / 100,
            col = layout$abanico$colour$xlab2,
            family = layout$abanico$font.type$xlab2,
            font = which(c("normal", "bold", "italic", "bold italic") ==
                           layout$abanico$font.deco$xlab2)[1],
            cex = cex * layout$abanico$font.size$xlab2/12)

      ## plot ticks
      axis(side = 2,
           at = c(xy.0[2], y.max),
           col = layout$abanico$colour$xtck2,
           col.axis = layout$abanico$colour$xtck2,
           labels = NA,
           tcl = layout$abanico$dimension$xtcl2 / 200,
           cex = cex)

      ## calculate scaling factor for histogram bar heights
      hist.scale <- (par()$usr[4] - xy.0[2]) / (KDE.max.plot * 1.05)

      ## draw each bar for each data set
      for(i in 1:length(data)) {
        for(j in 1:length(hist.data[[i]]$density)) {
          ## calculate x-coordinates
          hist.x.i <- c(xy.0[2],
                        xy.0[2],
                        xy.0[2] + hist.data[[i]]$density[j] * hist.scale,
                        xy.0[2] + hist.data[[i]]$density[j] * hist.scale)

          ## calculate y-coordinates
          hist.y.i <- c((hist.data[[i]]$breaks[j] - z.central.global) *
                          min(ellipse[,2]),
                        (hist.data[[i]]$breaks[j + 1] - z.central.global) *
                          min(ellipse[,2]),
                        (hist.data[[i]]$breaks[j + 1] - z.central.global) *
                          min(ellipse[,2]),
                        (hist.data[[i]]$breaks[j] - z.central.global) *
                          min(ellipse[,2]))

          ## remove data out of z-axis range
          hist.y.i <- ifelse(hist.y.i < min(ellipse[,1]),
                             min(ellipse[,1]),
                             hist.y.i)
          hist.y.i <- ifelse(hist.y.i > max(ellipse[,1]),
                             max(ellipse[,1]),
                             hist.y.i)

          ## draw the bars
          polygon(y = hist.x.i,
                  x = hist.y.i,
                  col = kde.fill[i],
                  border = kde.line[i])
        }
      }
    }

    ## optionally add dot plot
    if(dots == TRUE) {
      for(i in 1:length(data)) {
        for(j in 1:length(hist.data[[i]]$counts)) {

          ## calculate scaling factor for histogram bar heights
          dots.distance <- (par()$usr[4] - (xy.0[2] + par()$cxy[1] * 0.4)) / hist.max.plot

          dots.x.i <- seq(from = xy.0[2] + par()$cxy[2] * 0.4,
                          by = dots.distance,
                          length.out = hist.data[[i]]$counts[j])

          dots.y.i <- rep((hist.data[[i]]$mids[j] - z.central.global) *
                            min(ellipse[,2]), length(dots.x.i))

          ## remove data out of z-axis range
          dots.x.i <- dots.x.i[dots.y.i >= min(ellipse[,1]) &
                                 dots.y.i <= max(ellipse[,1])]
          dots.y.i <- dots.y.i[dots.y.i >= min(ellipse[,1]) &
                                 dots.y.i <= max(ellipse[,1])]

          if(max(c(0, dots.x.i), na.rm = TRUE) >= (par()$usr[4] -
                                                   par()$cxy[2] * 0.4)) {
            dots.y.i <- dots.y.i[dots.x.i < (par()$usr[4] - par()$cxy[2] * 0.4)]
            dots.x.i <- dots.x.i[dots.x.i < (par()$usr[4] - par()$cxy[2] * 0.4)]
            pch.dots <- c(rep(20, length(dots.x.i) - 1), 15)
          } else {
            pch.dots <- rep(20, length(dots.x.i))
          }

          ## plot points
          points(y = dots.x.i,
                 x = dots.y.i,
                 pch = "-",
                 cex = 0.7 * cex,
                 col = kde.line[i])
        }
      }
    }

    ## optionally add box plot
    if(boxplot == TRUE) {

      for(i in 1:length(data)) {

        ## draw median line
        lines(x = c((boxplot.data[[i]]$stats[3,1] - z.central.global) *
                      min(ellipse[,2]),
                    (boxplot.data[[i]]$stats[3,1] - z.central.global) *
                      min(ellipse[,2])),
              y = c(min(ellipse[,2]) + KDE.max * 0.91,
                    xy.0[2] + KDE.max * 0.96),
              lwd = 2,
              col = kde.line[i])

        ## draw p25-p75-polygon
        polygon(y = c(min(ellipse[,2]) + KDE.max * 0.91,
                      min(ellipse[,2]) + KDE.max * 0.91,
                      xy.0[2] + KDE.max * 0.96,
                      xy.0[2] + KDE.max * 0.96),
                x = c((boxplot.data[[i]]$stats[2,1] - z.central.global) *
                        min(ellipse[,2]),
                      (boxplot.data[[i]]$stats[4,1] - z.central.global) *
                        min(ellipse[,2]),
                      (boxplot.data[[i]]$stats[4,1] - z.central.global) *
                        min(ellipse[,2]),
                      (boxplot.data[[i]]$stats[2,1] - z.central.global) *
                        min(ellipse[,2])),
                border = kde.line[i])

        ## draw whiskers
        lines(y = rep(mean(c(min(ellipse[,2]) + KDE.max * 0.91,
                             xy.0[2] + KDE.max * 0.96)), 2),
              x = c((boxplot.data[[i]]$stats[2,1] - z.central.global) *
                      min(ellipse[,2]),
                    (boxplot.data[[i]]$stats[1,1] - z.central.global) *
                      min(ellipse[,2])),
              col = kde.line[i])

        lines(y = c(min(ellipse[,2]) + KDE.max * 0.91,
                    xy.0[2] + KDE.max * 0.96),
              x = rep((boxplot.data[[i]]$stats[1,1] - z.central.global) *
                        min(ellipse[,2]), 2),
              col = kde.line[i])

        lines(y = rep(mean(c(min(ellipse[,2]) + KDE.max * 0.91,
                             xy.0[2] + KDE.max * 0.96)), 2),
              x = c((boxplot.data[[i]]$stats[4,1] - z.central.global) *
                      min(ellipse[,2]),
                    (boxplot.data[[i]]$stats[5,1] - z.central.global) *
                      min(ellipse[,2])),
              col = kde.line[i])

        lines(y = c(min(ellipse[,2]) + KDE.max * 0.91,
                    xy.0[2] + KDE.max * 0.96),
              x = rep((boxplot.data[[i]]$stats[5,1] - z.central.global) *
                        min(ellipse[,2]), 2),
              col = kde.line[i])

        ## draw outlier points
        points(y = rep(mean(c(min(ellipse[,2]) + KDE.max * 0.91,
                              xy.0[2] + KDE.max * 0.96)),
                       length(boxplot.data[[i]]$out)),
               x = (boxplot.data[[i]]$out - z.central.global) *
                 min(ellipse[,2]),
               cex = cex * 0.8,
               col = kde.line[i])
      }
    }

    ## optionally add stats, i.e. min, max, median sample text
    if(length(stats) > 0) {
      text(y = stats.data[,1],
           x = stats.data[,2],
           pos = 2,
           labels = round(stats.data[,3], 1),
           family = layout$abanico$font.type$stats,
           font = which(c("normal", "bold", "italic", "bold italic") ==
                          layout$abanico$font.deco$stats)[1],
           cex = cex * layout$abanico$font.size$stats/12,
           col = layout$abanico$colour$stats)
    }

    ## optionally add rug
    if(rug == TRUE) {
      for(i in 1:length(rug.coords)) {
        lines(y = rug.coords[[i]][1,],
              x = rug.coords[[i]][2,],
              col = value.rug[data.global[i,10]])
      }
    }

    ## plot KDE base line
    lines(y = c(xy.0[2], xy.0[2]),
          x = c(min(ellipse[,1]), max(ellipse[,1])),
          col = layout$abanico$colour$border)

    ## draw border around plot
    polygon(y = c(limits.x[1], min(ellipse[,2]), y.max,
                  y.max, min(ellipse[,2])),
            x = c(0, max(ellipse[,1]), max(ellipse[,1]),
                  min(ellipse[,1]), min(ellipse[,1])),
            border = layout$abanico$colour$border,
            lwd = 0.8)

    ## optionally add legend content
    if(missing(legend) == FALSE) {
      ## store and change font familiy
      par.family <- par()$family
      par(family = layout$abanico$font.type$legend)

      legend(y = legend.pos[2],
             x = 0.8 * legend.pos[1],
             xjust = legend.adj[2],
             yjust = legend.adj[1],
             legend = legend,
             pch = pch,
             col = value.dot,
             text.col = value.dot,
             text.font = which(c("normal", "bold", "italic", "bold italic") ==
                                 layout$abanico$font.deco$legend)[1],
             cex = cex * layout$abanico$font.size$legend/12,
             bty = "n")

      ## restore font family
      par(family = par.family)
    }

    ## optionally add subheader text
    mtext(text = mtext,
          side = 3,
          line = (shift.lines - 2 + 3.5) * layout$abanico$dimension$mtext / 100,
          col = layout$abanico$colour$mtext,
          family = layout$abanico$font.type$mtext,
          font = which(c("normal", "bold", "italic", "bold italic") ==
                         layout$abanico$font.deco$mtext)[1],
          cex = cex * layout$abanico$font.size$mtext / 12)

    ## add summary content
    for(i in 1:length(data)) {
      if(summary.pos[1] != "sub") {
        text(x = summary.pos[1],
             y = summary.pos[2],
             adj = summary.adj,
             labels = label.text[[i]],
             col = summary.col[i],
             family = layout$abanico$font.type$summary,
             font = which(c("normal", "bold", "italic", "bold italic") ==
                            layout$abanico$font.deco$summary)[1],
             cex = cex * layout$abanico$font.size$summary / 12)
      } else {
        if(mtext == "") {
          mtext(side = 3,
                line = (shift.lines - 1 + 3.5 - i) *
                  layout$abanico$dimension$summary / 100 ,
                text = label.text[[i]],
                col = summary.col[i],
                family = layout$abanico$font.type$summary,
                font = which(c("normal", "bold", "italic", "bold italic") ==
                               layout$abanico$font.deco$summary)[1],
                cex = cex * layout$abanico$font.size$summary / 12)
        }
      }
    }
  }

  ##sTeve
  if (fun && !interactive) sTeve() # nocov

  ## create numeric output
  plot.output <- list(xlim = limits.x,
                      ylim = limits.y,
                      zlim = limits.z,
                      polar.box = c(limits.x[1],
                                    limits.x[2],
                                    min(ellipse[,2]),
                                    max(ellipse[,2])),
                      cartesian.box = c(xy.0[1],
                                        par()$usr[2],
                                        xy.0[2],
                                        max(ellipse[,2])),
                      plot.ratio = plot.ratio,
                      data = data,
                      data.global = data.global,
                      KDE = KDE,
                      par = par(no.readonly = TRUE))

  ## INTERACTIVE PLOT ----------------------------------------------------------
  if (interactive) {
    .require_suggested_package("plotly", "The interactive abanico plot")

    ## tidy data ----
    data <- plot.output
    kde <- data.frame(x = data$KDE[[1]][ ,2], y = data$KDE[[1]][ ,1])

    # radial scatter plot ----
    point.text <- paste0("Measured value:<br />",
                         data$data.global$De, " &plusmn; ",
                         data$data.global$error, "<br />",
                         "P(",format(data$data.global$precision,  digits = 2, nsmall = 1),", ",
                         format(data$data.global$std.estimate,  digits = 2, nsmall = 1),")")
    IAP <- plotly::plot_ly(data = data$data.global,
                           x = data$data.global$precision,
                           y = data$data.global$std.estimate,
                           type = "scatter", mode = "markers",
                           hoverinfo = "text", text = point.text,
                           name = "Points",
                           yaxis = "y")

    ellipse <- as.data.frame(ellipse)
    IAP <- plotly::add_trace(IAP, data = ellipse,
                             x = ~ellipse.x, y = ~ellipse.y,
                             type = "scatter", mode = "lines",
                             hoverinfo = "none", text = "",
                             name = "z-axis (left)",
                             line = list(color = "black",
                                         width = 1),
                             yaxis = "y")

    ellipse.right <- ellipse
    ellipse.right$ellipse.x <- ellipse.right$ellipse.x * 1/0.75

    IAP <- plotly::add_trace(IAP, data = ellipse.right,
                             x = ~ellipse.x, y = ~ellipse.y,
                             type = "scatter", mode = "lines",
                             hoverinfo = "none", text = "",
                             name = "z-axis (right)",
                             line = list(color = "black",
                                         width = 1),
                             yaxis = "y")

    # z-axis ticks
    major.ticks.x <- c(data$xlim[2] * 1/0.75,
                       (1 + 0.015 * layout$abanico$dimension$ztcl / 100) *
                         data$xlim[2] * 1/0.75)
    minor.ticks.x <- c(data$xlim[2] * 1/0.75,
                       (1 + 0.01 * layout$abanico$dimension$ztcl / 100) *
                         data$xlim[2] * 1/0.75)
    major.ticks.y <- (tick.values.major - z.central.global) *  min(ellipse[ ,1])
    minor.ticks.y <- (tick.values.minor - z.central.global) *  min(ellipse[ ,1])

    # major z-tick lines
    for (i in 1:length(major.ticks.y)) {
      major.tick <- data.frame(x = major.ticks.x, y = rep(major.ticks.y[i], 2))
      IAP <- plotly::add_trace(IAP, data = major.tick,
                               x = ~x, y = ~y, showlegend = FALSE,
                               type = "scatter", mode = "lines",
                               hoverinfo = "none", text = "",
                               line = list(color = "black",
                                           width = 1),
                               yaxis = "y")
    }

    # minor z-tick lines
    for (i in 1:length(minor.ticks.y)) {
      minor.tick <- data.frame(x = minor.ticks.x, y = rep(minor.ticks.y[i], 2))
      IAP <- plotly::add_trace(IAP, data = minor.tick,
                               x = ~x, y = ~y, showlegend = FALSE,
                               type = "scatter", mode = "lines",
                               hoverinfo = "none", text = "",
                               line = list(color = "black",
                                           width = 1),
                               yaxis = "y")
    }

    # z-tick label
    tick.text <- paste(" ", exp(tick.values.major))
    tick.pos <- data.frame(x = major.ticks.x[2],
                           y = major.ticks.y)

    IAP <- plotly::add_trace(IAP, data = tick.pos,
                             x = ~x, y = ~y, showlegend = FALSE,
                             hoverinfo = "none",
                             text = tick.text, textposition = "right",
                             type = "scatter", mode = "text",
                             yaxis = "y")

    # Central Line ----
    central.line <- data.frame(x = c(-100, data$xlim[2]*1/0.75), y = c(0, 0))
    central.line.text <- paste0("Central value: ",
                                format(exp(z.central.global), digits = 2, nsmall = 1))

    IAP <- plotly::add_trace(IAP, data = central.line,
                             x = ~x, y = ~y, name = "Central line",
                             type = "scatter", mode = "lines",
                             hoverinfo = "text", text = central.line.text,
                             yaxis = "y",
                             line = list(color = "black",
                                         width = 0.5,
                                         dash = 2))

    # KDE plot ----
    KDE.x <- xy.0[1] + KDE[[1]][ ,2] * KDE.scale
    KDE.y <- (KDE[[1]][ ,1] - z.central.global) * min(ellipse[,1])
    KDE.curve <- data.frame(x = KDE.x, y = KDE.y)
    KDE.curve <- KDE.curve[KDE.curve$x != xy.0[1], ]
    KDE.text <- paste0("Value:",
                       format(exp(KDE.curve$x), digits = 2, nsmall = 1), "<br />",
                       "Density:",
                       format(KDE.curve$y, digits = 2, nsmall = 1))

    IAP <- plotly::add_trace(IAP, data = KDE.curve,
                             x = ~x, y = ~y, name = "KDE",
                             type = "scatter", mode = "lines",
                             hoverinfo = "text",
                             text = KDE.text,
                             line = list(color = "red"),
                             yaxis = "y")

    # set layout ----
    IAP <- plotly::layout(IAP,
                          hovermode = "closest",
                          dragmode = "pan",
                          xaxis = list(range = c(data$xlim[1], data$xlim[2] * 1/0.65),
                                       zeroline = FALSE,
                                       showgrid = FALSE,
                                       tickmode = "array",
                                       tickvals = x.axis.ticks),
                          yaxis = list(range = data$ylim,
                                       zeroline = FALSE,
                                       showline = FALSE,
                                       showgrid = FALSE,
                                       tickmode = "array",
                                       tickvals = c(-2, 0, 2)),
                          shapes = list(list(type = "rect", # 2 sigma bar
                                             x0 = 0, y0 = -2,
                                             x1 = bars[1,3], y1 = 2,
                                             xref = "x", yref = "y",
                                             fillcolor = "grey",
                                             opacity = 0.2))
    )

    # show and return interactive plot ----
    #print(plotly::subplot(IAP, IAP.kde))
    print(IAP)
    return(IAP)
  }

  ## restore initial cex
  par(cex = cex_old)

  ## create and return numeric output
  invisible(plot.output)
}
