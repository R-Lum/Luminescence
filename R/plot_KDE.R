#' @title Plot kernel density estimate with statistics
#'
#' @description
#' Plot a kernel density estimate of measurement values in combination with the
#' actual values and associated error bars in ascending order. If enabled, the
#' boxplot will show the usual distribution parameters (median as
#' bold line, box delimited by the first and third quartile, whiskers defined
#' by the extremes and outliers shown as points) and also the mean and
#' standard deviation as pale bold line and pale polygon, respectively.
#'
#' The function allows passing several plot arguments, such as `main`,
#' `xlab`, `cex`. However, as the figure is an overlay of two
#' separate plots, `ylim` must be specified in the order: c(ymin_axis1,
#' ymax_axis1, ymin_axis2, ymax_axis2) when using the cumulative values plot
#' option. See examples for some further explanations. For details on the
#' calculation of the bin-width (parameter `bw`) see
#' [density].
#'
#'
#' A statistic summary, i.e. a collection of statistic measures of
#' centrality and dispersion (and further measures) can be added by specifying
#' one or more of the following keywords:
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
#'
#' **Note** that the input data for the statistic summary is sent to function
#' [calc_Statistics] depending on the log-option for the z-scale. If
#' `"log.z = TRUE"`, the summary is based on the logarithms of the input
#' data. If `"log.z = FALSE"` the linearly-scaled data is used.
#'
#' **Note** as well, that `"calc_Statistics()"` calculates these statistic
#' measures in three different ways: `unweighted`, `weighted` and
#' `MCM-based` (i.e., based on Monte Carlo Methods). By default, the
#' MCM-based version is used. This can be controlled via the `summary.method`
#' argument.
#'
#' @param data [data.frame], [vector] or [RLum.Results-class] object (**required**):
#' for `data.frame`: either two columns: De (`values[,1]`) and De error
#' (`values[,2]`), or one: De (`values[,1]`). If a numeric vector or a
#' single-column data frame is provided, De error is assumed to be 10^-9
#' for all measurements and error bars are not drawn.
#' For plotting multiple data sets, these must be provided as
#' `list` (e.g. `list(dataset1, dataset2)`).
#'
#' @param na.rm [logical] (*with default*):
#' exclude NA values from the data set prior to any further operation.
#'
#' @param values.cumulative [logical] (*with default*):
#' show cumulative individual data.
#'
#' @param order [logical] (*with default*):
#' Order data in ascending order.
#'
#' @param boxplot [logical] (*with default*):
#' optionally show a boxplot (depicting median as thick central line,
#' first and third quartile as box limits, whiskers denoting +/- 1.5
#' interquartile ranges and dots further outliers).
#'
#' @param rug [logical] (*with default*):
#' optionally add rug.
#'
#' @param summary [character] (*with default*):
#' add statistic measures of centrality and dispersion to the plot. Can be one
#' or more of several keywords. See details for available keywords.
#'
#' @param summary.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`)
#' for the statistical summary. Alternatively, the keyword `"sub"` may be
#' specified to place the summary below the plot header. However, this latter
#' option in only possible if `mtext` is not used. In case of coordinate
#' specification, y-coordinate refers to the right y-axis.
#'
#' @param summary.method [character] (*with default*):
#' keyword indicating the method used to calculate the statistic summary.
#' One out of `"MCM"` (default), `"weighted"` or `"unweighted"`.
#' See [calc_Statistics] for details.
#'
#' @param bw [character] (*with default*):
#' bin-width, chose a numeric value for manual setting.
#'
#' @param ... further arguments and graphical parameters passed to [plot].
#'
#' @note
#' The plot output is no 'probability density' plot (cf. the discussion
#' of Berger and Galbraith in Ancient TL; see references)!
#'
#' @section Function version: 3.6.0
#'
#' @author
#' Michael Dietze, GFZ Potsdam (Germany)\cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [density], [plot]
#'
#' @examples
#'
#' ## read example data set
#' data(ExampleData.DeValues, envir = environment())
#' ExampleData.DeValues <-
#'   convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))
#'
#' ## create plot straightforward
#' plot_KDE(data = ExampleData.DeValues)
#'
#' ## create plot with logarithmic x-axis
#' plot_KDE(data = ExampleData.DeValues,
#'          log = "x")
#'
#' ## create plot with user-defined labels and axes limits
#' plot_KDE(data = ExampleData.DeValues,
#'          main = "Dose distribution",
#'          xlab = "Dose (s)",
#'          ylab = c("KDE estimate", "Cumulative dose value"),
#'          xlim = c(100, 250),
#'          ylim = c(0, 0.08, 0, 30))
#'
#' ## create plot with boxplot option
#' plot_KDE(data = ExampleData.DeValues,
#'          boxplot = TRUE)
#'
#' ## create plot with statistical summary below header
#' plot_KDE(data = ExampleData.DeValues,
#'          summary = c("n", "median", "skewness", "in.2s"))
#'
#' ## create plot with statistical summary as legend
#' plot_KDE(data = ExampleData.DeValues,
#'          summary = c("n", "mean", "sd.rel", "se.abs"),
#'          summary.pos = "topleft")
#'
#' ## split data set into sub-groups, one is manipulated, and merge again
#' data.1 <- ExampleData.DeValues[1:15,]
#' data.2 <- ExampleData.DeValues[16:25,] * 1.3
#' data.3 <- list(data.1, data.2)
#'
#' ## create plot with two subsets straightforward
#' plot_KDE(data = data.3)
#'
#' ## create plot with two subsets and summary legend at user coordinates
#' plot_KDE(data = data.3,
#'          summary = c("n", "median", "skewness"),
#'          summary.pos = c(110, 0.07),
#'          col = c("blue", "orange"))
#'
#' ## example of how to use the numerical output of the function
#' ## return plot output to draw a thicker KDE line
#' KDE_out <- plot_KDE(data = ExampleData.DeValues)
#'
#' @md
#' @export
plot_KDE <- function(
  data,
  na.rm = TRUE,
  values.cumulative = TRUE,
  order = TRUE,
  boxplot = TRUE,
  rug = TRUE,
  summary = "",
  summary.pos = "sub",
  summary.method = "MCM",
  bw = "nrd0",
  ...
) {
  .set_function_name("plot_KDE")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  if (is(data, "list") && length(data) == 0) {
    .throw_error("'data' is an empty list")
  }

  ## Homogenise input data format
  if(is(data, "list") == FALSE) {
    data <- list(data)
  }

  ## check/adjust input data structure
  for(i in 1:length(data)) {
    .validate_class(data[[i]], c("RLum.Results", "data.frame", "numeric"),
                    name = "'data'")

    ## extract RLum.Results
    if (inherits(data[[i]], "RLum.Results")) {
      data[[i]] <- get_RLum(data[[i]], "data")[,1:2]
    }

      ## ensure that the dataset it not degenerate
      if (NROW(data[[i]]) == 0) {
       .throw_error("Input data ", i, " has 0 rows")
      }

      ## if `data[[i]]` is a numeric vector or a single-column data frame,
      ## append a second column with a small non-zero value (10^-9 for
      ## consistency with what `calc_Statistics() does)
      if (NCOL(data[[i]]) < 2) {
        data[[i]] <- data.frame(data[[i]], 10^-9)
        attr(data[[i]], "De.errors.available") <- FALSE
      } else {
        ## keep only the first two columns
        data[[i]] <- data[[i]][, 1:2]
        attr(data[[i]], "De.errors.available") <- TRUE
      }

    ## find the index Inf values in each of the two columns and remove the
    ## corresponding rows if needed
    inf.idx <- unlist(lapply(data[[i]], function(x) which(is.infinite(x))))
    if (length(inf.idx) > 0) {
      inf.row <- sort(unique(inf.idx))
      .throw_warning("Inf values removed in rows: ",
                     .collapse(inf.row, quote = FALSE), " in data.frame ", i)
      data[[i]] <- data[[i]][-inf.row, ]
      rm(inf.idx, inf.row)

      ##check if empty
      if(nrow(data[[i]]) == 0){
        data[i] <- NULL
      }
    }
  }

  ##check if list is empty
  if(length(data) == 0)
    .throw_error("Your input is empty due to Inf removal")

  .validate_logical_scalar(values.cumulative)
  .validate_logical_scalar(order)
  .validate_logical_scalar(boxplot)
  .validate_logical_scalar(rug)
  .validate_args(summary.method, c("MCM", "weighted", "unweighted"))
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

  ## set mtext output
  if("mtext" %in% names(list(...))) {
    mtext <- list(...)$mtext
  } else {
    mtext <- ""
  }

  ## check/set layout definitions
  if("layout" %in% names(list(...))) {
    layout <- get_Layout(layout = list(...)$layout)
  } else {
    layout <- get_Layout(layout = "default")
  }

  ## data preparation steps ---------------------------------------------------

  ## optionally, count and exclude NA values and print result
  if(na.rm == TRUE) {
    for(i in 1:length(data)) {
      na.idx <- which(is.na(data[[i]][, 1]))
      n.NA <- length(na.idx)
      if (n.NA > 0) {
        message(sprintf("%d NA value%s excluded from data set %d\n",
                        n.NA, ifelse(n.NA > 1, "s", ""), i))
        data[[i]] <- data[[i]][-na.idx, ]
      }
    }
  }

  ## optionally, order data set
  if(order == TRUE) {
    for(i in 1:length(data)) {
      data[[i]] <- data[[i]][order(data[[i]][,1]),]
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
  De.density <- list(NA)

  ## loop through all data sets
  for(i in 1:length(data)) {
    statistics <- calc_Statistics(data[[i]], na.rm = na.rm)[[summary.method]]

    De.stats[i,1] <- statistics$n
    De.stats[i,2] <- statistics$mean
    De.stats[i,3] <- statistics$median
    De.stats[i,5] <- statistics$sd.abs
    De.stats[i,6] <- statistics$sd.rel
    De.stats[i,7] <- statistics$se.abs
    De.stats[i,8] <- statistics$se.rel
    De.stats[i,9] <- quantile(data[[i]][,1], 0.25)
    De.stats[i,10] <- quantile(data[[i]][,1], 0.75)
    De.stats[i,11] <- statistics$skewness
    De.stats[i,12] <- statistics$kurtosis

    if(nrow(data[[i]]) >= 2){
      De.density[[length(De.density) + 1]] <- density(data[[i]][,1],
                                                      kernel = "gaussian",
                                                      bw = bw)
    }else{
      De.density[[length(De.density) + 1]] <- NA
      .throw_warning("Single data point found, no density calculated")
    }
  }

  ## remove dummy list element
  De.density[[1]] <- NULL

  ## create global data set
  De.global <- data[[1]][,1]
  De.error.global <- data[[1]][,2]
  De.density.range <- matrix(nrow = length(data),
                             ncol = 4)

  for(i in 1:length(data)) {
    ##global De and De.error vector
    De.global <- c(De.global, data[[i]][,1])
    De.error.global <- c(De.error.global, data[[i]][,2])

    ## density range
    if(!all(is.na(De.density[[i]]))){
      De.density.range[i,1] <- min(De.density[[i]]$x)
      De.density.range[i,2] <- max(De.density[[i]]$x)
      De.density.range[i,3] <- min(De.density[[i]]$y)
      De.density.range[i,4] <- max(De.density[[i]]$y)

      ## position of maximum KDE value
      De.stats[i,4] <- De.density[[i]]$x[which.max(De.density[[i]]$y)]

    }else{
      De.density.range[i,1:4] <- NA
      De.stats[i,4] <- NA
    }
  }

  ## Get global range of densities
  De.density.range <- c(min(De.density.range[,1]),
                        max(De.density.range[,2]),
                        min(De.density.range[,3]),
                        max(De.density.range[,4]))

  ## helper to generate an element of the statistical summary
  .summary_line <- function(keyword, summary, val, label = keyword,
                            percent = FALSE, sep = FALSE, digits = 2) {
    ifelse(keyword %in% summary,
           paste0(label, " = ", round(val, digits),
                  if (percent) " %" else NULL, if (sep) " | " else  "\n"),
           "")
  }

  ## initialize list with a dummy element, it will be removed afterwards
  label.text <- list(NA)

  is.sub <- summary.pos[1] == "sub"
  stops <- NULL
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
                        label = "sd"),
          .summary_line("sd.rel", summary[j], De.stats[i, 6], sep = is.sub,
                        label = "rel. sd", percent = TRUE),
          .summary_line("se.abs", summary[j], De.stats[i, 7], sep = is.sub,
                        label = "se"),
          .summary_line("se.rel", summary[j], De.stats[i, 8], sep = is.sub,
                        label = "rel. se", percent = TRUE),
          .summary_line("skewness", summary[j], De.stats[i, 11], sep = is.sub),
          .summary_line("kurtosis", summary[j], De.stats[i, 12], sep = is.sub),
          .summary_line("in.2s", summary[j],
                        sum(data[[i]][, 1] > (De.stats[i, 2] - 2 *
                                              De.stats[i, 5]) &
                            data[[i]][, 1] < (De.stats[i, 2] + 2 *
                                              De.stats[i, 5])) /
                        nrow(data[[i]]) * 100, sep = is.sub,
                        label = "in 2 sigma", percent = TRUE, digits = 1))
    }

    label.text[[length(label.text) + 1]] <- paste0(
        if (is.sub) "" else stops,
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

  ## read out additional parameters -------------------------------------------
  if("main" %in% names(list(...))) {
    main <- list(...)$main
  } else {
    main <- expression(bold(paste(D[e], " distribution")))
  }

  if("sub" %in% names(list(...))) {
    sub <- list(...)$sub
  } else {
    sub <- NULL
  }

  if("xlab" %in% names(list(...))) {
    xlab <- list(...)$xlab
  } else {
    xlab <- expression(paste(D[e], " [Gy]"))
  }

  if("ylab" %in% names(list(...))) {
    ylab <- list(...)$ylab
  } else {
    ylab <- c("Density", "Cumulative frequency")
  }

  if("xlim" %in% names(list(...))) {
    xlim.plot <- list(...)$xlim
  } else {
    xlim.plot <- c(min(c(De.global - De.error.global),
                       De.density.range[1],
                       na.rm = TRUE),
                   max(c(De.global + De.error.global),
                       De.density.range[2],
                       na.rm = TRUE))
  }

  if("ylim" %in% names(list(...))) {
    ylim.plot <- list(...)$ylim
    .validate_length(ylim.plot, 4, name = "'ylim'")
  } else {
    if(!is.na(De.density.range[1])){
      ylim.plot <- c(De.density.range[3],
                     De.density.range[4],
                     0,
                     max(De.stats[,1]))

    }else{
      ylim.plot <- c(0,
                     max(De.stats[,1]),
                     0,
                     max(De.stats[,1]))
    }
  }

  if("log" %in% names(list(...))) {
    log.option <- list(...)$log
  } else {
    log.option <- ""
  }

  if("col" %in% names(list(...))) {

    col.main <- list(...)$col
    col.xlab <- 1
    col.ylab1 <- 1
    col.ylab2 <- 1
    col.xtck <- 1
    col.ytck1 <- 1
    col.ytck2 <- 1
    col.box <- 1
    col.mtext <- 1
    col.stats <- list(...)$col
    col.kde.line <- list(...)$col
    col.kde.fill <- NA
    col.value.dot <- list(...)$col
    col.value.bar <- list(...)$col
    col.value.rug <- list(...)$col
    col.boxplot <- list(...)$col
    col.boxplot.line <- list(...)$col
    col.boxplot.fill <- NA
    col.mean.line <- adjustcolor(col = list(...)$col,
                                 alpha.f = 0.4)
    col.sd.bar <- adjustcolor(col = list(...)$col,
                              alpha.f = 0.4)
    col.background <- NA
  } else {

    .set_colour_value <- function(layout_value) {
      if (length(layout_value) == 1)
        c(layout_value, 2:length(data))
      else
        layout_value
    }
    col.main <- .set_colour_value(layout$kde$colour$main)
    col.xlab <- .set_colour_value(layout$kde$colour$xlab)
    col.ylab1 <- .set_colour_value(layout$kde$colour$ylab1)
    col.ylab2 <- .set_colour_value(layout$kde$colour$ylab2)
    col.xtck <- .set_colour_value(layout$kde$colour$xtck)
    col.ytck1 <- .set_colour_value(layout$kde$colour$ytck1)
    col.ytck2 <- .set_colour_value(layout$kde$colour$ytck2)
    col.box <- .set_colour_value(layout$kde$colour$box)
    col.mtext <- .set_colour_value(layout$kde$colour$mtext)
    col.stats <- .set_colour_value(layout$kde$colour$stats)
    col.kde.line <- .set_colour_value(layout$kde$colour$kde.line)
    col.kde.fill <- .set_colour_value(layout$kde$colour$kde.fill)
    col.value.dot <- .set_colour_value(layout$kde$colour$value.dot)
    col.value.bar <- .set_colour_value(layout$kde$colour$value.bar)
    col.value.rug <- .set_colour_value(layout$kde$colour$value.rug)
    col.boxplot.line <- .set_colour_value(layout$kde$colour$boxplot.line)
    col.boxplot.fill <- .set_colour_value(layout$kde$colour$boxplot.fill)
    col.mean.line <- .set_colour_value(layout$kde$colour$mean.point)
    col.sd.bar <- .set_colour_value(layout$kde$colour$sd.line)
    col.background <- .set_colour_value(layout$kde$colour$background)
  }

  if("lty" %in% names(list(...))) {
    lty <- list(...)$lty
  } else {
    lty <- rep(1, length(data))
  }

  if("lwd" %in% names(list(...))) {
    lwd <- list(...)$lwd
  } else {
    lwd <- rep(1, length(data))
  }

  if("cex" %in% names(list(...))) {
    cex <- list(...)$cex
  } else {
    cex <- 1
  }

  if("fun" %in% names(list(...))) {
    fun <- list(...)$fun # nocov
  } else {
    fun <- FALSE
  }

  ## convert keywords into summary placement coordinates
  coords <- .get_keyword_coordinates(summary.pos, xlim.plot, ylim.plot[1:2])
  summary.pos <- coords$pos
  summary.adj <- coords$adj

  ## plot data sets -----------------------------------------------------------

  ## setup plot area
  toplines <- 1
  if (length(summary) >= 1 && is.sub) {
    toplines <- length(data)
  }

  ## extract original plot parameters
  par(bg = layout$kde$colour$background)
  bg.original <- par()$bg

  par(mar = c(5, 5.5, 2.5 + toplines, 4.5),
      xpd = FALSE,
      cex = cex)

  dim <- layout$kde$dimension
  if (dim$figure.width != "auto" || dim$figure.height != "auto") {
    par(mai = dim$margin / 25.4,
        pin = c(dim$figure.width - dim$margin[2] - dim$margin[4],
                dim$figure.height - dim$margin[1] - dim$margin[3]) / 25.4)
  }

  ## create empty plot to get plot dimensions
  plot(NA,
       xlim = xlim.plot,
       ylim = ylim.plot[1:2],
       sub = sub,
       log = log.option,
       axes = FALSE,
       ann = FALSE)

  ## get line height in xy coordinates
  l_height <- par()$cxy[2]

  ## optionally update ylim
  if(boxplot == TRUE) {

    ylim.plot[1] <- ylim.plot[1] - 1.4 * l_height
  }

  ## create empty plot to set adjusted plot dimensions
  par(new = TRUE)
  plot(NA,
       xlim     = xlim.plot,
       ylim     = ylim.plot[1:2],
       log      = log.option,
       cex      = cex,
       axes = FALSE,
       ann = FALSE)

  ## add box
  graphics::box(which = "plot",
                col = layout$kde$colour$box)

  ## add x-axis
  axis(side = 1,
       col = layout$kde$colour$xtck,
       col.axis = layout$kde$colour$xtck,
       labels = NA,
       tcl = -layout$kde$dimension$xtcl / 200,
       cex = cex)

  axis(side = 1,
       line = 2 * layout$kde$dimension$xtck.line / 100 - 2,
       lwd = 0,
       col = layout$kde$colour$xtck,
       family = layout$kde$font.type$xtck,
       font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                      layout$kde$font.deco$xtck],
       col.axis = layout$kde$colour$xtck,
       cex.axis = layout$kde$font.size$xlab/12)

  mtext(text = xlab,
        side = 1,
        line = 3 * layout$kde$dimension$xlab.line / 100,
        col = layout$kde$colour$xlab,
        family = layout$kde$font.type$xlab,
        font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                       layout$kde$font.deco$xlab],
        cex = cex * layout$kde$font.size$xlab/12)

  ## add left y-axis
  axis(side = 2,
       at = pretty(x = range(De.density.range[3:4])),
       col = layout$kde$colour$ytck1,
       col.axis = layout$kde$colour$ytck1,
       labels = NA,
       tcl = -layout$kde$dimension$ytck1 / 200,
       cex = cex)

  axis(side = 2,
       at = pretty(x = range(De.density.range[3:4])),
       line = 2 * layout$kde$dimension$ytck1.line / 100 - 2,
       lwd = 0,
       col = layout$kde$colour$ytck1,
       family = layout$kde$font.type$ytck1,
       font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                      layout$kde$font.deco$ytck1],
       col.axis = layout$kde$colour$ytck1,
       cex.axis = layout$kde$font.size$ylab1/12)

  mtext(text = ylab[1],
        side = 2,
        line = 3 * layout$kde$dimension$ylab1.line / 100,
        col = layout$kde$colour$ylab1,
        family = layout$kde$font.type$ylab1,
        font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                       layout$kde$font.deco$ylab1],
        cex = cex * layout$kde$font.size$ylab1/12)

  for(i in 1:length(data)) {
    if(!all(is.na(De.density[[i]]))){
      polygon(x = c(par()$usr[1], De.density[[i]]$x, par()$usr[2]),
              y = c(min(De.density[[i]]$y),De.density[[i]]$y, min(De.density[[i]]$y)),
              border = col.kde.line[i],
              col = col.kde.fill[i],
              lty = lty[i],
              lwd = lwd[i])
    }
  }

  ## add plot title
  cex.old <- par()$cex
  par(cex = layout$kde$font.size$main / 12)
  title(main = main,
        family = layout$kde$font.type$main,
        font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                       layout$kde$font.deco$main],
        col.main = layout$kde$colour$main,
        line = (toplines + 1.2) * layout$kde$dimension$main / 100)
  par(cex = cex.old)

  ## optionally add mtext line
  if(mtext != "") {

    mtext(text = mtext,
          side = 3,
          line = 0.5,
          family = layout$kde$font.type$mtext,
          font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                         layout$kde$font.deco$mtext],
          col.main = layout$kde$colour$mtext,
          cex = layout$kde$font.size$mtext / 12)
  }

  ## add summary content
  for(i in 1:length(data)) {

    if (!is.sub) {
      text(x = summary.pos[1],
           y = summary.pos[2],
           adj = summary.adj,
           labels = label.text[[i]],
           col = col.stats[i],
           cex = layout$kde$font.size$stats / 12)
    } else {

      if(mtext == "") {
        mtext(side = 3,
              line = (toplines + 0.3 - i) * layout$kde$dimension$stats.line / 100,
              text = label.text[[i]],
              col = col.stats[i],
              cex = layout$kde$font.size$stats / 12)
      }
    }
  }

  if(values.cumulative == TRUE) {

    ## create empty overlay plot
    par(new = TRUE) # adjust plot options

    ## add empty plot, scaled to preliminary secondary plot content
    plot(x = NA,
         xlim = xlim.plot,
         ylim = ylim.plot[3:4],
         log  = log.option,
         ann = FALSE,
         axes = FALSE
         )

    ## get line height in xy coordinates
    l_height <- par()$cxy[2]

    ## optionally update ylim
    if(boxplot == TRUE) {

      ylim.plot[3] <- ylim.plot[3] - 1.4 * l_height
    }

    ## create correctly scaled empty overlay plot
    par(new = TRUE) # adjust plot options

    ## add empty plot, scaled to secondary plot content
    plot(NA,
         xlim = xlim.plot,
         ylim = ylim.plot[3:4],
         log  = log.option,
         ann = FALSE,
         axes = FALSE)

    ## optionally add boxplot
    if(boxplot == TRUE) {

      ## add zero line
      abline(h = 0)

      ## get extended boxplot data
      boxplot.data <- list(NA)

      for(i in 1:length(data)) {
        boxplot.i <- graphics::boxplot(x = data[[i]][,1],
                                       plot = FALSE)
        boxplot.i$group <- mean(x = data[[i]][,1],
                                                   na.rm = TRUE)
        boxplot.i$names <- sd(x = data[[i]][,1],
                                                   na.rm = TRUE)
        boxplot.data[[length(boxplot.data) + 1]] <- boxplot.i
      }

      ## remove dummy list object
      boxplot.data[[1]] <- NULL

      ## get new line hights
      l_height <- par()$cxy[2]

      for(i in 1:length(data)) {

        # ## draw sd line
        # lines(x = c(boxplot.data[[i]]$group[1] - boxplot.data[[i]]$names[1],
        #             boxplot.data[[i]]$group[1] + boxplot.data[[i]]$names[1]),
        #       y = c(-5/8 * l_height,
        #             -5/8 * l_height),
        #       col = col.mean.line[i])
        #
        # ## draw mean line
        # points(x = boxplot.data[[i]]$group[1],
        #       y = -5/8 * l_height,
        #       pch = 18,
        #       col = col.mean.line[i])

        ## draw median line
        lines(x = c(boxplot.data[[i]]$stats[3,1],
                    boxplot.data[[i]]$stats[3,1]),
              y = c(-11/8, -7/8) * l_height,
              lwd = 2,
              col = col.boxplot.line[i])

        ## draw q25-q75-polygon
        polygon(x = c(boxplot.data[[i]]$stats[2,1],
                      boxplot.data[[i]]$stats[2,1],
                      boxplot.data[[i]]$stats[4,1],
                      boxplot.data[[i]]$stats[4,1]),
                y = c(-11/8, -7/8, -7/8, -11/8) * l_height,
                col = col.boxplot.fill[i],
                border = col.boxplot.line[i])

        ## draw whiskers
        lines(x = c(boxplot.data[[i]]$stats[2,1],
                    boxplot.data[[i]]$stats[1,1]),
              y = c(-9/8, -9/8) * l_height,
              col = col.boxplot.line[i])

        lines(x = c(boxplot.data[[i]]$stats[1,1],
                    boxplot.data[[i]]$stats[1,1]),
              y = c(-10/8, -8/8) * l_height,
              col = col.boxplot.line[i])

        lines(x = c(boxplot.data[[i]]$stats[4,1],
                    boxplot.data[[i]]$stats[5,1]),
              y = c(-9/8, -9/8) * l_height,
              col = col.boxplot.line[i])

        lines(x = c(boxplot.data[[i]]$stats[5,1],
                    boxplot.data[[i]]$stats[5,1]),
              y = c(-10/8, -8/8) * l_height,
              col = col.boxplot.line[i])

        ## draw outliers
        points(x = boxplot.data[[i]]$out,
               y = rep(-9/8 * l_height,
                       length(boxplot.data[[i]]$out)),
               col = col.boxplot.line[i],
               cex = cex * 0.8)
      }
    }

    ## optionally add rug
    if(rug == TRUE) {

      for(i in 1:length(data)) {

        for(j in 1:nrow(data[[i]])) {

          lines(x = c(data[[i]][j,1],
                      data[[i]][j,1]),
                y = c(0,
                      -2/8 * l_height),
                col = col.value.rug[i])
        }
      }
    }

    ## add secondary y-axis
    ticks_axis <- pretty(x = c(1, ylim.plot[4]))
    ticks_axis <- ifelse(test = ticks_axis == 0,
                         yes = NA,
                         no = ticks_axis)

    ## add right y-axis
    axis(side = 4,
         at = ticks_axis,
         col = layout$kde$colour$ytck2,
         col.axis = layout$kde$colour$ytck2,
         labels = NA,
         tcl = -layout$kde$dimension$ytck2 / 200,
         cex = cex)

    axis(side = 4,
         at = ticks_axis,
         line = 2 * layout$kde$dimension$ytck2.line / 100 - 2,
         lwd = 0,
         col = layout$kde$colour$ytck2,
         family = layout$kde$font.type$ytck2,
         font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                        layout$kde$font.deco$ytck2],
         col.axis = layout$kde$colour$ytck2,
         cex.axis = layout$kde$font.size$ylab2/12)

    mtext(text = ylab[2],
          side = 4,
          line = 3 * layout$kde$dimension$ylab2.line / 100,
          col = layout$kde$colour$ylab2,
          family = layout$kde$font.type$ylab2,
          font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                         layout$kde$font.deco$ylab2],
          cex = cex * layout$kde$font.size$ylab2/12)

    ## add De error bars
    for(i in 1:length(data)) {
      if (attr(data[[i]], "De.errors.available")) {
        graphics::arrows(data[[i]][, 1] - data[[i]][, 2],
               1:length(data[[i]][,1]),
               data[[i]][, 1] + data[[i]][, 2],
               1:length(data[[i]][, 1]),
               code = 3,
               angle = 90,
               length = 0.05,
               col = col.value.bar[i])
      }
      ## add De measurements
      points(data[[i]][,1], 1:De.stats[i,1],
             col = col.value.dot[i],
             pch = 20)
    }
  }

  ## add empty plot
  par(new = TRUE)
  plot(NA,
       ann = FALSE,
       axes = FALSE,
       xlim     = xlim.plot,
       ylim     = ylim.plot[1:2],
       log      = log.option,
       cex      = cex,
       cex.lab  = cex,
       cex.main = cex,
       cex.axis = cex)

  ## FUN by R Luminescence Team
  if (fun == TRUE) sTeve() # nocov

  invisible(list(De.stats = De.stats,
                 summary.pos = summary.pos,
                 De.density = De.density))
}
