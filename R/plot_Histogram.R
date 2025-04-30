#' @title Plot a histogram with separate error plot
#'
#' @description
#' Function plots a predefined histogram with an accompanying error plot as
#' suggested by Rex Galbraith at the UK LED in Oxford 2010.
#'
#' If the normal curve is added, the y-axis in the histogram will show the
#' probability density.
#'
#' A statistic summary, i.e. a collection of statistic measures of
#' centrality and dispersion (and further measures) can be added by specifying
#' one or more of the following keywords:
#' - `"n"` (number of samples),
#' - `"mean"` (mean De value),
#' - `"mean.weighted"` (error-weighted mean),
#' - `"median"` (median of the De values),
#' - `"sdrel"` (relative standard deviation in percent),
#' - `"sdrel.weighted"` (error-weighted relative standard deviation in percent),
#' - `"sdabs"` (absolute standard deviation),
#' - `"sdabs.weighted"` (error-weighted absolute standard deviation),
#' - `"serel"` (relative standard error),
#' - `"serel.weighted"` (error-weighted relative standard error),
#' - `"seabs"` (absolute standard error),
#' - `"seabs.weighted"` (error-weighted absolute standard error),
#' - `"kurtosis"` (kurtosis) and
#' - `"skewness"` (skewness).
#'
#' @param data [data.frame] or [RLum.Results-class] object (**required**):
#' for `data.frame`: two columns: De (`data[,1]`) and De error (`data[,2]`).
#' If the error column is missing or only contains `NA` values, then the error
#' at each measurement is assumed to be 10^-9.
#'
#' @param na.rm [logical] (*with default*):
#' excludes `NA` values from the data set prior to any further operations.
#'
#' @param mtext [character] (*optional*):
#' further sample information ([mtext]).
#'
#' @param cex.global [numeric] (*with default*):
#' global scaling factor.
#'
#' @param se [logical] (*with default*):
#' plots standard error points over the histogram, default is `TRUE`.
#'
#' @param rug [logical] (*with default*):
#' adds rugs to the histogram, default is `TRUE`.
#'
#' @param normal_curve [logical] (*with default*):
#' adds a normal curve to the histogram. Mean and standard deviation are
#' calculated from the input data. If `TRUE`, the y-axis in the histogram
#' will show the probability density.
#'
#' @param summary [character] (*with default*):
#' add statistic measures of centrality and dispersion to the plot.
#' Can be one or more of several keywords. See details for available keywords.
#'
#' @param summary.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`)
#' for the statistical summary. Alternatively, the keyword `"sub"` may be
#' specified to place the summary below the plot header. However, this latter
#' option in only possible if `mtext` is not used. In case of coordinate
#' specification, y-coordinate refers to the right y-axis.
#'
#' @param colour [numeric] or [character] (*with default*):
#' optional vector of length 4 which specifies the colours of the following
#' plot items in exactly this order: histogram bars, rug lines and summary
#' text, normal distribution curve, standard error points
#' (e.g., `c("grey", "black", "red", "grey")`).
#'
#' @param interactive [logical] (*with default*):
#' create an interactive histogram plot (requires the 'plotly' package)
#'
#' @param ... further arguments and graphical parameters passed to [plot] or
#' [hist]. If y-axis labels are provided, these must be specified as a vector
#' of length 2 since the plot features two axes
#' (e.g. `ylab = c("axis label 1", "axis label 2")`). Y-axes limits
#' (`ylim`) must be provided as vector of length four, with the first two
#' elements specifying the left axes limits and the latter two elements giving
#' the right axis limits.
#'
#' @note The input data is not restricted to a special type.
#'
#' @section Function version: 0.4.5
#'
#' @author
#' Michael Dietze, GFZ Potsdam (Germany)\cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [hist], [plot]
#'
#' @examples
#'
#' ## load data
#' data(ExampleData.DeValues, envir = environment())
#' ExampleData.DeValues <- convert_Second2Gray(ExampleData.DeValues$BT998,
#'                                             dose.rate = c(0.0438,0.0019))
#'
#' ## plot histogram the easiest way
#' plot_Histogram(ExampleData.DeValues)
#'
#' ## plot histogram with some more modifications
#' plot_Histogram(ExampleData.DeValues,
#'                rug = TRUE,
#'                normal_curve = TRUE,
#'                cex.global = 0.9,
#'                pch = 2,
#'                colour = c("grey", "black", "blue", "green"),
#'                summary = c("n", "mean", "sdrel"),
#'                summary.pos = "topleft",
#'                main = "Histogram of De-values",
#'                mtext = "Example data set",
#'                ylab = c(expression(paste(D[e], " distribution")),
#'                         "Standard error"),
#'                xlim = c(100, 250),
#'                ylim = c(0, 0.1, 5, 20))
#'
#'
#' @md
#' @export
plot_Histogram <- function(
  data,
  na.rm = TRUE,
  mtext = "",
  cex.global = 1,
  se = TRUE,
  rug = TRUE,
  normal_curve = FALSE,
  summary = "",
  summary.pos = "sub",
  colour = c("white", "black", "red", "black"),
  interactive = FALSE,
  ...
) {
  .set_function_name("plot_Histogram")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("data.frame", "RLum.Results"))
  .validate_not_empty(data)
  if (inherits(data, "RLum.Results")) {
    data <- get_RLum(data)[,1:2]
  }

  .validate_class(cex.global, "numeric")
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
  .validate_logical_scalar(na.rm)
  .validate_logical_scalar(se)
  .validate_logical_scalar(rug)
  .validate_logical_scalar(normal_curve)
  .validate_length(colour, 4)

  ## handle error-free data sets
  if (length(data) < 2 || all(is.na(data[, 2]))) {
    data[, 2] <- 1e-9
    se <- FALSE
  }

  ## Set general parameters ---------------------------------------------------

  ## read out additional arguments list
  extraArgs <- list(...)

  ## optionally, count and exclude NA values and print result
  if(na.rm == TRUE) {
    n.NA <- sum(is.na(data[,1]))
    if (n.NA > 0) {
      cat(sprintf("%d NA value%s excluded\n", n.NA, ifelse(n.NA > 1, "s", "")))
      data <- data[!is.na(data[, 1]), ]
    }
  }

  if("breaks" %in% names(extraArgs)) {
    breaks.plot <- extraArgs$breaks
    breaks_calc <- hist(x = data[,1],
                        breaks = breaks.plot,
                        plot = FALSE)$breaks
  } else {
    breaks.plot <- hist(x = data[,1],
                        plot = FALSE)$breaks
    breaks_calc <- breaks.plot
  }

  if("ylim" %in% names(extraArgs)) {
    ylim.plot <- extraArgs$ylim
    .validate_length(ylim.plot, 4, name = "'ylim'")
  } else {
    H.lim <- hist(data[,1],
                  breaks = breaks.plot,
                  plot = FALSE)
    left.ylim <- c(0, if (normal_curve) max(H.lim$density) else max(H.lim$counts))
    range.error <- try(expr = range(data[,2], na.rm = TRUE),
                       silent = TRUE)
    range.error[1] <- ifelse(is.infinite(range.error[1]), 0, range.error[1])
    range.error[2] <- ifelse(is.infinite(range.error[2]), 0, range.error[2])
    ylim.plot <- c(left.ylim, range.error)
  }

  settings <- modifyList(list(
      main = "Histogram",
      xlab = expression(paste(D[e], " [Gy]")),
      ylab = c("Frequency", "Standard error"),
      xlim = range(breaks_calc),
      pch = 1,
      fun = FALSE
  ), extraArgs)

  ## Set plot area format
  par(mar = c(4.5, 4.5, 4.5, if (se) 4.5 else 1),
      cex = cex.global)

  ## Plot histogram -----------------------------------------------------------
  HIST <- hist(data[,1],
               main = settings$main,
               xlab = settings$xlab,
               ylab = settings$ylab[1],
               xlim = settings$xlim,
               ylim = settings$ylim[1:2],
               breaks = breaks.plot,
               freq = !normal_curve,
               col = colour[1]
  )

  ## Optionally, add rug ------------------------------------------------------
  if (rug) {
    graphics::rug(data[, 1], col = colour[2], quiet = TRUE)
  }

  ## Optionally, add a normal curve based on the data -------------------------
  if(normal_curve == TRUE){
    ## cheat the R check routine, tztztz how neat
    x <- NULL
    rm(x)

    ## add normal distribution curve
    curve(dnorm(x,
                mean = mean(na.exclude(data[,1])),
                sd = sd(na.exclude(data[,1]))),
          col = colour[3],
          add = TRUE,
          lwd = 1.2 * cex.global)
  }

  ## calculate and paste statistical summary
  data.stats <- list(data = data)

  ## calculate and paste statistical summary
  De.stats <- matrix(nrow = length(data), ncol = 18)
  colnames(De.stats) <- c("n",
                          "mean",
                          "mean.weighted",
                          "median",
                          "median.weighted",
                          "kde.max",
                          "sd.abs",
                          "sd.rel",
                          "se.abs",
                          "se.rel",
                          "q25",
                          "q75",
                          "skewness",
                          "kurtosis",
                          "sd.abs.weighted",
                          "sd.rel.weighted",
                          "se.abs.weighted",
                          "se.rel.weighted")

  for(i in 1:length(data)) {
    statistics <- calc_Statistics(data)
    De.stats[i,1] <- statistics$weighted$n
    De.stats[i,2] <- statistics$unweighted$mean
    De.stats[i,3] <- statistics$weighted$mean
    De.stats[i,4] <- statistics$unweighted$median
    De.stats[i,5] <- statistics$unweighted$median
    De.stats[i,7] <- statistics$unweighted$sd.abs
    De.stats[i,8] <- statistics$unweighted$sd.rel
    De.stats[i,9] <- statistics$unweighted$se.abs
    De.stats[i,10] <- statistics$weighted$se.rel
    De.stats[i,11] <- quantile(data[,1], 0.25)
    De.stats[i,12] <- quantile(data[,1], 0.75)
    De.stats[i,13] <- statistics$unweighted$skewness
    De.stats[i,14] <- statistics$unweighted$kurtosis
    De.stats[i,15] <- statistics$weighted$sd.abs
    De.stats[i,16] <- statistics$weighted$sd.rel
    De.stats[i,17] <- statistics$weighted$se.abs
    De.stats[i,18] <- statistics$weighted$se.rel

    ##kdemax - here a little doubled as it appears below again
    De.denisty <- NA
    De.stats[i,6] <- NA
    if(nrow(data) >= 2){
      De.density <-density(x = data[,1],
                           kernel = "gaussian",
                           from = settings$xlim[1],
                           to = settings$xlim[2])

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
      stops <- paste(rep("\n", (i - 1) * length(summary)), collapse = "")

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

  ## convert keywords into summary placement coordinates
  coords <- .get_keyword_coordinates(summary.pos, settings$xlim, ylim.plot)
  summary.pos <- coords$pos
  summary.adj <- coords$adj

  ## add summary content
  for(i in 1:length(data.stats)) {
    if(summary.pos[1] != "sub") {
      text(x = summary.pos[1],
           y = summary.pos[2],
           adj = summary.adj,
           labels = label.text[[i]],
           col = colour[2],
           cex = 0.8)
    } else {
      if(mtext == "") {
        mtext(side = 3,
              line = 1 - i,
              text = label.text[[i]],
              col = colour[2],
              cex = cex.global * 0.8)
      }
    }
  }

  ## Optionally, add standard error plot --------------------------------------

  if(se == TRUE) {
    par(new = TRUE)
    plot.data <- data[!is.na(data[,2]),]

    plot(x = plot.data[,1],
         y = plot.data[,2],
         xlim = settings$xlim,
         ylim = ylim.plot[3:4],
         pch = settings$pch,
         col = colour[4],
         main = "",
         xlab = "",
         ylab = "",
         axes = FALSE,
         frame.plot = FALSE
    )
    axis(side = 4,
         labels = TRUE,
         cex = cex.global
    )
    mtext(settings$ylab[2],
          side = 4,
          line = 3,
          cex = cex.global)

    #    par(new = FALSE)
  }

  ## Optionally add user-defined mtext
  mtext(side = 3,
        line = 0.5,
        text = mtext,
        cex = 0.8 * cex.global)

  ## FUN by R Luminescence Team
  if (settings$fun && !interactive) sTeve() # nocov

  ## Optionally: Interactive Plot ----------------------------------------------
  if (interactive) {
    .require_suggested_package("plotly", "The interactive histogram")

    ## tidy data ----
    data <- as.data.frame(data)
    colnames(data) <- c("x", "y")
    x <- y <- NULL # suffice CRAN check for no visible binding
    if (length(grep("paste", as.character(settings$xlab))) > 0)
      settings$xlab <- "Equivalent dose [Gy]"

    ## create plots ----

    # histogram
    hist <- plotly::plot_ly(data = data, x = ~x,
                            type = "histogram",
                            showlegend = FALSE,
                            name = "Bin", opacity = 0.75,
                            marker = list(color = "#428BCA",
                                          line = list(width = 1.0,
                                                      color = "white")),
                            histnorm = ifelse(normal_curve, "probability density", ""),
                            yaxis = "y"
    )

    # normal curve ----
    if (normal_curve) {

      density.curve <- density(data$x)
      normal.curve <- data.frame(x = density.curve$x, y = density.curve$y)

      hist <- plotly::add_trace(hist, data = normal.curve, x = ~x, y = ~y,
                                inherit = FALSE,
                                type = "scatter", mode = "lines",
                                line = list(color = "red"),
                                name = "Normal curve",
                                yaxis = "y")
    }

    # scatter plot of individual errors
    if (se) {
      yaxis2 <- list(overlaying = "y", side = "right",
                     showgrid = FALSE, title = settings$ylab[2],
                     ticks = "", showline = FALSE)

      se.text <- paste0("Measured value:</br>",
                        data$x, " &plusmn; ", data$y,"</br>")

      hist <- plotly::add_trace(hist, data = data, x = ~x, y = ~y,
                                inherit = FALSE,
                                type = "scatter", mode = "markers",
                                name = "Error", hoverinfo = "text",
                                text = se.text,
                                marker = list(color = "black"),
                                yaxis = "y2")

      hist <- plotly::layout(hist, yaxis2 = yaxis2)
    }

    # set layout ----
    hist <- plotly::layout(hist, hovermode = "closest",
                           title = paste("<b>", settings$main, "</b>"),
                           margin = list(r = 90),
                           xaxis = list(title = settings$xlab,
                                        ticks = ""),
                           yaxis = list(title = settings$ylab[1],
                                        ticks = "",
                                        showline = FALSE,
                                        showgrid = FALSE)
    )

    ## show and return plot ----
    print(hist)
    return(hist)
  }
}
