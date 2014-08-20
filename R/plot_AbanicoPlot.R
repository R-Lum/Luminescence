plot_AbanicoPlot <- structure(function(# Function to create an Abanico Plot.
  ### A plot is produced which allows comprehensive presentation of data
  ### precision and its dispersion around a central value as well as 
  ### illustration of a kernel density estimate of the dose values.
  
  # ===========================================================================
  ##author<<
  ## Michael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, JLU Giessen 
  ## (Germany)\cr
  ## Inspired by a plot introduced by Galbraith & Green (1990)\cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================
  
  data,
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
  ### (required): for \code{data.frame} two columns: De (\code{data[,1]})
  ### and De error (\code{data[,2]}). To plot several data sets in one plot
  ### the data sets must be provided as \code{list}, e.g. 
  ### \code{list(data.1, data.2)}.
  
  na.exclude = TRUE,
  ### \code{\link{logical}} (with default): exclude NA values from the data 
  ### set prior to any further operations.
  
  log.z = TRUE,
  ### \code{\link{logical}} (with default): Option to display the z-axis
  ### in logarithmic scale. Default is \code{TRUE}.
  
  central.value,
  ### \code{\link{numeric}}: User-defined central value, primarily used for
  ### horizontal centering of the z-axis.
  
  centrality = "mean.weighted",
  ### \code{\link{character}} (with default): measure of centrality, used for
  ### automatically centering the plot and drawing the central line. Can be
  ### one out of \code{"mean"}, \code{"median"}, \code{"mean.weighted"}, 
  ### and \code{"median.weighted"}. Default is \code{"mean.weighted"}.
  
  dispersion = "sd",
  ### \code{\link{character}} (with default): measure of dispersion, used for
  ### drawing the polygon that depicts the dose distribution. One out of
  ### \code{"sd"} (standard deviation),\code{"2sd"} (2 standard deviations) 
  ### \code{"qr"} (quartile range), default is \code{"sd"}.
  
  plot.ratio = 0.75,
  ### \code{\link{numeric}}: Relative space, given to the radial versus the
  ### cartesian plot part, deault is \code{0.75}.
  
  mtext,
  ### \code{\link{character}}: additional text below the plot title.
  
  summary,
  ### \code{\link{character}} (optional): adds numerical output to the plot. 
  ### Can be one or more out of: "n" (number of samples), "mean" (mean De 
  ### value), "mean.weighted" (error-weighted mean), "median" (median of 
  ### the De values), "sdrel" (relative standard deviation in 
  ### percent), "sdabs" (absolute standard deviation), "serel" (relative 
  ### standard error), "seabs" (absolute standard error) and "in.ci" (percent
  ### of samples in confidence interval, e.g. 2-sigma).
  
  summary.pos,
  ### \code{\link{numeric}} or \code{\link{character}} (with default): optional  
  ### position coordinates or keyword (e.g. \code{"topright"}) for the 
  ### statistical summary. Alternatively, the keyword \code{"sub"} may be
  ### specified to place the summary below the plot header. However, this
  ### latter option in only possible if \code{mtext} is not used.
  
  legend,
  ### \code{\link{character}} vector (optional): legend content to be added
  ### to the plot.
  
  legend.pos,
  ### \code{\link{numeric}} or \code{\link{character}} (with default): optional  
  ### position coordinates or keyword (e.g. \code{"topright"}) for the legend
  ### to be plotted.
  
  stats,
  ### \code{\link{character}}: additional labels of statistically important
  ### values in the plot. One or more out of the following: \code{"min"}, 
  ### \code{"max"}, \code{"median"}.
  
  y.axis = TRUE,
  ### \code{\link{logical}}: Option to hide y-axis labels. Useful for data  
  ### with small scatter.
  
  error.bars = FALSE,
  ### \code{\link{logical}}: Option to show De-errors as error bars on 
  ### De-points. Useful in combination with 
  ### \code{y.axis = FALSE, bar.col = "none"}.
  
  polygon.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the polygon showing the dose dispersion around the central value.
  ### To disable the polygon use \code{"none"}. Default is \code{"grey80"}.
  
  bar.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the bar showing the 2-sigma range of the dose error around the 
  ### central value. To disable the bar use \code{"none"}. Default is 
  ### \code{"grey50"}.
  
  line,
  ### \code{\link{numeric}}: numeric values of the additional lines to be
  ### added.
  
  line.col,
  ### \code{\link{character}} or \code{\link{numeric}}: colour of the
  ### additional lines.
  
  line.label,
  ### \code{\link{character}}: labels for the additional lines.
  
  grid.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the grid lines (originating at [0,0] and strechting to the z-scale).  
  ### To disable grid lines use \code{"none"}. Default is \code{"grey"}.
  
  bw = "nrd0",
  ### \code{\link{character}} (with default): bin-width for KDE, choose a  
  ### numeric value for manual setting.
  
  output = FALSE,
  ### \code{\link{logical}}: Optional output of numerical plot parameters.
  ### These can be useful to reproduce similar plots. Default is \code{FALSE}.
  
  ...
  ### Further plot arguments to pass. \code{xlab} must be a vector of length 2,
  ### specifying the upper and lower x-axes labels.
) {
  ## check data and parameter consistency--------------------------------------

  ## Homogenise input data format
  if(is(data, "list") == FALSE) {data <- list(data)}
  
  ## Check input data
  for(i in 1:length(data)) {
    if(is(data[[i]], "RLum.Results") == FALSE & 
         is(data[[i]], "data.frame") == FALSE) {
      stop(paste("[plot_AbanicoPlot] Error: Input data format is neither",
                 "'data.frame' nor 'RLum.Results'"))
    } else {
      if(is(data[[i]], "RLum.Results") == TRUE) {
        data[[i]] <- get_RLum.Results(data[[i]])[,1:2]
      }
    }
  }
  
  if(missing(stats) == TRUE) {stats <- numeric(0)}
  if(missing(bar.col) == TRUE) {bar.col <- rep("grey90", length(data))}
  if(missing(polygon.col) == TRUE) {polygon.col <- rep("grey60", length(data))}
  if(missing(grid.col) == TRUE) {grid.col <- rep("grey80", length(data))}
  if(missing(summary) == TRUE) {summary <- c("n", "in.ci")}
  if(missing(summary.pos) == TRUE) {summary.pos <- "sub"}
  if(missing(mtext) == TRUE) {mtext <- ""}
  
  ## check z-axis log-option for grouped data sets
  if(is(data, "list") == TRUE & length(data) > 1 & log.z == FALSE) {
    warning(paste("Option 'log.z' is not set to 'TRUE' altough more than one",
                  "data set (group) is provided."))
  }

  ## optionally, remove NA-values
  if(na.exclude == TRUE) {
    for(i in 1:length(data)) {
      n.NA <- sum(!complete.cases(data[[i]]))
      if(n.NA == 1) {print("1 NA value excluded.")
      } else if(n.NA > 1) {print(paste(n.NA, "NA values excluded."))}
      data[[i]] <- na.exclude(data[[i]])
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
  if("zlim" %in% names(extraArgs)) {
    limits.z <- extraArgs$zlim
  } else {
    z.span <- (mean(De.global) * 0.5) / (sd(De.global) * 100)
    z.span <- ifelse(z.span > 1, 0.9, z.span)
    limits.z <- c((ifelse(min(De.global) <= 0, 1.1, 0.9) - z.span) * min(De.global),
                  (1.1 + z.span) * max(De.global))
  }
  ticks <- round(pretty(limits.z, n = 5), 3)
  
  ## check for negative values
  if(min(De.global) <= 0) {
    stop("[plot_AbanicoPlot] Error: Data contains negative or zero values.")
  }
  
  #   ## calculate correction dose to shift negative values
#   if(min(De.global) <= 0) {
#     De.add <- abs(ticks[length(ticks) - sum(ticks > limits.z[1])])
#   } else {De.add <- 0}
  De.add <- 0
  
  ## optionally add correction dose to data set and adjust error
  for(i in 1:length(data)) {
    data[[i]][,1] <- data[[i]][,1] + De.add
    data[[i]][,2] <- data[[i]][,2] * data[[i]][,1] / abs(data[[i]][,1] - De.add)
  }

  ## adjust limits.z
  limits.z <- limits.z + De.add

  ## calculate and append statistical measures --------------------------------
  
  ## z-values based on log-option
  z <- sapply(1:length(data), function(x){
    if(log.z == TRUE) {log(data[[x]][,1])} else {data[[x]][,1]}})
  if(is(z, "list") == FALSE) {z <- list(z)}
  data <- lapply(1:length(data), function(x) {
     cbind(data[[x]], z[[x]])})
  rm(z)

  ## calculate dispersion based on log-option
  se <- sapply(1:length(data), function(x){
    if(log.z == TRUE) {data[[x]][,2] / data[[x]][,1]} else {data[[x]][,2]}})
  if(is(se, "list") == FALSE) {se <- list(se)}
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], se[[x]])})
  rm(se)
  
  ## calculate central values
  if(centrality == "mean") {
    z.central <- sapply(1:length(data), function(x){
      rep(mean(data[[x]][,3], na.rm = TRUE), length(data[[x]][,3]))})
  } else if(centrality == "median") {
    z.central <- sapply(1:length(data), function(x){
      rep(median(data[[x]][,3], na.rm = TRUE), length(data[[x]][,3]))})
  } else  if(centrality == "mean.weighted") {
    z.central <- sapply(1:length(data), function(x){
      sum(data[[x]][,3] / data[[x]][,4]^2) / 
        sum(1 / data[[x]][,4]^2)})
  } else if(centrality == "median.weighted") {
    ## define function after isotone::weighted.median
    median.w <- function (y, w)
    {
      ox <- order(y)
      y <- y[ox]
      w <- w[ox]
      k <- 1
      low <- cumsum(c(0, w))
      up <- sum(w) - low
      df <- low - up
      repeat {
        if (df[k] < 0) 
          k <- k + 1
        else if (df[k] == 0) 
          return((w[k] * y[k] + w[k - 1] * y[k - 1]) / (w[k] + w[k - 1]))
        else return(y[k - 1])
      }
    }
    z.central <- sapply(1:length(data), function(x){
      rep(median.w(y = data[[x]][,3], 
                   w = data[[x]][,4]), length(data[[x]][,3]))})
  } else {
    stop("Measure of centrality not supported!")
  }
  
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], z.central[[x]])})
  rm(z.central)
  
  ## calculate precision
  precision <- sapply(1:length(data), function(x){
    1 / data[[x]][,4]})
  if(is(precision, "list") == FALSE) {precision <- list(precision)}
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], precision[[x]])})
  rm(precision)
  
  ## calculate standardised estimate
  std.estimate <- sapply(1:length(data), function(x){
    (data[[x]][,3] - data[[x]][,5]) / data[[x]][,4]})
  if(is(std.estimate, "list") == FALSE) {std.estimate <- list(std.estimate)}
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], std.estimate[[x]])})
  
  ## append empty standard estimate for plotting
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], std.estimate[[x]])})
  rm(std.estimate)
  
  ## generate global data set
  data.global <- data[[1]]
  if(length(data) > 1) {
    for(i in 2:length(data)) {
      data.global <- rbind(data.global, data[[i]])
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
                             "std.estimate.plot")

  ## calculate global central value
  if(centrality == "mean") {
    z.central.global <- mean(data.global[,3], na.rm = TRUE)
  } else if(centrality == "median") {
    z.central.global <- median(data.global[,3], na.rm = TRUE)
  } else  if(centrality == "mean.weighted") {
    z.central.global <- sum(data.global[,3] / data.global[,4]^2) / 
      sum(1 / data.global[,4]^2)
  } else if(centrality == "median.weighted") {
    ## define function after isotone::weighted.mean
    median.w <- function (y, w) 
    {
      ox <- order(y)
      y <- y[ox]
      w <- w[ox]
      k <- 1
      low <- cumsum(c(0, w))
      up <- sum(w) - low
      df <- low - up
      repeat {
        if (df[k] < 0) 
          k <- k + 1
        else if (df[k] == 0) 
          return((w[k] * y[k] + w[k - 1] * y[k - 1])/(w[k] + 
                                                        w[k - 1]))
        else return(y[k - 1])
      }
    }
    z.central.global <- median.w(y = data.global[,3], w = data.global[,4])
  }
  
  ## optionally adjust zentral value by user-defined value
  if(missing(central.value) == FALSE) {
    
    ## adjust central value for De.add
    central.value <- central.value + De.add
    
    z.central.global <- ifelse(log.z == TRUE, 
      log(central.value), central.value)
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
                             "std.estimate.plot")
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
  
  ## print warning for too small scatter
  if(max(abs(1 / data.global[6])) < 0.02) {
    small.sigma <- TRUE
    print(paste("Attention, small standardised estimate scatter.",
                "Toggle off y.axis?"))
  }
  
  ## read out additional arguments---------------------------------------------
  extraArgs <- list(...)
  
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
    {expression(paste(D[e], " distribution"))}
  
  sub <- if("sub" %in% names(extraArgs)) {extraArgs$sub} else {""}
  
  if("xlab" %in% names(extraArgs)) {
    if(length(extraArgs$xlab) != 2) {
      stop("Argmuent xlab is not of length 2!")
    } else {xlab <- extraArgs$xlab}
  } else {
    xlab <- c(if(log.z == TRUE) {
      "Relative error [%]"
      } else {
        "Error"
        }, 
      "Precision")
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
  
  if("zlim" %in% names(extraArgs)) {
    limits.z <- extraArgs$zlim
  } else {
    z.span <- (mean(data.global[,1]) * 0.5) / (sd(data.global[,1]) * 100)
    z.span <- ifelse(z.span > 1, 0.9, z.span)
    limits.z <- c((0.9 - z.span) * min(data.global[[1]]),
                  (1.1 + z.span) * max(data.global[[1]]))
  }
  if(limits.z[1] <= 0) {
    limits.z <- limits.z + De.add
  }
  
  if("xlim" %in% names(extraArgs)) {
    limits.x <- extraArgs$xlim
  } else {
    limits.x <- c(0, max(data.global[,6]) * 1.05)
  }

  if(limits.x[1] != 0) {
    limits.x[1] <- 0
    warning("Lower x-axis limit not set to zero, issue corrected!")
  }
  
  if("ylim" %in% names(extraArgs)) {
    limits.y <- extraArgs$ylim
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
      rep(2, length(data))
    }
  
  lwd <- if("lwd" %in% names(extraArgs)) {
    extraArgs$lwd
    } else {
      rep(1, length(data))
    }
  
  pch <- if("pch" %in% names(extraArgs)) {
    extraArgs$pch
    } else {
      rep(20, length(data))
    }
  
  col <- if("col" %in% names(extraArgs)) {
    extraArgs$col
    } else {
      1:length(data)
    }
  
  tck <- if("tck" %in% names(extraArgs)) {
    extraArgs$tck
  } else {
    NA
  }
  
  tcl <- if("tcl" %in% names(extraArgs)) {
    extraArgs$tcl
  } else {
    -0.5
  }
  
  ## define auxiliary plot parameters -----------------------------------------
  
  ## create empty plot to update plot parameters
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
  
  ## calculate conversion factor for plot coordinates
  f <- 10^-6
  
  ## calculate major and minor z-tick values
  tick.values.major <- round(pretty(limits.z, n = 5), 0)
  tick.values.minor <- round(pretty(limits.z, n = 25), 0)
  
  tick.values.major <- tick.values.major[tick.values.major >= 
    min(tick.values.minor)]
  tick.values.major <- tick.values.major[tick.values.major <= 
    max(tick.values.minor)]
  tick.values.minor <- tick.values.minor[tick.values.minor >= 
    limits.z[1]]
  tick.values.minor <- tick.values.minor[tick.values.minor <= 
    limits.z[2]]
  
  if(log.z == TRUE) {
    tick.values.major <- log(tick.values.major)
    tick.values.minor <- log(tick.values.minor)
  }
  
  ## calculate z-axis radius
  r <- max(sqrt((limits.x[2])^2 + (data.global[,7] * f)^2))

  ## create z-axes labels
  if(log.z == TRUE) {
    label.z.text <- round(exp(tick.values.major), 0)
  } else {
    label.z.text <- round(tick.values.major, 0)
  }

  ## subtract De.add from label values
  if(De.add != 0) {
    label.z.text <- label.z.text - De.add
  }
  
  ## calculate node coordinates for semi-circle
  ellipse.values <- seq(from = ifelse(log.z == TRUE, 
                                      log(limits.z[1]), 
                                      limits.z[1]), 
                        to = max(ifelse(log.z == TRUE, 
                                    log(limits.z[2]), 
                                    limits.z[2]), 
                                 tick.values.major, 
                                 tick.values.minor), 
                        length.out = 500)
  ellipse.x <- r / sqrt(1 + f^2 * (ellipse.values - z.central.global)^2)
  ellipse.y <- (ellipse.values - z.central.global) * ellipse.x
  ellipse <- cbind(ellipse.x, ellipse.y)

  
  ## calculate statistical labels
  if(length(stats == 1)) {stats <- rep(stats, 2)}
  stats.data <- matrix(nrow = 3, ncol = 3)
  data.stats <- as.numeric(data.global[,1] - De.add)
  
  if("min" %in% stats == TRUE) {
    stats.data[1, 3] <- data.stats[data.stats == min(data.stats)]
    stats.data[1, 1] <- data.global[data.stats == stats.data[1, 3], 6]
    stats.data[1, 2] <- data.global[data.stats == stats.data[1, 3], 8]
  }
  
  if("max" %in% stats == TRUE) {
    stats.data[2, 3] <- data.stats[data.stats == max(data.stats)]
    stats.data[2, 1] <- data.global[data.stats == stats.data[2, 3], 6]
    stats.data[2, 2] <- data.global[data.stats == stats.data[2, 3], 8]
  }
  
  if("median" %in% stats == TRUE) {
    stats.data[3, 3] <- data.stats[data.stats == quantile(data.stats, 0.5, type = 3)]
    stats.data[3, 1] <- data.global[data.stats == stats.data[3, 3], 6][1]
    stats.data[3, 2] <- data.global[data.stats == stats.data[3, 3], 8][1]
  }
  
  ## re-calculate axes limits if necessary
  limits.z.x <- range(ellipse[,1])
  limits.z.y <- range(ellipse[,2])
  if(!("ylim" %in% names(extraArgs))) {
    if(limits.z.y[1] < 0.66 * limits.y[1]) {
      limits.y[1] <- 1.8 * limits.z.y[1]
    }
    if(limits.z.y[2] > 0.77 * limits.y[2]) {
      limits.y[2] <- 1.3 * limits.z.y[2]
    }
#    limits.y <- c(-max(abs(limits.y)), max(abs(limits.y)))
  }
  if(!("xlim" %in% names(extraArgs))) {
    if(limits.z.x[2] > 1.1 * limits.x[2]) {
      limits.x[2] <- limits.z.x[2]
    }
  }
  
  ## calculate and paste statistical summary
  label.text = list(NA)
  
  if(summary.pos[1] != "sub") {
    n.rows <- length(summary)
    
    for(i in 1:length(data)) {
      stops <- paste(rep("\n", (i - 1) * n.rows), collapse = "")
      label.text[[length(label.text) + 1]] <- paste(stops, paste(
        "  ",
        ifelse("n" %in% summary == TRUE,
               paste("n = ", 
                     nrow(data[[i]]), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("mean" %in% summary == TRUE,
               paste("mean = ", 
                     round(mean(data[[i]][,1]), 2), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("mean.weighted" %in% summary == TRUE,
               paste("weighted mean = ", 
                     round(weighted.mean(x = data[[i]][,1],
                                         w = 1 / data[[i]][,2]), 2), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("median" %in% summary == TRUE,
               paste("median = ", 
                     round(median(data[[i]][,1]), 2), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("sdrel" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(data[[i]][,1]) / mean(data[[i]][,1]) * 100,
                           2), " %",
                     "\n", 
                     sep = ""),
               ""),
        ifelse("sdabs" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(data[[i]][,1]), 2),
                     "\n", 
                     sep = ""),
               ""),
        ifelse("in.ci" %in% summary == TRUE,
               paste("in confidence interval = ", 
                     round(sum(data[[i]][,7] > -2 & data[[i]][,7] < 2) /
                             nrow(data[[i]]) * 100 , 1),
                     " % \n", 
                     sep = ""),
               ""),
        sep = ""), stops, sep = "")
      
    }
  } else {
    for(i in 1:length(data)) {
      label.text[[length(label.text) + 1]]  <- paste(
      "  ",
        ifelse("n" %in% summary == TRUE,
               paste("n = ", 
                     nrow(data[[i]]), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("mean" %in% summary == TRUE,
               paste("mean = ", 
                     round(mean(data[[i]][,1]), 2), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("mean.weighted" %in% summary == TRUE,
               paste("weighted mean = ", 
                     round(weighted.mean(x = data[[i]][,1],
                                         w = 1 / data[[i]][,2]), 2), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("median" %in% summary == TRUE,
               paste("median = ", 
                     round(median(data[[i]][,1]), 2), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("sdrel" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(data[[i]][,1]) / mean(data[[i]][,1]) * 100,
                           2), " %",
                     " | ", 
                     sep = ""),
               ""),
        ifelse("sdabs" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(data[[i]][,1]), 2),
                     " | ", 
                     sep = ""),
               ""),
        ifelse("in.ci" %in% summary == TRUE,
               paste("in confidence interval = ", 
                     round(sum(data[[i]][,7] > -2 & data[[i]][,7] < 2) /
                             nrow(data[[i]]) * 100 , 1),
                     " %  ", 
                     sep = ""),
               ""),
        sep = "")
    }
    ## remove outer vertical lines from string
    label.text[[2]] <- substr(x = label.text[[2]], 
                              start = 3, 
                              stop = nchar(label.text[[2]]) - 2)
  }
  
  ## remove dummy list element
  label.text[[1]] <- NULL
  
  ## convert keywords into summary placement coordinates
  if(missing(summary.pos) == TRUE) {
    summary.pos <- c(limits.x[1], limits.y[2])
    summary.adj <- c(0, 1)
  } else if(length(summary.pos) == 2) {
    summary.pos <- summary.pos
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "topleft") {
    summary.pos <- c(limits.x[1], limits.y[2])
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "top") {
    summary.pos <- c(mean(limits.x), limits.y[2])
    summary.adj <- c(0.5, 1)
  } else if(summary.pos[1] == "topright") {
    summary.pos <- c(limits.x[2], limits.y[2])
    summary.adj <- c(1, 1)
  }  else if(summary.pos[1] == "left") {
    summary.pos <- c(limits.x[1], mean(limits.y))
    summary.adj <- c(0, 0.5)
  } else if(summary.pos[1] == "center") {
    summary.pos <- c(mean(limits.x), mean(limits.y))
    summary.adj <- c(0.5, 0.5)
  } else if(summary.pos[1] == "right") {
    summary.pos <- c(limits.x[2], mean(limits.y))
    summary.adj <- c(1, 0.5)
  }else if(summary.pos[1] == "bottomleft") {
    summary.pos <- c(limits.x[1], limits.y[1])
    summary.adj <- c(0, 0)
  } else if(summary.pos[1] == "bottom") {
    summary.pos <- c(mean(limits.x), limits.y[1])
    summary.adj <- c(0.5, 0)
  } else if(summary.pos[1] == "bottomright") {
    summary.pos <- c(limits.x[2], limits.y[1])
    summary.adj <- c(1, 0)
  }
  
  ## convert keywords into legend placement coordinates
  if(missing(legend.pos) == TRUE) {
    legend.pos <- c(limits.x[1], limits.y[2])
    legend.adj <- c(0, 1)
  } else if(length(legend.pos) == 2) {
    legend.pos <- legend.pos
    legend.adj <- c(0, 1)
  } else if(legend.pos[1] == "topleft") {
    legend.pos <- c(limits.x[1], limits.y[2])
    legend.adj <- c(0, 1)
  } else if(legend.pos[1] == "top") {
    legend.pos <- c(mean(limits.x), limits.y[2])
    legend.adj <- c(0.5, 1)
  } else if(legend.pos[1] == "topright") {
    legend.pos <- c(limits.x[2], limits.y[2])
    legend.adj <- c(1, 1)
  } else if(legend.pos[1] == "left") {
    legend.pos <- c(limits.x[1], mean(limits.y))
    legend.adj <- c(0, 0.5)
  } else if(legend.pos[1] == "center") {
    legend.pos <- c(mean(limits.x), mean(limits.y))
    legend.adj <- c(0.5, 0.5)
  } else if(legend.pos[1] == "right") {
    legend.pos <- c(limits.x[2], mean(limits.y))
    legend.adj <- c(1, 0.5)
  } else if(legend.pos[1] == "bottomleft") {
    legend.pos <- c(limits.x[1], limits.y[1])
    legend.adj <- c(0, 0)
  } else if(legend.pos[1] == "bottom") {
    legend.pos <- c(mean(limits.x), limits.y[1])
    legend.adj <- c(0.5, 0)
  } else if(legend.pos[1] == "bottomright") {
    legend.pos <- c(limits.x[2], limits.y[1])
    legend.adj <- c(1, 0)
  }
    
  ## define cartesian plot origins
  xy.0 <- c(min(ellipse[,1]) * 1.03, min(ellipse[,2]))

  ## calculate coordinates for dispersion polygon overlay
  y.max.x <- 2 * limits.x[2] / max(data.global[6])
  
  polygons <- matrix(nrow = length(data), ncol = 14)
  for(i in 1:length(data)) {
    
    if(dispersion == "sd") {
      y.lower <- data[[i]][1,5] - sd(data[[i]][,3])
      y.upper <- data[[i]][1,5] + sd(data[[i]][,3])
    } else if(dispersion == "2sd") {
      y.lower <- data[[i]][1,5] - 2 * sd(data[[i]][,3])
      y.upper <- data[[i]][1,5] + 2 * sd(data[[i]][,3])
    } else if(dispersion == "qr") {
      y.lower <- quantile(data[[i]][,3], 0.25)
      y.upper <- quantile(data[[i]][,3], 0.75)
    } else {
      stop("Measure of dispersion not supported.")
    }
    
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
  }

  ## calculate coordinates for 2-sigma bar overlay
  bars <- matrix(nrow = length(data), ncol = 8)
  
  for(i in 1:length(data)) {
    bars[i,1:4] <- c(limits.x[1],
                     limits.x[1],
                     max(data.global$precision),
                     max(data.global$precision))
    bars[i,5:8] <- c(-2,
                     2,
                     (data[[i]][1,5] - z.central.global) * 
                       bars[i,3] + 2,
                     (data[[i]][1,5] - z.central.global) * 
                       bars[i,3] - 2)
  }
  
  ## calculate error bar coordinates
  if(error.bars == TRUE) {
    arrow.coords <- list(NA)
    for(i in 1:length(data)) {
      arrow.x1 <- data[[i]][,6]
      arrow.x2 <- data[[i]][,6]
      arrow.y1 <- data[[i]][,1] - data[[i]][,2]
      arrow.y2 <- data[[i]][,1] + data[[i]][,2]
      
      if(log.z == TRUE) {
        arrow.y1 <- log(arrow.y1)
        arrow.y2 <- log(arrow.y2)
      }
      
      arrow.coords[[length(arrow.coords) + 1]] <- cbind(
        arrow.x1, 
        arrow.x2, 
        (arrow.y1 - z.central.global) * arrow.x1, 
        (arrow.y2 - z.central.global) * arrow.x1)
    }
    arrow.coords[[1]] <- NULL
  }
  
  ## calculate KDE
  KDE <- list(NA)
  KDE.ext <- 0
  for(i in 1:length(data)) {
    KDE.i <- density(x = data[[i]][,3],
                     kernel = "gaussian", 
                     bw = bw)
    KDE.xy <- cbind(KDE.i$x, KDE.i$y)
    KDE.ext <- ifelse(max(KDE.xy[,2]) < KDE.ext, KDE.ext, max(KDE.xy[,2]))
    KDE.xy <- KDE.xy[KDE.xy[,1] >= min(tick.values.major,tick.values.minor) &
                   KDE.xy[,1] <= max(tick.values.major,tick.values.minor),]
    KDE[[length(KDE) + 1]] <- cbind(KDE.xy[,1], KDE.xy[,2])
  }
  KDE[1] <- NULL
  
  ## calculate line coordinates and further parameters
  if(missing(line) == FALSE) {
    if(log.z == TRUE) {line <- log(line)}
    
    line.coords <- list(NA)
    
    for(i in 1:length(line)) {
      line.x <- c(limits.x[1], min(ellipse[,1]), par()$usr[2])
      line.y <- c(0,
                  (line[i] - z.central.global) * min(ellipse[,1]),
                  (line[i] - z.central.global) * min(ellipse[,1]))
      line.coords[[length(line.coords) + 1]] <- rbind(line.x, line.y)
    }
    
    line.coords[1] <- NULL
    
    if(missing(line.col) == TRUE) {
      line.col <- seq(from = 1, to = length(line.coords))
    }
    
    if(missing(line.label) == TRUE) {
      line.label <- rep("", length(line.coords))
    }
  }
  
  ## Generate plot ------------------------------------------------------------

  ## determine number of subheader lines to shif the plot
  if(length(summary) > 0 & summary.pos[1] == "sub") {
    shift.lines <- length(data) + 1
  } else {shift.lines <- 1}
  
 ## extract original plot parameters
  oma.original <- par()$oma
  mar.original <- par()$mar
  xpd.original <- par()$xpd
  cex.original <- par()$cex
  bg.original <- par()$bg
  
  ## setup plot area
  par(oma = c(1, 1, 0, 0),
      mar = c(4, 4, shift.lines + 1.5, 7),
      xpd = TRUE,
      cex = cex)

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
  mtext(side = 2, 
        line = 2.5,
        at = 0,
        adj = 0.5,
        cex = cex,
        text = ylab)
  
  ## calculate upper x-axis label values
  label.x.upper <- if(log.z == TRUE) {
    as.character(round(1/axTicks(side = 1)[-1] * 100, 1))
  } else {
    as.character(round(1/axTicks(side = 1)[-1], 1))
  }
  
  ## optionally, plot dispersion polygon
  if(polygon.col[1] != "none") {
    for(i in 1:length(data)) {
      polygon(x = polygons[i,1:7], 
              y = polygons[i,8:14],
              lty = "blank",
              col = polygon.col[i])
    }
  }
  
  ## optionally, plot 2-sigma-bar
  if(bar.col[1] != "none") {
    for(i in 1:length(data)) {
      polygon(x = bars[i,1:4], 
              y = bars[i,5:8],
              col = adjustcolor(bar.col[i], alpha.f = 0.7),
              border = NA)
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

  ## optionally, add radial grid lines
  if(grid.col[1] != "none") {
    for(i in 1:length(tick.values.major)) {
      lines(x = c(limits.x[1], min(ellipse[,1])),
            y = c(0, (tick.values.major[i] - z.central.global) * min(ellipse[,1])),
            col = grid.col,
            lwd = 1)
    }
  }
  
  ## optionally, add cartesian grid lines
  if(grid.col[1] != "none") {
    for(i in 1:length(tick.values.major)) {
      lines(x = c(xy.0[1], par()$usr[2]),
            y = c((tick.values.major[i] - z.central.global) * min(ellipse[,1]),
                  (tick.values.major[i] - z.central.global) * min(ellipse[,1])),
            col = grid.col,
            lwd = 1)
    }
  }
  
## draw border around plot
polygon(x = c(limits.x[1], min(ellipse[,1]), par()$usr[2],
              par()$usr[2], min(ellipse[,1])),
        y = c(0, max(ellipse[,2]), max(ellipse[,2]), 
              min(ellipse[,2]), min(ellipse[,2])))

## optionally, plot central value lines
  if(lwd[1] > 0 & lty[1] > 0) {
    for(i in 1:length(data)) {
      x2 <- r / sqrt(1 + f^2 * (
        data[[i]][1,5] - z.central.global)^2)
      y2 <- (data[[i]][1,5] - z.central.global) * x2
      lines(x = c(limits.x[1], x2, xy.0[1], par()$usr[2]),
            y = c(0, y2, y2, y2),
            lty = lty[i],
            lwd = lwd[i],
            col = col[i])
    }
  }

  ## optionally add further lines
  if(missing(line) == FALSE) {
    for(i in 1:length(line)) {
      lines(x = line.coords[[i]][1,1:3],
            y = line.coords[[i]][2,1:3],
            col = line.col[i])
      text(x = line.coords[[i]][1,3],
           y = line.coords[[i]][2,3] + par()$cxy[2] * 0.3,
           labels = line.label[i],
           pos = 2,
           col = line.col[i],
           cex = cex * 0.9)      
    }
  }
  
  ## add plot title
  title(main = main, line = shift.lines)

  ## plot lower x-axis (precision)
  x.axis.ticks <- axTicks(side = 1)
  x.axis.ticks <- x.axis.ticks[c(TRUE, x.axis.ticks <= limits.x[2])]
  x.axis.ticks <- x.axis.ticks[x.axis.ticks <= max(ellipse[,1])]
    
  ## axis with lables and ticks
  axis(side = 1,
       at = x.axis.ticks, 
       lwd = 1,
       xlab = "")
  
  ## extend axis line to right side of the plot
  lines(x = c(max(x.axis.ticks), max(ellipse[,1])), 
        y = c(limits.y[1], limits.y[1]))
  
  ## draw closing tick on right hand side
  axis(side = 1, tcl = 0.5, lwd = 0, lwd.ticks = 1, at = limits.x[2],
       labels = FALSE)
  axis(side = 1, tcl = -0.5, lwd = 0, lwd.ticks = 1, at = limits.x[2],
       labels = FALSE)
  
  ## add upper axis label
  mtext(text = xlab[1],
        at = (limits.x[1] + max(ellipse[,1])) / 2,
        side = 1,
        line = -3.5,
        cex = cex)
  
  ## add lower axis label
  mtext(text = xlab[2],
        at = (limits.x[1] + max(ellipse[,1])) / 2,
        side = 1,
        line = 2.5,
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
    
  ## plot y-axis
  if(y.axis == TRUE) {
    char.height <- par()$cxy[2]
    tick.space <- axisTicks(usr = limits.y, log = FALSE)
    tick.space <- (max(tick.space) - min(tick.space)) / length(tick.space)
    if(tick.space < char.height * 1.7) {
      axis(side = 2, at = c(-2, 2), labels = c("", ""), las = 1)
      axis(side = 2, at = 0, tcl = 0, labels = paste("\u00B1", "2"), las = 1)
    } else {
      axis(side = 2, at = seq(-2, 2, by = 2), las = 1)
    }
  } else {
    axis(side = 2, at = 0)
  }
  
  ## plot minor z-ticks
  for(i in 1:length(tick.values.minor)) {
    lines(x = c(par()$usr[2], (1 + 0.007 * cex) * par()$usr[2]),
          y = c((tick.values.minor[i] - z.central.global) * min(ellipse[,1]),
                (tick.values.minor[i] - z.central.global) * min(ellipse[,1])))
  }

  ## plot major z-ticks
  for(i in 1:length(tick.values.major)) {
    lines(x = c(par()$usr[2], (1 + 0.015 * cex) * par()$usr[2]),
          y = c((tick.values.major[i] - z.central.global) * min(ellipse[,1]),
                (tick.values.major[i] - z.central.global) * min(ellipse[,1])))
  }
  
  ## plot z-axes
  lines(ellipse)
  lines(rep(par()$usr[2], nrow(ellipse)), ellipse[,2])
  
  ## plot z-values
  text(x = (1 + 0.04 * cex) * par()$usr[2],
       y = (tick.values.major - z.central.global) * min(ellipse[,1]),
       label = label.z.text, 0)
  
  ## plot z-label
  mtext(side = 4, 
        at = 0,
        line = 5, 
        las = 3,
        adj = 0.5,
        cex = cex,
        text = zlab)
  
  ## plot values
  for(i in 1:length(data)) {
    points(data[[i]][,6][data[[i]][,6] <= limits.x[2]], 
           data[[i]][,8][data[[i]][,6] <= limits.x[2]], 
           col = col[i],
           pch = pch[i])
  }

  if(error.bars == TRUE) {
    for(i in 1:length(data)) {
      arrows(x0 = arrow.coords[[i]][,1],
             x1 = arrow.coords[[i]][,2],
             y0 = arrow.coords[[i]][,3],
             y1 = arrow.coords[[i]][,4],
             length = 0.05,
             angle = 90,
             code = 3,
             col = col[i])
    }
  }
  
  
  ## plot KDE base line
  lines(x = c(xy.0[1], xy.0[1]), y = c(min(ellipse[,2]), max(ellipse[,2])))

  ## calculate KDE width
  KDE.max <- 0
  for(i in 1:length(data)) {
    KDE.max <- ifelse(KDE.max < max(KDE[[i]][,2]), max(KDE[[i]][,2]), KDE.max)
  }
  KDE.scale <- (par()$usr[2] - xy.0[1]) / (KDE.max * 1.05)
  
  ## plot KDE x-axis
  axis(side = 1, 
       at = c(xy.0[1], par()$usr[2]), 
       labels = as.character(round(c(0, KDE.ext), 2)))

  ## add axis label
  mtext(text = "Density",
        at = (xy.0[1] + par()$usr[2]) / 2,
        side = 1,
        line = 2.5,
        cex = cex)

  ## plot KDE lines
  for(i in 1:length(data)) {
    lines(x = xy.0[1] + KDE[[i]][,2] * KDE.scale,
          y = (KDE[[i]][,1] - z.central.global) * min(ellipse[,1]),
          col = col[i])
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
  if(missing(legend) == FALSE) {
    legend(x = legend.pos[1],
           y = 0.8 * legend.pos[2],
           xjust = legend.adj[1],
           yjust = legend.adj[2],
           legend = legend,
           pch = pch,
           col = col,
           text.col = col,
           cex = 0.8 * cex,
           bty = "n")
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
    } else {
      if(mtext == "") {
        mtext(side = 3, 
              line = shift.lines - 1 - i, 
              text = label.text[[i]],
              col = col[i],
              cex = 0.8 * cex)
      }
    }
  }
  
  ## restore previous plot parameters
  par(oma = oma.original)
  par(mar = mar.original)
  par(xpd = xpd.original)
  par(cex = cex.original)
  
  ## create and resturn numeric output
  if(output == TRUE) {
    return(list(xlim = limits.x,
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
                KDE = KDE))
  }
  
  ### returns a plot object and, optionally, a list with plot calculus data.
  
  ##details<<
  ## The Abanico Plot is a combination of the classic Radial Plot 
  ## (\code{plot_RadialPlot}) and a kernel density estimate plot (e.g.
  ## \code{plot_KDE}). It allows straightforward visualisation of data
  ## precision, error scatter around a user-defined central value and the 
  ## combined distribution of the values, on the actual scale of the measured
  ## data (e.g. seconds, equivalent dose, years). The principle of the plot
  ## is shown in Galbraith & Green (1990). The function authors are
  ## thankful for the thoughtprovocing figure in this article. \cr
  ## The semi circle (z-axis) of the classic Radial Plot is bent to a straight
  ## line here, which actually is the basis for combining this polar (radial)
  ## part of the plot with any other cartesian visualisation method (KDE,
  ## histogram, PDF and so on). Note that the plot allows dispaying two
  ## measures of distribution. One is the 2-sigma bar, which illustrates
  ## the spread in value errors, and the other is the polygon, which stretches
  ## over both parts of the Abanico Plot (polar and cartesian) and illustrates
  ## the actual spread in the values themselfes. \cr
  ## Since the 2-sigma-bar is a polygon, it can be (and is) filled with shaded
  ## lines. To change density (lines per inch, default is 15) and angle
  ## (default is 45 degrees) of the shading lines, specify these parameters.
  ## See \code{?polygon()} for further help. \cr
  ## The Abanico Plot supports other than the weighted mean as measure of 
  ## centrality. When it is obvious that the data is not (log-)normally
  ## distributed, the mean (weighted or not) cannot be a valid measure of
  ## centrality and hence central dose. Accordingly, the median and the
  ## weighted median can be chosen as well to represent a proper measure of
  ## centrality (e.g. \code{centrality = "median.weighted"}).\cr
  ## The proportion of the polar part and the cartesian part of the Abanico
  ## Plot can be modfied for display reasons (\code{plot.ratio = 0.75}). By 
  ## default, the polar part spreads over 75 % and leaves 25 % for the part
  ## that shows the KDE graph.
  
  ##references<<
  ## Galbraith, R. & Green, P., 1990. Estimating the component ages in a 
  ## finite mixture. International Journal of Radiation Applications and 
  ## Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 
  ## (3), pp. 197-206. 
  
  ##seealso<<
  ## \code{\link{plot_RadialPlot}}, \code{\link{plot_KDE}}, 
  ## \code{\link{plot_Histogram}}
  
}, ex=function(){
  ## load example data and recalculate to Gray
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <- 
    Second2Gray(values = ExampleData.DeValues, dose_rate = c(0.0438,0.0019))
  
  ## plot the example data straightforward
  plot_AbanicoPlot(data = ExampleData.DeValues)
  
  ## now with linear z-scale
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   log.z = FALSE)
  
  ## now with output of the plot parameters
  plot1 <- plot_AbanicoPlot(data = ExampleData.DeValues,
                            output = TRUE)
  str(plot1)
  plot1$zlim
  
  ## now with adjusted z-scale limits
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   zlim = c(100, 200))
  
  ## now with adjusted x-scale limits
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   xlim = c(0, 60))
  
  ## now with user-defined plot ratio
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   plot.ratio = 0.5)
  
  ## now with user-defined central value
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   central.value = 120)
  
  ## now with weighted median as measure of centrality
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   centrality = "median.weighted")
  
  ## now with median/quartile range as measure of centrality/dispersion
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   centrality = "median", 
                   dispersion = "qr")
  
  ## now with user-defined green line for MAM3 (i.e. 2936.3)
  MAM <- calc_MinDose3(input.data = ExampleData.DeValues,
                       sigmab = 0.3,
                       gamma.xub = 7000, 
                       output.plot = FALSE)
  
  MAM <- as.numeric(get_RLum.Results(object = MAM, 
                                     data.object = "results")$mindose)
  
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   xlim = c(0, 50),
                   line = MAM,
                   line.col = "darkgreen",
                   line.label = "MAM3-dose")
  
  ## now add lines (e.g. De = 100) completely manually
  ## 1. infer extra data
  extra <- plot_AbanicoPlot(data = ExampleData.DeValues,
                            output = TRUE)
  
  ## 2. transform De value to plot coordinates, only use log when 
  ##    log.z = TRUE. Don't mind the cryptic equation too much.
  De <- 100
  y.De <- (log(De) - extra$data.global$z.central[1]) * extra$polar.box[2]
  
  ## 3. create line coordinates (origin - polar margin - cartesian margin)
  line.x <- c(0, extra$polar.box[2], extra$cartesian.box[2])
  line.y <- c(0, y.De, y.De)
  
  ## 4. draw the line
  lines(x = line.x, y = line.y, lwd = 2, lty = 4, col = "tomato")
  
  ## now create plot with legend, colour, different points and smaller scale
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   legend = "Sample 1",
                   col = "tomato4",
                   bar.col = "peachpuff",
                   pch = "R",
                   cex = 0.8)
  
  ## now without 2-sigma bar, polygon, grid lines and central value line
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   bar.col = "none",
                   polygon.col = "none",
                   grid.col = "none",
                   y.axis = FALSE,
                   lwd = 0)
  
  ## now with direct display of De errors, without 2-sigma bar
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   bar.col = "none",
                   ylab = "",
                   y.axis = FALSE,
                   error.bars = TRUE)
  
  ## now with user-defined axes labels
  plot_AbanicoPlot(data = ExampleData.DeValues,
                   xlab = c("Data error [%]",
                            "Data precision"),
                   ylab = "Scatter",
                   zlab = "Equivalent dose [Gy]")
  
  ## now with minimum, maximum and median value indicated
  plot_AbanicoPlot(data = ExampleData.DeValues,
                  central.value = 150,
                  stats = c("min", "max", "median"))
  
  ## now with a brief statistical summary
  plot_AbanicoPlot(data = ExampleData.DeValues,
                  summary = c("n", "in.ci"))
  
  ## now with another statistical summary as subheader
  plot_AbanicoPlot(data = ExampleData.DeValues,
                  summary = c("mean.weighted", "median"),
                  summary.pos = "sub")
  
  ## now the data set is split into sub-groups, one is manipulated
  data.1 <- ExampleData.DeValues[1:15,]
  data.2 <- ExampleData.DeValues[16:25,] * 1.3
  
  ## now a common dataset is created from the two subgroups
  data.3 <- list(data.1, data.2)
  
  ## now the two data sets are plotted in one plot
  plot_AbanicoPlot(data = data.3)
  
  ## now with some graphical modification
  plot_AbanicoPlot(data = data.3,
                   col = c("steelblue4", "orange4"),
                   bar.col = c("steelblue3", "orange3"),
                   polygon.col = c("steelblue1", "orange1"),
                   pch = c(2, 6),
                   density = c(10, 20),
                   angle = c(30, 50),
                   summary = c("n", "in.ci"))
})