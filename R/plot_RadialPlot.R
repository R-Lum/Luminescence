plot_RadialPlot <- structure(function(# Function to create a Radial Plot
  ### A Galbraith's radial plot is produced on a logarithmic or a linear 
  ### scale.
  
  # ===========================================================================
  ##author<<
  ## Michael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, JLU Giessen 
  ## (Germany)\cr
  ## Based on a rewritten S script of Rex Galbraith, 2010\cr

  ##section<<
  ## version 0.5.2
  # ===========================================================================
  
  data,
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
  ### (required): for \code{data.frame} two columns: De (\code{data[,1]})
  ### and De error (\code{data[,2]}). To plot several data sets in one plot,
  ### the data sets must be provided as \code{list}, e.g. 
  ### \code{list(data.1, data.2)}.
  
  na.exclude = TRUE,
  ### \code{\link{logical}} (with default): excludes \code{NA} values from 
  ### the data set prior to any further operations.
  
  negatives = "remove",
  ### \code{\link{character}} (with default): rule for negative values. Default
  ### is \code{"remove"} (i.e. negative values are removed from the data set).
  
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
  
  mtext,
  ### \code{\link{character}}: additional text below the plot title.
  
  summary,
  ### \code{\link{character}} (optional): adds numerical output to the plot. 
  ### Can be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De 
  ### value), \code{"mean.weighted"} (error-weighted mean), \code{"median"} (median of 
  ### the De values), \code{"sdrel"} (relative standard deviation in 
  ### percent), \code{"sdabs"} (absolute standard deviation), \code{"serel"} (relative 
  ### standard error), \code{"seabs"} (absolute standard error) and \code{"in.ci"} (percent
  ### of samples in confidence interval, e.g. 2-sigma).
  
  summary.pos,
  ### \code{\link{numeric}} or \code{\link{character}} (with default): optional  
  ### position coordinates or keyword (e.g. \code{"topright"}) for the 
  ### statistical summary. Alternatively, the keyword \code{"sub"} may be
  ### specified to place the summary below the plot header. However, this
  ### latter option is only possible if \code{mtext} is not used.
  
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
  
  plot.ratio,
  ### \code{\link{numeric}}: User-defined plot area ratio (i.e. curvature of
  ### the z-axis). If omitted, the default value (\code{4.5/5.5}) is used and
  ### modified automatically to optimise the z-axis curvature. 
  ### The parameter should be decreased when data points are plotted outside
  ### the z-axis or when the z-axis gets too elliptic.
  
  bar.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the bar showing the 2-sigma range around the central value. To 
  ### disable the bar, use \code{"none"}. Default is \code{"grey"}.
  
  y.ticks = TRUE,
  ### \code{\link{logical}}: Option to hide y-axis labels. Useful for data  
  ### with small scatter.
  
  grid.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the grid lines (originating at [0,0] and stretching to the z-scale).  
  ### To disable grid lines, use \code{"none"}. Default is \code{"grey"}.
  
  line,
  ### \code{\link{numeric}}: numeric values of the additional lines to be
  ### added.
  
  line.col,
  ### \code{\link{character}} or \code{\link{numeric}}: colour of the
  ### additional lines.
  
  line.label,
  ### \code{\link{character}}: labels for the additional lines.
  
  output = FALSE,
  ### \code{\link{logical}}: Optional output of numerical plot parameters.
  ### These can be useful to reproduce similar plots. Default is \code{FALSE}.
  
  ...
  ### Further plot arguments to pass. \code{xlab} must be a vector of length 2,
  ### specifying the upper and lower x-axes labels.
) {
  ## Homogenise input data format
  if(is(data, "list") == FALSE) {data <- list(data)}
  
  ## Check input data
  for(i in 1:length(data)) {
    if(is(data[[i]], "RLum.Results") == FALSE & 
         is(data[[i]], "data.frame") == FALSE) {
      stop(paste("[plot_RadialPlot] Error: Input data format is neither",
                 "'data.frame' nor 'RLum.Results'"))
    } else {
      if(is(data[[i]], "RLum.Results") == TRUE) {
        data[[i]] <- get_RLum.Results(data[[i]])[,1:2]
      }
    }
  }
  
  ## check data and parameter consistency--------------------------------------
  if(missing(stats) == TRUE) {stats <- numeric(0)}
  if(missing(summary) == TRUE) {
    summary <- c("n", "in.ci")
    summary.pos = "sub"
  }
  if(missing(bar.col) == TRUE) {bar.col <- rep("grey80", length(data))}
  if(missing(grid.col) == TRUE) {grid.col <- rep("grey70", length(data))}
  if(missing(summary) == TRUE) {summary <- NULL}
  if(missing(summary.pos) == TRUE) {summary.pos <- "topleft"}
  if(missing(mtext) == TRUE) {mtext <- ""}
  
  
  ## check z-axis log-option for grouped data sets
  if(is(data, "list") == TRUE & length(data) > 1 & log.z == FALSE) {
    warning(paste("Option 'log.z' is not set to 'TRUE' altough more than one",
                  "data set (group) is provided."))
  }

  ## optionally, remove NA-values
  if(na.exclude == TRUE) {
    for(i in 1:length(data)) {
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
  De.delta <- ticks[2] - ticks[1]
  
#   ## calculate correction dose to shift negative values
#   if(min(De.global) <= 0) {
#     De.add <- abs(ticks[length(ticks) - sum(ticks > limits.z[1])])
#   } else {De.add <- 0}
  
  ## optionally, reassign De.add to remove negative values
  if(negatives == "remove") {
    De.add <- 0
    
    for(i in 1:length(data)) {
      data.test <- data[[i]][,1] <= 0
      data[[i]] <- data[[i]][!data.test,]
      data.negative <- paste(seq(1, length(data.test))[data.test == TRUE], 
                             collapse = ", ")
      if(sum(data.test) > 0) {
        print(paste("The following lines contain negative values: ",
                      data.negative,
                      ".", 
                      sep = ""))
      }
    }
  }
  
  ## optionally add correction dose to data set and adjust error
  for(i in 1:length(data)) {
    data[[i]][,1] <- data[[i]][,1] + De.add
    data[[i]][,2] <- data[[i]][,2] * data[[i]][,1] / abs(data[[i]][,1] - De.add)
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
  De.delta <- ticks[2] - ticks[1]
  
  ## calculate correction dose to shift negative values
  if(min(De.global) <= 0) {
    De.add <- abs(ticks[length(ticks) - sum(ticks > limits.z[1])])
  } else {De.add <- 0}
  
  if(negatives == "remove") {
    De.add <- 0
  }
  ## optionally add correction dose to data set and adjust error
  for(i in 1:length(data)) {
    data[[i]][,1] <- data[[i]][,1] + De.add
    data[[i]][,2] <- data[[i]][,2] * data[[i]][,1] / abs(data[[i]][,1] - De.add)
  }
  
  ## adjust limits.z
  limits.z <- limits.z + 2 * De.add
  
  ## calculate and append statistical measures --------------------------------
  
  ## z-values based on log-option
  z <- sapply(1:length(data), function(x){
    if(log.z == TRUE) {log(data[[x]][,1])} else {data[[x]][,1]}})
  if(is(z, "list") == FALSE) {z <- list(z)}
  data <- lapply(1:length(data), function(x) {
     cbind(data[[x]], z[[x]])})
  rm(z)

  ## calculate se-values based on log-option
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
  
  ## calculate standard estimate
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
    central.value <- central.value + 2 * De.add
    
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
                "Toggle off y.ticks?"))
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
    limits.z <- limits.z + 2 * De.add
  }
  
  if("xlim" %in% names(extraArgs)) {
    limits.x <- extraArgs$xlim
  } else {
    limits.x <- c(0, max(data.global[,6]))
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
                   (0.8 + y.span) * max(abs(data.global[,7])))
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
      rep(1, length(data))
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
  
  show <- if("show" %in% names(extraArgs)) {extraArgs$show} else {TRUE}
  if(show != TRUE) {show <- FALSE}
  
  fun <- if("fun" %in% names(extraArgs)) {
    extraArgs$fun
  } else {
    FALSE
  }
  
  ## define auxiliary plot parameters -----------------------------------------
  
  ## optionally adjust plot ratio
  if(missing(plot.ratio) == TRUE) {
    if(log.z == TRUE) {
      plot.ratio <- 1 /  (1 * ((max(data.global[,6]) - min(data.global[,6])) / 
        (max(data.global[,7]) - min(data.global[,7]))))
    } else {
      plot.ratio <- 4.5 / 5.5
    }
  } 
  
  ## calculate conversion factor for plot coordinates
  f <- (max(data.global[,6]) - min(data.global[,6])) / 
       (max(data.global[,7]) - min(data.global[,7])) * plot.ratio
  
  ## calculate major and minor z-tick values
  tick.values.major <- round(pretty(limits.z, n = 5), 0)
  tick.values.minor <- round(pretty(limits.z, n = 25), 0)
  
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

  if(log.z == TRUE) {
    tick.values.major <- log(tick.values.major)
    tick.values.minor <- log(tick.values.minor)
  }
  
  ## calculate z-axis radius
  r.x <- limits.x[2] / max(data.global[,6]) + 0.03
  r <- max(sqrt((data.global[,6])^2+(data.global[,7] * f)^2)) * r.x
   
  ## calculate major z-tick coordinates
  tick.x1.major <- r / sqrt(1 + f^2 * (
    tick.values.major - z.central.global)^2)
  tick.y1.major <- (tick.values.major - z.central.global) * tick.x1.major
  tick.x2.major <- (1 + 0.015 * cex) * r / sqrt(
    1 + f^2 * (tick.values.major - z.central.global)^2)
  tick.y2.major <- (tick.values.major - z.central.global) * tick.x2.major
  ticks.major <- cbind(tick.x1.major, 
                       tick.x2.major, 
                       tick.y1.major, 
                       tick.y2.major)
  
  ## calculate minor z-tick coordinates
  tick.x1.minor <- r / sqrt(1 + f^2 * (
    tick.values.minor - z.central.global)^2)
  tick.y1.minor <- (tick.values.minor - z.central.global) * tick.x1.minor
  tick.x2.minor <- (1 + 0.007 * cex) * r / sqrt(
    1 + f^2 * (tick.values.minor - z.central.global)^2)
  tick.y2.minor <- (tick.values.minor - z.central.global) * tick.x2.minor
  ticks.minor <- cbind(tick.x1.minor, 
                       tick.x2.minor, 
                       tick.y1.minor, 
                       tick.y2.minor)
  
  ## calculate z-label positions
  label.x <- 1.03 * r / sqrt(1 + f^2 *
    (tick.values.major - z.central.global)^2)
  label.y <- (tick.values.major - z.central.global) * tick.x2.major
  
  ## create z-axes labels
  if(log.z == TRUE) {
    label.z.text <- round(exp(tick.values.major), 0)
  } else {
    label.z.text <- round(tick.values.major, 0)
  }

  ## subtract De.add from label values
  if(De.add != 0) {
    label.z.text <- label.z.text - 2 * De.add
  }

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
  user.limits <- if(log.z == TRUE) {
    log(limits.z)
  } else{
    limits.z
  }

  ellipse.values <- seq(from = min(c(tick.values.major, 
                                     tick.values.minor,
                                     user.limits[2])), 
                        to = max(c(tick.values.major, 
                                   tick.values.minor,
                                   user.limits[2])), 
                        length.out = 500)
  ellipse.x <- r / sqrt(1 + f^2 * (ellipse.values - z.central.global)^2)
  ellipse.y <- (ellipse.values - z.central.global) * ellipse.x
  ellipse <- cbind(ellipse.x, ellipse.y)
  ellipse.lims <- rbind(range(ellipse[,1]), range(ellipse[,2]))
  
  ## calculate statistical labels
  if(length(stats == 1)) {stats <- rep(stats, 2)}
  stats.data <- matrix(nrow = 3, ncol = 3)
  data.stats <- as.numeric(data.global[,1] - 2 * De.add)

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
        "| ",
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
                     " % | ", 
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

  ## calculate line coordinates and further parameters
  if(missing(line) == FALSE) {
    if(log.z == TRUE) {line <- log(line)}
  
    line.coords <- list(NA)
  
    for(i in 1:length(line)) {
      line.x <- c(limits.x[1],  
                  r / sqrt(1 + f^2 * (line[i] - z.central.global)^2))
      line.y <- c(0, (line[i] - z.central.global) * line.x[2])
      
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
  
  ## check if plotting is enabled
  if(show == TRUE) {
   
    ## determine number of subheader lines to shif the plot
    if(length(summary) > 0 & summary.pos[1] == "sub") {
      shift.lines <- length(data) + 1
    } else {shift.lines <- 1}
    
    ## setup plot area
    par(oma = c(1, 1, 0, 0),
        mar = c(4, 4, shift.lines + 1.5, 7),
        xpd = TRUE,
        cex = cex)
    
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
    label.x.upper <- if(log.z == TRUE) {
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
        x2 <- r / sqrt(1 + f^2 * (
          data[[i]][1,5] - z.central.global)^2)
        y2 <- (data[[i]][1,5] - z.central.global) * x2
        lines(x = c(limits.x[1], x2),
              y = c(0, y2),
              lty = lty[i],
              lwd = lwd[i],
              col = col[i])
      }
    }
    
    ## optionally add further lines
    if(missing(line) == FALSE) {
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
    title(main = main, line = shift.lines)
    
    ## plot lower x-axis (precision)
    x.axis.ticks <- axTicks(side = 1)
    x.axis.ticks <- x.axis.ticks[c(TRUE, x.axis.ticks <= limits.x[2])]
    x.axis.ticks <- x.axis.ticks[x.axis.ticks <= limits.x[2]]
    
    ## axis with lables and ticks
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
    
    ## add upper axis label
    mtext(text = xlab[1],
          at = (limits.x[1] + limits.x[2]) / 2,
          side = 1,
          line = -3.5,
          cex = cex)
    
    ## add lower axis label
    mtext(text = xlab[2],
          at = (limits.x[1] + limits.x[2]) / 2,
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
    
    ## plot y-axis
    if(y.ticks == TRUE) {
      char.height <- par()$cxy[2]
      tick.space <- axisTicks(usr = limits.y, log = FALSE)
      tick.space <- (max(tick.space) - min(tick.space)) / length(tick.space)
      if(tick.space < char.height * 1.5) {
        axis(side = 2, at = c(-2, 2), labels = c("", ""), las = 1)
        axis(side = 2, at = 0, tcl = 0, labels = paste("\u00B1", "2"), las = 1)
      } else {
        axis(side = 2, at = seq(-2, 2, by = 2), las = 2)
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
    
    ##FUN by R Luminescence Team
    if(fun==TRUE){sTeve()}
  }

  if(output == TRUE) {
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
  
  ### Returns a plot object.
  
  ##details<<
  ## Details and the theoretical background of the radial plot are given 
  ## in the cited literature. This function is based on an S script of Rex 
  ## Galbraith. To reduce the manual adjustments, the function has been 
  ## rewritten. Thanks to Rex Galbraith for useful comments on this function.
  ## \cr Plotting can be disabled by adding the argument 
  ## \code{plot = "FALSE"}, e.g. to return only numeric plot output.\cr
  ##
  ## Earlier versions of the Radial Plot in this package had the 2-sigma-bar
  ## drawn onto the z-axis. However, this might have caused misunderstanding
  ## in that the 2-sigma range may also refer to the z-scale, which it does
  ## not! Rather it applies only to the x-y-coordinate system (standardised
  ## error vs. precision). A spread in doses or ages must be drawn as lines
  ## originating at zero precision (x0) and zero standardised estimate (y0).
  ## Such a range may be drawn by adding lines to the radial plot (
  ## \code{line}, \code{line.col}, \code{line.label}, cf. examples).
  
  ##references<<
  ## Galbraith, R.F., 1988. Graphical Display of Estimates Having Differing 
  ## Standard Errors. Technometrics, 30 (3), 271-281.
  ##
  ## Galbraith, R.F., 1990. The radial plot: Graphical assessment of spread in 
  ## ages. International Journal of Radiation Applications and Instrumentation. 
  ## Part D. Nuclear Tracks and Radiation Measurements, 17 (3), 207-214. 
  ##
  ## Galbraith, R. & Green, P., 1990. Estimating the component ages in a 
  ## finite mixture. International Journal of Radiation Applications and 
  ## Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 (3) 
  ## 197-206. 
  ##
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
  ## track ages. Nuclear Tracks And Radiation Measurements, 21 (4),  
  ## 459-470. 
  ##
  ## Galbraith, R.F., 1994. Some Applications of Radial Plots. Journal of the 
  ## American Statistical Association, 89 (428), 1232-1242.
  ##
  ## Galbraith, R.F., 2010. On plotting OSL equivalent doses. Ancient TL, 
  ## 28 (1), 1-10. 
  ##
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and 
  ## some recommendations. Quaternary Geochronology, 11, 1-27. 

  ##seealso<<
  ## \code{\link{plot}}, \code{\link{plot_KDE}}, \code{\link{plot_Histogram}}
  
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <- 
    Second2Gray(values = ExampleData.DeValues, dose_rate = c(0.0438,0.0019))
  
  ## plot the example data straightforward
  plot_RadialPlot(data = ExampleData.DeValues)
  
  ## now with linear z-scale
  plot_RadialPlot(data = ExampleData.DeValues,
                  log.z = FALSE)
  
  ## now with output of the plot parameters
  plot1 <- plot_RadialPlot(data = ExampleData.DeValues,
                           log.z = FALSE,
                           output = TRUE)
  plot1
  plot1$zlim
  
  ## now with adjusted z-scale limits
  plot_RadialPlot(data = ExampleData.DeValues,
                 log.z = FALSE,
                 zlim = c(100, 200))
  
  ## now the two plots with serious but seasonally changing fun
  #plot_RadialPlot(data = data.3, fun = TRUE)
  
  ## now with user-defined central value, in log-scale again
  plot_RadialPlot(data = ExampleData.DeValues,
                  central.value = 150)
  
  ## now with legend, colour, different points and smaller scale
  plot_RadialPlot(data = ExampleData.DeValues,
                  legend.text = "Sample 1",
                  col = "tomato4",
                  bar.col = "peachpuff",
                  pch = "R",
                  cex = 0.8)
  
  ## now without 2-sigma bar, y-axis, grid lines and central value line
  plot_RadialPlot(data = ExampleData.DeValues,
                  bar.col = "none",
                  grid.col = "none",
                  y.ticks = FALSE,
                  lwd = 0)
  
  ## now with user-defined axes labels
  plot_RadialPlot(data = ExampleData.DeValues,
                  xlab = c("Data error [%]",
                           "Data precision"),
                  ylab = "Scatter",
                  zlab = "Equivalent dose [Gy]")
  
  ## now with minimum, maximum and median value indicated
  plot_RadialPlot(data = ExampleData.DeValues,
                  central.value = 150,
                  stats = c("min", "max", "median"))
  
  ## now with a brief statistical summary
  plot_RadialPlot(data = ExampleData.DeValues,
                  summary = c("n", "in.ci"))
  
  ## now with another statistical summary as subheader
  plot_RadialPlot(data = ExampleData.DeValues,
                  summary = c("mean.weighted", "median"),
                  summary.pos = "sub")
  
  ## now the data set is split into sub-groups, one is manipulated
  data.1 <- ExampleData.DeValues[1:15,]
  data.2 <- ExampleData.DeValues[16:25,] * 1.3
  
  ## now a common dataset is created from the two subgroups
  data.3 <- list(data.1, data.2)
  
  ## now the two data sets are plotted in one plot
  plot_RadialPlot(data = data.3)
  
  ## now with some graphical modification
  plot_RadialPlot(data = data.3,
                  col = c("darkblue", "darkgreen"),
                  bar.col = c("lightblue", "lightgreen"),
                  pch = c(2, 6),
                  summary = c("n", "in.ci"),
                  summary.pos = "sub",
                  legend = c("Sample 1", "Sample 2"))
})

