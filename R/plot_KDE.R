plot_KDE <- structure(function( # Plot kernel density estimate with statistics
  ### Plot a kernel density estimate of measurement values in combination with 
  ### the actual values and associated error bars in ascending order. 
  ### Optionally, statistical measures such as mean, median, standard 
  ### deviation, standard error and quartile range can be provided visually 
  ### and numerically.
                      
  # ===========================================================================
  ##author<<
  ## Michael Dietze (GFZ Potsdam),
  ## Sebastian Kreutzer, JLU Giessen (Germany), \cr
  
  ##section<<
  ##version 3.2
  # ===========================================================================

  ## TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
  ## - allow shading density and angle
  ## - get colour parameters meaningful
  ## - maybe add line option as in plot_AbanicoPlot, add keywords to line option (e.g. "mean")
  
  data, 
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
  ### (required): for \code{data.frame}: two columns: De (\code{values[,1]})
  ### and De error (\code{values[,2]}). For plotting multiple data sets, these
  ### must be provided as \code{list} (e.g. \code{list(dataset1, dataset2)}).

  na.exclude = TRUE,
  ### \code{\link{logical}} (with default): exclude NA values from the data
  ### set prior to any further operations.
  
  weights = FALSE,
  ### \code{\link{logical}} (with default): calculate the KDE with De-errors
  ### as weights. Attention, using errors as weights will result in a plot
  ### similar to a a probability density plot, with all ambiguities related
  ### to this plot type!
  
  values.cumulative = TRUE,
  ### \code{\link{logical}} (with default): show cumulative individual data.
  
  centrality,
  ### \code{\link{character}}: measure(s) of centrality, used for
  ### plotting vertical lines of the respective measure. Can be
  ### one out of \code{"mean"}, \code{"median"}, \code{"mean.weighted"}, 
  ### \code{"median.weighted"} and \code{"kdemax"}.
  
  dispersion,
  ### \code{\link{character}}: measure of dispersion, used for
  ### drawing the polygon that depicts the dose distribution. One out of
  ### \code{"sd"} (standard deviation),\code{"2sd"} (2 standard deviations) 
  ### \code{"qr"} (quartile range).
  
  stats,
  ### \code{\link{character}} (optional): add numerical output to the plot. 
  ### Can be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De 
  ### value), \code{"mean.weighted"} (error-weighted mean), \code{"median.weighted"}
  ### (error-weighted median), \code{"median"} (median of the De values), \code{"kdemax"} 
  ### (maximum value of probability density function), \code{"kurtosis"} (kurtosis), 
  ### \code{"skewness"} (skewness), \code{"sdrel"} (relative standard deviation in 
  ### percent), \code{"sdabs"} (absolute standard deviation), \code{"serel"} (relative 
  ### standard error) and \code{"seabs"} (absolute standard error).
  
  stats.pos = "sub",
  ### \code{\link{numeric}} or \code{\link{character}} (with default): optional  
  ### position coordinates or keyword for the statistical summary. Y-coordinate  
  ### refers to the left y-axis.
  
  polygon.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the polygon showing the dose dispersion around the central value.
  ### Only relevant if \code{dispersion} is specified.
  
  order = TRUE,
  ### \code{\link{logical}}: Order data in ascending order.
  
  bw = "nrd0",
  ### \code{\link{character}} (with default): bin-width, chose a numeric 
  ### value for manual setting.
  
  output = FALSE,
  ### \code{\link{logical}}: Optional output of numerical plot parameters.
  ### These can be useful to reproduce similar plots. Default is \code{FALSE}.
  
  ...
  ### further arguments and graphical parameters passed to \code{\link{plot}}.
) {
  
  ## check data and parameter consistency -------------------------------------
  
  ## Homogenise input data format
  if(is(data, "list") == FALSE) {data <- list(data)}
  
  ## check/adjust input data structure
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
  
  ## check/set function parameters
  if(missing(stats) == TRUE) {stats <- numeric(0)}
  if(missing(polygon.col) == TRUE) {polygon.col <- rep("grey80", length(data))}
  if(missing(centrality) == TRUE) {centrality <- character(0)}
  if(missing(dispersion) == TRUE) {dispersion <- ""}
  
  ## data preparation steps ---------------------------------------------------

  ## Optionally, count and exclude NA values and print result
  if(na.exclude == TRUE) {
    for(i in 1:length(data)) {
      n.NA <- sum(!complete.cases(data[[i]]))
      if(n.NA == 1) {print("1 NA value excluded.")
      } else if(n.NA > 1) {print(paste(n.NA, "NA values excluded."))}
      data[[i]] <- na.exclude(data[[i]])
    }
  }
  
  ## optionally, order data set
  if(order == TRUE) {
    for(i in 1:length(data)) {
      data[[i]] <- data[[i]][order(data[[i]][,1]),]
    }
  }
  
  ## create output variables
  De.stats <- matrix(nrow = length(data), ncol = 14)
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
                          "kurtosis")
  De.density <- list(NA)
  
  ## loop through all data sets
  for(i in 1:length(data)) {
    statistics <- calc_Statistics(data[[i]])
    De.stats[i,1] <- statistics$weighted$n
    De.stats[i,2] <- statistics$unweighted$mean
    De.stats[i,3] <- statistics$weighted$mean
    De.stats[i,4] <- statistics$unweighted$median
    De.stats[i,5] <- statistics$weighted$median
    De.stats[i,7] <- statistics$weighted$sd.abs
    De.stats[i,8] <- statistics$weighted$sd.rel
    De.stats[i,9] <- statistics$weighted$se.abs
    De.stats[i,10] <- statistics$weighted$se.rel
    De.stats[i,11] <- quantile(data[[i]][,1], 0.25)
    De.stats[i,12] <- quantile(data[[i]][,1], 0.75)
    De.stats[i,13] <- statistics$unweighted$skewness
    De.stats[i,14] <- statistics$unweighted$kurtosis
    
    De.density[[length(De.density) + 1]] <- if(weights == TRUE) {
      density(data[[i]][,1], 
              kernel = "gaussian", 
              bw = bw, 
              weights = data[[i]][,2] / sum(data[[i]][,2]))
    } else {
      density(data[[i]][,1], 
              kernel = "gaussian", 
              bw = bw)
    }
  }
  
  ## remove dummy list element
  De.density[[1]] <- NULL
  
  ## create global data set
  De.global <- data[[1]][,1]
  De.error.global <- data[[1]][,2]
  De.density.range <- c(1, 0)
  for(i in 1:length(data)) {
    ##global De and De.error vector
    De.global <- c(De.global, data[[i]][,1])
    De.error.global <- c(De.error.global, data[[i]][,2])
    
    ## y-axis range
    De.density.range[1] <- ifelse(min(De.density[[i]]$y) < 
                                    De.density.range[1],
                                  min(De.density[[i]]$y),
                                  De.density.range[1])
    De.density.range[2] <- ifelse(max(De.density[[i]]$y) > 
                                    De.density.range[1],
                                  max(De.density[[i]]$y),
                                  De.density.range[1])
    
    ## position of maximum KDE value
    De.stats[i,6] <- De.density[[i]]$x[
    De.density[[i]]$y == (max(De.density[[i]]$y))]
  }
  
  ## create stats expression for sub-header output
  if(stats.pos[1] == "sub" & length(stats) >= 1) {
    stats.sub <- rep("|", length(data))
    
    for(i in 1:length(stats)) {
      stats.expression <- if(stats[i] == "n") {
        paste(" n = ", round(De.stats[,1], 2), sep = "")
      } else if(stats[i] == "mean") {
        paste(" mean = ", round(De.stats[,2], 2), sep = "")
      } else if(stats[i] == "mean.weighted") {
        paste(" weighted mean = ", round(De.stats[,3], 2), sep = "")
      } else if(stats[i] == "median") {
        paste(" median = ", round(De.stats[,4], 2), sep = "")
      } else if(stats[i] == "median.weighted") {
        paste(" weighted median = ", round(De.stats[,5], 2), sep = "")
      } else if(stats[i] == "kdemax") {
        paste(" KDE max = ", round(De.stats[,6], 2), sep = "")
      } else if(stats[i] == "sdrel") {
        paste(" rel. sd = ", round(De.stats[,8], 2), sep = "")
      } else if(stats[i] == "serel") {
        paste(" rel. se = ", round(De.stats[,10], 2), sep = "")
      } else if(stats[i] == "sdabs") {
        paste(" abs. sd = ", round(De.stats[,7], 2), sep = "")
      } else if(stats[i] == "seabs") {
        paste(" abs. se = ", round(De.stats[,9], 2), sep = "")
      } else if(stats[i] == "qr") {
        paste(" quartile range = ", round(De.stats[,11], 2), " - ",
          round(De.stats[,12], 2), sep = "")
      } else if(stats[i] == "skewness") {
        paste(" skewness = ", round(De.stats[,13], 2), sep = "")
      } else if(stats[i] == "kurtosis") {
        paste(" kurtosis = ", round(De.stats[,14], 2), sep = "")
      }
      
      stats.sub <- paste(stats.sub, 
                         stats.expression,
                         " |", sep = "")
    }
  }
  
  ## create stats-list for legend output
  if(stats.pos[1] != "sub" & length(stats) >= 1) {
    stats.legend <- rep("", length(data))
    
    for(i in 1:length(stats)) {
      stats.expression <- if(stats[i] == "n") {
        paste("n = ", round(De.stats[,1], 2), "\n", sep = "")
      } else if(stats[i] == "mean") {
        paste("mean = ", round(De.stats[,2], 2), "\n", sep = "")
      } else if(stats[i] == "mean.weighted") {
        paste("weighted mean = ", round(De.stats[,3], 2), "\n", sep = "")
      } else if(stats[i] == "median") {
        paste("median = ", round(De.stats[,4], 2), "\n", sep = "")
      } else if(stats[i] == "median.weighted") {
        paste("weighted median = ", round(De.stats[,5], 2), "\n", sep = "")
      } else if(stats[i] == "kdemax") {
        paste("KDE max = ", round(De.stats[,6], 2), "\n", sep = "")
      } else if(stats[i] == "sdrel") {
        paste("rel. sd = ", round(De.stats[,8], 2), "\n", sep = "")
      } else if(stats[i] == "serel") {
        paste("rel. se = ", round(De.stats[,10], 2), "\n", sep = "")
      } else if(stats[i] == "sdabs") {
        paste("abs. sd = ", round(De.stats[,7], 2), "\n", sep = "")
      } else if(stats[i] == "seabs") {
        paste("abs. se = ", round(De.stats[,9], 2), "\n", sep = "")
      } else if(stats[i] == "qr") {
        paste("quartile range = ", round(De.stats[,11], 2), " - ",
              round(De.stats[,12], 2), "\n", sep = "")
      } else if(stats[i] == "skewness") {
        paste("skewness = ", round(De.stats[,13], 2), "\n", sep = "")
      } else if(stats[i] == "kurtosis") {
        paste("kurtosis = ", round(De.stats[,14], 2), "\n", sep = "")
      }
      
      stats.legend <- paste(stats.legend, 
                            stats.expression,
                            sep = "")
    }
  }  
  
  ## read out additional parameters -------------------------------------------
  if("main" %in% names(list(...))) {
    main <- list(...)$main
  } else {
    main <- expression(bold(paste(D[e], " distribution")))
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
    xlim <- list(...)$xlim
  } else {
    xlim <- c(min(De.global - De.error.global), 
              max(De.global + De.error.global))
  }
  
  if("ylim" %in% names(list(...))) {
      ylim <- list(...)$ylim
    } else {
      ylim <- c(De.density.range[1],
                De.density.range[2],
                1, 
                max(De.stats[,1]))
    }

  if("log" %in% names(list(...))) {
    log.option <- list(...)$log
  } else {
    log.option <- ""
  }

  if(length(data) > 1) {
    if("col" %in% names(list(...))) {  
      colours <- matrix(rep(list(...)$col, each = 4), 
                        nrow = length(data), 
                        byrow = TRUE)
    } else {
      colours <- matrix(rep(1:length(data), 4), 
                        nrow = length(data))
    }
  } else {
    if("col" %in% names(list(...))) {
      colours <- matrix(c(list(...)$col), 
                 nrow = 1)
    } else {
      colours <- matrix(c("#3F489D", 
                          "black", 
                          "black", 
                          "grey90"), 
                        nrow = 1)
    }
  }
  
  if("lty" %in% names(list(...))) {
    lty <- list(...)$lty
  } else {
    lty <- seq(2, 7 * length(data))
  }
  
  if("cex" %in% names(list(...))) {
    cex <- list(...)$cex
  } else {
    cex <- 1
  }

  if("fun" %in% names(list(...))) {
    fun <- list(...)$fun
  } else {
    fun <- FALSE
  }

  ## assign polygon coordinates
  polygons <- matrix(nrow = length(data), ncol = 8)
  
  for(i in 1:length(data)) {
    lims.x <- if(dispersion == "sd") {
      c(De.stats[i,3] - De.stats[i,7],
        De.stats[i,3] - De.stats[i,7],
        De.stats[i,3] + De.stats[i,7],
        De.stats[i,3] + De.stats[i,7])
    } else if(dispersion == "2sd") {
      c(De.stats[i,3] - 2 * De.stats[i,7],
        De.stats[i,3] - 2 * De.stats[i,7],
        De.stats[i,3] + 2 * De.stats[i,7],
        De.stats[i,3] + 2 * De.stats[i,7])
    } else if(dispersion == "qr") {
      c(De.stats[i,11],
        De.stats[i,11],
        De.stats[i,12],
        De.stats[i,12])
    } else {
      rep(NA, 4)
    }
    
    polygons[i,] <- c(lims.x, c(-2 * ylim[2], 
                                2 * ylim[2],
                                2 * ylim[2],
                                -2 * ylim[2]))
  }
  
  ## plot data sets -----------------------------------------------------------

  ## setup plot area
  if(length(stats) >= 1 & stats.pos[1] == "sub") {
    toplines <- length(data)
  } else {toplines <- 1}
  
  par(oma = c(0, 0, 0, 2),
      mar = c(5, 5, 2.5 + toplines, 3),
      xpd = FALSE,
      cex = cex)
  
  ## create empty plot to set plot dimensions
  plot(NA, 
       xlim = xlim,
       ylim = ylim[1:2],
       main = "",
       xlab = "",
       ylab = "",
       log = log.option,
       axes = FALSE,
       frame.plot = FALSE)
     
  ## plot dispersion polygons
  if(length(dispersion) == 1) {
    for(i in 1:length(data)) {
      polygon(x = polygons[i,1:4],
              y = polygons[i,5:8],
              col = polygon.col[i],
              border = FALSE)
    }
  }

  ## plot measures of centrality
  if(length(centrality) >= 1) {
    for(i in 1:length(data)) {
      for(j in 1:length(centrality)) {
        if(centrality[j] == "mean") {
          abline(v = De.stats[i,2], col = colours[i,1], lty = lty[j + 1])
          text(De.stats[i,2] - par()$cxy[1] * 0.5, 
               ylim[2] * 0.99, "mean",
               srt = 90, adj = 1, col = colours[i, 2], cex = 0.8 * cex)
        } else if(centrality[j] == "mean.weighted") {
          abline(v = De.stats[i,3], col = colours[i,1], lty = lty[j + 1])
          text(De.stats[i,3] - par()$cxy[1] * 0.5, 
               ylim[2] * 0.99, "weighted mean",
               srt = 90, adj = 1, col = colours[i, 2], cex = 0.8 * cex)
        } else if(centrality[j] == "median") {
          abline(v = De.stats[i,4], col = colours[i,1], lty = lty[j + 1])
          text(De.stats[i,4] - par()$cxy[1] * 0.5, 
               ylim[2] * 0.99, "median",
               srt = 90, adj = 1, col = colours[i, 2], cex = 0.8 * cex)
        } else if(centrality[j] == "median.weighted") {
          abline(v = De.stats[i,5], col = colours[i,1], lty = lty[j + 1])
          text(De.stats[i,5] - par()$cxy[1] * 0.5, 
               ylim[2] * 0.99, "weighted median",
               srt = 90, adj = 1, col = colours[i, 2], cex = 0.8 * cex)
        } else if(centrality[j] == "kdemax") {
          abline(v = De.stats[i,6], col = colours[i,1], lty = lty[j + 1])
          text(De.stats[i,6] - par()$cxy[1] * 0.5, 
               ylim[2] * 0.99, "KDE max",
               srt = 90, adj = 1, col = colours[i, 2], cex = 0.8 * cex)
        }
      }
      j <- 1
    }
  }
  
  ## add probability density plot
  par(new = TRUE)
  plot(NA,
       main     = "", 
       xlab     = xlab, 
       ylab     = ylab[1],
       xlim     = xlim,
       ylim     = ylim[1:2],
       log      = log.option,
       cex      = cex,
       cex.lab  = cex,
       cex.main = cex,
       cex.axis = cex)
  
  for(i in 1:length(data)) {
    lines(x = De.density[[i]]$x, 
          y = De.density[[i]]$y, 
          col = colours[i, 1])
  }
  
  ## add plot title
  title(main, line = toplines + 1.2)
  
  ## add stats-expressions for sub-header
  if(stats.pos[1] == "sub" & length(stats) >= 1) {
    for(i in 1:length(stats.sub)) {
      mtext(line = toplines + 0.5 - i,
            text = stats.sub[i],
            col = colours[i,1],
            cex = 0.9 * cex)
    }
  }
  
  ## convert placement keywords to coordinates
  if(stats.pos[1] != "sub" &length(stats) >= 1) {
    if(missing(stats.pos) == TRUE) {
      stats.pos <- c(xlim[1], ylim[2])
      stats.adj <- c(0, 1)
    } else if(length(stats.pos) == 2) {
      stats.pos <- stats.pos
      stats.adj <- c(0, 1)
    } else if(stats.pos[1] == "topleft") {
      stats.pos <- c(xlim[1], ylim[2])
      stats.adj <- c(0, 1)
    } else if(stats.pos[1] == "top") {
      stats.pos <- c(mean(xlim), ylim[2])
      stats.adj <- c(0.5, 1)
    } else if(stats.pos[1] == "topright") {
      stats.pos <- c(xlim[2], ylim[2])
      stats.adj <- c(1, 1)
    }  else if(stats.pos[1] == "left") {
      stats.pos <- c(xlim[1], mean(ylim[1:2]))
      stats.adj <- c(0, 0.5)
    } else if(stats.pos[1] == "center") {
      stats.pos <- c(mean(xlim), mean(ylim[1:2]))
      stats.adj <- c(0.5, 0.5)
    } else if(stats.pos[1] == "right") {
      stats.pos <- c(xlim[2], mean(ylim[1:2]))
      stats.adj <- c(1, 0.5)
    }else if(stats.pos[1] == "bottomleft") {
      stats.pos <- c(xlim[1], ylim[1])
      stats.adj <- c(0, 0)
    } else if(stats.pos[1] == "bottom") {
      stats.pos <- c(mean(xlim), ylim[1])
      stats.adj <- c(0.5, 0)
    } else if(stats.pos[1] == "bottomright") {
      stats.pos <- c(xlim[2], ylim[1])
      stats.adj <- c(1, 0)
    }
    
    ## generate sequence of empty lines
    empty.lines <- ""
    for(i in 1:((length(data) -1) * length(stats))) {
      empty.lines[i + 1] <- paste(empty.lines[i], "\n", sep = "")
    }
    empty.lines <- empty.lines[seq(from = 0, 
                                   to = length(empty.lines), 
                                   by = length(stats)) + 1]
    
    ## add empty lines to stats content
    for(i in 1:length(data)) {
      ## concatenate stats legend expressions (i.e. empty lines)
      stats.legend[i] <- paste(empty.lines[i], stats.legend[i], sep = "")
      
      ## add stats content to plot
      text(x = stats.pos[1],
           y = stats.pos[2],
           adj = stats.adj,
           labels = stats.legend[i],
           cex = 0.8 * cex,
           col = colours[i])
    }
  }
  
  if(values.cumulative == TRUE) {
    ## create empty overlay plot
    par(new = TRUE) # adjust plot options
    plot(NA, # add empty plot, scaled to secondary plot content
         xlim = xlim,
         ylim = ylim[3:4],
         log  = log.option,
         main = "",
         xlab = "",
         ylab = "",
         axes = FALSE,
         frame.plot = FALSE)
    
    ## add secondary y-axis
    axis(side = 4, labels = TRUE, cex.axis = cex) # add second y-axis
    mtext(ylab[2], side = 4, line = 3, cex = cex) # add second y-axis label
    
    ## add De error bars
    for(i in 1:length(data)) {
      arrows(data[[i]][,1] - data[[i]][,2]/2,
             1:length(data[[i]][,1]), 
             data[[i]][,1] + data[[i]][,2]/2, 
             1:length(data[[i]][,1]), 
             code = 3,
             angle = 90,
             length = 0.05,
             col = colours[i, 3])
      
      ## add De measurements
      points(data[[i]][,1], 1:De.stats[i,1],
             col = colours[i, 3], 
             pch = 20)
    }
  }
  
  ## add empty plot
  par(new = TRUE)
  plot(NA,
       ann = FALSE,
       axes = FALSE,
       xlim     = xlim,
       ylim     = ylim[1:2],
       log      = log.option,
       cex      = cex,
       cex.lab  = cex,
       cex.main = cex,
       cex.axis = cex)

  ## FUN by R Luminescence Team
  if(fun==TRUE){sTeve()}
  
  if(output == TRUE) {
    return(list(De.stats = De.stats,
                stats.pos = stats.pos,
                De.density = De.density))
  }
    
  
  ##details<<
  ## The function allow passing several plot arguments, such as \code{main}, 
  ## \code{xlab}, \code{cex}. However, as the figure is an overlay of two 
  ## separate plots, \code{ylim} must be specified in the order: c(ymin_axis1, 
  ## ymax_axis1, ymin_axis2, ymax_axis2) when using the cumulative values plot
  ## option. Similarly, if other than the default 
  ## colours are desired, the argument col must be provided with colours in 
  ## the following order: probability density function, De values, De error 
  ## bars, sd or qr polygon. The line type (\code{lty}) for additional 
  ## measures of centrality will cycle through the default values (1, 2, ...)
  ## by default, i.e. KDE line solid, further vertical lines dashed, dotted, 
  ## dash-dotted and so on. To change this behaviour specify the desired
  ## order of line types (e.g. \code{lty = c(1, 3, 2, 5)}).
  ## See examples for some further explanations. For 
  ## details on the calculation of the bin-width (parameter \code{bw}) see 
  ## \code{\link{density}}.
  
  ##seealso<<
  ## \code{\link{density}}, \code{\link{plot}}
  
  ##referencs<<
  ## Berger, G.W., 2010. An alternate form of probability-distribution plot 
  ## for De values. Ancient TL 28, pp. 11-21. \cr
  ## Berger, G.W., 2011. Response to Galbraith. Ancient TL 29, pp. 48-50. \cr
  ## Galbraith, R.F., 2011. Some comments arising from Berger (2010). Ancient 
  ## TL 29, pp. 41-47. \cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and 
  ## some recommendations. Quaternary Geochronology, 11, pp.1-27.
  
  ##note<<
  ## The plot output is no 'PD' plot (cf. the discussion of Berger and 
  ## Galbraith in Ancient TL; see references)!

}, ex=function(){
  ## read example data set
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <- 
    Second2Gray(values = ExampleData.DeValues, dose_rate = c(0.0438,0.0019))
  
  ## create plot straightforward
  plot_KDE(data = ExampleData.DeValues)
  
  ## create plot with logarithmic x-axis
  plot_KDE(data = ExampleData.DeValues, 
           log = "x")
  
  ## create plot with user-defined labels and axes limits
  plot_KDE(data = ExampleData.DeValues,
           main = "Dose distribution",
           xlab = "Dose [s]",
           ylab = c("KDE estimate", "Cumulative dose value"),
           xlim = c(100, 250),
           ylim = c(0, 0.08, 0, 30))
  
  ## create plot with centrality lines and distribution polygons
  plot_KDE(data = ExampleData.DeValues,
           ylim = c(0, 0.08, 0, 35),
           centrality = c("median", "mean"),
           dispersion = "sd",
           polygon.col = "lightblue")
  
  ## create plot with statistical summary below header
  plot_KDE(data = ExampleData.DeValues,
           stats = c("n", "median", "skewness", "qr"))

  ## create plot with statistical summary as legend
  plot_KDE(data = ExampleData.DeValues,
           stats = c("n", "mean", "sdrel", "seabs"),
           stats.pos = "topleft")

  ## split data set into sub-groups, one is manipulated, and merge again
  data.1 <- ExampleData.DeValues[1:15,]
  data.2 <- ExampleData.DeValues[16:25,] * 1.3
  data.3 <- list(data.1, data.2)
  
  ## create plot with two subsets straightforward
  plot_KDE(data = data.3)
  
  ## create plot with two subsets and summary legend at user coordinates
  plot_KDE(data = data.3,
           stats = c("n", "median", "skewness"),
           stats.pos = c(110, 0.07),
           col = c("blue", "orange"))
  
  ## example of how to use the numerical output of the function
  ## return plot output to draw a thicker KDE line
  KDE <- plot_KDE(data = ExampleData.DeValues,
                  output = TRUE)
  
  ## read out coordinates of KDE graph
  KDE.x <- KDE$De.density[[1]]$x
  KDE.y <- KDE$De.density[[1]]$y
  
  ## transform y-values to right y-axis dimensions
  KDE.y <- KDE.y / max(KDE.y) * (nrow(ExampleData.DeValues) - 1) + 1
  
  ## draw the KDE line
  lines(x = KDE.x,
        y = KDE.y,
        lwd = 3)
})