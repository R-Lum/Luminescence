plot_DRTResults <- structure(function(# Visualise dose recovery test results
  ### The function provides a standardised plot output for dose recovery test
  ### measurements.
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany),
  ## Michael Dietze, GFZ Potsdam (Germany), \cr
  
  ## FIXED ISSUES
  ## - code added: ca. line 505, when only one boxplot is to be done, a label is added
  
  ##section<<
  ## version 0.1.2
  # ===========================================================================

  values, 
  ### \code{\linkS4class{RLum.Results}} or
  ### \code{\link{data.frame}}, (\bold{required}): input values 
  ### containing at least De and De error. To plot more than one data
  ### set in one figure, a \code{list} of the individual data sets
  ### must be provided (e.g. \code{list(dataset.1, dataset.2)}).
  
  given.dose,
  ### \code{\link{numeric}}: given dose from the dose recovery test 
  ### (in Seconds or Gray, unit has to be the same as from the input values).
  
  error.range = 10,
  ### \code{\link{numeric}}: symmetric error range in percent will be shown 
  ### as dashed lines in the plot. Set \code{error.range} to 0 to void  
  ### plotting of error ranges.
  
  preheat,
  ### \code{\link{numeric}}: optional vector of preheat temperatures to be
  ### used for grouping the De values. If specified, the temperatures are
  ### assigned to the x-axis.
  
  boxplot = FALSE,
  ### \code{\link{logical}}: optionally plot values, that are grouped by 
  ### preheat temperature as boxplots. Only possible when \code{preheat}  
  ### vector is specified.
  
  mtext,
  ### \code{\link{character}}: additional text below the plot title.
  
  summary,
  ### \code{\link{character}} (optional): adds numerical output to the plot. 
  ### Can be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De 
  ### value), \code{"mean.weighted"} (error-weighted mean), \code{"median"} (median of 
  ### the De values), \code{"sdrel"} (relative standard deviation in 
  ### percent), \code{"sdabs"} (absolute standard deviation), \code{"serel"} (relative 
  ### standard error) and \code{"seabs"} (absolute standard error).
  
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
  
  na.exclude  = FALSE, 
  ### \code{\link{logical}}: indicating wether \code{NA} values are removed 
  ### before plotting from the input data set
  
  ...
  ### further arguments and graphical parameters passed to \code{\link{plot}}.

  ){

  ## Validity checks ----------------------------------------------------------
  if(missing(given.dose)){stop("[plot_DRTResults: 'given.dose' missing!")}

  if(missing(summary) == TRUE) {summary <- NULL}
  if(missing(summary.pos) == TRUE) {summary.pos <- "topleft"}
  if(missing(legend.pos) == TRUE) {legend.pos <- "topright"}
  if(missing(mtext) == TRUE) {mtext <- ""}
  
  ## Homogenise and check input data
  if(is(values, "list") == FALSE) {values <- list(values)}
  
  for(i in 1:length(values)) {
    if(is(values[[i]], "RLum.Results")==FALSE & 
         is(values[[i]], "data.frame")==FALSE){
      stop(paste("[plot_DRTResults] Error: Wrong input data format",
                 "(!= 'data.frame' or 'RLum.Results')"))
    } else {
      if(is(values[[i]], "RLum.Results")==TRUE){
        values[[i]] <- get_RLum.Results(values[[i]])[,1:2]
      }
    }
  } 
  
  ## Check input arguments ----------------------------------------------------
  for(i in 1:length(values)) {
    if(na.exclude  == TRUE){
      values[[i]] <- na.exclude(values[[i]])
    }
    
    if(missing(preheat) == FALSE) {
      if(length(preheat) != nrow(values[[i]])){
        stop("[plot_DRTResults: number of preheat temperatures != De values!")
      }
    }
  }
  
  ## create global data set
  values.global <- NULL
  n.values <- NULL
  for(i in 1:length(values)) {
    values.global <- rbind(values.global, values[[i]])
    n.values <- c(n.values, nrow(values[[i]]))
  }

  ## Set plot format parameters -----------------------------------------------
  extraArgs <- list(...) # read out additional arguments list
  
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {"Dose recovery test"}
  
  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else {
    ifelse(missing(preheat) == TRUE, "# Aliquot", "Preheat temperature [\u00B0C]")
  }
  
  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylabs} else
  {expression(paste("Normalised ", D[e], sep=""))}
  
  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else
  {c(1, max(n.values) + 1)}
  
  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else
  {c(0.75, 1.25)}

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else {1}
  
  pch <- if("pch" %in% names(extraArgs)) {extraArgs$pch} else {
    abs(seq(from = 20, to = -100))
  }
  
  fun <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}

  ## calculations and settings-------------------------------------------------

  ## normalise data
  for(i in 1:length(values)) {
    values[[i]] <- values[[i]]/given.dose
  }
  
  ## optionally group data by preheat temperature
  if(missing(preheat) == FALSE) {
    modes <- as.numeric(rownames(as.matrix(table(preheat))))
    values.preheat <- list(NA)
    values.boxplot <- list(NA)
    for(i in 1:length(modes)) {
      for(j in 1:length(values)) {
        values.preheat[[length(values.preheat) + 1]] <- 
          cbind(values[[j]][preheat == modes[i],],
                preheat[preheat == modes[i]])
        values.boxplot[[length(values.boxplot) + 1]] <- 
          values[[j]][preheat == modes[i],1]
      }
      j <- 1
    }
    values.preheat[[1]] <- NULL
    values.boxplot[[1]] <- NULL
    modes.plot <- rep(modes, each = length(values))
  } else {modes <- 1}
  
  ## assign colour indices
  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else {
    if(missing(preheat) == TRUE) {
      rep(seq(from = 1, to = length(values)), each = length(modes))
    } else {
      rep(seq(from = 1, to = length(values)), length(modes))
    }
    
  }
  
  ## calculate and paste statistical summary
  label.text = list(NA)

  if(summary.pos[1] != "sub") {
    n.rows <- length(summary)
    
    for(i in 1:length(values)) {
      stops <- paste(rep("\n", (i - 1) * n.rows), collapse = "")
      label.text[[length(label.text) + 1]] <- paste(stops, paste(
        ifelse("n" %in% summary == TRUE,
               paste("n = ", 
                     nrow(values[[i]]), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("mean" %in% summary == TRUE,
               paste("mean = ", 
                     round(mean(values[[i]][,1]), 2), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("mean.weighted" %in% summary == TRUE,
               paste("weighted mean = ", 
                     round(weighted.mean(x = values[[i]][,1],
                                         w = 1 / values[[i]][,2]), 2), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("median" %in% summary == TRUE,
               paste("median = ", 
                     round(median(values[[i]][,1]), 2), 
                     "\n", 
                     sep = ""),
               ""),
        ifelse("sdrel" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(values[[i]][,1]) / mean(values[[i]][,1]) * 100,
                           2), " %",
                     "\n", 
                     sep = ""),
               ""),
        ifelse("sdabs" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(values[[i]][,1]), 2),
                     "\n", 
                     sep = ""),
               ""),
        sep = ""), stops, sep = "")
      
    }
  } else {
    for(i in 1:length(values)) {
      label.text[[length(label.text) + 1]]  <- paste(
        "| ",
        ifelse("n" %in% summary == TRUE,
               paste("n = ", 
                     nrow(values[[i]]), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("mean" %in% summary == TRUE,
               paste("mean = ", 
                     round(mean(values[[i]][,1]), 2), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("mean.weighted" %in% summary == TRUE,
               paste("weighted mean = ", 
                     round(weighted.mean(x = values[[i]][,1],
                                         w = 1 / values[[i]][,2]), 2), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("median" %in% summary == TRUE,
               paste("median = ", 
                     round(median(values[[i]][,1]), 2), 
                     " | ", 
                     sep = ""),
               ""),
        ifelse("sdrel" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(values[[i]][,1]) / mean(values[[i]][,1]) * 100,
                           2), " %",
                     " | ", 
                     sep = ""),
               ""),
        ifelse("sdabs" %in% summary == TRUE,
               paste("sd = ", 
                     round(sd(values[[i]][,1]), 2),
                     " | ", 
                     sep = ""),
               ""),
        sep = "")
    }
  }
  
  ## remove dummy list element
  label.text[[1]] <- NULL
  
  ## convert keywords into summary placement coordinates
  if(missing(summary.pos) == TRUE) {
    summary.pos <- c(xlim[1], ylim[2])
    summary.adj <- c(0, 1)
  } else if(length(summary.pos) == 2) {
    summary.pos <- summary.pos
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "topleft") {
    summary.pos <- c(xlim[1], ylim[2])
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "top") {
    summary.pos <- c(mean(xlim), ylim[2])
    summary.adj <- c(0.5, 1)
  } else if(summary.pos[1] == "topright") {
    summary.pos <- c(xlim[2], ylim[2])
    summary.adj <- c(1, 1)
  }  else if(summary.pos[1] == "left") {
    summary.pos <- c(xlim[1], mean(ylim))
    summary.adj <- c(0, 0.5)
  } else if(summary.pos[1] == "center") {
    summary.pos <- c(mean(xlim), mean(ylim))
    summary.adj <- c(0.5, 0.5)
  } else if(summary.pos[1] == "right") {
    summary.pos <- c(xlim[2], mean(ylim))
    summary.adj <- c(1, 0.5)
  }else if(summary.pos[1] == "bottomleft") {
    summary.pos <- c(xlim[1], ylim[1])
    summary.adj <- c(0, 0)
  } else if(summary.pos[1] == "bottom") {
    summary.pos <- c(mean(xlim), ylim[1])
    summary.adj <- c(0.5, 0)
  } else if(summary.pos[1] == "bottomright") {
    summary.pos <- c(xlim[2], ylim[1])
    summary.adj <- c(1, 0)
  }
  
  ## convert keywords into legend placement coordinates
  if(missing(legend.pos) == TRUE) {
    legend.pos <- c(xlim[2], ylim[2])
    legend.adj <- c(1, 1)
  } else if(length(legend.pos) == 2) {
    legend.pos <- legend.pos
    legend.adj <- c(0, 1)
  } else if(legend.pos[1] == "topleft") {
    legend.pos <- c(xlim[1], ylim[2])
    legend.adj <- c(0, 1)
  } else if(legend.pos[1] == "top") {
    legend.pos <- c(mean(xlim), ylim[2])
    legend.adj <- c(0.5, 1)
  } else if(legend.pos[1] == "topright") {
    legend.pos <- c(xlim[2], ylim[2])
    legend.adj <- c(1, 1)
  } else if(legend.pos[1] == "left") {
    legend.pos <- c(xlim[1], mean(ylim))
    legend.adj <- c(0, 0.5)
  } else if(legend.pos[1] == "center") {
    legend.pos <- c(mean(xlim), mean(ylim))
    legend.adj <- c(0.5, 0.5)
  } else if(legend.pos[1] == "right") {
    legend.pos <- c(xlim[2], mean(ylim))
    legend.adj <- c(1, 0.5)
  } else if(legend.pos[1] == "bottomleft") {
    legend.pos <- c(xlim[1], ylim[1])
    legend.adj <- c(0, 0)
  } else if(legend.pos[1] == "bottom") {
    legend.pos <- c(mean(xlim), ylim[1])
    legend.adj <- c(0.5, 0)
  } else if(legend.pos[1] == "bottomright") {
    legend.pos <- c(xlim[2], ylim[1])
    legend.adj <- c(1, 0)
  }
  
  ## Plot output --------------------------------------------------------------

  ## determine number of subheader lines to shif the plot
  shift.lines <- if(summary.pos[1] == "sub" & mtext == "") {
    length(label.text) - 1
  } else {1}
  
  ## setup plot area
  
  par(mfrow = c(1, 1), cex = cex, oma = c(0, 1, shift.lines - 1, 1))
  
  ## optionally plot values and error bars
  if(boxplot == FALSE) {
    ## plot data and error
    if(missing(preheat) == TRUE) {
      ## create empty plot
      plot(NA,NA,
           xlim = xlim,
           ylim = ylim,
           xlab = xlab,
           ylab = ylab,
           main = "")
      
      ## add title
      title(main = main,
            line = shift.lines + 2)
      
      ## add additional lines
      abline(h = 1)
      
      if(error.range > 0){
        ## error range lines
        abline(h = 1 * (1 + error.range / 100), lty = 2)
        abline(h = 1 * (1 - error.range / 100), lty = 2)
        
        ## error range labels
        text(par()$usr[2], (1 + error.range / 100) + 0.02,
             paste("+", error.range ," %", sep = ""), pos = 2, cex = 0.8)
        text(par()$usr[2], (1 - error.range / 100) - 0.02,
             paste("-", error.range ,"%", sep = ""), pos = 2, cex = 0.8)
      }
      
      ## add data and error bars
      for(i in 1:length(values)) {
        points(x = c(1:nrow(values[[i]])),
               y = values[[i]][,1],
               pch = pch[i],
               col = col[i],
               cex = 1.2 * cex)
        
        arrows(c(1:nrow(values[[i]])),
               values[[i]][,1] + values[[i]][,2],
               c(1:nrow(values[[i]])),
               values[[i]][,1] - values[[i]][,2],
               angle = 90,
               length = 0.075,
               code = 3,
               col = col[i])
        
        ## add summary content
        if(summary.pos[1] != "sub") {
          text(x = summary.pos[1],
               y = summary.pos[2],
               adj = summary.adj,
               labels = label.text[[i]],
               cex = 0.8 * cex,
               col = col[i])
        } else {
          if(mtext == "") {
            mtext(side = 3, 
                  line = - i + 2.5, 
                  text = label.text[[i]],
                  col = col[i],
                  cex = cex * 0.8)
          }
        }  
      } 
    } else {  
      
    ## option for provided preheat data
      ## create empty plot
      plot(NA,NA,
           xlim = c(min(modes.plot) * 0.9, max(modes.plot) * 1.1),
           ylim = ylim,
           xlab = xlab,
           ylab = ylab,
           main = "",
           axes = FALSE,
           frame.plot = TRUE)
      
      ## add axes
      axis(1, 
           at = modes.plot, 
           labels = modes.plot)
      axis(2)
      
      ## add title
      title(main = main,
            line = shift.lines + 2)
      
      ## add additional lines
      abline(h = 1)
      
      if(error.range > 0){
        ## error range lines
        abline(h = 1 * (1 + error.range / 100), lty = 2)
        abline(h = 1 * (1 - error.range / 100), lty = 2)
        
        ## error range labels
        text(par()$usr[2], (1 + error.range / 100) + 0.02,
             paste("+", error.range ," %", sep = ""), pos = 2, cex = 0.8)
        text(par()$usr[2], (1 - error.range / 100) - 0.02,
             paste("-", error.range ,"%", sep = ""), pos = 2, cex = 0.8)
      }
      
      ## plot values
      for(i in 1:length(values.preheat)) {
        points(x = values.preheat[[i]][,3],
               y = values.preheat[[i]][,1],
               pch = pch[i],
               col = col[i],
               cex = 1.2 * cex)
        
        arrows(values.preheat[[i]][,3],
               values.preheat[[i]][,1] + values.preheat[[i]][,2],
               values.preheat[[i]][,3],
               values.preheat[[i]][,1] - values.preheat[[i]][,2],
               angle = 90,
               length = 0.075,
               code = 3,
               col = col[i])
      }
    }
  }
  
  ## optionally, plot boxplot
  if(boxplot == TRUE) {
    ## create empty plot
    boxplot(values.boxplot,
            names = modes.plot,
            ylim = ylim,
            xlab = xlab,
            ylab = ylab,
            main = "",
            border = col)
    
    ## add axis label, if necessary
    if(length(modes.plot) == 1) {
      axis(side = 1, at = 1, labels = modes.plot)
    }

    ## add title
    title(main = main,
          line = shift.lines + 2)
    
    ## add additional lines
    abline(h = 1)
    
    if(error.range > 0){
      ## error range lines
      abline(h = 1 * (1 + error.range / 100), lty = 2)
      abline(h = 1 * (1 - error.range / 100), lty = 2)
      
      ## error range labels
      text(par()$usr[2], (1 + error.range / 100) + 0.02,
           paste("+", error.range ," %", sep = ""), pos = 2, cex = 0.8)
      text(par()$usr[2], (1 - error.range / 100) - 0.02,
           paste("-", error.range ,"%", sep = ""), pos = 2, cex = 0.8)
    }
    
    ## plot data and error
    for(i in 1:length(values)) {
      ## add summary content
      if(summary.pos[1] != "sub") {
        text(x = summary.pos[1],
             y = summary.pos[2],
             adj = summary.adj,
             labels = label.text[[i]],
             cex = 0.8 * cex,
             col = col[i])
      } else {
        if(mtext == "") {
          mtext(side = 3, 
                line = - i + 2.5, 
                text = label.text[[i]],
                col = col[i],
                cex = cex * 0.8)
        }
      }
    }
  }
  
  ## optionally add legend content
  if(missing(legend) == FALSE) {
    legend(x = legend.pos[1],
           y = legend.pos[2],
           xjust = legend.adj[1],
           yjust = legend.adj[2],
           legend = legend,
           col = col,
           pch = pch,
           lty = 1,
           cex = cex * 0.8)
  }
  
  ## optionally add subheader text
  mtext(side = 3, 
        line = shift.lines,
        text = mtext, 
        cex = 0.8 * cex)
  
  ##FUN by R Luminescence Team
  if(fun == TRUE) {sTeve()}
  
  ##details<<
  ## Procedure to test the accuracy of a measurement protocol to reliably 
  ## determine the dose of a specific sample. Here, the natural signal is 
  ## erased and a known laboratory dose administered which is treated as 
  ## unknown. Then the De measurement is carried out and the degree of 
  ## congruence between administered and recovered dose is a measure of the 
  ## protocol's accuracy for this sample.\cr
  ## In the plot the normalised De is shown on the y-axis, i.e. obtained 
  ## De/Given Dose.
  
  ##value<<
  ## A plot is returned.
  
  ##seealso<<
  ## \code{\link{plot}}
  
  ##references<<
  ## Wintle, A.G., Murray, A.S., 2006. A review of quartz optically stimulated 
  ## luminescence characteristics and their relevance in single-aliquot 
  ## regeneration dating protocols. Radiation Measurements, 41, 369-391.
  
  ##note<<
  ## Further data and plot arguments can be added by using the appropiate
  ## R commands.
  
  ##keyword<<
  ## dplot
  
  
}, ex=function(){
  
  ## read example data set and misapply them for this plot type
  data(ExampleData.DeValues, envir = environment())
  
  ## plot values 
  plot_DRTResults(values = ExampleData.DeValues[7:11,], 
  given.dose = 2800, mtext = "Example data")
  
  ## plot values with legend
  plot_DRTResults(values = ExampleData.DeValues[7:11,], 
                  given.dose = 2800,
                  legend = "Test data set")
  
  ## create and plot two subsets with randomised values
  x.1 <- ExampleData.DeValues[7:11,]
  x.2 <- ExampleData.DeValues[7:11,] * c(runif(5, 0.9, 1.1), 1)
  
  plot_DRTResults(values = list(x.1, x.2),
                  given.dose = 2800)
  
  ## some more user-defined plot parameters
  plot_DRTResults(values = list(x.1, x.2),
                  given.dose = 2800,
                  pch = c(2, 5),
                  col = c("orange", "blue"),
                  xlim = c(0, 8),
                  ylim = c(0.85, 1.15),
                  xlab = "Sample aliquot")

  ## plot the data with user-defined statistical measures as legend
  plot_DRTResults(values = list(x.1, x.2),
                  given.dose = 2800,
                  summary = c("n", "mean.weighted", "sd"))

  ## plot the data with user-defined statistical measures as sub-header
  plot_DRTResults(values = list(x.1, x.2),
                  given.dose = 2800,
                  summary = c("n", "mean.weighted", "sd"),
                  summary.pos = "sub")
  
  ## plot the data grouped by preheat temperatures
  plot_DRTResults(values = ExampleData.DeValues[7:11,], 
                  given.dose = 2800,
                  preheat = c(200, 200, 200, 240, 240))
  
  ## plot two data sets grouped by preheat temperatures
  plot_DRTResults(values = list(x.1, x.2), 
                  given.dose = 2800,
                  preheat = c(200, 200, 200, 240, 240))

  ## plot the data grouped by preheat temperatures as boxplots
  plot_DRTResults(values = ExampleData.DeValues[7:11,], 
                  given.dose = 2800,
                  preheat = c(200, 200, 200, 240, 240),
                  boxplot = TRUE)
})
