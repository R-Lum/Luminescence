plot_Histogram <- structure(function(# Plot a histogram with a separate error plot
  ### Function plots a predefined histogram with an accompanying error plot as suggested by 
  ### Rex Galbraith at the UK LED in Oxford 2010.

  # ===========================================================================
  ##author<<
  ## Michael Dietze (GFZ Potsdam), \cr
  ## Sebastian Kreutzer, JLU Giessen (Germany), \cr
  
  ##section<<
  ##version 0.4.1
  # ===========================================================================
  
  values,
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object (required): 
  ### for \code{data.frame}: two columns: De 
  ### (\code{values[,1]}) and De error (\code{values[,2]})
  
  na.exclude = TRUE,
  ### \code{\link{logical}} (with default): excludes \code{NA} values from the 
  ### data set prior to any further operations.
  
  mtext,
  ### \code{\link{character}} (optional): further sample information 
  ### (\link{mtext}).
  
  cex.global,
  ### \code{\link{numeric}} (with default): global scaling factor.
  
  breaks,
  ### (with default): sets breakpoints for histogram. Works as in \link{hist}.
  
  se,
  ### \code{\link{logical}} (optional): plots standard error points over the 
  ### histogram, default is \code{FALSE}.
  
  rug,
  ### \code{\link{logical}} (optional): adds rugs to the histogram, default is 
  ### \code{TRUE}.
  
  normal_curve,
  ### \code{\link{logical}} (with default): adds a normal curve to the histogram. 
  ### Mean and sd are calculated from the input data. More see details section.
  
  summary,
  ### \code{\link{character}} (optional): adds numerical output to the plot. Can 
  ### be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De value), 
  ### \code{"median"} (median of the De values), \code{"kdemax"} (maximum value of probability 
  ### density function), \code{"sdrel"} (relative standard deviation), \code{"sdabs"} 
  ### (absolute standard deviation), \code{"serel"} (relative standard error) and 
  ### \code{"seabs"} (absolute standard deviation).
  
  summary.pos,
  ### \code{\link{numeric}} (with default): optional position coordinates for 
  ### the statistical summary. Y-coordinate refers to the right hand y-axis.
  
  colour,
  ### \code{\link{numeric}} or \link{character} (with default): optional vector 
  ### of length 4 which specifies the colours of the following plot items in 
  ### exactly this order: histogram bars, rug lines, normal distribution curve 
  ### and standard error points\cr
  ### (e.g., \code{c("grey", "black", "red", "grey")}).
  
  ...
  ### further arguments and graphical parameters passed to \code{\link{plot}}. 
  ### If y-axis labels are provided, these must be specified as a vector of 
  ### length 2 since the plot features two axes (e.g. \code{ylab = c("axis 
  ### label 1", "axis label 2")}). Y-axes limits (\code{ylim}) must be
  ### provided as vector of length four, with the first two elements specifying
  ### the left axes limits and the latter two elements giving the right axis
  ### limits.
) {
  
  # Integrity tests ---------------------------------------------------------
  if(is(values, "RLum.Results") == TRUE) {
    values <- get_RLum.Results(values)
  } else if(is(values, "data.frame") == FALSE) {
    stop("[plot_Histogramm] Error: 'values' has to be an 
          'RLum.Results' object or a 'data.frame'.")
  }
  
  ## Set general parameters ---------------------------------------------------
  ## Check/set default parameters
  if(missing(cex.global) == TRUE) {cex.global = 1}
  if(missing(mtext) == TRUE) {mtext <- ""}
  if(missing(se) == TRUE) {se = TRUE}
  if(missing(rug) == TRUE) {rug = TRUE}
  if(missing(colour) == TRUE) {colour = c("white", "black", "red", "black")}
  if(missing(summary) == TRUE) {summary <- ""}
  if(missing(normal_curve) == TRUE) {normal_curve = FALSE}  
  
  extraArgs <- list(...) # read out additional arguments list
  fun <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}
  
  ## optionally, count nd exclude NA values and print result
  if(na.exclude == TRUE) {
    n.NA <- sum(!complete.cases(values))
    if(n.NA == 1) {print("1 NA value excluded.")
    } else if(n.NA > 1) {print(paste(n.NA, "NA values excluded."))}
    values <- na.exclude(values)}
  
  ## Check/set additional plot parameters
  extraArgs <- list(...) # read out additional arguments list
  
  main.plot <- if("main" %in% names(extraArgs)) {
    extraArgs$main
    } else {
      "Histogram"
    }
  
 xlab.plot <- if("xlab" %in% names(extraArgs)) {
    extraArgs$xlab
    } else {
      expression(paste(D[e], " [Gy]"))
    }
  
  ylab.plot <- if("ylab" %in% names(extraArgs)) {
    extraArgs$ylab
    } else {
      c("Frequency",
        "Error")
    }
  
  breaks.plot <- if("breaks" %in% names(extraArgs)) {
    extraArgs$breaks
  } else {
    hist(values[,1], plot = FALSE)$breaks
  }
  
  xlim.plot <- if("xlim" %in% names(extraArgs)) {
    extraArgs$xlim
  } else {
    range(breaks.plot)
  }
 
 ylim.plot <- if("ylim" %in% names(extraArgs)) {
   extraArgs$ylim
 } else {
   H.lim <- hist(values[,1],
                 breaks = breaks.plot,
                 plot = FALSE)
   if(normal_curve == TRUE) {
     left.ylim <- c(0, max(H.lim$density))
   } else {
     left.ylim <- c(0, max(H.lim$counts))
   }
   c(left.ylim, range(values[,2]))
 }
  
  pch.plot <- if("pch" %in% names(extraArgs)) {
    extraArgs$pch
  } else {
    1
  }
  
  ## Set plot area format
  par(oma = c(1, 1, 1, 3),
      cex = cex.global)

  ## Plot histogram -----------------------------------------------------------
  HIST <- hist(values[,1],
	  	 main = "",
       xlab = xlab.plot,
       ylab = ylab.plot[1],
       xlim = xlim.plot,
       ylim = ylim.plot[1:2],
       breaks = breaks.plot,
       freq = !normal_curve,
       col = colour[1]
       )
 
  
  ## add title
  title(line = 2, 
        main = main.plot)
  
  ## Optionally, add rug ------------------------------------------------------
  if(rug == TRUE) {rug(values[,1], col = colour[2])}

  ## Optionally, add a normal curve based on the data -------------------------
  if(normal_curve == TRUE){
    ## cheat the R check routine, tztztz how neat
    x <- NULL
    rm(x)
    
    ## add normal distribution curve
    curve(dnorm(x,
                mean = mean(na.exclude(values[,1])),
                sd = sd(na.exclude(values[,1]))),
          col = colour[3],
          add = TRUE,
          lwd = 1.2 * cex.global)
  }
  
  ## Optionally, add standard error plot --------------------------------------
  if(se == TRUE) {
    par(new = TRUE)
    plot(values[,1:2],
         xlim = xlim.plot,
         ylim = ylim.plot[3:4],
         pch = pch.plot,
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
    mtext(ylab.plot[2], 
          side = 4, 
          line = 3,
          cex = cex.global)
    
    par(new = FALSE)
  }
  
  ## Optionally add user-defined mtext
  mtext(side = 3,
        line = 0.5,
        text = mtext,
        cex = 0.8 * cex.global) 
  
  ## Optionally add statistical summary ---------------------------------------
  ## Calculate statistical summary
  n_De      <- nrow(values) # number of samples
  mean_De   <- mean(values[,1]) # mean
  median_De <- median(values[,1]) # median
  sd_De     <- sd(values[,1]) # standard deviation
  
  ## Paste statistics
  label.text <- paste(ifelse("n" %in% summary == TRUE,
                             paste("n = ", 
                                   n_De, 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("mean" %in% summary == TRUE,
                             paste("mean = ", 
                                   round(mean_De, 2), 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("mean.weighted" %in% summary == TRUE,
                             paste("weighted mean = ", 
                                   round(weighted.mean(values[,1]), 2), 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("median" %in% summary == TRUE,
                             paste("median = ", 
                                   round(median_De, 2), 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("sdrel" %in% summary == TRUE,
                             paste("sd = ", 
                                   round(sd_De/mean_De * 100, 2), " %",
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("sdrabs" %in% summary == TRUE,
                             paste("sd = ", 
                                   round(sd_De, 2),
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("serel" %in% summary == TRUE,
                             paste("se = ", 
                                   round(sd_De/(n_De * mean_De) * 100, 2), 
                                   " %\n", 
                                   sep = ""),
                             ""),
                      ifelse("sebas" %in% summary == TRUE,
                             paste("se = ", 
                                   round(sd_De/n_De, 2),
                                   "\n", 
                                   sep = ""),
                             ""),
                      sep = "")

  ## convert keywords into summary placement coordinates
  if(missing(summary.pos) == TRUE) {
    summary.pos <- c(xlim.plot[1], ylim.plot[4])
    summary.adj <- c(0, 1)
  } else if(length(summary.pos) == 2) {
    summary.pos <- summary.pos
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "topleft") {
    summary.pos <- c(xlim.plot[1], ylim.plot[4])
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "top") {
    summary.pos <- c(mean(xlim.plot), ylim.plot[4])
    summary.adj <- c(0.5, 1)
  } else if(summary.pos[1] == "topright") {
    summary.pos <- c(xlim.plot[2], ylim.plot[4])
    summary.adj <- c(1, 1)
  }  else if(summary.pos[1] == "left") {
    summary.pos <- c(xlim.plot[1], mean(ylim.plot[3:4]))
    summary.adj <- c(0, 0.5)
  } else if(summary.pos[1] == "center") {
    summary.pos <- c(mean(xlim.plot), mean(ylim.plot[3:4]))
    summary.adj <- c(0.5, 0.5)
  } else if(summary.pos[1] == "right") {
    summary.pos <- c(xlim.plot[2], mean(ylim.plot[3:4]))
    summary.adj <- c(1, 0.5)
  }else if(summary.pos[1] == "bottomleft") {
    summary.pos <- c(xlim.plot[1], ylim.plot[3])
    summary.adj <- c(0, 0)
  } else if(summary.pos[1] == "bottom") {
    summary.pos <- c(mean(xlim.plot), ylim.plot[3])
    summary.adj <- c(0.5, 0)
  } else if(summary.pos[1] == "bottomright") {
    summary.pos <- c(xlim.plot[2], ylim.plot[3])
    summary.adj <- c(1, 0)
  }
  
  ## add summary text
  text(x = summary.pos[1],
       y = summary.pos[2],
       adj = summary.adj,
       labels = label.text,
       col = "black", 
       cex = 0.8 * cex.global)
  
  ## FUN by R Luminescence Team
  if(fun==TRUE){sTeve()}
  
  ##details<<
  ## If the normal curve is added, the y-axis in the histogram will show 
  ## the probability density. 
  
  ##seealso<<
  ## \code{\link{hist}}, \code{\link{plot}}
  
  ##referencs<<
  ## Galbraith, R., 2010. Statistics in OSL: Some Current Questions; Ask Rex. 
  ## Oral presentation during the UK TL/OSL/ESR Meeting at the School of 
  ## Geography and the Environment, University of Oxford, 8-10 September 
  ## 2010.\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and 
  ## some recommendations. Quaternary Geochronology, 11, 1-27.
  
  ##note<<
  ## The input data is not restricted to a special type.
  
}, ex=function(){
  ## load data
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <- 
    Second2Gray(values = ExampleData.DeValues, dose_rate = c(0.0438,0.0019))
  
  ## plot histogram the easiest way
  plot_Histogram(ExampleData.DeValues)
  
  ## plot histogram with some more modifications
  plot_Histogram(ExampleData.DeValues, 
                 rug = TRUE, 
                 normal_curve = TRUE, 
                 cex.global = 0.9, 
                 pch = 2,
                 colour = c("grey", "black", "blue", "green"),
                 summary = c("n", "mean", "sdrel"),
                 summary.pos = "topleft",
                 main = "Histogram of De-values",
                 mtext = "Example data set", 
                 ylab = c(expression(paste(D[e], " distribution")),
                          "Error"),
                 xlim = c(100, 250),
                 ylim = c(0, 0.08, 50, 200))
})