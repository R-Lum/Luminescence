analyse_IRSAR.RF<- structure(function(# Analyse IRSAR RF measurements
  ### Function to analyse IRSAR RF measurements on K-feldspar samples, performed 
  ### using the protocol according to Erfurt et al. (2003)
  
  # ===========================================================================
  ##author<< 
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
  
  ##section<<
  ## version 0.2.1
  # ===========================================================================

  ##TODO - keep fit.range in mind for De calculation
  ##Rejection criteria for curves input should be implemented. 
  ##Problem: User needs visuale feedback... 

  object,
  ### \code{\linkS4class{RLum.Analysis}} (\bold{required}): 
  ### input object containing data for protocol analysis
  
  sequence.structure = c("NATURAL", "REGENERATED"),
  ### \code{\link{vector}} \link{character} (with default): specifies the general 
  ### sequence structure. Allowed steps are \code{NATURAL}, \code{REGENERATED}.
  ### In addition any other character is allowed in the sequence structure; 
  ### such curves will be ignored. 
  
  method = "FIT", 
  ### \code{\link{character}} (with default): setting method applied for the data analysis.
  ### Possible options are \code{"FIT"} or \code{"SLIDE"}.
  
  fit.range.min, 
  ### \code{\link{integer}} (optional): set the minimum channel range for signal fitting and sliding.   
  ### Usually the entire data set is used for curve fitting, but there might be 
  ### reasons to limit the channels used for fitting.
  ### Note: This option also limits the values used for natural signal calculation.
  
  fit.range.max,
  ### \code{\link{integer}} (optional): set maximum channel range for signal fitting and sliding. 
  ### Usually the entire data set is used for curve fitting, but there might be 
  ### reasons to limit the channels used for fitting.
  
  fit.trace = FALSE,
  ### \code{\link{logical}} (with default): trace fitting (for debugging use)
  
  fit.MC.runs = 10, 
  ### \code{\link{numeric}} (with default): set number of Monte Carlo runs for start 
  ### parameter estimation. Note: Higher values will significantly increase 
  ### the calculation time.   
  
  slide.outlier.rm = FALSE,
  ### \code{\link{logical}} (with default): enable or disable outlier removal. 
  ### Outliers are removed from the natural signal curve only.
  
  slide.trend.corr = FALSE, 
  ### \code{\link{logical}} (with default): enable or disable trend correction. 
  ### If \code{TRUE}, the sliding is applied to a previously trend corrected data set.
  
  output.plot = TRUE, 
  ### \code{\link{logical}} (with default): plot output (\code{TRUE} or \code{FALSE})
  
  xlab.unit = "s",
  ### \code{\link{character}} (with default): set unit for x-axis
  
  legend.pos,
  ### \code{\link{character}} (with default): useful keywords are \code{bottomright}, 
  ### \code{bottom}, \code{bottomleft}, \code{left}, \code{topleft}, 
  ### \code{top}, \code{topright}, \code{right} and \code{center}. 
  ### For further details see \code{\link{legend}.}

  ...
  ### further arguments that will be passed to the plot output. 
  ### Currently supported arguments are \code{main}, \code{xlab}, \code{ylab}.
  
){
  

##=============================================================================#
## INTEGRITY TESTS
##=============================================================================#

  ##MISSING INPUT
  if(missing("object")==TRUE){
    stop("[analyse_IRSAR.RF()] No value set for 'object'!")
  }
  
  ##INPUT OBJECTS
  if(is(object, "RLum.Analysis")==FALSE){
    stop("[analyse_IRSAR.RF()] Input object is not of type 'RLum.Analyis'!")
  }

  # Protocol Integrity Checks -------------------------------------------------- 
  
  ##REMOVE predefined curves if they are availabe
  if(grepl("curveType", 
           as.character(get_structure.RLum.Analysis(object)$info.elements))[1] == TRUE){
    
    object <- set_RLum.Analysis(
      records = get_RLum.Analysis(object, curveType="measured"),
      protocol = object@protocol)
    
  }
  
  
  ##ANALYSE SEQUENCE OBJECT STRUCTURE
  
  ##set vector for sequence structure 
  temp.protocol.step <- rep(sequence.structure, length(object@records))[1:length(object@records)]
  
  ##grep object strucute
  temp.sequence.structure <- get_structure.RLum.Analysis(object)
  
 
  ##set values for step
  temp.sequence.structure[,"protocol.step"] <- temp.protocol.step
    
  ##set fit.range for fitting
    
    if(missing(fit.range.min)==TRUE){fit.range.min <- 1}
    if(missing(fit.range.max)==TRUE){fit.range.max <- max(
      temp.sequence.structure$n.channels)}
    
    ##set to full range if no value is given
    fit.range <- c(fit.range.min:fit.range.max)
       
    ##if value if given, check the validity
    if(min(fit.range)< 1 | max(fit.range)>max(temp.sequence.structure$n.channels)){
      
      fit.range <- c(1:max(temp.sequence.structure$n.channels))
      warning("Fit range out of bounds, set to full data set extend.")

  }
  
  ##apply fit range also for the natural curve
  fit.range.natural <- fit.range
    
   
  
##=============================================================================#
## PLOT PARAMETERS
##=============================================================================#  
  
  ##get channel resolution (should be equal for all curves)
  resolution.RF <- round(object@records[[1]]@data[2,1]-
                   object@records[[1]]@data[1,1], digits=2)
  
  if(missing(legend.pos)){
    
    legend.pos  <- ifelse(method == "FIT", "bottom", "top")
    
  }


  # Set plot format parameters -----------------------------------------------------------------------
  extraArgs <- list(...) # read out additional arguments list
  
  main      <- if("main" %in% names(extraArgs)) {extraArgs$main} else # assign main text
  {"IR-RF"}
  
  xlab      <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else # assign x axis label
  {paste("Time [", xlab.unit, "]", sep="")}
  
  ylab     <- if("ylabs" %in% names(extraArgs)) {extraArgs$ylabs} else # assign y axes labels
  {paste("IR-RF [cts/",resolution.RF," ", xlab.unit,"]",sep = "")}

  cex     <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else # assign y axes labels
  {1}
  
  
##=============================================================================#
## ANALYSIS
##=============================================================================#
  
  ##IMPLEMENT OVERALL REJECTION CRITERIA TODO for curves
  # set values for fitting --------------------------------------------------
  
  ##grep first regenerated curve 
  values.regenerated <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="REGENERATED","id"]]]@data)
  
 values.regenerated<- as.data.frame(object@records[[2]]@data)
 values.regenerated.x <- values.regenerated[fit.range,1]
 values.regenerated.y <- values.regenerated[fit.range,2]

  ##grep values from natural signal 
  values.natural <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="NATURAL","id"]]]@data)

  ##limit values to fit range (at least to the minimum)
  values.natural.limited<- values.natural[min(fit.range.natural):nrow(values.natural),]

  ##calculate some use paremeters 
  values.natural.mean <- mean(values.natural.limited[,2])
  values.natural.sd <- sd(values.natural.limited[,2])

  values.natural.error.lower <- values.natural.mean + values.natural.sd 
  values.natural.error.upper <- values.natural.mean - values.natural.sd 


##METHOD FIT
if(method == "FIT"){  
## REGENERATED SIGNAL
# set function for fitting ------------------------------------------------

fit.function <- as.formula(y~phi.0-(delta.phi*((1-exp(-lambda*x))^beta))) 

##stretched expontial function according to Erfurt et al. (2003)
## + phi.0 >> initial IR-RF flux
## + delta.phi >> dose dependent change of the IR-RF flux
## + lambda >> exponential parameter
## + beta >> dispersive factor

# set start parameter estimation ------------------------------------------
 
  fit.parameters.start <- c(
    phi.0 = max(values.regenerated.y),
    lambda = 0.0001,
    beta = 1,
    delta.phi = 2*(max(values.regenerated.y)-min(values.regenerated.y)))    

# start nls fitting -------------------------------------------------------
  
##Monte Carlo approach for fitting

  fit.parameters.results.MC.results <- data.frame()
  
  ##produce set of start paramters
  phi.0.MC <- rep(fit.parameters.start["phi.0"], fit.MC.runs)
  lambda.MC <- seq(0.0001, 0.001, by=(0.001-0.0001)/fit.MC.runs) ##TODO
  beta.MC <- rep(fit.parameters.start["beta"], fit.MC.runs)
  delta.phi.MC <- rep(fit.parameters.start["delta.phi"], fit.MC.runs)
  
  ##start fitting loop
  for(i in 1:fit.MC.runs){
  
  fit.MC <-try(nls(fit.function, 
                trace = FALSE, 
                data = data.frame(x=values.regenerated.x, y=values.regenerated.y), 
                algorithm = "port",
                start = list(
                  phi.0 = phi.0.MC[i],
                  delta.phi = delta.phi.MC[i],
                  lambda = lambda.MC[i],
                  beta = beta.MC[i]),
                nls.control(
                  maxiter = 100,
                  warnOnly = FALSE,
                  minFactor = 1/1024),
                lower = c(phi.0 = .Machine$double.xmin, 
                          delta.phi = .Machine$double.xmin, 
                          lambda = .Machine$double.xmin, 
                          beta = .Machine$double.xmin),
                upper = c(phi.0 = max(values.regenerated.y), 
                          delta.phi = max(values.regenerated.y),     
                          lambda = 1, 
                          beta = 100)),
               silent=TRUE)
           
   if(inherits(fit.MC,"try-error") == FALSE){  
   
      temp.fit.parameters.results.MC.results <- coef(fit.MC)
      
      fit.parameters.results.MC.results[i,"phi.0"] <- temp.fit.parameters.results.MC.results["phi.0"]
      fit.parameters.results.MC.results[i,"lambda"] <- temp.fit.parameters.results.MC.results["lambda"]
      fit.parameters.results.MC.results[i,"delta.phi"] <- temp.fit.parameters.results.MC.results["delta.phi"]
      fit.parameters.results.MC.results[i,"beta"] <- temp.fit.parameters.results.MC.results["beta"]
    
   }       
  }

 ##FINAL fitting after successful MC
 if(length(na.omit(fit.parameters.results.MC.results)) != 0){
   
      ##choose median as final fit version
      fit.parameters.results.MC.results <- sapply(na.omit(fit.parameters.results.MC.results), median)

  
      ##try final fitting 
      fit <-try(nls(fit.function, 
                trace = fit.trace, 
                data = data.frame(x=values.regenerated.x, y=values.regenerated.y), 
                algorithm = "port",
                start = list(
                  phi.0 = fit.parameters.results.MC.results["phi.0"],
                  delta.phi = fit.parameters.results.MC.results["delta.phi"],
                  lambda = fit.parameters.results.MC.results["lambda"],
                  beta = fit.parameters.results.MC.results["beta"]),
                nls.control(
                  maxiter = 500,
                  warnOnly = FALSE,
                  minFactor = 1/4096),
                lower = c(phi.0 = .Machine$double.xmin, 
                        delta.phi = .Machine$double.xmin, 
                        lambda = .Machine$double.xmin, 
                        beta = .Machine$double.xmin),
                upper = c(phi.0 = max(values.regenerated.y), 
                          delta.phi = max(values.regenerated.y), 
                          lambda = 1, beta = 100)),
                silent=FALSE)
 }else{
   
   fit <- NA
   class(fit) <- "try-error"
   
 }
# get parameters ----------------------------------------------------------

if(inherits(fit,"try-error") == FALSE){
 
  fit.parameters.results <- coef(fit)
  
}else{
  
  fit.parameters.results <- NA
  
}

  ##calculate De value
  if(is.na(fit.parameters.results[1]) == FALSE){
    
  De.mean <- suppressWarnings(round(log(-((values.natural.mean - fit.parameters.results["phi.0"])/
                      -fit.parameters.results["delta.phi"])^(1/fit.parameters.results["beta"])+1)/
                      -fit.parameters.results["lambda"], digits=2))
          
  De.error.lower <- suppressWarnings(
    round(log(-((values.natural.error.lower - fit.parameters.results["phi.0"])/
                     -fit.parameters.results["delta.phi"])^(1/fit.parameters.results["beta"])+1)/
                     -fit.parameters.results["lambda"],digits=2))
    
  De.error.upper <- suppressWarnings(
    round(log(-((values.natural.error.upper - fit.parameters.results["phi.0"])/
          -fit.parameters.results["delta.phi"])^(1/fit.parameters.results["beta"])+1)/
          -fit.parameters.results["lambda"],digits=2))
  
  }else{
    
    De.mean <- NA
    De.error.lower <- NA
    De.error.upper <- NA
       
  }
}

##METHOD SLIDE
else if(method == "SLIDE"){
  
  ## TODO
  ## Check for rejection criteria for input data
  ## implement error calculation 
  ## Extent the manual
  ## Test the data analysis
  
  ##convert to matrix (in fact above the matrix data were first transfered to data.frames ... here
  ##we correct this ... again)
  values.natural.limited <- as.matrix(values.natural.limited)
  values.regenerated.limited <- matrix(c(values.regenerated.x, values.regenerated.y), ncol = 2)
  
  ##===============================================================================================#
  ## Check for outliers and remove potential outliers
  ## Procedure based partly on the code used in the function apply_CosmicRayRemoval()
  
    if(length(values.natural.limited)<=32 & slide.outlier.rm == TRUE){
      
      warning("> 30 data points are needed for the outlier correction. Nothing done!") 
      
    }
  
  if(slide.outlier.rm == TRUE & length(values.natural.limited) > 32){
    ##the threshold 30 has been set artifically to avoid problems during 
    ##outlier removoval 
    
  ##remove outliers for sliding (but just at the beginning and at the tail, nothing in between)
  temp.median.roll <- zoo::rollmedian(values.natural.limited, k = 11)
                                 
  
    ##use interpolation to fill the gaps in the data
    temp.median.roll <-  matrix(unlist(approx(temp.median.roll[,1],
                                temp.median.roll[,2],                               
                                xout = values.natural.limited[,1], rule=2)), ncol=2)
    
    ##detect outliers in the natural signal using histogramm; mark them 
    temp.outlier.residual <- (temp.median.roll[,2] - values.natural.limited[,2])
      temp.outlier.hist <- hist(temp.outlier.residual, 
           breaks = length(values.natural.limited[,2])/2, plot = FALSE)
    
    ##find mode of the histogram (e.g. peak)
    temp.outlier.hist.max <- which.max(temp.outlier.hist$counts)
  
    ##find gaps in the histogram (bins with zero value)
    temp.outlier.hist.zerobin <- which(temp.outlier.hist$counts == 0)
  
    ##select non-zerobins
    temp.outlier.hist.nonzerobin <- which(temp.outlier.hist$counts != 0)
         
    ##define threshold, e.g. if a gap is broader than this value, all values above
    ##are marked as outlier
    temp.threshold  <- 2 * sd(temp.median.roll[,2])
  
    temp.outlier.hist.nonzerobin.diff <- diff(
        temp.outlier.hist$breaks[temp.outlier.hist.nonzerobin])
        
     ## select the first value where the thershold is reached
     ## factor 3 is defined by Pych (2003)
     temp.outlier.hist.thres <- which(
        temp.outlier.hist.nonzerobin.diff >= temp.threshold)
      
     ##check for the case that no threshold is found
     if(length(temp.outlier.hist.thres) != 0){
       
     ##Find: Which ID value?
     temp.outlier.hist.thres.min  <- min(temp.outlier.hist$breaks[
       temp.outlier.hist.nonzerobin][temp.outlier.hist.thres])
  
     temp.outlier.hist.thres.max  <- max(temp.outlier.hist$breaks[
       temp.outlier.hist.nonzerobin][temp.outlier.hist.thres]) 
  
      if( temp.outlier.hist.thres.min <0){
        
        temp.outlier.ID.max <- unique(
          which(temp.outlier.residual > temp.outlier.hist.thres.max))
        
        temp.outlier.ID.min <- unique(
          which(temp.outlier.residual < temp.outlier.hist.thres.min))
        
        temp.outlier.ID <- c(temp.outlier.ID.min, temp.outlier.ID.max)
        
      }else{
        
        temp.outlier.ID <- unique(
          which(temp.outlier.residual > temp.outlier.hist.thres.max))
        
      }
     
      ##remove from data set for further analysis
      values.natural.limited.full <-  values.natural.limited
  
      if(length(temp.outlier.ID)>0){
        values.natural.limited <-  values.natural.limited[- temp.outlier.ID,]
        
        warning(paste(length(temp.outlier.ID)), " values removed as outlier for sliding!")
      }
     
     }else{
       
       values.natural.limited.full <-  values.natural.limited
       temp.outlier.ID <- NULL
       
     }
  
  }else{
    
    values.natural.limited.full <-  values.natural.limited
    temp.outlier.ID <- NULL
    
  }
      
  ##(1) calculate sum of residual squares
  temp.sum.residuals <- sapply(1:(nrow(values.regenerated.limited)-nrow(values.natural.limited)), 
                                       function(x){
                                          
   sum((values.regenerated.limited[
     x:((nrow(values.natural.limited)+x)-1),2]-values.natural.limited[,2])^2)
    
  })
  
  ##(2) get index of minimum value
  temp.sum.min.id <- which.min(temp.sum.residuals)
  temp.sum.min.time.value <- values.regenerated.limited[temp.sum.min.id]
  
  temp.sliding.step <- temp.sum.min.time.value - values.natural.limited[1,1]
  
  
  ##(3) slide curve (with the full data)
  values.natural.limited.full.preSlide <- values.natural.limited.full
  values.natural.limited.full[,1] <- values.natural.limited.full[,1] + temp.sliding.step 

  
  ##(4) grep residuals
  if(length(temp.outlier.ID)>0){
    
    values.residuals <- values.natural.limited.full[-temp.outlier.ID,2] - 
      values.regenerated.limited[
        values.regenerated.limited[,1]%in%values.natural.limited.full[-temp.outlier.ID,1], 2]
    
    
  }else{
    
    values.residuals <- values.natural.limited.full[,2] - 
      values.regenerated.limited[
        values.regenerated.limited[,1]%in%values.natural.limited.full[,1], 2]
    
  }
  
  
  ##(4.1) calculate De from the first channel
  De.mean <- round(values.natural.limited.full[1,1], digits = 2)
  
  if(slide.trend.corr == TRUE){
  
   ##(5) fit residual data and correct for trend
   temp.trend.fit <- coef(lm(y~x, data.frame(x = values.natural.limited[,1], y = values.residuals)))
   temp.trend.fit.slope  <- temp.trend.fit[2]
  
   ##(5.1) recalculate trend corrected values
   values.natural.limited.corr <- data.frame(
     x = values.natural.limited[,1],
     y = (-temp.trend.fit.slope * values.natural.limited[,1] + 
     values.natural.limited[,2]))
  
   ##(5.2) calcualte sum of residual squares
   temp.sum.residuals.corr <- sapply(1:(nrow(values.regenerated.limited)-nrow(values.natural.limited.corr)), 
                               function(x){
                                 
                                 sum((values.regenerated.limited[
                                   x:((nrow(values.natural.limited.corr)+x)-1),2]-values.natural.limited.corr[,2])^2)
                                 
                               })
  
   ##(5.3) find minimum value
   temp.sum.min.id.corr <- which.min(temp.sum.residuals.corr)
   temp.sum.min.time.value.corr <- values.regenerated.limited[temp.sum.min.id.corr]
  
   ##(5.4) slide correct values
   temp.sliding.step.corr <- temp.sum.min.time.value.corr - values.natural.limited.corr[1,1]
  
   ##(5.5) slide curve (with the full data)
   values.natural.limited.full.corr <- values.natural.limited.full.preSlide
   values.natural.limited.full.corr[,1] <- values.natural.limited.full.corr[,1] + temp.sliding.step.corr 
  
   ##(5.6) calculate De
   De.mean.corr <- round(values.natural.limited.full.corr[1,1], digits = 2)
  }

}

##ANY OTHER METHOD
else{
  
  stop("[analyse_IRSAR.RF()] method is not supported!")
  
}
##=============================================================================#
## PLOTTING
##=============================================================================#
if(output.plot==TRUE){
  
  ##grep par default
  def.par <- par(no.readonly = TRUE)
  
  ##colours 
  col <- get("col", pos = .LuminescenceEnv)

  ##set plot frame
  layout(matrix(c(1,2),2,1,byrow=TRUE),c(2), c(1.5,0.4), TRUE)
  par(oma=c(1,1,1,1), mar=c(0,4,3,0), cex = cex)
  
  ##open plot area
  plot(NA,NA,
       xlim = c(0,max(temp.sequence.structure$x.max)),
       ylim = c(min(temp.sequence.structure$y.min), max(temp.sequence.structure$y.max)),
       xlab = "",
       xaxt = "n",
       ylab = ylab,
       main = main, 
       log = "")

  ##plotting measured signal 
  points(values.regenerated[,1], values.regenerated[,2], pch=3, col="grey")
 
  ##mark values used for further analysis fitting
  points(values.regenerated.x,values.regenerated.y, pch=3, col=col[18])

##METHOD FIT
  if(method == "FIT"){

  ##show fitted curve COLORED
  
    ##dummy to cheat R CMD check
    x<-NULL; rm(x)
  
  curve(fit.parameters.results["phi.0"]-
          (fit.parameters.results["delta.phi"]*
          ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])), 
        add=TRUE,
        from = values.regenerated[min(fit.range), 1],
        to = values.regenerated[max(fit.range), 1],
        col="red")

  ##show fitted curve GREY (previous red curve)
  curve(fit.parameters.results["phi.0"]-
        (fit.parameters.results["delta.phi"]*
           ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])), 
      add=TRUE,
      from = min(values.regenerated[, 1]),
      to = values.regenerated[min(fit.range), 1],
      col="grey")

  ##show fitted curve GREY (after red curve)
  curve(fit.parameters.results["phi.0"]-
        (fit.parameters.results["delta.phi"]*
           ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])), 
      add=TRUE,
      from = values.regenerated[max(fit.range), 1],
      to = max(values.regenerated[, 1]),
      col="grey")

  ##at points
  points(values.natural, pch = 20, col = "grey")
  points(values.natural.limited, pch = 20, col = "red")
  
  ##legend
  legend(legend.pos, legend=c("reg. measured","reg. used for fit", "natural"),  
         pch=c(3,3, 20), col=c("grey", col[18], "red"), 
         horiz=TRUE, bty="n", cex=.7)
  
  ##plot range choosen for fitting
  abline(v=values.regenerated[min(fit.range), 1], lty=2)
  abline(v=values.regenerated[max(fit.range), 1], lty=2)
  
  ##plot De if De was calculated 
  if(is.na(De.mean) == FALSE & is.nan(De.mean) == FALSE){
    
    lines(c(0,De.error.lower), c(values.natural.error.lower,values.natural.error.lower), lty=2, col="grey")
    lines(c(0,De.mean), c(values.natural.mean,values.natural.mean), lty=2, col="red")
    lines(c(0,De.error.upper), c(values.natural.error.upper,values.natural.error.upper), lty=2, col="grey")
    
    lines(c(De.error.lower, De.error.lower), 
          c(0,values.natural.error.lower), lty=2, col="grey")
    lines(c(De.mean,De.mean), c(0, values.natural.mean), lty=2, col="red")
    lines(c(De.error.upper, De.error.upper), 
          c(0,values.natural.error.upper), lty=2, col="grey")
    
  }
  
  ##Insert fit and result
  if(is.na(De.mean) != TRUE & (is.nan(De.mean) == TRUE |
                                 De.mean > max(values.regenerated.x) | 
                                 De.error.upper > max(values.regenerated.x))){
    
    try(mtext(side=3, substitute(D[e] == De.mean, 
                                 list(De.mean=paste(
                                   De.mean," (",De.error.lower," ", De.error.upper,")", sep=""))),
              line=0, cex=0.8, col="red"), silent=TRUE)
    
    De.status <- "VALUE OUT OF BOUNDS"
    
  } else{
    
    if("mtext" %in% names(extraArgs)) {extraArgs$mtext
    }else{
      
      try(mtext(side=3, 
                substitute(D[e] == De.mean, 
                           list(De.mean=paste(
                             De.mean," (",De.error.lower," ", De.error.upper,")", sep=""))),
                line=0, 
                cex=0.7), 
          silent=TRUE)
    }
    
    De.status <- "OK"
  }
  
  
  ##==lower plot==##    
  par(mar=c(4.2,4,0,0))
  
  ##plot residuals  
  if(is.na(fit.parameters.results[1])==FALSE){
    
    plot(values.regenerated.x,residuals(fit), 
         xlim=c(0,max(temp.sequence.structure$x.max)),
         xlab="Time [s]", 
         type="p", 
         pch=20,
         col="grey", 
         ylab="Resid. [a.u.]",
         #lwd=2,
         log="")
    
    ##add 0 line
    abline(h=0)
  }else{
    plot(NA,NA,
         xlim=c(0,max(temp.sequence.structure$x.max)),
         ylab="Resid. [a.u.]",
         xlab=xlab, 
         ylim=c(-1,1)
    )    
    text(x = max(temp.sequence.structure$x.max)/2,y=0, "Fitting Error!")   
  } 
}

##METHOD SLIDE  
  else if(method == "SLIDE"){
    
  ##add points
  points(values.natural.limited.full, pch = 20, col = rgb(0,0,1,.5))
  
  if(length(temp.outlier.ID)>0){
   ##mark points markes as outlier
    points(values.natural.limited.full[temp.outlier.ID,], pch = 1, col = "red")  
  }
  
  ##DEBUG
  #points(values.natural.limited.full.corr, col = "red")
  
  ##plot range choosen for fitting
  abline(v=values.regenerated[min(fit.range), 1], lty=2)
  abline(v=values.regenerated[max(fit.range), 1], lty=2)
  
  ##legend
  if(length(temp.outlier.ID)>0){

  legend(legend.pos, legend=c("reg. measured","reg. selected", "natural", "outlier"),  
           pch=c(3,3,20,1), col=c("grey", col[18], "blue", "red"), 
           horiz=TRUE, bty="n", cex=.7)
  
  }else{
    
    legend(legend.pos, legend=c("reg. measured","reg. selected", "natural"),  
           pch=c(3,3,20), col=c("grey", col[18], "blue"), 
           horiz=TRUE, bty="n", cex=.7)
    
  }
    
  ##write information on the De in the plot
  if("mtext" %in% names(extraArgs)) {extraArgs$mtext
  }else{
            
      if(exists("De.mean.corr")){
        try(mtext(side=3, 
                  substitute(D[e] == De.mean,  list(
                  De.mean=paste0(De.mean," | ", De.mean.corr, " (corr. value)"))),
                  line=0, 
                  cex=0.7), 
            silent=TRUE)
        
      }else{
        
        try(mtext(side=3, 
                  substitute(D[e] == De.mean),
                  line=0, 
                  cex=0.7), 
            silent=TRUE)

      }
    }
    
    ##mark selected De
    arrows(x0 = De.mean, y0 = c(min(temp.sequence.structure$y.min)),
           x1 = De.mean, y1 = par("usr")[3], lwd = 3*cex, col = "blue", lty = 1)
  
    ##mark selected De.corr
    if(exists("De.mean.corr")){
     
      arrows(x0 = De.mean.corr, y0 = c(min(temp.sequence.structure$y.min)),
            x1 = De.mean.corr, y1 = par("usr")[3], lwd = 3*cex, col = "red", lty = 1)
    
    }
  
    ##==lower plot==##    
    par(mar=c(4.2,4,0,0))
    
    if(length(temp.outlier.ID)>0){
          
    plot(values.natural.limited.full[-temp.outlier.ID,1], values.residuals, 
         xlim=c(0,max(temp.sequence.structure$x.max)),
         xlab="Time [s]", 
         type="p", 
         pch=20,
         col="grey", 
         ylab="Resid. [a.u.]",
         #lwd=2,
         log="")
    
    if(exists("De.mean.corr")){
     lines(values.natural.limited.full[-temp.outlier.ID,1], 
           temp.trend.fit[2] * values.natural.limited.full[-temp.outlier.ID,1] + temp.trend.fit[1], 
           col = "red")
    }
    
    }else{
    
      plot(values.natural.limited.full[,1], values.residuals, 
           xlim=c(0,max(temp.sequence.structure$x.max)),
           xlab="Time [s]", 
           type="p", 
           pch=20,
           col="grey", 
           ylab="Resid. [a.u.]",
           #lwd=2,
           log="")
      
      if(exists("De.mean.corr")){
        lines(values.natural.limited.full[,1], 
              temp.trend.fit[2] * values.natural.limited.full[,1] + temp.trend.fit[1], 
              col = "red")
      }
      
    }

    
  
    ##add 0 line
    abline(h=0)
    abline(v = De.mean, lty = 2, col = "blue")
    if(exists("De.mean.corr")){abline(v = De.mean.corr, lty = 2, col = "red")}
       
    
    ##add numeric value
    if(exists("De.mean.corr")){
     
      axis(side = 1, at = De.mean.corr, labels = De.mean.corr, cex.axis = 0.8*cex,
          col = "red", padj = -1.55,)
   
    }else{
     
     axis(side = 1, at = De.mean, labels = De.mean, cex.axis = 0.8*cex,
          col = "blue", padj = -1.55,)
    }
    
    
  }

  par(def.par)  #- reset to default
  
}#endif::output.plot
##=============================================================================#
## RETURN
##=============================================================================#
  
  ##catch up worst case scenarios
  if(!exists("De.mean")){De.mean  <- NA}
  if(!exists("De.mean.corr")){De.mean.corr  <- NA}
  if(!exists("De.error.lower")){De.error.lower  <- NA}
  if(!exists("De.error.upper")){De.error.upper  <- NA}
  if(!exists("De.status")){De.status  <- NA}
  if(!exists("fit")){fit  <- NA}


  ##combine values
  De.values <- data.frame(De = De.mean,
                          De.corr = De.mean.corr,
                          De.error.lower = De.error.lower,
                          De.error.upper = De.error.upper,
                          De.status = De.status,
                          row.names=NULL)
  
  newRLumResults.analyse_IRSAR.RF <- set_RLum.Results(
    data = list(
      De.values = De.values,
      fit = fit))

  return(newRLumResults.analyse_IRSAR.RF)
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##value<<
  ## A plot (optional) and an \code{\linkS4class{RLum.Results}} object is returned 
  ## containing the following elements: \cr
  ## \item{De.values}{\code{\link{data.frame}} containing De-values with error 
  ## (gray dashed lines in the plot) and further parameters. Corrected De values are only 
  ## provided for the method \code{"SLIDE"}, provided the trend correction is applied.}
  ## \item{fit}{\link{nls} \code{nlsModel} object}\cr
  ## \bold{Note:} The output (\code{De.values}) should be accessed using the 
  ## function \code{\link{get_RLum.Results}}
  
  ##details<<
  ## The function performs an IRSAR analysis described for K-feldspar samples by 
  ## Erfurt et al. (2003) assuming a negligible sensitivity change of the RF signal.\cr
  ## \bold{General Sequence Structure} (according to Erfurt et al. (2003))
  ## \enumerate{
  ## \item Measuring IR-RF intensity of the natural dose for a few seconds (\eqn{D_{natural}})     
  ## \item Bleach the samples under solar conditions for at least 30 min without changing 
  ## the geometry 
  ## \item Waiting for at least one hour 
  ## \item Regeneration of the IR-RF signal to at least the natural level
  ## \item Fitting data with a stretched exponential function
  ## \item Calculate the the palaeodose \eqn{D_{e}} using the parameters from the fitting
  ## }
  ## \bold{Function Used For The Fitting} (according to Erfurt et al. (2003))\cr
  ## \deqn{\phi(D) = \phi_{0}-\Delta\phi(1-exp(-\lambda*D))^\beta}
  ## with \eqn{\phi(D)} the dose dependent IR-RF flux, \eqn{\phi_{0}} the inital IR-RF flux, 
  ## \eqn{\Delta\phi} the dose dependent change of the IR-RF flux, \eqn{\lambda} 
  ## the exponential parameter, \eqn{D} the dose and \eqn{\beta} the dispersive 
  ## factor.\cr\cr
  ## To obtain the palaeodose \eqn{D_{e}} the function is changed to:\cr
  ## \deqn{D_{e} = ln(-(\phi(D) - \phi_{0})/(-\lambda*\phi)^{1/\beta}+1)/-\lambda}\cr
  ## The fitting is done using the \code{port} algorithm of the \code{\link{nls}} function.\cr
  ##
  ## Two methods are supported to obtain the De:\cr
  ##
  ## \bold{\code{method = "FIT"}}\cr
  ## 
  ## The principle is described above and follows the orignal suggestions from Erfurt et al., 2003.\cr
  ##
  ## \bold{\code{method = "SLIDE"}}\cr
  ##
  ## For this method the natural curve is slided along the x-axis until congruence with the 
  ## regenerated curve is reached. Instead of fitting this allows to work with the original data 
  ## without the need of any physical model. 
  ## This approach was introduced for RF curves by Buylaert et al., 2012 and 
  ## Lapp et al., 2012.
  ## 
  ## Here the sliding is done by searching for the minimum of the residual squares. 
  ##
  ## \deqn{min(\Sigma(RF.reg_{k.i} - RF.nat_{k.i})^2)}
  ## for 
  ## \deqn{k = {t.0+i,...,t.max+i}}
  ## 
  ##
  ## \bold{Correction for outliers} (\code{slide.outlier.rm = TRUE})\cr
  ##
  ## By using \code{method = "SLIDE"} and setting the argument \code{slide.outlier.rm = TRUE}
  ## an automatic outlier removal can be applied to the natural curve. Outliers may be observed also
  ## on the regenerative curve, but here the impact of single outliers on the curve adjustment (sliding)
  ## is considered as negligible. \cr
  ## The applied outlier removal algorithm consists of three steps:\cr
  ##
  ## (a) Input data are smoothed using the function \code{\link{rollmedian}}. Value \code{k} for 
  ## the rolling window is fixed to 11. Therefore, the natural curve needs to comprise at least of 33 
  ## values, otherwise outlier removal is rejected. \cr
  ##
  ## (b) To subsequently remove outliers, code blocks from the function \code{\link{apply_CosmicRayRemoval}} 
  ## were recycled, therefore in general the outlier correction works as described by Pych (2003). 
  ## In contrast, here no sigma clipping before constructing the histograms is applied.\cr
  ##
  ## (c) Outliers are marked in the data set and visualised in the graphical output. The subsequent
  ## adjustement of both curves (natural and regenerative) is done without outliers, whereas the 
  ## sliding itself is done with the entire data set.\cr
  ## 
  ##
  ## \bold{Trend correction} (\code{slide.trend.corr = TRUE})\cr
  ##
  ## This option allows for correcting any linear trend in the natural curve in comparison to the 
  ## regenerative curve. The trend correction is based on regression analysis of the residuals from 
  ## the slided curve. The corrected De is obtained by sliding the trend corrected values (again)
  ## along the regenerative data curve. This correction is driven by the idea that the 
  ## rediduals from the regenerative and the natural curve should be free of any trend, as long as 
  ## they are comparable. \cr
  ##
  ## \bold{Error estimation}
  ## 
  ## For \bold{\code{method = "FIT"}} the asymmetric error range is taken from the standard deviation
  ## of the natural signal.\cr
  ##
  ## For \bold{\code{method = "SLIDE"}} so far no error estimation is implemented. Instead, to asses
  ## the error of the De several aliquots should be measured and the error obtained from the 
  ## De distribution.

  
  ##references<<
  ## Buylaert, J.P., Jain, M., Murray, A.S., Thomsen, K.J., Lapp, T., 2012. 
  ## IR-RF dating of sand-sized K-feldspar extracts: A test of accuracy. 
  ## Radiation Measurements 44 (5-6), 560-565. doi: 10.1016/j.radmeas.2012.06.021
  ##
  ## Erfurt, G., Krbetschek, M.R., 2003. IRSAR - A single-aliquot regenerative-dose 
  ## dating protocol applied to the infrared radiofluorescence (IR-RF) of coarse- grain 
  ## K-feldspar. Ancient TL 21, 35-42.
  ## 
  ## Erfurt, G., 2003. Infrared luminescence of Pb+ centres in potassium-rich feldspars. 
  ## physica status solidi (a) 200, 429-438.
  ##
  ## Erfurt, G., Krbetschek, M.R., 2003. Studies on the physics of the infrared 
  ## radioluminescence of potassium feldspar and on the methodology of its application 
  ## to sediment dating. Radiation Measurements 37, 505-510.
  ##
  ## Erfurt, G., Krbetschek, M.R., Bortolot, V.J., Preusser, F., 2003. 
  ## A fully automated multi-spectral radioluminescence reading system for 
  ## geochronometry and dosimetry. Nuclear Instruments and Methods in Physics Research 
  ## Section B: Beam Interactions with Materials and Atoms 207, 487-499.
  ##
  ## Lapp, T., Jain, M., Thomsen, K.J., Murray, A.S., Buylaert, J.P., 2012. New luminescence measurement
  ## facilities in retrospective dosimetry. Radiation Measurements 47, 803-808. 
  ## doi:10.1016/j.radmeas.2012.02.006
  ##
  ## Pych, W., 2003. A Fast Algorithm for Cosmic-Ray Removal from Single Images.
  ## Astrophysics 116, 148-153.
  ## \url{http://arxiv.org/pdf/astro-ph/0311290.pdf?origin=publication_detail}
  ##
  ## Trautmann, T., 2000. A study of radioluminescence kinetics of natural feldspar 
  ## dosimeters: experiments and simulations. Journal of Physics D: Applied Physics 33, 2304-2310.
  ##
  ## Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1998. 
  ## Investigations of feldspar radioluminescence: potential for a new dating technique. 
  ## Radiation Measurements 29, 421-425.
  ##
  ## Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1999. Feldspar 
  ## radioluminescence: a new dating method and its physical background. 
  ## Journal of Luminescence 85, 45-58.
  ##
  ## Trautmann, T., Krbetschek, M.R., Stolz, W., 2000. A systematic study of the 
  ## radioluminescence properties of single feldspar grains. 
  ## Radiation Measurements 32, 685-690.
  
  ##note<<
  ## This function assumes that there is no sensitivity change during the measurements (natural
  ## vs. regenerated signal), which is in contrast to the findings from Buylaert et al. (2012).
  
  ##seealso<<
  ## \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}},
  ## \code{\link{get_RLum.Results}}, \code{\link{nls}}
  
  ##keyword<<
  ## datagen
  
  
},ex=function(){  
  ##load data
  data(ExampleData.RLum.Analysis, envir = environment())
  
  ##perform analysis
  temp <- analyse_IRSAR.RF(object = IRSAR.RF.Data) 
  
})#END OF STRUCTURE