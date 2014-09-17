analyse_IRSAR.RF<- structure(function(# Analyse IRSAR RF measurements
  ### Function to analyse IRSAR RF measurements on K-feldspar samples, performed 
  ### using the protocol according to Erfurt et al. (2003)
  
  # ===========================================================================
  ##author<< 
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
  
  ##section<<
  ## version 0.1.4
  # ===========================================================================

  #TODO - keep fit.range in mind for De calculation

  object,
  ### \code{\linkS4class{RLum.Analysis}} (\bold{required}): 
  ### input object containing data for protocol analysis
  
  sequence.structure = c("NATURAL", "REGENERATED"),
  ### \code{\link{vector}} \link{character} (with default): specifies the general 
  ### sequence structure. Allowed steps are \code{NATURAL}, \code{REGENERATED}
  ### In addition any other character is allowed in the sequence structure; 
  ### such curves will be ignored. 
  
  method = "FIT", 
  ### \code{\link{character}} (with default): setting the method applied for the data analysis
  ### Possible options are \code{"FIT"} or \code{"SLIDE"}
  
  fit.range.min, 
  ### \code{\link{integer}} (optional): set the minimum channel range for signal fitting.   
  ### Usually the entire data set is used for curve fitting, but there might be 
  ### reasons to limit the channels used for fitting.
  ### Note: This option also limits the values used for natural signal calculation.
  
  fit.range.max,
  ### \code{\link{integer}} (optional): set maximum channel range for signal fitting. 
  ### Usually the entire data set is used for curve fitting, but there might be 
  ### reasons to limit the channels used for fitting.
  
  fit.trace = FALSE,
  ### \code{\link{logical}} (with default): trace fitting (for debugging use)
  
  fit.MC.runs = 10, 
  ### \code{\link{numeric}} (with default): set number of Monte Carlo runs for start 
  ### parameter estimation. Note: Higher values will significantly increase 
  ### the calculation time   
  
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
  ### Currently supported arguments are \code{main}, \code{xlab}, \code{ylab}
  
){
  

##=============================================================================#
## INTEGRITY TESTS
##=============================================================================#

  ##MISSING INPUT
  if(missing("object")==TRUE){
    stop("[analyse_IRSAR.RF] Error: No value set for 'object'!")
  }
  
  ##INPUT OBJECTS
  if(is(object, "RLum.Analysis")==FALSE){
    stop("[analyse_IRSAR.RF] Error: Input object is not of type 'RLum.Analyis'!")
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
      warning("[analyse_IRSAR.RF] Fit range out of bounds, set to full data set extend.")

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
## FITTING 
##=============================================================================#
 
  # set values for fitting --------------------------------------------------
  
  ##grep first regenerated curve 
  values.regenerated <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="REGENERATED","id"]]]@data)
  
 values.regenerated<- as.data.frame(object@records[[2]]@data)
 values.regenerated.x <- values.regenerated[fit.range,1]
 values.regenerated.y <- values.regenerated[fit.range,2]
  
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
}

##=============================================================================#
## CALCULATING
##=============================================================================#
  
      
  ##grep values from natural signal 
  values.natural <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="NATURAL","id"]]]@data)

  ##limit values to fit range (at least to the minimum)
  values.natural.limited<- values.natural[min(fit.range.natural):nrow(values.natural),]

  values.natural.mean <- mean(values.natural.limited[,2])
  values.natural.sd <- sd(values.natural.limited[,2])
  
  values.natural.error.lower <- values.natural.mean + values.natural.sd 
  values.natural.error.upper <- values.natural.mean - values.natural.sd 
  
  if(method == "FIT"){
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
  


# METHOD SLIDE --------------------------------------------------------------------------------

}else if(method == "SLIDE"){
  
  ## TODO
  ## Check for rejection criteria for input data
  ## check graphical appearance
  ## show De.value
  ## implement error
  ## visualisation vai transparence -- alpha
  
  ##convert to matrix
  values.natural.limited <- as.matrix(values.natural.limited)
  values.regenerated.limited <- matrix(c(values.regenerated.x, values.regenerated.y), ncol = 2)
  
  ##(1) sum up natural values
  temp.sum.natural.curve <- sum(values.natural.limited[,2])
  
  ##(2) sum up regenerated values until the maximum value
  temp.sum.regenerated.curve <- sapply(1:(nrow(values.regenerated.limited)-nrow(values.natural.limited)), 
                                       function(x){
    
   sum(values.regenerated.limited[x:(nrow(values.natural.limited)+x)-1,2])
    
  })
  
  ##(3) difference
  temp.sum.diff <- abs(temp.sum.natural.curve - temp.sum.regenerated.curve)
  
  ##(4) get index of minimal value
  temp.sum.min.id <- which.min(temp.sum.diff)
  temp.sliding.step <- diff(values.natural.limited[1:2,1])
  
  ##(5) slide curve
  values.natural.limited[,1] <- values.natural.limited[,1] + temp.sum.min.id * temp.sliding.step 

  ##(6) calculate De
  De.mean <- values.natural.limited[1,1]
  
}else{
  
  stop("[analyse_IRSAR.RF()] method is not supported!")
  
}
##=============================================================================#
## PLOTTING
##=============================================================================#
if(output.plot==TRUE){
  
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
  

  if(method == "FIT"){
    
  ##mark values used for fitting
  points(values.regenerated.x,values.regenerated.y, pch=3, col=col[18])

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
  }
  
  
  ##PLOT NATURAL VALUES
  if(method == "FIT"){
    points(values.natural, pch = 20, col = "grey")
    points(values.natural.limited, pch = 20, col = "red")
 
  }else if(method == "SLIDE"){
    
    points(values.natural.limited, pch = 20, col = "red")
    
  }


  ##plot range choosen for fitting
  abline(v=values.regenerated[min(fit.range), 1], lty=2)
  abline(v=values.regenerated[max(fit.range), 1], lty=2)
  
  ##legend
  if(method == "FIT"){
  legend(legend.pos, legend=c("reg. measured","reg. used for fit", "natural"),  
          pch=c(3,3, 20), col=c("grey", col[18], "red"), 
          horiz=TRUE, bty="n", cex=.7)
 
  }else if(method == "SLIDE"){
    
    legend(legend.pos, legend=c("reg. measured","natural"),  
           pch=c(3, 20), col=c("grey", col[18], "red"), 
           horiz=TRUE, bty="n", cex=.7)
    
  }

  
  if(method == "FIT"){
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
     ylab="Residual [a.u.]",
     #lwd=2,
     log="")

  ##add 0 line
  abline(h=0)
  }else{
    plot(NA,NA,
         xlim=c(0,max(temp.sequence.structure$x.max)),
         ylab="Residual [a.u.]",
         xlab=xlab, 
         ylim=c(-1,1)
         )    
    text(x = max(temp.sequence.structure$x.max)/2,y=0, "Fitting Error!")   
  }
  }else if(method == "SLIDE"){
    
    arrows(x0 = De.mean, y0 = c(min(temp.sequence.structure$y.min)),
          x1 = De.mean, y1 = par("usr")[3], lwd = 3*cex)
    
        
    ##==lower plot==##    
    par(mar=c(4.2,4,0,0))
    

    values.residuals <- values.natural.limited[,2] - 
      values.regenerated.limited[
        values.regenerated.limited[,1]%in%values.natural.limited[,1], 2]
   
    
    plot(values.natural.limited[,1], values.residuals, 
         xlim=c(0,max(temp.sequence.structure$x.max)),
         xlab="Time [s]", 
         type="p", 
         pch=20,
         col="grey", 
         ylab="Residual [a.u.]",
         #lwd=2,
         log="")
    
    ##add 0 line
    abline(h=0)
    abline(v = De.mean)
    
  }
  
  
}#endif::output.plot
##=============================================================================#
## RETURN
##=============================================================================#
  
  ##catch up worst case scenarios
  if(!exists("De.mean")){De.mean  <- NA}
  if(!exists("De.error.lower")){De.error.lower  <- NA}
  if(!exists("De.error.upper")){De.error.upper  <- NA}
  if(!exists("De.status")){De.status  <- NA}
  if(!exists("fit")){fit  <- NA}


  ##combine values
  De.values <- data.frame(De = De.mean,
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
  ## (gray dashed lines in the plot) and further parameters}
  ## \item{fit}{\link{nls} \code{nlsModel} object}\cr
  ## \bold{Note:} The output (\code{De.values}) should be accessed using the 
  ## function \code{\link{get_RLum.Results}}
  
  ##details<<
  ## The function performs an IRSAR analysis described for feldspar samples by 
  ## Erfurt et al. (2003) assuming a negligible sensitivity change of the RF signal.\cr
  ## \bold{General Sequence Structure} (according to Erfurt et al. (2003))
  ## \enumerate{
  ## \item Measuring IR-RF intensity of the natural dose for a few seconds (\eqn{D_{natural}})     
  ## \item Bleach the samples under solar conditions for at least 30 min without changing 
  ## the geometry 
  ## \item Waiting for at least one hour 
  ## \item Regeneration of the IR-RF signal to at least the natural level
  ## \item Fitting data with a stretched exponential function
  ## \item Calculate the the palaedose \eqn{D} using the parameters from the fitting
  ## }
  ## \bold{Function Used For The Fitting} (according to Erfurt et al. (2003))\cr
  ## \deqn{\phi(D) = \phi_{0}-\Delta\phi(1-exp(-\lambda*D))^\beta}
  ## with \eqn{\phi(D)} the dose dependent IR-RF flux, \eqn{\phi_{0}} the inital IR-RF flux, 
  ## \eqn{\Delta\phi} the dose dependent change of the IR-RF flux, \eqn{\lambda} 
  ## the exponential parameter, \eqn{D} the dose and \eqn{\beta} the dispersive 
  ## factor.\cr\cr
  ## To obtain the palaedose the function is changed to:\cr
  ## \deqn{D = ln(-(\phi(D) - \phi_{0})/(-\lambda*\phi)^{1/\beta}+1)/-\lambda}\cr
  ## The fitting is done using the \code{port} algorithm of the \code{\link{nls}} function.
  
  ##references<<
  ## Buylaert, J.P., Jain, M., Murray, A.S., Thomsen, K.J., Lapp, T., 2012. 
  ## IR-RF dating of sand-sized K-feldspar extracts: A test of accuracy. 
  ## Radiation Measurements 1-7. doi: 10.1016/j.radmeas.2012.06.021
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

# library(Luminescence)
# 
# if(!exists("temp.raw")){
# temp.raw <- readXSYG2R("~/Lumi/Bordeaux/pourNorbert/XSGYG/2014-09-12_20140912_Courville5_RF70_Test.xsyg")
# }
# 
# temp.header  <- temp.raw[[1]]$Sequence.Header
# temp.sequence <- temp.raw[[1]]$Sequence.Object
# 
# temp.RF <- get_RLum.Analysis(temp.sequence, recordType = "RF (NIR50)", keep.object = TRUE)
# 
# 
# ##perform analysis
# temp <- analyse_IRSAR.RF(object = temp.RF, method = "SLIDE") 