analyse_pIRIRSequence<- structure(function(#Analyse post-IR IRSL sequences
  ### The function performs an analysis of post-IR IRSL sequences including 
  ### curve fitting on \code{\linkS4class{RLum.Analysis}}
  ### objects.
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)\cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================

  object,
  ### \code{\linkS4class{RLum.Analysis}}(\bold{required}): 
  ### input object containing data for analysis
  
  signal.integral.min,
  ### \code{\link{integer}} (\bold{required}): lower bound of the signal integral.
  ### Provide this value as vector for different integration limits for the different
  ### IRSL curves.
  
  signal.integral.max,
  ### \code{\link{integer}} (\bold{required}): upper bound of the signal integral.
  ### Provide this value as vector for different integration limits for the different
  ### IRSL curves.
  
  background.integral.min,
  ### \code{\link{integer}} (\bold{required}): lower bound of the background integral.
  ### Provide this value as vector for different integration limits for the different
  ### IRSL curves.
  
  background.integral.max,
  ### \code{\link{integer}} (\bold{required}): upper bound of the background integral.
  ### Provide this value as vector for different integration limits for the different
  ### IRSL curves.
  
  dose.points,
  ### \code{\link{numeric}} (optional): a numeric vector containg the dose points values
  ### Using this argument overwrites dose point values in the signal curves. 
  
  sequence.structure = c("TL", "IR50", "pIRIR225"),
  ### \link{vector} \link{character} (with default): specifies the general 
  ### sequence structure. Allowed values are \code{"TL"} 
  ### and any \code{"IR"} combination (e.g., \code{"IR50"},\code{"pIRIR225"}). 
  ### Addiationally a parameter \code{"EXCLUDE"} is allowed to exclude curves 
  ### from the analysis  
  ### (Note: If a preheat without PMT measurement is used, remove the TL step.)
  
  output.plot = TRUE,
  ### \code{\link{logical}} (with default): enables or disables plot output.
  
  output.plot.single = FALSE,
  ### \code{\link{logical}} (with default): 
  ### single plot output (\code{TRUE/FALSE}) to allow for plotting the results 
  ### in single plot windows. 
  ### Requires \code{output.plot = TRUE}.
  
  ... 
  ### further arguments that will be passed to the function 
  ### \code{\link{analyse_SAR.CWOSL}}

){

# CONFIG  -----------------------------------------------------------------


# General Integrity Checks ---------------------------------------------------

  ##GENERAL 

    ##MISSING INPUT
    if(missing("object")==TRUE){
      stop("[analyse_pIRIRSequence()] No value set for 'object'!")
    }

    ##INPUT OBJECTS
    if(is(object, "RLum.Analysis")==FALSE){
      stop("[analyse_pIRIRSequence()] Input object is not of type 'RLum.Analyis'!")
    }

    ##CHECK ALLOWED VALUES IN SEQUENCE STRUCTURE
    temp.collect.invalid.terms <- paste(sequence.structure[
      (!grepl("TL",sequence.structure)) &
      (!grepl("IR",sequence.structure)) &
      (!grepl("EXCLUDE",sequence.structure))], 
      collapse = ", ")
      
    if(temp.collect.invalid.terms != ""){
      stop("[analyse_pIRIRSequence()] ", 
        temp.collect.invalid.terms, " not allowed in sequence.strucutre!")
    }
      
    
# Deal with extra arguments -------------------------------------------------------------------
  
  ##deal with addition arguments 
  extraArgs <- list(...) 

  mtext.outer <- if("mtext.outer" %in% names(extraArgs)) {extraArgs$mtext.outer} else 
  {"MEASUREMENT INFO"}
  
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {""}
  
  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else 
  {""}

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else 
  {1}


# Protocol Integrity Checks -------------------------------------------------- 
  
  ##(1) Check structure and remove curves that fit not the recordType criteria 
 
  ##get sequence structure
  temp.sequence.structure  <- get_structure.RLum.Analysis(object)
  
  ##remove data types that fit not to allow values
  temp.sequence.rm.id <- temp.sequence.structure[ 
    (!grepl("TL",temp.sequence.structure[, "recordType"])) &
    (!grepl("OSL", temp.sequence.structure[, "recordType"])) &
    (!grepl("IRSL", temp.sequence.structure[, "recordType"])) 
    ,"id"]
    
  if(length(temp.sequence.rm.id)>0){
    
  ##removed record from data set
  object <- get_RLum.Analysis(object, record.id = -temp.sequence.rm.id, 
                              keep.object = TRUE)
  
  ##compile warning message  
  temp.sequence.rm.warning <- paste(
    temp.sequence.structure[temp.sequence.rm.id, "recordType"], collapse = ", ")
  
  temp.sequence.rm.warning <- paste(
    "Record types are unrecognised and have been removed:", temp.sequence.rm.warning)

  warning(temp.sequence.rm.warning)
  }
  
  ##(2) Apply user sequence structure 

  ##get sequence structure
  temp.sequence.structure  <- get_structure.RLum.Analysis(object)

  ##set values to structure data.frame
  temp.sequence.structure[, "protocol.step"] <- rep(
    sequence.structure, nrow(temp.sequence.structure)/2/length(sequence.structure))
  
  ##remove values that have been excluded
  temp.sequence.rm.id <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "EXCLUDE" ,"id"]
  
  if(length(temp.sequence.rm.id)>0){
    
    ##remove from object
    object  <- get_RLum.Analysis(
      object, record.id = -temp.sequence.rm.id, keep.object = TRUE)
    
    ##remove from sequence structure
    sequence.structure  <- sequence.structure[sequence.structure != "EXCLUDE"]
    
    ##set new structure 
    temp.sequence.structure  <- get_structure.RLum.Analysis(object)
    
    temp.sequence.structure[, "protocol.step"] <- rep(
      sequence.structure, nrow(temp.sequence.structure)/2/length(temp.sequence.structure))
    
    ##print warning message
    warning(length(temp.sequence.rm.id), " records have been removed due to EXCLUDE!")
    
  }

##============================================================================##
# Analyse data and Plotting ----------------------------------------------------
##============================================================================##

  ##(1) find out how many runs are needed for the analysis by checking for "IR"
  ##    should be now everything except the TL curves
  n.loops <- as.numeric(length(grepl("TL", sequence.structure)) - 
                table(grepl("TL", sequence.structure))["TRUE"])

  ##grep ids of TL curves (we need them later on)
  TL.curves.id <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "TL","id"]

  ##grep ids of all OSL curves (we need them later on)
  IRSL.curves.id <- temp.sequence.structure[
    grepl("IR", temp.sequence.structure[,"protocol.step"]),"id"]  

  ##grep information on the names of the IR curves, we need them later on
  pIRIR.curve.names  <- unique(temp.sequence.structure[
    temp.sequence.structure[IRSL.curves.id,"id"],"protocol.step"])

  ##===========================================================================#
  ## set graphic layout using the layout option
  ## unfortunately a little bit more complilcated then expeced due 
  ## the order of the produced plots by the previous functions
  
  if(output.plot.single == FALSE){
  ##first (Tx,Tn, Lx,Ln)
  temp.IRSL.layout.vector.first <- c(3,5,6,7,3,5,6,8)

  ##middle (any other Lx,Ln)
  if(n.loops > 2){
  temp.IRSL.layout.vector.middle <- as.vector(sapply(2:n.loops-1, function(x){

    offset <- (5*x)-1
    c((offset):(offset+3), 
      (offset):(offset+2),offset+4)

  }))}
  
  ##last (Lx,Ln and legend)
  temp.IRSL.layout.vector.last <- c(
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 1, 
           max(temp.IRSL.layout.vector.first) + 1),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 2, 
           max(temp.IRSL.layout.vector.first) + 2),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 4, 
           max(temp.IRSL.layout.vector.first) + 4),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 5, 
           max(temp.IRSL.layout.vector.first) + 5),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 1, 
           max(temp.IRSL.layout.vector.first) + 1),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 2, 
           max(temp.IRSL.layout.vector.first) + 2),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 4, 
           max(temp.IRSL.layout.vector.first) + 4),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 6, 
           max(temp.IRSL.layout.vector.first) + 6))
    
  ##options for different sets of curves
  if(n.loops > 2){
    
    temp.IRSL.layout.vector <- c(temp.IRSL.layout.vector.first, 
                                 temp.IRSL.layout.vector.middle,
                                 temp.IRSL.layout.vector.last)
    
  }else{
    
    temp.IRSL.layout.vector <- c(temp.IRSL.layout.vector.first, 
                                 temp.IRSL.layout.vector.last)
    
  }      

  ##get layout information
  def.par <- par(no.readonly = TRUE)
  
  ##set up layout matrix linked to the number of plot areas needed
  layout.matrix  <- c(
    rep(c(2,4,1,1),2), #header row with TL curves and info window
    temp.IRSL.layout.vector, #IRSL curves, 
    rep((max(temp.IRSL.layout.vector)-3),8), #legend,
    rep((max(temp.IRSL.layout.vector)+1),2), #GC
    rep((max(temp.IRSL.layout.vector)+2),2),#TnTc
    rep((max(temp.IRSL.layout.vector)+1),2), #GC
    rep((max(temp.IRSL.layout.vector)+2),2))#TnTc

  ##set layout
  nf <- layout(
    matrix(layout.matrix,(max(layout.matrix)/2+2), 4, byrow = TRUE),
     widths = c(rep(c(1,1,1,.75),6),c(1,1,1,1)),
     heights = c(rep(c(1),(2+2*n.loops)),c(0.20, 0.20)))


  ## show the regions that have been allocated to each plot for debug
  ##layout.show(nf) 

  }
  
  ##(1) INFO PLOT
  plot(NA,NA,
       ylim = c(0,1), xlab = "",
       xlim = c(0,1), ylab = "",
       axes = FALSE,
       main = main)
      
  text(0.5,0.5, paste(sequence.structure, collapse = "\n"), cex = cex *2)

       

  ##(2) set loop
  for(i in 1:n.loops){
    
    ##compile record ids
    temp.id.sel <- 
      sort(c(TL.curves.id, IRSL.curves.id[seq(i,length(IRSL.curves.id),by=n.loops)]))
    
    ##(a) select data set (TL curves has to be considered for the data set)
    temp.curves<- get_RLum.Analysis(object, record.id = temp.id.sel, keep.object = TRUE)
    
    ##(b) grep integral limits as they might be different for different curves
    if(length(signal.integral.min)>1){
      
      temp.signal.integral.min <- signal.integral.min[i]
      temp.signal.integral.max <- signal.integral.max[i]
      temp.background.integral.min <- background.integral.min[i]
      temp.backbround.integral.max <- background.integral.max[i]
      
    }else{
      
      temp.signal.integral.min <- signal.integral.min
      temp.signal.integral.max <- signal.integral.max
      temp.background.integral.min <- background.integral.min
      temp.background.integral.max <- background.integral.max

    }
    
    ##(c) call analysis sequence and plot
  
    ## call single plots
    if(i == 1){
      
      temp.output.plot.single  <- c(1,2,3,4,6)
      
    }else if(i == n.loops){
      
      temp.output.plot.single  <- c(2,4,5,6)
      
  }else{
    
     temp.output.plot.single  <- c(2,4,6)
    
  }
  
    ##start analysis
    temp.results <- analyse_SAR.CWOSL(
                    temp.curves, 
                    signal.integral.min = temp.signal.integral.min,
                    signal.integral.max = temp.signal.integral.max,
                    background.integral.min = temp.background.integral.min, 
                    background.integral.max = temp.background.integral.max, 
                    output.plot = output.plot,
                    dose.points = dose.points,
                    output.plot.single = temp.output.plot.single,
                    output.plotExtended.single = TRUE,
                    ...) ##TODO should be replaced be useful explizit arguments
    
 
    ##add signal nformation to the protocol step 
    temp.results.pIRIR.De <- as.data.frame(
      c(get_RLum.Results(temp.results, "De.values"), 
        data.frame(Signal = pIRIR.curve.names[i])))
    
    temp.results.pIRIR.LnLxTnTx <- as.data.frame(
     c(get_RLum.Results(temp.results, "LnLxTnTx.table"), 
       data.frame(Signal = pIRIR.curve.names[i])))
  
    temp.results.pIRIR.rejection.criteria <- as.data.frame(
      c(get_RLum.Results(temp.results, "rejection.criteria"), 
      data.frame(Signal = pIRIR.curve.names[i])))
  
    temp.results.pIRIR.formula <- list(get_RLum.Results(temp.results, 
                                                             "Formula"))
    names(temp.results.pIRIR.formula)  <- pIRIR.curve.names[i]
    
    ##create now object
    temp.results  <- set_RLum.Results(
      data = list(
        De.values = temp.results.pIRIR.De,
        LnLxTnTx.table = temp.results.pIRIR.LnLxTnTx, 
        rejection.criteria = temp.results.pIRIR.rejection.criteria,
        Formula =temp.results.pIRIR.formula))
     
     
    ##merge results
    if(exists("temp.results.final")){
    
      temp.results.final <- merge_RLum.Results(
        list(temp.results.final, temp.results))
    
    }else{
    
      temp.results.final <- temp.results
    
   }

  }


##============================================================================##
# Plotting additionals--------------------------------------------------------
##============================================================================##

if(output.plot == TRUE){
  
  ##plot growth curves
  plot(NA, NA, 
       xlim = range(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$Dose),
       ylim = c(
         min(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx)+
         max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx.Error),
         max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx)+
         max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx.Error)),
       xlab = "Dose [s]",
       ylab = expression(L[x]/T[x]),
       main = "Summarised growth curves")
  
      
    ##set x for expression evaluation 
    x <- seq(0,
             max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$Dose)*1.05,
             length = 100)
  
    for(j in 1:length(pIRIR.curve.names)){

     ##dose points
     temp.curve.points <- get_RLum.Results(
       temp.results.final,"LnLxTnTx.table")[,c("Dose", "LxTx", "LxTx.Error", "Signal")]
     
     temp.curve.points <- temp.curve.points[
       temp.curve.points[,"Signal"] == pIRIR.curve.names[j],
       c("Dose", "LxTx", "LxTx.Error")] 
      
     points(temp.curve.points[-1,c("Dose", "LxTx")], col = j, pch = j)
     segments(x0 = temp.curve.points[-1,c("Dose")],
              y0 = temp.curve.points[-1,c("LxTx")] - 
                temp.curve.points[-1,c("LxTx.Error")],
              x1 = temp.curve.points[-1,c("Dose")],
              y1 = temp.curve.points[-1,c("LxTx")] + 
                temp.curve.points[-1,c("LxTx.Error")], 
              col = j)
     
     ##De values    
     lines(c(0, get_RLum.Results(temp.results.final, "De.values")[j,1]), 
           c(temp.curve.points[1,c("LxTx")], temp.curve.points[1,c("LxTx")]),
           col = j, 
           lty = 2)
     
     lines(c(rep(get_RLum.Results(temp.results.final, "De.values")[j,1], 2)), 
           c(temp.curve.points[1,c("LxTx")], 0),
           col = j, 
           lty = 2)
     
     ##curve  
     temp.curve.formula  <- get_RLum.Results(
        temp.results.final, "Formula")[[pIRIR.curve.names[j]]]
        
     try(lines(x, eval(temp.curve.formula), col = j), silent = TRUE)
      
    }
  
    rm(x)
  
    ##plot legend
    legend("bottomright", legend = pIRIR.curve.names, 
           lty = 1, col = c(1:length(pIRIR.curve.names)), 
           bty = "n",
           pch = c(1:length(pIRIR.curve.names))
           )
           
    ##plot Tn/Tx curves
    ##select signal
    temp.curve.TnTx <- 
      get_RLum.Results(temp.results.final, "LnLxTnTx.table")[, c("TnTx", "Signal")]
  
    temp.curve.TnTx.matrix <- matrix(NA,
                                    nrow = nrow(temp.curve.TnTx)/
                                      length(pIRIR.curve.names), 
                                    ncol =  length(pIRIR.curve.names))
    
    ##calculate normalised values
    for(j in 1:length(pIRIR.curve.names)){
      
      temp.curve.TnTx.sel <- temp.curve.TnTx[
        temp.curve.TnTx[,"Signal"] == pIRIR.curve.names[j]
        , "TnTx"]
      
      temp.curve.TnTx.matrix[,j] <- temp.curve.TnTx.sel/temp.curve.TnTx.sel[1]
      
    }
     
    plot(NA, NA, 
       xlim = c(0,nrow(get_RLum.Results(temp.results.final, "LnLxTnTx.table"))/
                     n.loops),
       ylim = range(temp.curve.TnTx.matrix),   
       xlab = "# Cycle",
       ylab = expression(T[x]/T[n]),
       main = "Sensitivity change")
  
    ##zero line
    abline(h = 1:nrow(temp.curve.TnTx.matrix), col = "gray")
  
    for(j in 1:length(pIRIR.curve.names)){
     
     lines(1:nrow(temp.curve.TnTx.matrix), 
           temp.curve.TnTx.matrix[,j], 
           type = "b",
           col = j, 
           pch = j)  
    }
  
   ##plot legend
   legend("bottomleft", legend = pIRIR.curve.names, 
         lty = 1, col = c(1:length(pIRIR.curve.names)), 
         bty = "n", 
         pch = c(1:length(pIRIR.curve.names))
         )

   #reset graphic settings  
   if(output.plot.single == FALSE){par(def.par)} 
     
    
  
}##end output.plot == TRUE


##============================================================================##
# Return Values -----------------------------------------------------------
##============================================================================##
  
  temp.results.final
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------

  ##details<<
  ## To enable post-IR IRSL protocol (Thomsen et al., 2008) measurement analysis 
  ## this function has been written as wrapper function for the function 
  ## \code{\link{analyse_SAR.CWOSL}} facilitating an entire sequence analysis 
  ## in one run. On the other hand, its functionality is stritctly limited 
  ## by the functionality of the function \code{\link{analyse_SAR.CWOSL}}
  ##

  ##value<<
  ## Plots (optional) and an \code{\linkS4class{RLum.Results}} object is 
  ## returned containing the following elements: 
  ## \item{De.values}{\link{data.frame} containing De-values, 
  ## De-error and further parameters}
  ## \item{LnLxTnTx.values}{\link{data.frame} of all calculated Lx/Tx values 
  ## including signal, background counts and the dose points.}
  ## \item{rejection.criteria}{\link{data.frame} with values that might by 
  ## used as rejection criteria. NA is produced if no R0 dose point 
  ## exists.}\cr
  ##
  ## The output should be accessed using the function 
  ## \code{\link{get_RLum.Results}}
  
  ##references<<
  ## Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. 
  ## Laboratory fading rates of various luminescence signals from feldspar-rich 
  ## sediment extracts. Radiation Measurements 43, 1474-1486. 
  ## doi:10.1016/j.radmeas.2008.06.002

  ##note<<
  ## -

  ##seealso<<
  ## \code{\link{analyse_SAR.CWOSL}}, \code{\link{calc_OSLLxTxRatio}}, 
  ## \code{\link{plot_GrowthCurve}}, 
  ## \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}}
  ## \code{\link{get_RLum.Results}}

  ##keyword<<
  ## datagen
  ## plot


}, ex=function(){
  
  ## TODO provide pIRIR measurements? 
  
  
})#END OF STRUCTURE