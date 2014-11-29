plot_RLum.Analysis<- structure(function(#Plot function for an RLum.Analysis S4 class object
  ### The function provides a standardised plot output for curve data of an 
  ### RLum.Analysis S4 class object
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
  
  ##section<<
  ## version 0.1.5
  # ===========================================================================

  object, 
  ### \code{\linkS4class{RLum.Analysis}} (\bold{required}): S4 object of class 
  ### \code{RLum.Analysis}
  
  nrows = 3, 
  ### \code{\link{integer}} (with default): sets number of rows for plot output
  
  ncols = 2, 
  ### \code{\link{integer}} (with default): sets number of columns for plot output
  
  abline,
  ### \code{\link{list}} (optional): allows to set similar ablines in each plot. This option 
  ### uses the function \code{\link{do.call}}, meaning that every argument in the \code{list} has
  ### to be provided as \code{list}, e.g. \code{abline = list(list(v = 120), list(v = 350))} produces
  ### two vertical ablines: One at 150 and another one at 350. Within the call all arguments 
  ### supported by \code{\link{abline}} are fully supported,
  
  combine = FALSE, 
  ### \code{\link{logical}} (with default): allows to combine all 
  ### code{\linkS4class{RLum.Data.Curve}} objects in one single plot.
  ### Works only for \code{\linkS4class{RLum.Analysis}} that comprises a single 
  ### curve object (option is currently only roughly implemented)
  
  ...
  ### further arguments and graphical parameters will be passed to the \code{plot} function.
  ### Supported arguments: \code{main}, \code{mtext}, \code{log}, \code{lwd}, \code{lty}
  ### \code{type}, \code{pch}, \code{col}

){
  
  # Integrity check ----------------------------------------------------------------------------
  
  ##check if object is of class RLum.Data.Curve
  if(is(object,"RLum.Analysis") == FALSE){
    
    stop("[plot_RLum.Analysis()]: Input object is not of type 'RLum.Analysis'")
    
  }
  
  ##deal with addition arguments 
  extraArgs <- list(...) 
  
  ##main
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {""}

  ##mtext
  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else 
  {""}
  
  ##log
  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else
  {""}
  
  ##lwd
  lwd <- if("lwd" %in% names(extraArgs)) {extraArgs$lwd} else
  {1}
  
  ##lty
  lty <- if("lty" %in% names(extraArgs)) {extraArgs$lty} else
  {1}
  
  ##type
  type <- if("type" %in% names(extraArgs)) {extraArgs$type} else
  {"l"}
  
  ##pch
  pch <- if("pch" %in% names(extraArgs)) {extraArgs$pch} else
  {1}
  
  ##col
  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else
  {"black"}
  
  ##norm (for RLum.Data.Curve)
  norm <- if("norm" %in% names(extraArgs)) {extraArgs$norm} else
  {FALSE}
  
  ##cex
  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else
  {1}
  
  
  # Plotting ------------------------------------------------------------------
    
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(1) NORMAL (combine == FALSE) 
  if(combine == FALSE){
  
      ##grep RLum.Data.Curve or RLum.Data.Spectrum objects 
      temp <- lapply(1:length(object@records), function(x){
                
                if(is(object@records[[x]], "RLum.Data.Curve") == TRUE || 
                   is(object@records[[x]], "RLum.Data.Spectrum") == TRUE){
                   
                  object@records[[x]]
                  
                }})
  
      ##calculate number of pages for mtext
      if(length(temp)%%(nrows*ncols)>0){
        
        n.pages <- round(length(temp)/(nrows*ncols), digits=0)+1
        
      }else{
        
        n.pages <- length(temp)/(nrows*ncols)
        
      }
 
      ##set par
      par.default <- par("mfrow")
      par(mfrow=c(nrows,ncols))          
  
      ##plot curves
      for(i in 1:length(temp)){
                
            if(is(temp[[i]], "RLum.Data.Curve") == TRUE){
            plot_RLum.Data.Curve(temp[[i]], 
                 col = if(col != "black"){col} else{
                   if(grepl("IRSL", temp[[i]]@recordType) == TRUE){"red"} else 
                     if(grepl("OSL", temp[[i]]@recordType) == TRUE){"blue"} else
                     {col}
                 },
                     mtext = paste("#",i,sep=""),
                     par.local = FALSE,
                     main = if(main==""){temp[[i]]@recordType}else{main},
                     log = log,
                     lwd = lwd,
                     type = type,
                     lty = lty, 
                     pch = pch,
                     norm = norm,
                     cex = cex)
            
            ##add abline
            if(!missing(abline)){
              
              for(k in 1:length(abline)){
                
                do.call("abline", abline[[k]])
                
              }
              
            }
          
            
           } else if(is(temp[[i]], "RLum.Data.Spectrum") == TRUE) {
             
             plot_RLum.Data.Spectrum(temp[[i]],
                  
                  mtext = paste("#",i,sep=""),
                  par.local = FALSE,
                  main = if(main==""){temp[[i]]@recordType}else{main})
        
           }  
            
          if(i%%(nrows*ncols)==0){
              mtext(mtext, outer = TRUE, side=3, line=-2)
          }
            
        }#end for loop
            
        
        ##reset par
        par(mfrow = par.default)

   }else{
    
   ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ##(2) NORMAL (combine == TRUE) 
     
   ##(1) check RLum objects in the set
    object.list <- get_RLum.Analysis(object) 
     
    sapply(1:length(object.list), function(x){
      
      if(is(object.list[[x]])[1] != "RLum.Data.Curve"){
        
        stop("[plot_RLum.Analysis()] Using 'combine' is limited to 'RLum.Data.Curve' objects.")
        
      }
      
    }) 
    
    
    ##(2) check for similar types 
    object.structure  <- get_structure.RLum.Analysis(object)
    
    if(length(unique(object.structure$recordType))>1){
      
      stop("[plot_RLum.Analysis()] Using 'combine' is limited 'RLum.Data.Curve' objects of similar type.")
      
    }
    
    
    ##(3) PLOT values  
    
    ##get some extra arguments
    xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else 
    {"x"}
    
    ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else 
    {"y"}
    
    ##colours and double for plotting
    col.curves <- get("col", pos = .LuminescenceEnv)
       
    ##change graphic settings
    par.default <- par()[c("mfrow", "mar", "xpd", "cex")]
    par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 8.1), xpd = TRUE, cex = cex)
    
    ##open plot area
    plot(NA,NA,
         xlim = c(min(object.structure$x.min), max(object.structure$x.max)),
         ylim = if(norm == FALSE){c(min(object.structure$y.min), max(object.structure$y.max))}
                else{c(0,1)},
         main = main,
         xlab = xlab,
         ylab = ylab,
         log = log)
         

    ##loop over all records
    for(i in 1:length(object.list)){
      
       temp.data <- as(object.list[[i]], "data.frame")
      
       ##normalise curves if argument has been set    
       if(norm == TRUE){
        
        temp.data[,2] <- temp.data[,2]/max(temp.data[,2])
        
       }
      
      lines(temp.data, col = col.curves[i])
   
    }      
    
    ##legend
    legend("topright",
           inset=c(-0.3*cex,0),
           legend = paste("Curve", 1:length(object.list)), 
           lwd= lwd, 
           col = col.curves[1:length(object.list)],
           lty = 1,
           bty = "n",
           cex = 0.6*cex)
    
    
    ##reset graphic settings
    par(par.default)
    rm(par.default)

    
    
  }
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##details<<
  ## The function produces a multiple plot output. 
  ## A file output is recommended (e.g., \code{\link{pdf}}).
    
  ##value<<
  ## Returns multiple plots.
  
  ##references<<
  ## #
  
  ##note<<
  ## Not all arguments available for \code{\link{plot}} will be passed!
  ## Only plotting of \code{RLum.Data.Curve} and \code{RLum.Data.Spectrum}
  ## objects are currently supported. 
  
  ##seealso<<
  ## \code{\link{plot}}, \code{\link{plot_RLum}}, 
  ## \code{\link{plot_RLum.Data.Curve}}
  
  ##keyword<<
  ## aplot
  
}, ex=function(){
  
  ###load data
  data(ExampleData.BINfileData, envir = environment())
  
  ##convert values for position 1
  temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
  
  ##plot all values
  plot_RLum.Analysis(temp)
  
  ##plot (combine) TL curves in one plot
  temp.sel <- get_RLum.Analysis(temp, recordType = "TL", keep.object = TRUE)
  plot_RLum.Analysis(temp.sel, combine = TRUE, norm = TRUE, main = "TL combined")
  
})#END OF STRUCTURE
