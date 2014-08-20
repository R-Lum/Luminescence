plot_RLum.Analysis<- structure(function(#Plot function for an RLum.Analysis S4 class object
  ### The function provides a standardised plot output for curve data of an 
  ### RLum.Analysis S4 class object
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany), \cr
  
  ##section<<
  ## version 0.1.2
  # ===========================================================================

  object, 
  ### \code{\linkS4class{RLum.Analysis}} (\bold{required}): S4 object of class 
  ### \code{RLum.Analysis}
  
  nrows = 3, 
  ### \code{\link{integer}} (with default): sets number of rows for plot output
  
  ncols = 2, 
  ### \code{\link{integer}} (with default): sets number of columns for plot output
  
  ...
  ### further arguments and graphical parameters will be passed to the \code{plot} function.

){
  
  # Integrity check ----------------------------------------------------------------------------
  
  ##check if object is of class RLum.Data.Curve
  if(is(object,"RLum.Analysis") == FALSE){
    
    stop("[plot_RLum.Analysis]: Input object is not of type 'RLum.Analysis'")
    
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
  
  # Plotting ------------------------------------------------------------------
  
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
                     pch = pch)
          
            
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
  
  ##plot
  plot_RLum.Analysis(temp)
  
})#END OF STRUCTURE
