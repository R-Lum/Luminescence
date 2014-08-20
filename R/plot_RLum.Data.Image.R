plot_RLum.Data.Image<- structure(function(#Plot function for an RLum.Data.Image S4 class object
  ### The function provides a standardised plot output for image data of an 
  ### RLum.Data.Image S4 class object mainly using the plot functions provided by the
  ### \code{\link{raster}} package [beta version]. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, Universite Bordeaux Montaigne (France), \cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================
  
  object, 
  ### \code{\linkS4class{RLum.Data.Image}} (\bold{required}): 
  ### S4 object of class \code{RLum.Data.Image}
  
  par.local = TRUE,
  ### \code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
  ### the plot is shown in one column and one row. If \code{par.local = FALSE} 
  ### global parameters are inherited.
  
  plot.type = "plot.raster",
  ### \code{\link{character}} (with default): plot types. Supported types are  
  ### \code{plot.raster}, \code{plotRGB} or \code{contour}
  
  ...
  ### further arguments and graphical parameters that will be passed to the 
  ### specific plot functions.
){
  
  
  # Integrity check -----------------------------------------------------------
  
  ##check if object is of class RLum.Data.Image
  if(class(object) != "RLum.Data.Image"){
    
    stop("[plot_RLum.Data.Image] Input object is not of type RLum.Data.Image")
    
  }
  
  ##deal with addition arguments 
  extraArgs <- list(...) 
  
  ##TODO
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {"RLum.Data.Image"}
  
  axes <- if("axes" %in% names(extraArgs)) {extraArgs$axes} else 
  {TRUE}
  
  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else 
  {"Length [px]"}
  
  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else 
  {"Height [px]"}
  
  stretch <- if("stretch" %in% names(extraArgs)) {extraArgs$stretch} else 
  {NULL}
  
  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else 
  {topo.colors(255)}
  
#   
#   #for zlim see below
#   
#   mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else 
#   {""}
#   
  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else 
  {1}
   
  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local == TRUE){
  
    par(mfrow=c(1,1), cex = cex)
  
  }##TODO - if the par setting is sufficient

  ##grep raster   


  if(plot.type == "plotRGB"){
  ## ==========================================================================#
  ## standard raster plotRGB (package raster)
  ## ==========================================================================#    
  
      plotRGB(get_RLum.Data.Image(object),
           main = main, 
           axes = TRUE,
           xlab = xlab,
           ylab = ylab,
           stretch = stretch)
           
   
  ## ==========================================================================#
  ## standard raster plot (package raster)
  ## ==========================================================================#      
  }else if(plot.type == "plot.raster"){
    
    plot(get_RLum.Data.Image(object),
         main = main, 
         xlab = xlab,
         ylab = ylab,
         col = col,
         stretch = stretch) 
    
  ## ==========================================================================#
  ## standard contour (package raster)
  ## ==========================================================================#     
  }else if(plot.type == "contour"){
    
    contour(get_RLum.Data.Image(object),
         main = main, 
         xlab = xlab,
         ylab = ylab,
         col = col,
         stretch = stretch) 
      
  }else{
    
    stop("[plot_RLum.Data.Image] Unknown plot type.")
    
  }
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##details<<
  ## \bold{Details on the plot functions} \cr
  ## 
  ## Image is visualised as 2D plot usinng generic plot types provided by other 
  ## package.
  ## R plot functions. \cr
  ##
  ## \bold{\code{plot.type = "plot.raster"}}\cr
  ##
  ## Uses the standard plot function for raster data from the package \code{\link{raster}}:
  ## \code{\link[raster]{plot}}
  ##
  ## \bold{\code{plot.type = "plotRGB"}}\cr
  ##
  ## Uses the function \code{\link[raster]{plotRGB}} from the \code{\link{raster}} package.
  ##
  ## \bold{\code{plot.type = "contour"}}\cr
  ##
  ## Uses the function contour plot function from the \code{\link{raster}}
  ## function (\code{\link[raster]{contour}}).

                                
  ##value<<
  ## Returns a plot.
  
  ##references<<
  ## -
  
  ##note<<
  ## This function has been created to faciliate the plotting of image data imported
  ## by the function \code{\link{readSPE2R}}. However, so far the function 
  ## is not optimized to handle image data > ca. 200 MByte and thus the plot
  ## my or extremely slow. 
  
  ##seealso<<
  ## \code{\linkS4class{RLum.Data.Image}}, 
  ## \code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{raster}}, 
  
  ##keyword<<
  ## aplot
  
}, ex=function(){
  
 ##load example data
 ##TODO
  
})#END OF STRUCTURE

#plot_RLum.Data.Image(temp, plot.type = "plotRGB")#TODO