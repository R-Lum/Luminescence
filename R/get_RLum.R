get_RLum<- structure(function(#General accessor function for RLum S4 class objects
  ### Function calls object-specific get functions for RLum S4 class objects. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================

  object, 
  ### \code{\linkS4class{RLum}} (\bold{required}): S4 object of class \code{RLum}
  
  ...
  ### further arguments that one might want to pass to the specific get function 
 
){
  
   # Integrity check ----------------------------------------------------------
  
 
   ##check if object is of class RLum
   if(!"RLum"%in%is(object)){
      
       stop("[get_RLum()]: Input object  is not of class 'RLum' or a derivative class!")
   
     }
     
    ##grep object class
    object.class <-  is(object)[1]
    
    ##select which get function should be used
    switch (object.class,
            
            RLum.Data.Curve = get_RLum.Data.Curve(object, ...),
            RLum.Data.Image = get_RLum.Data.Image(object, ...),
            RLum.Data.Spectrum = get_RLum.Data.Spectrum(object, ...),
            RLum.Analysis = get_RLum.Analysis(object, ...),
            RLum.Results = get_RLum.Results(object, ...)            
           
    )


   # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
   
   ##details<<
   ## The function provides a generalised access point for specific 
   ## \code{\linkS4class{RLum}} objects.\cr
   ## Depending on the input object, the corresponding get function will be selected. 
   ## Allowed arguments can be found in the documentations of each get function. 
   ## \tabular{lll}{
   ## \bold{object} \tab \tab \bold{corresponding get function} \cr
   ##  
   ## \code{\linkS4class{RLum.Data.Curve}} \tab : \tab \code{\link{get_RLum.Data.Curve}}\cr
   ## \code{\linkS4class{RLum.Data.Image}} \tab : \tab \code{\link{get_RLum.Data.Image}}\cr
   ## \code{\linkS4class{RLum.Data.Spectrum}} \tab : \tab \code{\link{get_RLum.Data.Spectrum}}\cr
   ## \code{\linkS4class{RLum.Analysis}} \tab : \tab \code{\link{get_RLum.Analysis}}\cr 
   ## \code{\linkS4class{RLum.Results}} \tab : \tab \code{\link{get_RLum.Results}} 
   ## }
   
   ##value<<
   ## Return is the same as input objects as provided in the list.
   
   ##references<<
   ## -
   
   ##note<<
   ## -
   
   ##seealso<<
   ## \code{\link{get_RLum.Data.Curve}}, \code{\linkS4class{RLum.Data.Curve}}, 
   ## \code{\link{get_RLum.Data.Image}},  \code{\linkS4class{RLum.Data.Image}},
   ## \code{\link{get_RLum.Data.Spectrum}},  \code{\linkS4class{RLum.Data.Spectrum}},
   ## \code{\link{get_RLum.Analysis}}, \code{\linkS4class{RLum.Analysis}},
   ## \code{\link{get_RLum.Results}}, \code{\linkS4class{RLum.Results}}
  
   ##keyword<<
   ## utilities
     
}, ex=function(){
  
  ##Example based using data and from the calc_CentralDose() function
  
  ##load example data
  data(ExampleData.DeValues, envir = environment())
  
  ##apply the central dose model 1st time 
  temp1 <- calc_CentralDose(ExampleData.DeValues)
  
  ##get results and store them in a new object
  temp.get<- get_RLum(object = temp1)
  
  
})#END OF STRUCTURE