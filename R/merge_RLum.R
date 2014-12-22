merge_RLum<- structure(function(#General merge function for RLum S4 class objects
  ### Function calls object-specific merge functions for RLum S4 class objects. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================

  objects, 
  ### \code{\link{list}} of \code{\linkS4class{RLum}} (\bold{required}): list of S4 object of class \code{RLum}
  
  ...
  ### further arguments that one might want to pass to the specific merge function 
 
){
  
   # Integrity check ----------------------------------------------------------
  
 
   ##check if objects are of class RLum
   temp.class.test <- unique(sapply(1:length(objects), function(x){
   
     if(is(objects[[x]], "RLum") ==FALSE){
       
       temp.text <- paste("[merge_RLum()]: At least element", x, "is not of class 'RLum' or a derivative class!")
       stop(temp.text)
     }
     
     ##provide class of objects ... so far they should be similar
     is(objects[[x]])[1]
     
    }))
     
    ##check if objects are consitent
    if(length(temp.class.test)>1){
      
      ##This is not valid for RLum.Analysis objects
      if(!"RLum.Analysis"%in%temp.class.test){
      
          stop("[merge_RLum] So far only similar input objects in the list are supported!")
          
      }
    }
   
 
    ##grep object class
    objects.class <- ifelse("RLum.Analysis"%in%temp.class.test, "RLum.Analysis", temp.class.test)
    
    ##select which merge function should be used
  
    switch (objects.class,
            
            RLum.Data.Curve = stop("[merge_RLum] Sorry, merging of 'RLum.Data.Curve' objects currently not supported!"),
            RLum.Data.Image = stop("[merge_RLum] Sorry, merging of 'RLum.Data.Image' objects currently not supported!"),
            RLum.Data.Spectrum = stop("[merge_RLum] Sorry, merging of 'RLum.Data.Spectrum' objects currently not supported!"),
            RLum.Analysis = merge_RLum.Analysis(objects, ...),
            RLum.Results = merge_RLum.Results(objects, ...)            
           )


   # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
   
   ##details<<
   ## The function provides a generalised access point for merge specific 
   ## \code{\linkS4class{RLum}} objects.\cr
   ## Depending on the input object, the corresponding merge function will be selected. 
   ## Allowed arguments can be found in the documentations of each merge function. 
   ## \tabular{lll}{
   ## \bold{object} \tab \tab \bold{corresponding merge function} \cr
   ##    
   ## \code{\linkS4class{RLum.Results}} \tab : \tab \code{\link{merge_RLum.Results}} 
   ## }
   
   ##value<<
   ## Return is the same as input objects as provided in the list.
   
   ##references<<
   ## #
   
   ##note<<
   ## So far not for every \code{RLum} object a merging function exists.
   
   ##seealso<<
   ## \code{\link{merge_RLum.Results}}, \code{\linkS4class{RLum.Results}},

   
   ##keyword<<
   ## utilities
   
   
}, ex=function(){
  
  ##Example based using data and from the calc_CentralDose() function
  
  ##load example data
  data(ExampleData.DeValues, envir = environment())
  
  ##apply the central dose model 1st time 
  temp1 <- calc_CentralDose(ExampleData.DeValues)
  
  ##apply the central dose model 2nd time
  temp2 <- calc_CentralDose(ExampleData.DeValues)
  
  ##merge the results and store them in a new object
  temp.merged <- get_RLum.Results(merge_RLum(objects = list(temp1, temp2)))
  
  
})#END OF STRUCTURE