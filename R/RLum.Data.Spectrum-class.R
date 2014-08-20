##//////////////////////////////////////////////////////////////////////////////
##//RLum.Data.Spectrum-class.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen
##version: 0.1
##date: 2013-11-23
##==============================================================================
##[SK]  The curve type slot is intentionally named 'recordType' against 
##      the internal naming conventions. 


setClass("RLum.Data.Spectrum",
         representation(
           recordType = "character",
           curveType = "character",
           data = "matrix",
           info = "list"
           ),
         contains = "RLum.Data",
         prototype = list (
           recordType = character(),
           curveType = character(),
           data = matrix(),      
           info = list()
           ),                
         S3methods=TRUE)


# setAs - coerce methods ------------------------------------------------------

##----------------------------------------------
##COERCE FROM AND TO data.frame

setAs("data.frame", "RLum.Data.Spectrum", 
      function(from,to){
        
              new(to, 
                  recordType = "unkown curve type",
                  curveType = "NA",
                  data = as.matrix(from),
                  info = list())
            })    

setAs("RLum.Data.Spectrum", "data.frame", 
      function(from){
        
        data.frame(x = from@data[,1], 
                   y = from@data[,2])
        
      })    


# ##----------------------------------------------
##COERCE FROM AND TO matrix

setAs("matrix", "RLum.Data.Spectrum", 
      function(from,to){
                
        new(to, 
            recordType = "unkown curve type",
            curveType = "NA",
            data = from,
            info = list())   
      })   

setAs("RLum.Data.Spectrum", "matrix", 
      function(from){
        
        from@data
        
      })   




# show method for object ------------------------------------------------------

setMethod("show", 
          signature(object = "RLum.Data.Spectrum"),
          function(object){
          
            x.range <- range(as.numeric(rownames(object@data)))
            y.range <- range(as.numeric(colnames(object@data)))
            z.range <- range(object@data)
    
            ##print information
        
            cat("\n [RLum.Data.Spectrum]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t .. recorded frames:", length(object@data[1,]))
            cat("\n\t .. .. measured values per frame:", length(object@data[,1]))
            cat("\n\t .. .. range wavelength/pixel:", x.range)
            cat("\n\t .. .. range time/temp.:", y.range)            
            cat("\n\t .. .. range count values:", z.range)          
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))          
          }         
)


# # constructor (set) method for object class -----------------------------------

setGeneric("set_RLum.Data.Spectrum",
           function(recordType, curveType, data, info) {standardGeneric("set_RLum.Data.Spectrum")})


setMethod("set_RLum.Data.Spectrum", 
            signature = c(recordType = "ANY", curveType = "ANY", data = "ANY", info = "ANY"), 
          
            function(recordType, curveType, data, info){             
              
              ##check for missing curveType
              if(missing(curveType)==TRUE){
                
                curveType <- "NA"
                
              }else if (is(curveType, "character") == FALSE){
                              
                stop("[set_RLum.Data.Spectrum] Error: 'curveType' has to be of type 'character'!")
                
              }
              
              ##check for missing arguments
              if(missing(recordType) | missing(data)){
                     
                temp.error.missing <- paste(c(
                  
                  if(missing(recordType)){"'recordType'"}else{},
                  if(missing(data)){"'data'"}else{}), 
                                            collapse=", ")
                  
                ##set error message   
                temp.error.message <- paste("[set_RLum.Data.Spectrum] Error: Missing required arguments " ,
                                       temp.error.missing,"!", sep="")
                stop(temp.error.message)
              }
              
              ##handle missing info argument
              if(missing(info)){
                
                info <- list()
                
              }else if (is(info, "list") == FALSE){
                
                stop("[set_RLum.Data.Spectrum] Error: 'info' has to be of type 'list'!")
    
              }
              
              new("RLum.Data.Spectrum", 
                  recordType = recordType,
                  curveType = curveType,
                  data = data,
                  info = info)
  
            })

# constructor (get) method for object class -----------------------------------

setGeneric("get_RLum.Data.Spectrum",
           function(object, info.object) {standardGeneric("get_RLum.Data.Spectrum")})

setMethod("get_RLum.Data.Spectrum", 
          signature(object="ANY", info.object = "ANY"), 
          definition = function(object, info.object) {
            
            ##Check if function is of type RLum.Data.Spectrum
            if(is(object, "RLum.Data.Spectrum") == FALSE){
              
              stop("[get_RLum.Data.Spectrum] Function valid for 'RLum.Data.Spectrum' objects only!")
              
            }
            
            ##if missing info.object just show the curve values
            
            if(missing(info.object) == FALSE){
              
              if(is(info.object, "character") == FALSE){            
                stop("[get_RLum.Data.Spectrum] Error: 'info.object' has to be a character!")           
              }
              
              if(info.object %in% names(object@info) == TRUE){
                
                unlist(object@info[info.object])
                
              }else{
                
                ##grep names
                temp.element.names <- paste(names(object@info), collapse = ", ")
                
                stop.text <- paste("[get_RLum.Data.Spectrum] Error: Invalid element name. Valid names are:", temp.element.names)
                
                stop(stop.text)  
                
              }
              
              
            }else{
              
              object@data
              
            } 
          }) 
