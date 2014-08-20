##//////////////////////////////////////////////////////////////////////////////
##//RisoeBINfileData-class.R
##//////////////////////////////////////////////////////////////////////////////

# ===========================================================================
##author<<
## Sebastian Kreutzer, JLU Giessen (Germany), \cr
##
## version 0.3.1
# ===========================================================================

setClass("Risoe.BINfileData",
         representation(
           METADATA="data.frame",
           DATA = "list"           
           ),
         S3methods=TRUE
         )

##set generic S4 function for object
setMethod("show", signature(object = "Risoe.BINfileData"),
          function(object){
            
            version<-paste(unique(object@METADATA[,"VERSION"]), collapse = ", ")
            systemID<-paste(unique(object@METADATA[,"SYSTEMID"]), collapse = ", ")
            filename <- as.character(object@METADATA[1,"FNAME"])
            records.overall<-length(object@DATA)
            records.type<-table(object@METADATA[,"LTYPE"])
            user<-paste(unique(as.character(object@METADATA[,"USER"])), collapse = ", ")
            date<-paste(unique(as.character(object@METADATA[,"DATE"])), collapse = ", ")      
            run.range<-range(object@METADATA[,"RUN"])
            set.range<-range(object@METADATA[,"SET"])
            pos.range<-range(object@METADATA[,"POSITION"])
            
            records.type.count <- sapply(1:length(records.type),
              function(x){paste(
              names(records.type)[x],"\t(n = ",records.type[x],")",sep="")
              })
            
            records.type.count <- paste(records.type.count, 
                                        collapse="\n\t                      ")
 
            ##print
            cat("\nRisoe.BINfileData Object")
            cat("\n\tBIN Format Version:  ", version)
            if(version>=6){
              cat("\n\tFile Name:           ", filename)
            }
            cat("\n\tObject Date:         ", date) 
            cat("\n\tUser:                ", user)
            cat("\n\tSystem ID:           ", systemID)
            cat("\n\tOverall Records:     ", records.overall)
            cat("\n\tRecords Type:        ", records.type.count)
            cat("\n\tPosition Range:      ",pos.range[1],":",pos.range[2])
            cat("\n\tRun Range:           ",run.range[1],":",run.range[2])
            cat("\n\tSet Range:           ",set.range[1],":",set.range[2])
          }#end function          
          )#end setMethod


# constructor (set) method for object class -----------------------------------

setGeneric("set_Risoe.BINfileData",
           function(METADATA, DATA) {standardGeneric("set_Risoe.BINfileData")})


setMethod("set_Risoe.BINfileData", 
          signature = c(METADATA = "data.frame", DATA = "list"), 
          
          function(METADATA, DATA){             
                        
            new("Risoe.BINfileData", 
                METADATA = METADATA,
                DATA = DATA
            )
            
          })


# accessor (get) method for object class -----------------------------------

setGeneric("get_Risoe.BINfileData",
           function(object) {standardGeneric("get_Risoe.BINfileData")})


setMethod("get_Risoe.BINfileData", 
          signature=signature(object = "Risoe.BINfileData"), 
          definition = function(object) {
                     
            cat("[get_Risoe.BINfileData]: No direct access is provided object type. Use the function 'Risoe.BINfileData2RLum.Analysis' for object coercing instead.")
            
          })##end setMethod

##-------------------------------------------------------------------------------------------------##
##=================================================================================================##
