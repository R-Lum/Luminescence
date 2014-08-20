Risoe.BINfileData2RLum.Data.Curve<- structure(function(#Convert an element from a Risoe.BINfileData object to an RLum.Data.Curve object
  ### The function converts one specified single record from a Risoe.BINfileData 
  ### object to an RLum.Data.Curve object. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, Freiberg Instruments/JLU Giessen (Germany), \cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================

  object,
  ### \code{\linkS4class{Risoe.BINfileData}} (\bold{required}): \code{Risoe.BINfileData} object
  
  id,
  ### \code{\link{integer}} (\bold{required}): record id in the \code{Risoe.BINfileData} object
  ### of the curve that is to be stored in the \code{RLum.Data.Curve} object. If no value 
  ### for id is provided, the record has to be specified by \code{pos}, \code{set} and \code{run}.
  
  pos,
  ### \code{\link{integer}} (optional): record position number in the \code{Risoe.BINfileData} object
  ### of the curve that is to be stored in the \code{RLum.Data.Curve} object. If a value
  ### for \code{id} is provided, this argument is ignored.
  
  run,
  ### \code{\link{integer}} (optional): record run number in the \code{Risoe.BINfileData} object
  ### of the curve that is to be stored in the \code{RLum.Data.Curve} object. If a value
  ### for \code{id} is provided, this argument is ignored.
  
  set
  ### \code{\link{integer}} (optional): record set number in the \code{Risoe.BINfileData} object
  ### of the curve that is to be stored in the \code{RLum.Data.Curve} object. If a value
  ### for \code{id} is provided, this argument is ignored.
  
){
  
  
# Integrity Check ---------------------------------------------------------

  if (is(object,"Risoe.BINfileData")==FALSE){
    stop("[Risoe.BINfileData2RLum.Data.Curve] Error: Input object is not of type 'Risoe.BINfileData'.")
  }

  ##if id is set, no input for pos and rund is nescessary
  if(missing(id) == TRUE){
    
   if(missing(pos) == TRUE | missing(run) == TRUE | missing(set) == TRUE){
     
     temp.missing.arguments <- paste(c(if(missing(pos)==TRUE){"pos"},
                                 if(missing(set)==TRUE){"set"},
                                 if(missing(run)==TRUE){"run"}), collapse=", ")
     
     stop(paste("[Risoe.BINfileData2RLum.Data.Curve] Error: Arguments are missing: ",
          temp.missing.arguments, ". Or set id.", sep = "")) 
     
   }
       
   if (is(pos,"numeric")==FALSE){
     stop("[Risoe.BINfileData2RLum.Data.Curve] Error: Argument 'pos' has to be of data type integer.")
    }
   
   if (is(set,"numeric")==FALSE){
     stop("[Risoe.BINfileData2RLum.Data.Curve] Error: Argument 'set' has to be of data type integer.")
   }
   
   if (is(run,"numeric")==FALSE){
     stop("[Risoe.BINfileData2RLum.Data.Curve] Error: Argument 'run' has to be of data type integer.")
   }

    if (length(which(pos/1:48 == 1)) == 0){
     stop("[Risoe.BINfileData2RLum.Data.Curve] Error: Value for 'pos' out of bounds.")
    }

    ##get and check valid positions
    positions.valid <- paste(as.character(unique(object@METADATA[,"POSITION"])), collapse=", ")

    if ((pos %in% unique(object@METADATA[,"POSITION"])) == FALSE){
     stop(paste("[Risoe.BINfileData2RLum.Data.Curve] Error: pos = ",pos, " is not valid. 
               Valid positions are: ", positions.valid, sep=""))
    }
   
   ##get and check valid positions
   positions.valid <- paste(as.character(unique(object@METADATA[,"SET"])), collapse=", ")
   
   if ((set %in% unique(object@METADATA[,"SET"])) == FALSE){
     stop(paste("[Risoe.BINfileData2RLum.Data.Curve] Error: set = ",set, " is not valid. 
               Valid values are: ", positions.valid, sep=""))
   }
   
   
   ##get and check valid positions
   positions.valid <- paste(as.character(unique(object@METADATA[,"RUN"])), collapse=", ")
   
   if ((run %in% unique(object@METADATA[,"RUN"])) == FALSE){
     stop(paste("[Risoe.BINfileData2RLum.Data.Curve] Error: run = ",run, " is not valid. 
               Valid values are: ", positions.valid, sep=""))
   }
   
   }else{
    
    ##check if id is valid
    temp.range.id <- range(object@METADATA[,"ID"])
   
    if ((id %in% unique(object@METADATA[,"ID"])) == FALSE){
      stop(paste("[Risoe.BINfileData2RLum.Data.Curve] Error: id = ",id, " is not a valid record id. Allowed value range ", min(temp.range.id), " : ", max(temp.range.id),".", sep=""))
      
    }
    
  }


# grep id of record -------------------------------------------------------

    ##if id is set, no input for pos and rund is nescessary
    if(missing(id) == TRUE){
     
      id <- object@METADATA[object@METADATA[,"POSITION"] == pos & 
                            object@METADATA[,"SET"] == set & 
                            object@METADATA[,"RUN"] == run,                             
                            "ID"]
      
      
     }


# Select values -----------------------------------------------------------

  ##build matrix
  temp.x <- seq(object@METADATA[id,"HIGH"]/object@METADATA[id,"NPOINTS"],
                object@METADATA[id,"HIGH"],
                by=object@METADATA[id,"HIGH"]/object@METADATA[id,"NPOINTS"])

  temp.y <- unlist(object@DATA[id])


  temp.data <- matrix(c(temp.x,temp.y), ncol=2, byrow=FALSE)
  temp.recordType <- as.character(object@METADATA[id,"LTYPE"])
  temp.info <- as.list(object@METADATA[id,])  


# Build object ------------------------------------------------------------

  newRLumDataCurve.Risoe.BINfileData2RLum.Data.Curve <- set_RLum.Data.Curve(
    recordType = temp.recordType,
    data = temp.data,
    info = temp.info)
    
  return(newRLumDataCurve.Risoe.BINfileData2RLum.Data.Curve) 

  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------

  ##details<<
  ## The function extracts all \code{METADATA} from the \code{Risoe.BINfileData} 
  ## object and stores them in the \code{RLum.Data.Curve} object.

  ##value<<
  ## Returns an \code{\linkS4class{RLum.Data.Curve}} object. 

  ##references<<
  ## #

  ##note<<
  ## The function is intended for experimental usage. Normally, the function 
  ## \code{\link{Risoe.BINfileData2RLum.Analysis}} should be used for the conversion.

  ##seealso<<
  ## \code{\link{Risoe.BINfileData2RLum.Analysis}}, \code{\link{set_RLum.Data.Curve}},
  ## \code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Analysis}}, 
  ## \code{\linkS4class{Risoe.BINfileData}}, \code{\link{plot_RLum}}

  ##keyword<<
  ## manip

}, ex=function(){
  
  ##get package example data
  data(ExampleData.BINfileData, envir = environment())
  
  ##convert one record
  Risoe.BINfileData2RLum.Data.Curve(CWOSL.SAR.Data, id = 1)
  
})#END OF STRUCTURE 
