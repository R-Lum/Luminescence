##//////////////////////////////////////////////////////////////////////////////
##//RisoeBINfileData-class.R
##//////////////////////////////////////////////////////////////////////////////

# ===========================================================================
##author<<
## Sebastian Kreutzer, IRAMAT-CPR2A, Universite Bordeaux Montaigne (France), \cr
##
## version 0.4.0
# ===========================================================================

setClass("Risoe.BINfileData",
         representation(
           METADATA="data.frame",
           DATA = "list",
           .RESERVED = "list"
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
            cat("\n[Risoe.BINfileData object]")
            cat("\n\n\tBIN/BINX version     ", version)
            if(version>=6){
              cat("\n\tFile name:           ", filename)
            }
            cat("\n\tObject date:         ", date)
            cat("\n\tUser:                ", user)
            cat("\n\tSystem ID:           ", ifelse(systemID == 0,"0 (unknown)", systemID))
            cat("\n\tOverall records:     ", records.overall)
            cat("\n\tRecords type:        ", records.type.count)
            cat("\n\tPosition range:      ",pos.range[1],":",pos.range[2])
            cat("\n\tRun range:           ",run.range[1],":",run.range[2])
            cat("\n\tSet range:           ",set.range[1],":",set.range[2])
          }#end function
          )#end setMethod


# constructor (set) method for object class -----------------------------------

setGeneric("set_Risoe.BINfileData",
           function(METADATA, DATA,.RESERVED) {
             standardGeneric("set_Risoe.BINfileData")
           })


setMethod("set_Risoe.BINfileData",
          signature = c(
            METADATA = "data.frame", DATA = "list", .RESERVED = "ANY"
          ),

          function(METADATA, DATA, .RESERVED) {
            if (missing(.RESERVED)) {
              .RESERVED <- list()
            }

            new(
              "Risoe.BINfileData",
              METADATA = METADATA,
              DATA = DATA,
              .RESERVED = .RESERVED
            )

          })


# accessor (get) method for object class -----------------------------------

setGeneric("get_Risoe.BINfileData",
           function(object) {standardGeneric("get_Risoe.BINfileData")})


setMethod("get_Risoe.BINfileData",
          signature=signature(object = "Risoe.BINfileData"),
          definition = function(object) {

            cat("[get_Risoe.BINfileData()]: No direct access is provided for this object type. Use the function 'Risoe.BINfileData2RLum.Analysis' for object coercing.")

          })##end setMethod

##-------------------------------------------------------------------------------------------------##
##=================================================================================================##
