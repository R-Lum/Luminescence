#' Class \code{"TLum.Analysis"}
#'
#' Object class containing analysis data for protocol analysis.
#'
#'
#' @name TLum.Analysis-class
#' @rdname TLum.Analysis-class
#'
#' @aliases TLum.Analysis-class
#' show,TLum.Analysis-method
#' set_TLum.Analysis set_TLum.Analysis,TLum.Analysis-method set_TLum.Analysis,list-method
#' get_TLum.Analysis get_TLum.Analysis-methods get_TLum.Analysis,TLum.Analysis-method get_structure.TLum.Analysis get_structure.TLum.Analysis,TLum.Analysis-method length_TLum.Analysis
#' length_TLum.Analysis-methods length_TLum.Analysis,TLum.Analysis-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass TLum.Analysis


# class definition
setClass("TLum.Analysis",
         representation(
           records = "list",
           protocol = "character"
         ),
         contains = "TLum",
         prototype = list (
           records = list(),
           protocol = character()
         ),
         S3methods = FALSE
)


# show method for object -------------------------------------------------------

setMethod("show",
          signature(object = "TLum.Analysis"),
          function(object){

            protocol <- object@protocol
            nRecords <- length(object@records)

            ##print
            cat("\n [TLum.Analysis]")
            cat("\n\t protocol:", protocol)
            cat("\n\t number of records:", nRecords)

            #skip this part if nothing is included in the object
            if(nRecords > 0){

              ##get object class types

              class.type <- vector()
              recordType <- vector()
              for (i in 1:nRecords){
                class.type[i] <- is(object@records[[i]])[1]
                recordType[i] <- as.character(object@records[[i]]@recordType)
              }

              table.class <- table(class.type)

              for(i in 1:length(table.class)){
                cat("\n\t .. :",names(table.class)[i],":",table.class[i])

                temp <- NULL
                k <- 1

                for(j in 1:nRecords){
                  if(names(table.class)[i] == class.type[j]){
                    temp <- paste(temp, recordType[i])
                    k <- k+1

                    if(k>10){
                      cat("\n\t .. :", temp)
                      temp <- NULL
                      k <- 1
                    }
                  }
                }

                if(!is.null(temp)){
                  cat("\n\t .. :", temp)
                  temp <- NULL
                }
              }

            }else{

              cat("\n\t >> This is an empty object and cannot be used for further analysis! <<")

            }
          }
)##end show method

# get object structure ----------------------------------------------------

##method to show the object structure
setGeneric("get_structure.TLum.Analysis",
           function(object) {standardGeneric("get_structure.TLum.Analysis")})

setMethod("get_structure.TLum.Analysis",
          signature=signature(object = "TLum.Analysis"),
          definition = function(object) {

            ##check if the object containing other elements than allowed
            if(length(grep(FALSE, sapply(object@records, is, class="TLum.Data.Curve")))!=0){

              stop("[get_structure.TLum.Analysis()]  Only 'TLum.Data.Curve' objects are allowed!" )

            }

            ##get length object
            nRecords <- length(object@records)

            id <- vector()
            nChannels <- vector()
            recordTypes <- vector()
            Tmax <- vector()
            Tmin <- vector()

            Smax <- vector()
            Smin <- vector()

            info.elements <- list()

            for (i in 1:nRecords){
              temp.curve <- object@records[[i]]

              temp.recordType <- temp.curve@recordType
              temp.metadata <- temp.curve@metadata
              temp.data <- temp.curve@data
              temp.temperature <- temp.curve@temperatures

              id[i] <- temp.metadata$ID
              nChannels[i] <- length(temp.data)
              recordTypes[i] <- temp.recordType

              Tmax[i] <- max(temp.temperature)
              Tmin[i] <- min(temp.temperature)

              Smax[i] <- max(temp.data)
              Smin[i] <- min(temp.data)

              temp.info.elements <- list(names(temp.metadata))

              info.elements <- c(info.elements,temp.info.elements)


            }

            ##combine output to a data.frame
            return(data.frame(id=id,
                              recordType=recordTypes,
                              n.channels=nChannels,
                              x.min=Tmin, x.max=Tmax,
                              y.min=Smin, y.max=Smax,
                              info.elements=info.elements))

          })


# constructor (set) method for object class ------------------------------------------

setGeneric("set_TLum.Analysis",
           function(records, protocol) {standardGeneric("set_TLum.Analysis")})


setMethod(f = "set_TLum.Analysis",
          signature = c(records = "list",
                        protocol= "ANY"),

          definition = function(records, protocol){
            if(missing(protocol)){
              protocol <- "UNKNOWN"

            }else if (is(protocol, "character") == FALSE){
              stop("[set_TLum.Analysis] Error: 'protocol' has to be of type 'character'!")
            }

            new("TLum.Analysis",
                protocol = protocol,
                records = records
            )
          })

# constructor (set) method for object class ------------------------------------------

setGeneric("get_TLum.Analysis",
           function(object, record.id, recordType, curveType, TLum.type, info.object, get.index, keep.object = FALSE) {
             standardGeneric("get_TLum.Analysis")})


setMethod("get_TLum.Analysis",
          signature = c(object = "TLum.Analysis",
                        record.id = "ANY",
                        recordType = "ANY",
                        curveType = "ANY",
                        TLum.type = "ANY",
                        info.object = "ANY",
                        get.index = "ANY",
                        keep.object = "ANY"),

          function(object, record.id, recordType, curveType, TLum.type, info.object, get.index, keep.object = FALSE){

            ##record.id
            if(missing(record.id)){

              record.id <- c(1:length(object@records))

            }else if (is(record.id, "numeric") == FALSE){

              stop("[get_TLum.Analysis()] 'record.id' has to be of type 'numeric'!")

            }

            ##check if record.id exists
            if(FALSE%in%(abs(record.id)%in%(1:length(object@records)))){

              stop("[get_TLum.Analysis()] At least one 'record.id' is invalid!")

            }

            ##recordType
            if(missing(recordType)){

              recordType <- unique(
                unlist(
                  lapply(1:length(object@records),
                         function(x){object@records[[x]]@recordType})))

            }else{

              if (is(recordType, "character") == FALSE){

                stop("[get_TLum.Analysis()] Error: 'recordType' has to be of type 'character'!")

              }

            }

            ##curveType
            if(missing(curveType) == TRUE){

              curveType <- unique(
                unlist(
                  lapply(1:length(object@records),
                         function(x){object@records[[x]]@curveType})))

            }else if (is(curveType, "character") == FALSE){

              stop("[get_TLum.Analysis()] Error: 'curveType' has to be of type 'character'!")

            }

            ##TLum.type
            if(missing(TLum.type) == TRUE){

              TLum.type <- c("TLum.Data.Curve","TLum.Data.Spectrum")

            }else if (is(TLum.type, "character") == FALSE){

              stop("[get_TLum.Analysis()] Error: 'TLum.type' has to be of type 'character'!")

            }

            ##get.index
            if(missing(get.index) == TRUE){

              get.index <- FALSE

            }else if (is(get.index, "logical") == FALSE){

              stop("[get_TLum.Analysis()] Error: 'get.index' has to be of type 'logical'!")

            }




            ##-----------------------------------------------------------------##

            ##a pre-selection is necessary to support negative index selection
            object@records <- object@records[record.id]
            record.id <- 1:length(object@records)


            ##select curves according to the chosen parameter
            if(length(record.id)>1){

              temp <- sapply(record.id, function(x){

                if(is(object@records[[x]])[1]%in%TLum.type == TRUE){

                  ##as input a vector is allowed
                  temp <- sapply(1:length(recordType), function(k){


                    ##translate input to regular expression
                    recordType[k] <- glob2rx(recordType[k])
                    recordType[k] <- substr(recordType[k],
                                            start = 2,
                                            stop = nchar(recordType[k])-1)

                    if(grepl(recordType[k],object@records[[x]]@recordType) == TRUE &
                       object@records[[x]]@curveType%in%curveType){

                      if(get.index == FALSE){

                        object@records[[x]]

                      }else{x}

                    }

                  })

                  ##remove empty entries and select just one to unlist
                  temp <- temp[!sapply(temp, is.null)]

                  ##if list has length 0 skip entry
                  if(length(temp) != 0){temp[[1]]}else{temp <- NULL}

                }

              })


              ##remove empty list element
              temp <- temp[!sapply(temp, is.null)]

              ##check if produced object is empty and show warning message
              if(length(temp) == 0){

                warning("This request has produced an empty 'TLum.Analysis' object!")

              }

              ##remove list for get.index
              if(get.index == TRUE){

                return(unlist(temp))

              }else{

                if(keep.object == TRUE){

                  temp <- set_TLum.Analysis(records = temp, protocol = object@protocol)
                  return(temp)

                }else{

                  if(length(temp) == 1){

                    return(temp[[1]])

                  }else{

                    return(temp)

                  }

                }

              }

            }else{

              if(get.index == FALSE){


                if(keep.object == TRUE){

                  ##needed to keep the argument keep.object == TRUE
                  temp <- set_TLum.Analysis(records = list(object@records[[record.id]]),
                                            protocol = object@protocol)
                  return(temp)

                }else{

                  return(object@records[[record.id]])

                }


              }else{

                return(record.id)

              }
            }


          })

# constructor (length) method for object class ------------------------------------------

setGeneric("length_TLum.Analysis",
           function(object) {
             standardGeneric("length_TLum.Analysis")})

setMethod("length_TLum.Analysis",
          signature(object = "TLum.Analysis"),
          function(object){

            length(object@records)

          })
