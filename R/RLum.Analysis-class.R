##//////////////////////////////////////////////////////////////////////////////
##//RLum.Analysis-class.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
##version: 0.1.8
##date: 2015-02-03
##==============================================================================

##class definition
setClass("RLum.Analysis",
         representation(
           records = "list",
           protocol = "character"
         ),
         contains = "RLum",
         prototype = list (
           records = list(),
           protocol = character()
         ),
         S3methods = FALSE
)


# show method for object -------------------------------------------------------

  setMethod("show",
            signature(object = "RLum.Analysis"),
             function(object){

              ##print
              cat("\n [RLum.Analysis]")
              cat("\n\t protocol:", object@protocol)
              cat("\n\t number of records:", length(object@records))

              #skip this part if nothing is included in the object
              if(length(object@records) > 0){

                ##get object class types
                temp <- sapply(1:length(object@records), function(x){

                              is(object@records[[x]])[1]

                             })

                ##print object class types
                sapply(1:length(table(temp)), function(x){

                  ##show RLum class type
                  cat("\n\t .. :",names(table(temp)[x]),":",table(temp)[x])


                  ##show structure
                  ##set width option ... just an implementation for the tutorial output
                  ifelse(getOption("width")<=50, temp.width <- 4, temp.width  <- 10)

                  cat("\n\t .. .. : ",
                      unlist(sapply(1:length(object@records),  function(i) {

                        if(names(table(temp)[x]) == is(object@records[[i]])[1]){
                           paste(object@records[[i]]@recordType,
                           if(i%%temp.width==0 & i!=length(object@records)){"\n\t .. .. : "})
                        }
                      })))

              })

            }else{

              cat("\n\t >> This is an empty object and cannot be used for further analysis! <<")

            }


            }
            )##end show method

# get object structure ----------------------------------------------------

##method to show the object structure
setGeneric("get_structure.RLum.Analysis",
           function(object) {standardGeneric("get_structure.RLum.Analysis")})

setMethod("get_structure.RLum.Analysis",
          signature=signature(object = "RLum.Analysis"),
          definition = function(object) {

            ##check if the object containing other elements than allowed
            if(length(grep(FALSE, sapply(object@records, is, class="RLum.Data.Curve")))!=0){

              stop("[get_structure.RLum.Analysis()]  Only 'RLum.Data.Curve' objects are allowed!" )

            }

            ##get length object
            temp.object.length <- length(object@records)

            ##ID
            temp.id <- 1:temp.object.length

            ##OBJECT TYPE
            temp.recordType <- c(NA)
            length(temp.recordType) <- temp.object.length
            temp.recordType <- sapply(1:temp.object.length,
                                      function(x){object@records[[x]]@recordType})

            ##PROTOCOL STEP
            temp.protocol.step <- c(NA)
            length(temp.protocol.step) <- temp.object.length

            ##n.channels
            temp.n.channels <- sapply(1:temp.object.length,
                                      function(x){length(object@records[[x]]@data[,1])})

            ##X.MIN
            temp.x.min <- sapply(1:temp.object.length,
                                 function(x){min(object@records[[x]]@data[,1])})

            ##X.MAX
            temp.x.max <- sapply(1:temp.object.length,
                                 function(x){max(object@records[[x]]@data[,1])})

            ##y.MIN
            temp.y.min <- sapply(1:temp.object.length,
                                 function(x){min(object@records[[x]]@data[,2])})

            ##X.MAX
            temp.y.max <- sapply(1:temp.object.length,
                                 function(x){max(object@records[[x]]@data[,2])})

            ##info elements as character value
            temp.info.elements <- unlist(sapply(1:temp.object.length, function(x){

                  if(length(object@records[[x]]@info)!=0){
                    do.call(paste, as.list(names(object@records[[x]]@info)))
                  }else{NA}

                  }))

            ##combine output to a data.frame
            return(data.frame(id=temp.id, recordType=temp.recordType,
                              protocol.step=temp.protocol.step,
                              n.channels=temp.n.channels,
                              x.min=temp.x.min, x.max=temp.x.max,
                              y.min=temp.y.min, y.max=temp.y.max,
                              info.elements=temp.info.elements))

          })


# constructor (set) method for object class ------------------------------------------

setGeneric("set_RLum.Analysis",
           function(records, protocol) {standardGeneric("set_RLum.Analysis")})


setMethod("set_RLum.Analysis",
          signature = c(records = "list", protocol= "ANY"),

          function(records, protocol){

            if(missing(protocol)){

              protocol <- "UNKNOWN"

            }else if (is(protocol, "character") == FALSE){

              stop("[set_RLum.Analysis] Error: 'protocol' has to be of type 'charcter'!")

            }

            new("RLum.Analysis",
                records = records,
                protocol = protocol
               )

          })

# constructor (set) method for object class ------------------------------------------

setGeneric("get_RLum.Analysis",
           function(object, record.id, recordType, curveType, RLum.type, info.object, get.index, keep.object = FALSE) {
             standardGeneric("get_RLum.Analysis")})


setMethod("get_RLum.Analysis",
          signature = c(object = "RLum.Analysis",
                        record.id = "ANY",
                        recordType = "ANY",
                        curveType = "ANY",
                        RLum.type = "ANY",
                        info.object = "ANY",
                        get.index = "ANY",
                        keep.object = "ANY"),

          function(object, record.id, recordType, curveType, RLum.type, info.object, get.index, keep.object = FALSE){

            ##record.id
            if(missing(record.id) == TRUE){

              record.id <- c(1:length(object@records))

            }else if (is(record.id, "numeric") == FALSE){

              stop("[get_RLum.Analysis()] 'record.id' has to be of type 'numeric'!")

            }

            ##check if record.id exists
            if(FALSE%in%(abs(record.id)%in%(1:length(object@records)))){

              stop("[get_RLum.Analysis()] At least one 'record.id' is invalid!")

            }

            ##recordType
            if(missing(recordType) == TRUE){

              recordType <- unique(
                              unlist(
                                lapply(1:length(object@records),
                                       function(x){object@records[[x]]@recordType})))

            }else{

              if (is(recordType, "character") == FALSE){

                stop("[get_RLum.Analysis()] Error: 'recordType' has to be of type 'character'!")

              }

            }

            ##curveType
            if(missing(curveType) == TRUE){

              curveType <- unique(
                unlist(
                  lapply(1:length(object@records),
                         function(x){object@records[[x]]@curveType})))

            }else if (is(curveType, "character") == FALSE){

              stop("[get_RLum.Analysis()] Error: 'curveType' has to be of type 'character'!")

            }

            ##RLum.type
            if(missing(RLum.type) == TRUE){

              RLum.type <- c("RLum.Data.Curve","RLum.Data.Spectrum")

            }else if (is(RLum.type, "character") == FALSE){

              stop("[get_RLum.Analysis()] Error: 'RLum.type' has to be of type 'character'!")

            }

            ##get.index
            if(missing(get.index) == TRUE){

             get.index <- FALSE

            }else if (is(get.index, "logical") == FALSE){

              stop("[get_RLum.Analysis()] Error: 'get.index' has to be of type 'logical'!")

            }




           ##-----------------------------------------------------------------##

           ##a pre-selection is necessary to support negative index selection
           object@records <- object@records[record.id]
           record.id <- 1:length(object@records)


           ##select curves according to the chosen parameter
           if(length(record.id)>1){

            temp <- sapply(record.id, function(x){

                if(is(object@records[[x]])[1]%in%RLum.type == TRUE){

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

              warning("This request has produced an empty 'RLum.Analysis' object!")

            }

            ##remove list for get.index
            if(get.index == TRUE){

              return(unlist(temp))

            }else{

              if(keep.object == TRUE){

                temp <- set_RLum.Analysis(records = temp, protocol = object@protocol)
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
                 temp <- set_RLum.Analysis(records = list(object@records[[record.id]]),
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

setGeneric("length_RLum.Analysis",
           function(object) {
             standardGeneric("length_RLum.Analysis")})

setMethod("length_RLum.Analysis",
          signature(object = "RLum.Analysis"),
          function(object){

            length(object@records)

          })
