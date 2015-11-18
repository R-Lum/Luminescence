#' @include get_RLum.R set_RLum.R length_RLum.R structure_RLum.R names_RLum.R
NULL

#' Class \code{"RLum.Analysis"}
#'
#' Object class containing analysis data for protocol analysis.
#'
#'
#' @name RLum.Analysis-class
#'
#' @docType class
#'
#' @slot records Object of class "list" containing objects of class RLum.Data
#'
#' @slot protocol Object of class "character" describing the applied measurement protocol
#'
#' @note The method \code{get_structure} is currently just
#' avaiblable for objects containing \code{\linkS4class{RLum.Data.Curve}}.
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Analysis", ...)}.
#'
#' @section Class version: 0.2.1
#'
#' @return
#'
#' \bold{\code{set_RLum}}:\cr
#'
#' Returns an \code{\linkS4class{RLum.Analysis}} object.
#'
#' \bold{\code{get_RLum}}:\cr
#'
#' Returns: \cr
#' (1) \code{\link{list}} of \code{\linkS4class{RLum.Data}} objects or \cr
#' (2) Single \code{\linkS4class{RLum.Data}} object, if only one object is contained and \code{recursive = FALSE} or\cr
#' (3) \code{\linkS4class{RLum.Analysis}} ojects for \code{drop = FALSE} \cr
#'
#' \bold{\code{merge_RLum}}:\cr
#'
#' Returns an \code{\linkS4class{RLum.Analysis}} object.
#'
#' \bold{\code{structure_RLum}}:\cr
#'
#' Returns \code{\linkS4class{data.frame}} showing the structure.
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso \code{\link{Risoe.BINfileData2RLum.Analysis}},
#' \code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum}}
#'
#' @keywords classes methods
#'
#' @examples
#'
#' showClass("RLum.Analysis")
#'
#' ## usage of get_RLum() with returning an RLum.Analysis object
#' #  get_RLum(object, drop = TRUE)
#'
#' @export
setClass("RLum.Analysis",
         slots = list(
           records = "list",
           protocol = "character"
         ),
         contains = "RLum",
         prototype = list (
           records = list(),
           protocol = character()
         )
)


##----------------------------------------------
##COERCE FROM AND TO list
setAs("list", "RLum.Analysis",
      function(from,to){

        new(to,
            protocol = "unknown",
            records = from)
      })

setAs("RLum.Analysis", "list",
      function(from){

        lapply(1:length(from@records), function(x){
          from@records[[x]]

        })

      })


# show method for object -------------------------------------------------------

#' @describeIn RLum.Analysis
#' Show structure of RLum and Risoe.BINfile class objects
#' @export
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

#' @describeIn RLum.Analysis
#' Method to show the structure of an \code{\linkS4class{RLum.Analysis}} object.
#'
#' @param object [\code{structure_RLum}] an object of class \code{\linkS4class{RLum.Analysis}} (\bold{required})
#'
#' @export
setMethod("structure_RLum",
          signature= "RLum.Analysis",
          definition = function(object) {

            ##check if the object containing other elements than allowed
            if(length(grep(FALSE, sapply(object@records, is, class="RLum.Data.Curve")))!=0){

              stop("[structure_RLum()]  Only 'RLum.Data.Curve' objects are allowed!" )

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
            return(
              data.frame(
                id = temp.id, recordType = temp.recordType,
                protocol.step = temp.protocol.step,
                n.channels = temp.n.channels,
                x.min = temp.x.min,
                x.max = temp.x.max,
                y.min = temp.y.min,
                y.max = temp.y.max,
                info.elements = temp.info.elements
              )
            )

          })


# constructor (set) method for object  ------------------------------------------

#' @describeIn RLum.Analysis
#' Construction method for \code{\linkS4class{RLum.Analysis}} objects.
#'
#' @param class [\code{set_RLum}] \code{\link{character}} (\bold{required}): name of the \code{RLum} class to be created
#'
#' @param records [\code{set_RLum}] \code{\link{list}} (\bold{required}): list of \code{\linkS4class{RLum.Analysis}} objects
#'
#' @param protocol [\code{set_RLum}] \code{\link{character}} (optional): sets protocol type for
#' analysis object. Value may be used by subsequent analysis functions. \code{UNKNOWN}
#' by default.
#'
#' @export
setMethod("set_RLum",
          signature = "RLum.Analysis",

          definition = function(class, records, protocol) {

            if(missing(protocol)){

              protocol <- "UNKNOWN"

            }else if (is(protocol, "character") == FALSE){

              stop("[set_RLum] 'protocol' has to be of type 'character'!")

            }

            new("RLum.Analysis",
                records = records,
                protocol = protocol
            )

          })

# constructor (set) method for object class ------------------------------------------

#' @describeIn RLum.Analysis
#' Accessor method for RLum.Analysis object.
#'
#' The slots record.id, recordType, curveType and RLum.type are optional to allow for records
#' limited by their id (list index number), their record type (e.g. recordType = "OSL")
#' or object type.
#'
#' Example: curve type (e.g. curveType = "predefined" or curveType ="measured")
#'
#' The selection of a specific RLum.type object superimposes the default selection.
#' Currently supported objects are: RLum.Data.Curve and RLum.Data.Spectrum
#'
#' @param record.id [\code{get_RLum}] \code{\link{numeric}} (optional): IDs of specific records
#'
#' @param recordType [\code{get_RLum}] \code{\link{character}} (optional): record type (e.g. "OSL")
#'
#' @param curveType [\code{get_RLum}] \code{\link{character}} (optional): curve type (e.g. "predefined" or "measured")
#'
#' @param RLum.type [\code{get_RLum}] \code{\link{character}} (optional): RLum object type. Defaults to "RLum.Data.Curve"
#' and "RLum.Data.Spectrum".
#'
#' @param subset [\code{get_RLum}] \code{\link{list}}: currently not used.
#'
#' @param get.index [\code{get_RLum}] \code{\link{logical}} (optional): return a numeric vector with the index of each
#' element in the RLum.Analysis object.
#'
#' @param recursive [\code{get_RLum}] \code{\link{logical}} (with default): if \code{TRUE} (the default)
#' and the result of the 'get_RLum' request is a single object this object will be unlisted, means
#' only the object itself and no list containing exactly one object is returned. Mostly this makes things
#' easier, however, if this method is used within a loop this might undesired.
#'
#' @param drop [\code{get_RLum}] \code{\link{logical}} (with default): coerce to the next possible layer
#' (which are \code{RLum.Data}-objects), \code{drop = FALSE} keeps the original \code{RLum.Analysis}
#'
#' @export
setMethod("get_RLum",
          signature = ("RLum.Analysis"),

          function(object, record.id, recordType, curveType, RLum.type,
                   protocol = "UNKNOWN", subset, get.index, drop = TRUE, recursive = TRUE){

            ##record.id
            if (missing(record.id)) {
              record.id <- c(1:length(object@records))

            }else if (!is(record.id, "numeric")) {
              stop("[get_RLum()] 'record.id' has to be of type 'numeric'!")

            }

            ##check if record.id exists
            if (FALSE %in% (abs(record.id) %in% (1:length(object@records)))) {
              stop("[get_RLum()] At least one 'record.id' is invalid!")

            }

            ##recordType
            if (missing(recordType)) {
              recordType <- unique(unlist(lapply(1:length(object@records),
                                                 function(x) {
                                                   object@records[[x]]@recordType
                                                 })))

            }else{
              if (!is(recordType, "character")) {
                stop("[get_RLum()] 'recordType' has to be of type 'character'!")

              }

            }

            ##curveType
            if(missing(curveType)) {
              curveType <- unique(unlist(lapply(1:length(object@records),
                                                function(x) {
                                                  object@records[[x]]@curveType
                                                })))

            }else if (!is(curveType, "character")) {
              stop("[get_RLum()] 'curveType' has to be of type 'character'!")

            }

            ##RLum.type
            if (missing(RLum.type)) {
              RLum.type <- c("RLum.Data.Curve","RLum.Data.Spectrum")

            }else if (!is(RLum.type, "character")) {
              stop("[get_RLum()] 'RLum.type' has to be of type 'character'!")

            }

            ##get.index
            if (missing(get.index)) {
              get.index <- FALSE

            }else if (!is(get.index, "logical")) {
              stop("[get_RLum()] 'get.index' has to be of type 'logical'!")

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

                      if(!get.index){

                          object@records[[x]]

                      }else{
                        x
                      }

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

              ##check if the produced object is empty and show warning message
              if(length(temp) == 0){

                warning("[get_RLum] This request has produced an empty 'RLum.Analysis' object!")

              }

              ##remove list for get.index
              if (get.index) {
                return(unlist(temp))

              }else{
                if (!drop) {
                  temp <- set_RLum(class = "RLum.Analysis",
                                   records = temp,
                                   protocol = object@protocol)
                  return(temp)

                }else{
                  if (length(temp) == 1 & recursive == TRUE) {
                    return(temp[[1]])

                  }else{
                    return(temp)

                  }

                }

              }

            }else{

              if(get.index == FALSE){


                if(drop == FALSE){

                  ##needed to keep the argument drop == TRUE
                  temp <- set_RLum(class = "RLum.Analysis",
                                   records = list(object@records[[record.id]]),
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

# length method for object class ------------------------------------------

#' @describeIn RLum.Analysis
#' Returns the length of the object, i.e., number of stored records.
#'
#' @export
setMethod("length_RLum",
          "RLum.Analysis",
          function(object){

            length(object@records)

          })

# names method for object class ------------------------------------------


#' @describeIn RLum.Analysis
#' Returns the names of the \code{\linkS4class{RLum.Data}} objects objects (same as shown with the show method)
#'
#' @export
setMethod("names_RLum",
          "RLum.Analysis",
          function(object){
            sapply(1:length(object@records), function(x){
              object@records[[x]]@recordType})

          })


