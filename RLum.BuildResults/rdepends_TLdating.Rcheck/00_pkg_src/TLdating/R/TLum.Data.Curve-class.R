#' Class \code{"TLum.Data.Curve"}
#'
#' Class for luminescence curve data.
#'
#'
#' @name TLum.Data.Curve-class
#' @rdname TLum.Data.Curve-class
#'
#' @aliases TLum.Data.Curve-class
#' coerce,TLum.Data.Curve-method
#' show,TLum.Data.Curve-method
#' set_TLum.Data.Curve set_TLum.Data.Curve-methods set_TLum.Data.Curve,TLum.Data.Curve-method set_TLum.Data.Curve,ANY-method
#' get_TLum.Data.Curve get_TLum.Data.Curve-methods get_TLum.Data.Curve,ANY-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass TLum.Data.Curve

setClass(Class = "TLum.Data.Curve",
        representation = list(recordType = "character",
                              curveType = "character",
                              temperatures = "numeric",
                              data = "numeric",
                              error= "numeric",
                              metadata = "list",
                              analysis = "list",
                              .RESERVED = "list"
                              ),
         contains = "TLum.Data",
         prototype = list(recordType = character(),
                          curveType = character(),
                          temperatures = vector(mode="numeric"),
                          data = vector(mode="numeric"),
                          error = vector(mode="numeric"),
                          metadata = list(),
                          analysis = list(),
                          .RESERVED = list()
                          )
         )

# Show
setMethod(f = "show",
          signature = "TLum.Data.Curve",
          definition = function(object){

            cat("\n [TLum.Data.Curve]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t measured values:", length(object@data))
            cat("\n\t .. range of temperatures:", range(object@temperatures))
            cat("\n\t .. range of values:", range(object@data))
            cat("\n\t .. range of uncertainties:", range(object@error))
            cat("\n\t additional information:", length(object@metadata))
            cat("\n\t additional analysis data:", length(object@analysis))

          })

# set
setGeneric(name = "set_TLum.Data.Curve",
           def = function(recordType, curveType, temperatures, data, error, metadata, analysis, .RESERVED) {standardGeneric("set_TLum.Data.Curve")}
           )

setMethod(f="set_TLum.Data.Curve",
          signature = c(recordType = "ANY",
                        curveType = "ANY",
                        temperatures= "ANY",
                        data = "ANY",
                        error = "ANY",
                        metadata = "ANY",
                        analysis = "ANY",
                        .RESERVED = "ANY"
                        ),
          definition = function(recordType, curveType, temperatures, data, error, metadata, analysis,  .RESERVED){

            if(missing(recordType)){
              stop("[set_TLum.Data.Curve] Error: Input 'recordType' is missing.")
            }else if(!is.character(recordType)){
              stop("[set_TLum.Data.Curve] Error: Input 'recordType' is not of type 'character'.")
            }

            if(missing(curveType)){
              curveType <- "NA"
            }else if(!is.character(curveType)){
              stop("[set_TLum.Data.Curve] Error: Input 'curveType' is not of type 'character'.")
            }

            if(missing(temperatures)){
              stop("[set_TLum.Data.Curve] Error: Input 'temperatures' is missing.")
            }else if(!is.numeric(temperatures)){
              stop("[set_TLum.Data.Curve] Error: Input 'temperatures' is not of type 'numeric'.")
            }

            if(missing(data)){
              stop("[set_TLum.Data.Curve] Error: Input 'data' is missing.")
            }else if(!is.numeric(data)){
              stop("[set_TLum.Data.Curve] Error: Input 'data' is not of type 'numeric'.")
            }

            if(missing(error)){
              stop("[set_TLum.Data.Curve] Error: Input 'error' is missing.")
            }else if(!is.numeric(error)){
              stop("[set_TLum.Data.Curve] Error: Input 'error' is not of type 'numeric'.")
            }

            if(missing(metadata)){
              metadata <- list()
            }else if(!is.list(metadata)){
              stop("[set_TLum.Data.Curve] Error: Input 'metadata' is not of type 'list'.")
            }
            if(length(metadata) < 1 ){
              warning("[set_TLum.Data.Curve] Warning: Input 'metadata' is empty.")
            }

            if(missing(analysis)){
              analysis <- list()
            }else if(!is.list(analysis)){
              stop("[set_TLum.Data.Curve] Error: Input 'analysis' is not of type 'list'.")
            }

            if(missing(.RESERVED)){
              .RESERVED <- list()
            }else if(!is.list(.RESERVED)){
              stop("[set_TLum.Data.Curve] Error: Input '.RESERVED' is not of type 'list'.")
            }

            new(Class = "TLum.Data.Curve",
                recordType=recordType,
                curveType=curveType,
                temperatures=temperatures,
                data=data,
                error=error,
                metadata=metadata,
                analysis=analysis,
                .RESERVED = .RESERVED)
          })

#get

setGeneric(name = "get_TLum.Data.Curve",
           def = function(object, ref) {standardGeneric("get_TLum.Data.Curve")}
           )

setMethod(f = "get_TLum.Data.Curve",
          signature = c(object="ANY",
                        ref = "ANY"),
          definition = function(object, ref){

            if(!is(object, "TLum.Data.Curve")){
              stop("[get_TLum.Data.Curve] Error: Function valids for 'TLum.Data.Curve' objects only!")
            }

            if(missing(ref)){
              res <- object@data

            }else{
              if(!is.character(ref)){
                stop("[get_TLum.Data.Curve] Error: Input 'ref' is not of type 'character'.")
              }

              if(ref %in% names(object@metadata)){
                res <- unlist(object@metadata[ref])

              }else if(ref %in% names(object@analysis)){
                res <- unlist(object@analysis[ref])

              }else if(ref == "data" || ref == "values"){
                res <- object@data

              }else if(ref == "error" || ref == "uncertainty"){
                res <- object@error

              }else if(ref == "temperatures"){
                res <- object@temperatures

              }else if(ref == "metadata"){
                res <- object@metadata

              }else if(ref == "analysis"){
                res <- object@analysis

              }else if(ref== ".RESERVED"){
                res <- object@.RESERVED

              }else{
                stop("[get_TLum.Data.Curve] Error: Input 'ref' is unknown.")
              }

              return(res)
            }
          })
