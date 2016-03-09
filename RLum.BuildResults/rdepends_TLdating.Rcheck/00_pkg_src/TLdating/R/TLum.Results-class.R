#' Class \code{"TLum.Results"}
#'
#' Object class contains results data from functions.
#'
#'
#' @name TLum.Results-class
#' @rdname TLum.Results-class
#'
#' @aliases TLum.Results-class
#' show,TLum.Results-method
#' set_TLum.Results set_TLum.Results,TLum.Results-method set_TLum.Results,ANY,list-method
#' get_TLum.Results get_TLum.Results,TLum.Results-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass TLum.Results


##class definition
setClass("TLum.Results",
         representation(originator = "character",
                        data = "list"
                        ),
         contains = "TLum",
         prototype = list (
           originator = character(),
           data = list()
         ),
         S3methods=TRUE
)


# show method for object ------------------------------------------------------

setMethod("show",
          signature(object = "TLum.Results"),
          function(object){


            ##data elements
            temp.names <- names(object@data)
            temp.type <- sapply(1:length(object@data),
                                function(x){

                                  paste("\t .. $", temp.names[x],
                                        " : ",
                                        is(object@data[[x]])[1],
                                        sep = "")


                                })
            temp.type <- paste(temp.type, collapse="\n")

            ##print information
            cat("\n [TLum.Results]")
            cat("\n\t originator: ", object@originator,"()", sep="")
            cat("\n\t data:", length(object@data))
            cat("\n", temp.type)


          }
)



# constructor (set) method for object class -------------------------------

setGeneric("set_TLum.Results",
           function(originator, data) {standardGeneric("set_TLum.Results")})

setMethod(f = "set_TLum.Results",
          signature = c(originator = "ANY",
                        data = "list"),

          definition = function(originator, data){

            if(missing(originator)){
              originator <- "Unknown"

            }else if(!is.character(originator)){
              stop("[set_TLum.Results] Error: 'originator' is not of type 'character'.")
            }

            if(missing(data)){
              stop("[set_TLum.Results] Error: 'data' is missing.")
            }

            new("TLum.Results",
                originator = originator,
                data = data)
          })


# GetMethods --------------------------------------------------------------


setGeneric("get_TLum.Results",
           function(object, ref) {standardGeneric("get_TLum.Results")})

setMethod("get_TLum.Results",
          signature=signature(object = "TLum.Results",
                              ref = "ANY"),
          definition = function(object, ref) {

            if(missing(ref)){
              res <- object@data

            }else if(!is.character(ref)){
                stop("[get_TLum.Results] Error: 'ref' has to be a character!")

            }else if(ref %in% names(object@data)){
              res <- object@data[[ref]]

            }else if(ref == "data"){
              res <- object@data

            }else if(ref == "originator"){
              res <- object@originator

            }else{
              stop("[get_TLum.Results] Error: Input 'ref' is unknown.")
            }

            return(res)
          })
