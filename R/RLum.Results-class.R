#' @include get_RLum.R set_RLum.R merge_RLum.R
NULL

#' Class \code{"RLum.Results"}
#'
#' Object class contains results data from functions.
#'
#' @name RLum.Results-class
#'
#' @docType class
#'
#' @slot originator Object of class "character" containing name of the producing function
#'
#' @slot data Object of class "list" containing output data
#'
#' @note The class is intended to store results from functions to be used by
#' other functions. The data in the object should always be accessed by the
#' method \code{get_RLum}.
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Results", ...)}.
#'
#' @section Class version: 0.3.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso \code{\linkS4class{RLum}}
#'
#' @keywords classes methods
#'
#' @examples
#'
#' showClass("RLum.Results")
#'
#' @export
setClass(
  "RLum.Results",
  slots = list(originator = "character",
               data = "list"),
  contains = "RLum",
  prototype = list (originator = character(),
                    data = list())
)


# Validation --------------------------------------------------------------

##Not used currenlty
# setValidity("RLum.Results",
#             function(object){
#
#               ##calc_OSLLxTxRatio
#               if(object@originator == "calc_OSLLxTxRatio"){
#
#                 #print(is(object@data[[1]], "data.frame"))
#
#               }
#
#
#             }
# )


# show method for object ------------------------------------------------------
#' @describeIn RLum.Results
#' Show structure of RLum and Risoe.BINfile class objects
#' @export
setMethod("show",
          signature(object = "RLum.Results"),
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
            cat("\n [RLum.Results]")
            cat("\n\t originator: ", object@originator,"()", sep="")
            cat("\n\t data:", length(object@data))
            cat("\n", temp.type)


          }
)



# constructor (set) method for object class -------------------------------

#' @describeIn RLum.Results
#' Construction method for RLum.Results object. The slot originator is optional
#' and predefined as the function that calls the function set_RLum.
#'
#' @param class [\code{set_RLum}] \code{\link{character}} (required): name of the \code{RLum} class to create
#' @param originator [\code{set_RLum}] \code{\link{character}} (optional): argument to manually set
#' the originator.
#' @param data [\code{set_RLum}] \code{\link{list}} (optional): a list containing the data to be stored in the object
#'
#' @export
setMethod("set_RLum",
          signature = signature("RLum.Results"),

          function(class, originator, data){

            if(missing(originator) == TRUE){

              ##originator is the calling function (function in which the
              ##function set_RLum is called)
              originator <- as.character(sys.call(which=-3)[[1]])

              # temporary fallback due to the change of set_RLum in 0.5.0:
              # When a user/function calls the deprecated set_RLum.Results()
              # the call stack decreases by 1
              if (originator == "set_RLum.Results")
                originator <- as.character(sys.call(which=-4)[[1]])

            }

            new("RLum.Results",
                originator = originator,
                data = data)
          })


# GetMethods --------------------------------------------------------------


#' @describeIn RLum.Results
#' Accessor method for RLum.Results object. The argument data.object allows
#' directly accessing objects delivered within the slot data. The default
#' return object depends on the object originator (e.g., \code{fit_LMCurve}).
#' If nothing is specified always the first \code{data.object} will be returned.
#'
#' Note: Detailed specification should be made in combination with the originator slot in the
#' receiving function if results are pipped.
#'
#' @param object [\code{get_RLum}] \code{\linkS4class{RLum.Results}} (required): an object of class
#' \code{\linkS4class{RLum.Results}} to be evaluated
#'
#' @param data.object [\code{get_RLum}] \code{\link{character}} or
#' \code{\link{numeric}}: name or index of the data slot to be returned
#'
#' @param drop [\code{get_RLum}] \code{\link{logical}} (with default): coerce to the next possible layer
#' (which are data objects, \code{drop = FALSE} keeps the original \code{RLum.Results}
#'
#' @return
#'
#' \bold{\code{set_RLum}}:\cr
#'
#' Returns an \code{\linkS4class{RLum.Results}} object.  \cr
#'
#' \bold{\code{get_RLum}}:\cr
#'
#' Returns: \cr
#' (1) Data object from the specified slot \cr
#' (2) \code{\link{list}} of data objects from the slots if 'data.object' is vector or \cr
#' (3) an \code{\linkS4class{RLum.Results}} for \code{drop = FALSE}.\cr
#'
#' \bold{\code{merge_RLum}}:\cr
#'
#' Returns an \code{\linkS4class{RLum.Results}} object.
#'
#' @export
setMethod("get_RLum",
          signature = signature("RLum.Results"),
          definition = function(object, data.object, drop = TRUE) {

            if (!missing(data.object)) {

              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##CASE1: data.object is of type 'character'
              if (is(data.object, "character")) {

                #check if the provided names are available
                if (all(data.object %in% names(object@data))) {

                  ##account for multiple inputs
                  if (length(data.object) > 1) {
                    temp.return <- sapply(data.object, function(x){
                      object@data[[x]]

                    })

                  } else{
                    temp.return <- list(data.object = object@data[[data.object]])

                  }


                } else{
                  error.message <- paste0(
                    "[get_RLum()] data.object(s) unknown, valid names are: ",
                    paste(names(object@data), collapse = ", ")

                  )
                  stop(error.message)

                }

              }

              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##CASE2: data.object is of type 'numeric'
              else if (is(data.object, "numeric")) {
                ##check if index is valid
                if (max(data.object) > length(object@data)) {
                  stop("[get_RLum] 'data.object' index out of bounds!")

                } else if (length(data.object) > 1) {
                  temp.return <- lapply(data.object, function(x) {
                    object@data[[x]]

                  })


                } else{
                  temp.return <- list(object@data[[data.object]])

                }

                ##restore names as that get los with this method
                names(temp.return) <- names(object@data)[data.object]

              }
              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##CASE3: data.object is of an unsupported type
              else{
                stop("[get_RLum] 'data.object' has to be of type character or numeric!")
              }

            ##the CASE data.object is missing
            } else{

              ##return always the first object if nothing is specified
              temp.return <- object@data[1]

            }

          ##CHECK whether an RLum.Results object needs to be produced ...
          ##This will just be the case if the funtion havn't returned something before
          if (drop) {
            ##we need to access the list here, otherwise we get unexpected behaviour as drop = TRUE
            ##should always return the lowest possible element here
            return(temp.return[[1]])

          } else{

            return(set_RLum(
              "RLum.Results",
              originator = object@originator,
              data = temp.return
            ))


          }
    })

##=============================================================================##
# merge_RLum.Results ------------------------------------------------------
## merging is done by append objects to the first object in a list

#' @describeIn RLum.Results
#' merge method for RLum.Results objects. The argument object.list requires a list of RLum.Results objects.
#' Merging is done by appending similar elements to the first object of the input list.
#'
#' @param object.list [\code{merge_RLum.Results}] \code{\link{list}} (required): a list of \code{\linkS4class{RLum.Results}} objects
#'
#' @export
setMethod("merge_RLum.Results",
          signature=signature(object.list = "list"),
          definition = function(object.list){

            ##-------------------------------------------------------------
            ##Some integrity checks

            ##check if input object is a list
            if(!is(object.list, "list")){

              stop("[merge_RLum.Results()] 'object.list' has to of type 'list'!")

            }else{

              ##check if objects in the list are of type RLum.Results
              temp.originator <- sapply(1:length(object.list), function(x){

                if(is(object.list[[x]], "RLum.Results") == FALSE){

                  stop("[merge_RLum.Results()] objects to merge have
                       to be of type 'RLum.Results'!")

                }

                object.list[[x]]@originator

              })
            }

            ##check if originator is different
            if(length(unique(temp.originator))>1){

              stop("[merge_RLum.Results()] 'RLum.Results' object originator
differs!")
            }

            ##-------------------------------------------------------------
            ##merge objects depending on the data structure

            for(i in 1:length(object.list[[1]]@data)){

              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##numeric vector or data.frame or matrix
              if(is(object.list[[1]]@data[[i]], "data.frame")||
                 is(object.list[[1]]@data[[i]], "numeric") ||
                 is(object.list[[1]]@data[[i]], "matrix")){

                ##grep elements and combine them into a list
                temp.list <-
                  lapply(1:length(object.list), function(x) {
                    object.list[[x]]@data[[i]]

                  })

                ##check whetger the objects can be combined by rbind
                if(length(unique(unlist(lapply(temp.list, FUN = ncol)))) > 1){

                  stop("[merge_RLum.Results()] Objects cannot be combined, number of columns differs.")

                }

                ##combine them using rbind or data.table::rbindList (depends on the data type)
                if(is(object.list[[1]]@data[[i]], "numeric")){
                  object.list[[1]]@data[[i]] <- unlist(temp.list)

                }else if(is(object.list[[1]]@data[[i]], "matrix")){
                  object.list[[1]]@data[[i]] <- do.call("rbind", temp.list)

                }else{
                  object.list[[1]]@data[[i]] <- as.data.frame(data.table::rbindlist(temp.list))

                }


              }else{

                ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ##all other elements

                ##grep elements and write them into a list
                object.list[[1]]@data[[i]] <- lapply(1:length(object.list),
                                                     function(x){

                                                       object.list[[x]]@data[[i]]

                                                     })


                ##unlist to flatten list if necessary for the elements
                if(is(object.list[[1]]@data[[i]][[1]])[1] == "list"){

                  object.list[[1]]@data[[i]] <- unlist(object.list[[1]]@data[[i]],
                                                       recursive = FALSE)
                }
              }


            }##end loop

            ##return
            return(object.list[[1]])


          })##end set method
