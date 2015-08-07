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
#' @section Class version: 0.2.0
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
setClass(
  "RLum.Results",
  slots = list(originator = "character",
               data = "list"),
  contains = "RLum",
  prototype = list (originator = character(),
                    data = list())
)


# Validation --------------------------------------------------------------

setValidity("RLum.Results",
            function(object){

              ##calc_OSLLxTxRatio
              if(object@originator == "calc_OSLLxTxRatio"){

                #print(is(object@data[[1]], "data.frame"))

              }


            }
)


# show method for object ------------------------------------------------------

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
#' directly accessing objects delivered within the slot data. If no
#' data.object is specified, a preselected object is returned. The default
#' return object depends on the object originator (e.g., \code{fit_LMCurve}).
#'
#'
#' @param object [\code{get_RLum}] \code{\linkS4class{RLum.Results}} (required): an object of class
#' \code{\linkS4class{RLum.Results}} to be evaluated
#' @param data.object [\code{get_RLum}] \code{\link{character}}: name of the data slot to be returned
setMethod("get_RLum",
          signature = signature("RLum.Results"),
          definition = function(object, data.object) {

            if(missing(data.object)==FALSE){
              if(is(data.object, "character")==FALSE){

                stop("[get_RLum] 'data.object' has to be a character!")

              }
            }

            ##allow to access a specific data object
            if(!missing(data.object)){

              if(is.null(try(object@data[[data.object]]))){

                error.message1 <- paste(names(object@data), collapse = ", ")
                error.message <- paste0("[get_RLum()] data.object '",data.object ,"' unknown. Valid object names are: ", error.message1)

                stop(error.message)

              }else{

                return(object@data[[data.object]])

              }


            }else{

              ##-------------------------------------------------------------
              ##calc_OSLLxTxRatio
              if(object@originator == "calc_OSLLxTxRatio") {

                if(missing(data.object)==TRUE){

                  return(object@data$LxTx.table)

                }else{

                  if(data.object%in%names(object@data)==FALSE){

                    #valid.names <- names(object@data))
                    stop(paste("\n[get_RLum()] Error: 'data.object' is unknown for this RLum.Results object produced by ", object@originator,"()!
                             Valid 'data.objects' are: ",paste(names(object@data), collapse=", "), sep=""))

                  }else{

                    return(object@data[data.object][[1]])

                  }

                }

              }


              ##-------------------------------------------------------------
              ##calc_TLLxTxRatio
              if(object@originator == "calc_TLLxTxRatio") {


                if(missing(data.object)==TRUE){

                  return(object@data$LxTx.table)

                }else{

                  if(data.object%in%names(object@data)==FALSE){

                    #valid.names <- names(object@data))
                    stop(paste("\n[get_RLum()] Error: 'data.object' is unknown for this RLum.Results object produced by ", object@originator,"()!
                             Valid 'data.objects' are: ",paste(names(object@data), collapse=", "), sep=""))

                  }else{

                    return(object@data[data.object][[1]])

                  }

                }




              }

              ##-------------------------------------------------------------
              ##calc_SourceDoseRate
              if(object@originator == "calc_SourceDoseRate") {

                return(object@data[[1]])

              }



              ##-------------------------------------------------------------
              ##plot_GrowthCurve
              if(object@originator == "plot_GrowthCurve") {

                if(missing(data.object)==TRUE){

                  return(object@data$De)

                }else{

                  if(data.object%in%names(object@data)==FALSE){

                    #valid.names <- names(object@data))
                    stop(paste("\n[get_RLum] Error: 'data.object' is unknown for this RLum.Results object produced by ", object@originator,"()!
                             Valid 'data.objects' are: ",paste(names(object@data), collapse=", "), sep=""))

                  }else{

                    return(object@data[data.object][[1]])

                  }

                }

              }

              ##-------------------------------------------------------------
              ##analyse_SAR.CWOSL
              if(object@originator == "analyse_SAR.CWOSL" | object@originator == "analyse_pIRIRSequence") {

                return(object@data[[1]])

              }

              ##-------------------------------------------------------------
              ##analyse_IRSAR.RF
              if(object@originator == "analyse_IRSAR.RF") {

                return(object@data$De.values)

              }

              ##-------------------------------------------------------------
              ##fit_CWCurve
              if(object@originator == "fit_CWCurve") {

                return(object@data$output.table)

              }

              ##-------------------------------------------------------------
              ##fit_LMCurve
              if(object@originator == "fit_LMCurve") {

                return(object@data$output.table)

              }

              ##-------------------------------------------------------------
              ##calc_MinDose
              if(object@originator == "calc_MinDose") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ##calc_MinDose
              if(object@originator == "calc_MaxDose") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ##calc_MinDose3
              if(object@originator == "calc_MinDose3") {

                return(object@data$results)

              }

              ##-------------------------------------------------------------
              ##calc_MinDose4
              if(object@originator == "calc_MinDose4") {

                return(object@data$results)

              }

              ##-------------------------------------------------------------
              ## calc_CommonDose
              if(object@originator == "calc_CommonDose") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ## calc_CentralDose
              if(object@originator == "calc_CentralDose") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ## calc_CosmicDoseRate
              if(object@originator == "calc_CosmicDoseRate") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ## calc_HomogeneityTest
              if(object@originator == "calc_HomogeneityTest") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ##calc_FadingCorr()

              if(object@originator == "calc_FadingCorr") {

                return(object@data$age.corr)

              }

              ##-------------------------------------------------------------
              ## calc_FuchsLang2001
              if(object@originator == "calc_FuchsLang2001") {

                return(object@data$summary)

              }

              ##-------------------------------------------------------------
              ## extract_IrradiationTimes()
              if(object@originator == "extract_IrradiationTimes") {

                return(object@data$irr.times)

              }



            }##end if missing data.object
          })

##=============================================================================##
# merge_RLum.Results ------------------------------------------------------
## merging is done by append objects to the first object in a list

#' @describeIn RLum.Results
#' merge method for RLum.Results objects. The argument object.list requires a list of RLum.Results objects.
#' Merging is done by appending similar elements to the first object of the input list.
#'
#' @param object.list [\code{merge_RLum.Results}] \code{\link{list}} (required): a list of \code{\linkS4class{RLum.Results}} objects
setMethod("merge_RLum.Results",
          signature=signature(object.list = "list"),
          definition = function(object.list){

            ##-------------------------------------------------------------
            ##Some integrity checks

            ##check if input object is a list
            if(is(object.list, "list") == FALSE){

              stop("[merge_RLum.Results()] 'object.list' has to of type 'list'!")

            }else{

              ##check if objects in the list are of type RLum.Results
              temp.originator <- sapply(1:length(object.list), function(x){

                if(is(object.list[[x]], "RLum.Results") == FALSE){

                  stop("[merge_RLum.Results] Error: objects to merge have
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
              ##vector or data.frame or matrix
              if(is(object.list[[1]]@data[[i]]) == "data.frame"||
                 is(object.list[[1]]@data[[i]])[1] == "vector" ||
                 is(object.list[[1]]@data[[i]])[1] == "matrix"){

                ##grep elements and combine them into a list
                temp.list <-
                  lapply(1:length(object.list), function(x) {
                    object.list[[x]]@data[[i]]

                  })

                ##check whetger the objects can be combined by rbind
                if(length(unique(unlist(lapply(temp.list, FUN = ncol)))) > 1){

                  stop("[merge_RLum.Results()] Objects cannot be combined, number of columns differs.")

                }

                ##combine them using rbind
                object.list[[1]]@data[[i]] <- do.call(rbind, temp.list)


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
