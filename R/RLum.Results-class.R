#' Class `"RLum.Results"`
#'
#' Object class contains results data from functions (e.g., [Luminescence::analyse_SAR.CWOSL]).
#'
#' @name RLum.Results-class
#'
#' @docType class
#'
#' @slot data
#' Object of class [list] containing output data
#'
#' @note
#' The class is intended to store results from functions to be used by
#' other functions. The data in the object should always be accessed by the
#' method [Luminescence::get_RLum].
#'
#' @section Objects from the Class:
#' Objects can be created by calls of the form `new("RLum.Results", ...)`.
#'
#' @section Class version: 0.5.2
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum-class], [Luminescence::plot_RLum], [Luminescence::merge_RLum]
#'
#' @keywords classes methods
#'
#' @examples
#'
#' showClass("RLum.Results")
#'
#' ##create an empty object from this class
#' set_RLum(class = "RLum.Results")
#'
#' ##use another function to show how it works
#'
#' ##Basic calculation of the dose rate for a specific date
#'  dose.rate <-  calc_SourceDoseRate(
#'    measurement.date = "2012-01-27",
#'    calib.date = "2014-12-19",
#'    calib.dose.rate = 0.0438,
#'    calib.error = 0.0019)
#'
#' ##show object
#' dose.rate
#'
#' ##get results
#' get_RLum(dose.rate)
#'
#' ##get parameters used for the calcualtion from the same object
#' get_RLum(dose.rate, data.object = "parameters")
#'
#' ##alternatively objects can be accessed using S3 generics, such as
#' dose.rate$parameters
#'
#' @export
setClass(
  Class = "RLum.Results",
  slots = list(data = "list"),
  contains = "RLum",
  prototype = list(data = list())
)


## as() ---------------------------------------------------------------------
##LIST
##COERCE RLum.Results >> list AND list >> RLum.Results
#' as() - RLum-object coercion
#'
#' for `[RLum.Results-class]`
#'
#' **[Luminescence::RLum.Results-class]**
#'
#' \tabular{ll}{
#'  **from** \tab **to**\cr
#'   `list` \tab `list`\cr
#' }
#'
#' Given that the [list] consists of [Luminescence::RLum.Results-class] objects.
#'
#' @name as
setAs("list", "RLum.Results",
      function(from,to){
        new(to,
            originator = "coercion",
            data = from)
      })

setAs("RLum.Results", "list",
      function(from){
        from@data
      })

## show() -------------------------------------------------------------------
#' @describeIn show
#' Show the structure of `RLum.Results` objects.
#'
#' @export
setMethod("show",
          signature(object = "RLum.Results"),
          function(object) {
            ##data elements
            temp.names <- names(object@data)

            data_list <- if (length(object) > 0) object@data else list(object@data)
            temp.type <- paste0("\t .. $", temp.names, " : ",
                                vapply(data_list, function(x) is(x)[1], character(1)))
            temp.type <- paste(temp.type, collapse = "\n")

            ##print information
            cat("\n [RLum.Results-class]")
            cat("\n\t originator: ", object@originator, "()", sep = "")
            cat("\n\t data:", length(object@data))
            cat("\n", temp.type)
            cat("\n\t additional info elements: ", length(object@info),"\n")
          })


## set_RLum() ---------------------------------------------------------------
#' @describeIn set_RLum
#' Construction method for [Luminescence::RLum.Results-class] objects.
#'
#' @export
setMethod("set_RLum",
          signature = signature("RLum.Results"),
          function(class,
                   originator,
                   .uid,
                   .pid,
                   data = list(),
                   info = list()) {

            ##create new class
            newRLumReuslts <- new("RLum.Results")

            ##fill object
            newRLumReuslts@originator <- originator
            newRLumReuslts@data <- data
            newRLumReuslts@info <- info
            newRLumReuslts@.uid <- .uid
            newRLumReuslts@.pid <- .pid

            return(newRLumReuslts)
          })


## get_RLum() ---------------------------------------------------------------
#' @describeIn get_RLum
#' Accessor method for [Luminescence::RLum.Results-class] object.
#' The argument `data.object` allows to access directly objects stored
#' within the slot data. The default return object depends on the object
#' originator (e.g., `fit_LMCurve`). If nothing is specified always the first
#' `data.object` will be returned.
#'
#' Note: Detailed specification should be made in combination with the
#' originator slot in the receiving function if results are piped.
#'
#' Returns:
#'
#' 1. Data object from the specified slot
#' 2. [list] of data objects from the slots if 'data.object' is vector or
#' 3. an [Luminescence::RLum.Results-class] for `drop = FALSE`.
#'
#' @param data.object [character] or [numeric]:
#' name or index of the data slot to be returned.
#'
#' @param info.object [character] (*optional*):
#' name of the wanted info element.
#'
#' @export
setMethod(
  "get_RLum",
  signature = signature("RLum.Results"),
  definition = function(object, data.object, info.object = NULL, drop = TRUE) {
    .set_function_name("get_RLum")
    on.exit(.unset_function_name(), add = TRUE)

    ## if info.object is set, only the info objects are returned if present
    if (!is.null(info.object)) {
      if (!info.object %in% names(object@info)) {
        if (length(object@info) == 0) {
          .throw_warning("This 'RLum.Results' object has no info objects, ",
                         "NULL returned")
        } else {
          .throw_warning("Invalid 'info.object' name, valid names are: ",
                         .collapse(names(object@info)))
        }
        return(NULL)
      }
      return(unlist(object@info[info.object]))
    }

    if (!missing(data.object)) {
      ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      .validate_class(data.object, c("character", "numeric"))

      ## CASE 1: data.object is of type 'character'
      if (is.character(data.object) &&
          !all(data.object %in% names(object@data))) {
            .throw_error("Unknown 'data.object', valid names are: ",
                         .collapse(names(object@data)))
      }

      ## CASE 2: data.object is of type 'numeric'
      if (is.numeric(data.object) && max(data.object) > length(object@data)) {
            .throw_error("'data.object' index out of bounds")
      }

      ## extract the chosen subset of objects
      temp.return <- lapply(data.object, function(x) object@data[[x]])

      ## restore names as they get lost if data.object is numeric
      names(temp.return) <- names(object@data)[data.object]

    } else {
      ## CASE 3: data.object is missing
        ##return always the first object if nothing is specified
        temp.return <- object@data[1]
    }

    ## check whether an RLum.Results object needs to be produced
    if (drop) {
        ##we need to access the list here, otherwise we get unexpected behaviour as drop = TRUE
        ##should always return the lowest possible element here
        return(temp.return[[1]])
    }
    set_RLum(
          "RLum.Results",
          originator = object@originator,
          data = temp.return
        )
  }
)

## length_RLum() ------------------------------------------------------------
#' @describeIn length_RLum
#' Returns the number of stored data elements.
#'
#' @export
setMethod("length_RLum",
          "RLum.Results",
          function(object){
            length(object@data)
          })

## names_RLum() -------------------------------------------------------------
#' @describeIn names_RLum
#' Returns the names of the `data` field stored in the object.
#'
#' @export
setMethod("names_RLum",
          "RLum.Results",
          function(object){
             names(object@data)
          })

## view() -------------------------------------------------------------------
#' @describeIn view
#' View method for [Luminescence::RLum.Results-class] objects.
#'
#' @param element [integer] (*with default*):
#' index of the element to display.
#'
#' @export
setMethod("view",
          signature = "RLum.Results",
          definition = function(object, element = 1, ...) {
    .set_function_name("view")
    on.exit(.unset_function_name(), add = TRUE)

    .validate_not_empty(object)

    ## set title
    name <- list(...)$title
    if (is.null(name))
      name <- deparse(substitute(object))

    ## run view
    data <- get_RLum(object, data.object = element)
    .view(x = data, title = name)
})
