#' General accessors function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr
#' Depending on the input object, the corresponding get function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum` or an object of type [list] containing only objects
#' of type [RLum-class]
#'
#' @param ... further arguments that will be passed to the object specific methods. For
#' further details on the supported arguments please see the class
#' documentation: [RLum.Data.Curve-class], [RLum.Data.Spectrum-class],
#' [RLum.Data.Image-class], [RLum.Analysis-class] and [RLum.Results-class]
#'
#' @return Return is the same as input objects as provided in the list.
#'
#' @section Function version: 0.3.3
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#'  [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ##Example based using data and from the calc_CentralDose() function
#'
#' ##load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ##apply the central dose model 1st time
#' temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)
#'
#' ##get results and store them in a new object
#' temp.get <- get_RLum(object = temp1)
#'
#' @md
#' @export
setGeneric("get_RLum", function (object, ...) {standardGeneric("get_RLum") })

# Method for get_RLum method for RLum objects in a list for a list of objects  -------------------
#' @describeIn get_RLum
#' Returns a list of [RLum-class] objects that had been passed to [get_RLum]
#'
#' @param class [character] (*optional*): allows to define the class that gets selected if
#' applied to a list, e.g., if a list consists of different type of RLum-class objects, this
#' arguments allows to make selection. If nothing is provided, all RLum-objects are treated.
#'
#' @param null.rm [logical] (*with default*): option to get rid of empty and NULL objects
#'
#' @md
#' @export
setMethod("get_RLum",
          signature = "list",
          function(object, class = NULL, null.rm = FALSE, ...){
            ##take care of the class argument
            if(!is.null(class)){
              sel <- class[1] == vapply(object, function(x) class(x), character(1))
              if(any(sel))
                object <- object[sel]

              rm(sel)
            }

            ##make remove all non-RLum objects
            selection <- lapply(1:length(object), function(x){
              ##get rid of all objects that are not of type RLum, this is better than leaving that
              ##to the user
              if(inherits(object[[x]], what = "RLum")){
                ##it might be the case the object already comes with empty objects, this would
                ##cause a crash
                if(inherits(object[[x]], "RLum.Analysis") && length(object[[x]]@records) == 0)
                  return(NULL)

                get_RLum(object[[x]], ...)

              } else {
                warning(paste0("[get_RLum()] object #",x," in the list was not of type 'RLum' and has been removed!"),
                        call. = FALSE)
                return(NULL)

              }

            })

            ##remove empty or NULL objects after the selection ... if wanted
            if(null.rm){
              ##first set all empty objects to NULL ... for RLum.Analysis objects
              selection <- lapply(1:length(selection), function(x){
                if(length(selection[[x]]) == 0 ||
                   (inherits(selection[[x]], "RLum.Analysis") &&
                    length(selection[[x]]@records) == 0))
                  return(NULL)
                else
                  return(selection[[x]])

              })
              ##get rid of all NULL objects
              selection <- selection[!vapply(selection, is.null, logical(1))]

            }
            return(selection)

          })

#' Method to handle NULL if the user calls get_RLum
#'
#' @describeIn get_RLum
#'
#' Returns NULL
#'
#' @md
#' @export
setMethod("get_RLum",
          signature = "NULL",
          function(object, ...){NULL})
