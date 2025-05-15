# bin_RLum.Data -----------------------------------------------------------
#' @title Channel binning for RLum.Data S4 class objects.
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum.Data-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum.Data-class] class.
#'
#' @param object [RLum.Data-class] (**required**):
#' S4 object of class `RLum.Data`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.2.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @note Currently only `RLum.Data` objects of class [RLum.Data.Curve-class]
#' and [RLum.Data.Spectrum-class] are supported.
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Spectrum-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ## create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ## plot data without and with 2 and 4 channel binning
#' plot_RLum(curve)
#' plot_RLum(bin_RLum.Data(curve, bin_size = 2))
#' plot_RLum(bin_RLum.Data(curve, bin_size = 4))
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("bin_RLum.Data", function(object, ...) {
  standardGeneric("bin_RLum.Data")
})

# get_Risoe.BINfileData() -------------------------------------------------
#' @title General accessor function for Risoe.BINfileData objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Risoe.BINfileData-class] objects. Depending on the input object, the
#' corresponding function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [Risoe.BINfileData-class] class.
#'
#' @param object [Risoe.BINfileData-class] (**required**):
#' S4 object of class `Risoe.BINfileData`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Risoe.BINfileData-class]
#'
#' @keywords utilities internal
#'
#' @md
#' @export
setGeneric("get_Risoe.BINfileData", function(object, ...) {
  standardGeneric("get_Risoe.BINfileData")
})


# get_RLum() --------------------------------------------------------------
#' @title General accessor function for RLum S4 class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum` or an object of type [list] containing only objects
#' of type [RLum-class]
#'
#' @param ... further arguments passed to the specific class method.
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.3.3
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## Example based using data and from the calc_CentralDose() function
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## apply the central dose model 1st time
#' temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)
#'
#' ## get results and store them in a new object
#' temp.get <- get_RLum(object = temp1)
#'
#' @md
#' @export
setGeneric("get_RLum", function (object, ...) {
  standardGeneric("get_RLum")
})

#' @describeIn get_RLum
#' Returns a list of [RLum-class] objects that had been passed to [get_RLum]
#'
#' @param class [character] (*optional*): allows to define the class that gets
#' selected if applied to a list, e.g., if a list consists of different type
#' of [RLum-class] objects, this arguments allows to make selection. If nothing
#' is provided, all RLum-objects are treated.
#'
#' @param null.rm [logical] (*with default*): remove empty and `NULL` objects.
#'
#' @md
#' @export
setMethod("get_RLum", signature = "list",
    function(object, class = NULL, null.rm = FALSE, ...) {
      ## take care of the class argument
      if (!is.null(class)) {
        sel <- class[1] == vapply(object, function(x) class(x), character(1))
        if (any(sel))
          object <- object[sel]

        rm(sel)
      }

      ## make remove all non-RLum objects
      selection <- lapply(seq_along(object), function(x) {
        ## get rid of all objects that are not of type RLum, this is better
        ## than leaving that to the user
        if (inherits(object[[x]], what = "RLum")) {
          ## it might be the case the object already comes with empty objects,
          ## this would cause a crash
          if (inherits(object[[x]], "RLum.Analysis") &&
              length(object[[x]]@records) == 0)
            return(NULL)

          get_RLum(object[[x]], ...)

        } else {
          warning(paste0("[get_RLum()] object #",x," in the list was not of type 'RLum' and has been removed!"),
                  call. = FALSE)
          return(NULL)
        }
      })

      ## remove empty or NULL objects after the selection ... if wanted
      if (null.rm) {
        ## first set all empty objects to NULL ... for RLum.Analysis objects
        selection <- lapply(1:length(selection), function(x) {
          if (length(selection[[x]]) == 0 ||
              (inherits(selection[[x]], "RLum.Analysis") &&
               length(selection[[x]]@records) == 0))
            return(NULL)
          else
            return(selection[[x]])
        })
        ## get rid of all NULL objects
        selection <- selection[!vapply(selection, is.null, logical(1))]
      }
      return(selection)
    })

#' Method to handle NULL if the user calls get_RLum
#'
#' @describeIn get_RLum
#'
#' Returns `NULL`.
#'
#' @md
#' @export
setMethod("get_RLum", signature = "NULL",
    function(object, ...) {
      NULL
    })


# remove_RLum() -------------------------------------------------------------
#' @title Strips records from RLum-class objects
#'
#' @description
#' Remove records from an RLum-class object in a convenient way using
#' [get_RLum] for the selection.
#'
#' @param object [RLum-class] (**required**):  S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' Same as input, can be empty
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class]
#'
#' @examples
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#' sar <- OSL.SARMeasurement$Sequence.Object[1:9]
#'
#' ## strop only OSL curves
#' sar <- remove_RLum(sar, recordType = "OSL")
#' sar
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("remove_RLum", function(object, ...) {
  standardGeneric("remove_RLum")
})

#' @describeIn remove_RLum
#' Returns a list of [RLum-class] objects where the selected records are stripped
#'
#' @md
#' @export
setMethod("remove_RLum", signature = "list",
          function(object, ...) {
            ## apply method in the objects and return the same
            tmp <- lapply(object, function(x) {
              if (inherits(x, "RLum.Analysis")) {
                return(remove_RLum(x,...))
              } else {
                return(x)
              }
            })

            ## remove empty elements
            tmp[vapply(tmp,length, numeric(1)) != 0]

})

# length_RLum() -----------------------------------------------------------
#' @title Length retrieval function for RLum S4 class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @return
#' An [integer] indicating the length of the object.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso
#' [RLum.Data.Curve-class],
#' [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class],
#' [RLum.Analysis-class],
#' [RLum.Results-class]
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("length_RLum", function(object) {
  standardGeneric("length_RLum")
})


# melt_RLum() -------------------------------------------------------------
#' @title Melt RLum-class objects into a flat data.frame
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' A flat [data.frame].
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Analysis-class]
#'
#' @examples
#' data(ExampleData.XSYG, envir = environment())
#' melt_RLum(OSL.SARMeasurement[[2]][[1]])
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("melt_RLum", function(object, ...) {
  standardGeneric("melt_RLum")
})

#' @describeIn melt_RLum
#' Returns a list a single [data.frame]
#'
#' @md
#' @export
setMethod("melt_RLum", signature = "list",
    function(object, ...) {
      ## silently remove non-RLum objects
      l <- .rm_nonRLum(object)

      ## just return NULL
      if (length(l) == 0)
        return(NULL)

      ## apply method in the objects and return the same
      l <- lapply(object, function(x) {
        t <- try(melt_RLum(x), silent = TRUE)

        if (inherits(t, "try-error"))
          return(NULL)
        else
          t
      })

      ## remove NULL
      l <- l[!vapply(l, is.null, logical(1))]

      ## now bind the data.frame
      as.data.frame(data.table::rbindlist(l))
    })


# add_metadata<-() --------------------------------------------------------
#' @title Safe manipulation of object metadata
#'
#' @description
#' Generic functions for manipulation of metadata in [Risoe.BINfileData-class],
#' [RLum.Analysis-class] and [RLum.Data-class] objects.
#'
#' @param object (**required**) object to manipulate
#'
#' @param ... further arguments passed to the specific class method
#'
#' @param value the value to be assigned
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data-class], [RLum.Analysis-class], [Risoe.BINfileData-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ## show data
#' CWOSL.SAR.Data
#'
#' ## add a new field
#' add_metadata(CWOSL.SAR.Data,
#'              info_element = "INSTITUTE") <- "Heidelberg University"
#'
#' ## rename a field
#' rename_metadata(CWOSL.SAR.Data,
#'                 info_element = "INSTITUTE") <- "INSTITUTION"
#'
#' ## replace all LTYPE to RSL
#' ## but only for the first position
#' replace_metadata(
#'  object = CWOSL.SAR.Data,
#'  info_element = "LTYPE",
#'  subset = (POSITION == 1)) <- "RSL"
#'
#' ## replacing a field with NULL allows to remove that field
#' replace_metadata(CWOSL.SAR.Data,
#'                  info_element = "PREVIOUS") <- NULL
#'
#' ## show the modified data
#' CWOSL.SAR.Data
#'
#' @rdname metadata
#' @md
#' @export
setGeneric("add_metadata<-", function (object, ..., value) {
  standardGeneric("add_metadata<-")
})

#' @rdname metadata
#' @export
setGeneric("rename_metadata<-", function (object, ..., value) {
  standardGeneric("rename_metadata<-")
})

#' @rdname metadata
#' @export
setGeneric("replace_metadata<-", function (object, ..., value) {
  standardGeneric("replace_metadata<-")
})


# names_RLum() ------------------------------------------------------------
#' @title Name retrieval function for RLum S4 class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @return
#' A [character] vector.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#' @keywords utilities
#'
#' @aliases names_RLum
#'
#' @md
#' @export
setGeneric("names_RLum", function(object) {
  standardGeneric("names_RLum")
})

#' @describeIn names_RLum
#' Returns a list of [RLum-class] objects that had been passed to [names_RLum]
#'
#' @md
#' @export
setMethod("names_RLum", signature = "list",
    function(object) {
      ## apply method in the objects and return the same
      lapply(object, function(x) {
        if (inherits(x, "RLum")) {
          return(names_RLum(x))
        } else {
          return(x)
        }
      })
    })


# replicate_RLum() --------------------------------------------------------
#' @title General replication function for RLum S4 class objects
#'
#' @description
#' The function replicates RLum S4 class objects and returns a list of such
#' objects.
#'
#' @param object [RLum-class] (**required**):
#' an [RLum-class] object
#'
#' @param times [integer] (*optional*):
#' number for times each element is repeated element
#'
#' @return
#' A [list] with the object repeated.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum-class]
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("replicate_RLum", function (object, times = NULL) {
   standardGeneric("replicate_RLum")
})


# set_Risoe.BINfileData() -------------------------------------------------
#' @title General setter function for Risoe.BINfileData objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Risoe.BINfileData-class] objects. Depending on the input object, the
#' corresponding function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [Risoe.BINfileData-class] class.
#'
#' @param METADATA x
#'
#' @param DATA x
#'
#' @param .RESERVED x
#'
#' @return
#' A [Risoe.BINfileData-class] object.
#'
#' @section Function version: 0.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Risoe.BINfileData-class]
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("set_Risoe.BINfileData", function(METADATA = data.frame(),
                                             DATA = list(), .RESERVED = list()) {
  standardGeneric("set_Risoe.BINfileData")
})



# set_RLum() --------------------------------------------------------------
#' @title General setter function for RLum S4 class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the given class, the corresponding
#' method to create an object from this class will be selected.
#' Allowed additional arguments can be found in the documentations of the
#' corresponding [RLum-class] class:
#' - [RLum.Data.Curve-class],
#' - [RLum.Data.Image-class],
#' - [RLum.Data.Spectrum-class],
#' - [RLum.Analysis-class],
#' - [RLum.Results-class]
#'
#' @param class [character] (**required**):
#' name of the S4 class to create, must correspond to one of the [RLum-class]
#' classes.
#'
#' @param originator [character] (*automatic*):
#' contains the name of the calling function (the function that produces this object);
#' can be set manually.
#'
#' @param .uid [character] (*automatic*):
#' unique ID for this object, by default set using the internal C++ function
#' `create_UID`.
#'
#' @param .pid [character] (*with default*):
#' option to provide a parent id for nesting at will.
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the specified [RLum-class] class.
#'
#' @section Function version: 0.3.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## produce empty objects from each class
#' set_RLum(class = "RLum.Data.Curve")
#' set_RLum(class = "RLum.Data.Spectrum")
#' set_RLum(class = "RLum.Data.Spectrum")
#' set_RLum(class = "RLum.Analysis")
#' set_RLum(class = "RLum.Results")
#'
#' ## produce a curve object with arbitrary curve values
#' object <- set_RLum(
#' class = "RLum.Data.Curve",
#' curveType = "arbitrary",
#' recordType = "OSL",
#' data = matrix(c(1:100,exp(-c(1:100))),ncol = 2))
#'
#' ## plot this curve object
#' plot_RLum(object)
#'
#' @md
#' @export
setGeneric("set_RLum", function (class, originator, .uid = create_UID(),
                                 .pid = NA_character_, ... ) {
  .set_function_name("set_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_class(class, "character")
  class(class) <- as.character(class)

  if (missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "language")) {
      originator <- as.character(sys.call(which = -1)[[1]])

      ## account for calls using the double colons, in this case the vector
      ## is of length 3, not only 1
      if (length(originator) == 3) {
        originator <- originator[3]
      }

    } else {
      originator <- NA_character_
    }
  }

  standardGeneric("set_RLum")
})


# smooth_RLum() -----------------------------------------------------------
#' @title Smoothing of data for RLum S4-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class. The smoothing is based on an internal function
#' called `.smoothing`.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @note
#' Currently only `RLum` objects of class `RLum.Data.Curve` and `RLum.Analysis`
#' (with curve data) are supported.
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Analysis-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ## create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ## plot data without and with smoothing
#' plot_RLum(curve)
#' plot_RLum(smooth_RLum(curve))
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("smooth_RLum", function(object, ...) {
  standardGeneric("smooth_RLum")
})

#' @describeIn smooth_RLum
#' Returns a list of [RLum-class] objects that had been passed to [smooth_RLum]
#'
#' @md
#' @export
setMethod("smooth_RLum", signature = "list",
    function(object, ...) {
      ## apply method in the objects and return the same
      lapply(object, function(x) {
        if (inherits(x, "RLum")) {
          return(smooth_RLum(x,...))
        } else {
          return(x)
        }
      })
    })


# sort_RLum() -------------------------------------------------------------
#' @title Sort data for RLum S4-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] or [Risoe.BINfileData-class] (**required**):
#' S4 object of class `RLum.Analysis` or `Risoe.BINfileData`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class], [Risoe.BINfileData-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#' obj <- OSL.SARMeasurement$Sequence.Object[1:9]
#'
#' sort_RLum(obj, slot = "recordType")
#' sort_RLum(obj, info_element = "curveDescripter")
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("sort_RLum", function(object, ...) {
  standardGeneric("sort_RLum")
})

#' @describeIn sort_RLum
#' Returns a list of [RLum-class] objects that had been passed to [sort_RLum]
#'
#' @md
#' @export
setMethod("sort_RLum", signature = "list",
          function(object, ...) {
            ## apply method in the objects and return the same
            lapply(object, function(x) {
              if (inherits(x, "RLum.Analysis")) {
                return(sort_RLum(x,...))
              } else {
                return(x)
              }
            })
          })


# structure_RLum() --------------------------------------------------------
#' @title General structure function for RLum S4 class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' Returns a [data.frame] with structure of the object.
#'
#' @section Function version: 0.2.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ## show structure
#' structure_RLum(OSL.SARMeasurement$Sequence.Object)
#'
#' @md
#' @export
setGeneric("structure_RLum", function(object, ...) {
  standardGeneric("structure_RLum")
})

#' @describeIn structure_RLum
#' Returns a list of [RLum-class] objects that were passed to [structure_RLum]
#'
#' @md
#' @export
setMethod("structure_RLum", signature = "list",
    function(object, ...) {
      ## apply method in the objects and return the same
      lapply(object, function(x) {
        if (inherits(x, "RLum")) {
          return(structure_RLum(x, ...))
        } else {
          return(x)
        }
      })
    })


# view() ------------------------------------------------------------------
#' @title Convenience data visualisation function
#'
#' @description
#' Invokes the [utils::View] function tailored to objects in the package.
#' If started from RStudio, it uses the RStudio viewer.
#'
#' @param object (**required**) object to view
#'
#' @param ... further arguments passed to the specific class method
#'
#' @seealso [utils::View()]
#'
#' @returns
#' `NULL` and opens the data viewer.
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("view", function (object, ... ) {
  standardGeneric("view")
})

## ensure that we can use the internal RStudio view function
## https://stackoverflow.com/questions/48234850/how-to-use-r-studio-view-function-programatically-in-a-package
#' @md
#' @noRd
.view <- function(x, title) {
  get("View", envir = as.environment("package:utils"))(x, title) # nocov
}
