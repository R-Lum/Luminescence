#' @title Class `"RLum.Data"`
#'
#' @description Generalized virtual data class for luminescence data.
#'
#' @name RLum.Data-class
#'
#' @docType class
#'
#' @section Objects from the Class:
#' A virtual class: no objects can be created from it.
#'
#' @section Class version: 0.2.1
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum-class], [Luminescence::RLum.Data.Curve-class],
#' [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Data.Image-class]
#'
#' @keywords classes internal
#'
#' @examples
#'
#' showClass("RLum.Data")
#'
#' @export
setClass("RLum.Data",
         contains = c("RLum", "VIRTUAL")
)

## dim() --------------------------------------------------------------------
#' @title Retrieve the dimension of an object
#'
#' @param x [Luminescence::RLum.Data-class] (**required**):
#' S4 object of class `RLum.Data`.
#'
#' @return
#' The dimension of the object's `data` slot.
#'
#' @keywords utilities
setMethod("dim",
          "RLum.Data",
          function(x) dim(x@data))


## hist() -------------------------------------------------------------------
#' @rdname hist
#' @return
#' - [Luminescence::RLum.Data-class]: the histogram of the object's `data` slot.
setMethod("hist",
          "RLum.Data",
          function(x, ...) hist(x@data, ...))


## names() ------------------------------------------------------------------
#' @rdname names
#' @return
#' - [Luminescence::RLum.Data-class]: the names of the object's `info` slot.
setMethod("names",
          "RLum.Data",
          function(x) names(x@info))


## set_RLum() ---------------------------------------------------------------
#' @describeIn set_RLum
#' Construction method for [Luminescence::RLum.Data.Curve-class],
#' [Luminescence::RLum.Data.Image-class] and
#' [Luminescence::RLum.Data.Spectrum-class] objects.
#'
#' @param recordType [character] (*optional*):
#' record type (e.g., "OSL").
#'
#' @param curveType [character] (*optional*):
#' curve type (e.g., "predefined" or "measured").
#'
#' @param data [matrix] or [array] (*with default*):
#' raw data to be stored in the object. If `data` is an `RLum.Data` object of
#' the requested class, the new object is re-constructed from it. All slots
#' are copied excep `.uid`, `.pid` and `originator`. Any other argument
#' supllied explicitly overrides the copied value.
#'
#' @param info [list] (*optional*):
#' a list containing additional info data for the object.
#'
#' @export
setMethod("set_RLum", signature = "RLum.Data", function(
  class,
  originator,
  .uid,
  .pid,
  recordType = NA_character_,
  curveType = NA_character_,
  data = matrix(0, nrow = 0, ncol = 2),
  info = list()
) {
  .set_function_name("set_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  ## an existing object of the requested class can be used for construction
  if (inherits(data, class)) {
    if (missing(curveType))
      curveType <- data@curveType
    if (missing(recordType))
      recordType <- data@recordType
    if (missing(info))
      info <- data@info

    originator <- data@originator
    .uid <- data@.uid
    .pid <- data@.pid
    data <- data@data
  }

  if (is(class, "RLum.Data.Curve") && NCOL(data) != 2) {
    .throw_error("'data' should be a matrix with 2 columns")
  }

  ## set empty class from object
  object <- new(class)

  ## fill - this is the faster way, filling in new() costs ...
  object@originator <- originator
  object@recordType <- recordType
  object@curveType <- curveType
  object@data <- data
  object@info <- info
  object@.uid <- .uid
  object@.pid <- .pid

  object
})

## summary() ----------------------------------------------------------------
#' @rdname summary
#' @return
#' - [Luminescence::RLum.Data-class]: the summary of the object's `data` slot.
setMethod("summary",
          "RLum.Data",
          function(object, ...) summary(object@data, ...))


## add_metadata() -----------------------------------------------------------
#' @describeIn metadata
#' Add metadata entries to [Luminescence::RLum.Data-class] objects.
#'
#' @export
setMethod("add_metadata<-",
          signature = "RLum.Data",
          definition = function(object, info_element, value) {
            .set_function_name("add_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## Integrity checks ---------------------------------------------
            .validate_class(info_element, "character", length = 1)
            valid.names <- names(object@info)
            if (info_element %in% valid.names) {
              .throw_error("'info_element' already present, to modify it ",
                           "you should use `replace_metadata()`")
            }
            if (is.null(value)) {
              .throw_error("Cannot store a metadata entry with NULL value")
            }

            ## add the metadata element
            object@info[[info_element]] <- value
            assign(x = deparse(substitute(object))[1], object)
          })

## rename_metadata() --------------------------------------------------------
#' @describeIn metadata
#' Rename a metadata entry of [Luminescence::RLum.Data-class] objects.
#'
#' @export
setMethod("rename_metadata<-",
          signature = "RLum.Data",
          definition = function(object, info_element, value) {
            .set_function_name("rename_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## Integrity checks ---------------------------------------------
            .validate_class(info_element, "character", length = 1)
            valid.names <- names(object@info)
            if (!info_element %in% valid.names) {
              .throw_error("'info_element' not recognised (",
                           .collapse(info_element), "), valid terms are: ",
                           .collapse(valid.names, quote = FALSE))
            }
            if (is.null(value)) {
              .throw_error("Cannot rename a metadata entry to NULL, ",
                           "to remove it you should use `replace_metadata()`")
            }

            ## rename the metadata element
            name.idx <- grep(info_element, valid.names)
            names(object@info)[name.idx] <- value
            assign(x = deparse(substitute(object))[1], object)
          })

## replace_metadata() -------------------------------------------------------
#' @describeIn metadata
#' Replaces or removes metadata of [Luminescence::RLum.Data-class] objects.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @export
setMethod("replace_metadata<-",
          signature = "RLum.Data",
          definition = function(object, info_element, subset = NULL,
                                verbose = TRUE, value = NULL) {
            .set_function_name("replace_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## Integrity checks ---------------------------------------------
            .validate_class(info_element, "character")
            valid.names <- names(object@info)
            not.found <- setdiff(info_element, valid.names)
            if (length(not.found) > 0) {
              .throw_error("'info_element' not recognised (",
                           .collapse(not.found), "), valid terms are: ",
                           .collapse(valid.names, quote = FALSE))
            }

            ## select relevant rows
            sel <- TRUE
            if (!is.null(substitute(subset))) {

              ## assigning `NULL` indicates that we want to remove a field,
              ## but that is incompatible with choosing a subset of rows
              if (is.null(value)) {
                .throw_error("'subset' is incompatible with assigning NULL")
              }

              ## evaluate the expression to produce a selection
              sel <- tryCatch(eval(
                  expr = substitute(subset),
                  envir = object@info,
                  enclos = parent.frame()
              ), error = function(e) {
                .throw_error("Invalid 'subset' expression, valid terms are: ",
                             .collapse(valid.names, quote = FALSE))
              })
              if (!is.logical(sel)) {
                .throw_error("'subset' should contain a logical expression")
              }
              if (all(is.na(sel))) {
                sel <- FALSE
              }
              if (!any(sel)) {
                if (verbose)
                  .throw_message("'subset' expression produced an ",
                                 "empty selection, nothing done")
                return(object)
              }
            }

            if (!is.null(substitute(subset))) {
              ## replace the metadata elements
              for (field in info_element)
                object@info[[field]][sel] <- value
            } else {
              ## remove the metadata elements
              object@info[info_element] <- value
            }
            assign(x = deparse(substitute(object))[1], object)
          })

## view() -------------------------------------------------------------------
#' @describeIn view
#' View method for [Luminescence::RLum.Data-class] objects.
#'
#' @export
setMethod("view",
          signature = "RLum.Data",
          definition = function(object, ...) {
  .set_function_name("view")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_not_empty(object@info, what = "RLum.Data")

    ## set title
    name <- list(...)$title
    if (is.null(name))
      name <- deparse(substitute(object))

    ## run view
    .view(x = object@info, title = name)
})
