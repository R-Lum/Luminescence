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
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
