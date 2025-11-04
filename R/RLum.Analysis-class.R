#' Class `"RLum.Analysis"`
#'
#' Object class to represent analysis data for protocol analysis, i.e. all curves,
#' spectra etc. from one measurements. Objects from this class are produced,
#' by e.g. [read_XSYG2R], [read_Daybreak2R]
#'
#' @name RLum.Analysis-class
#'
#' @docType class
#'
#' @slot protocol
#' Object of class [character] describing the applied measurement protocol
#'
#' @slot records
#' Object of class [list] containing objects of class [RLum.Data-class]
#'
#' @note
#' The method [structure_RLum] is currently just available for objects
#' containing [RLum.Data.Curve-class].
#'
#' @section Objects from the Class:
#' Objects can be created by calls of the form `set_RLum("RLum.Analysis", ...)`.
#'
#' @section Class version: 0.4.18
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Risoe.BINfileData2RLum.Analysis],
#' [Risoe.BINfileData-class], [RLum-class]
#'
#' @keywords classes methods
#'
#' @examples
#'
#' ## show method
#' showClass("RLum.Analysis")
#'
#' ##set an empty object
#' set_RLum(class = "RLum.Analysis")
#'
#' ## use example data
#' ##load data
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' ##show curves in object
#' get_RLum(IRSAR.RF.Data)
#'
#' ##show only the first object, but by keeping the object
#' get_RLum(IRSAR.RF.Data, record.id = 1, drop = FALSE)
#'
#' ## subsetting with SAR sample data
#' data(ExampleData.BINfileData, envir = environment())
#' sar <- object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)
#'
#' ## get
#' get_RLum(sar, subset = "NPOINTS == 250")
#'
#' ## remove
#' remove_RLum(sar, subset = "NPOINTS == 250")
#'
#' @export
setClass("RLum.Analysis",
         slots = list(
           protocol = "character",
           records = "list"
         ),
         contains = "RLum",
         prototype = list(
           protocol = NA_character_,
           records = list()
         )
)


## as() ---------------------------------------------------------------------
##LIST
##COERCE RLum.Analyse >> list AND list >> RLum.Analysis
#' as() - RLum-object coercion
#'
#' for `[RLum.Analysis-class]`
#'
#' **[RLum.Analysis-class]**
#'
#' \tabular{ll}{
#'  **from** \tab **to**\cr
#'   `list` \tab `list`\cr
#' }
#'
#' Given that the [list] consists of [RLum.Analysis-class] objects.
#'
#' @name as
setAs("list", "RLum.Analysis",
      function(from,to){
        new(to,
            protocol = NA_character_,
            records = from)
      })

setAs("RLum.Analysis", "list",
      function(from){
        lapply(from@records, function(x) x)
      })


## show() -------------------------------------------------------------------
#' @describeIn show
#' Show the structure of `RLum.Analysis` objects.
#'
#' @export
setMethod("show",
          signature(object = "RLum.Analysis"),
          function(object){

            ##print
            cat("\n [RLum.Analysis-class]")
            cat("\n\t originator:", paste0(object@originator, "()"))

            cat("\n\t protocol:", object@protocol)
            cat("\n\t additional info elements: ", if(.hasSlot(object, "info")){length(object@info)}else{0})
            cat("\n\t number of records:", length(object@records))

            #skip this part if nothing is included in the object
            if(length(object@records) > 0){
              ##get object class types
              temp <- vapply(object@records, function(x){
                class(x)[1]
              }, FUN.VALUE = character(1))

              ##print object class types
              table.temp <- table(temp)
              lapply(names(table.temp), function(x) {

                ##show RLum class type
                cat("\n\t .. :", x, ":", table.temp[x])

                ##show structure
                ##set width option ... just an implementation for the tutorial output
                temp.width <- if (getOption("width") <= 50) 4 else 7

                ##set line break variable
                linebreak <- FALSE
                env <- environment()

                ##create terminal output
                terminal_output <-
                  vapply(1:length(object@records),  function(i) {
                    if (inherits(object@records[[i]], x)) {
                      if (i %% temp.width == 0 & i != length(object@records)) {
                        assign(x = "linebreak", value = TRUE, envir = env)
                      }

                      ##FIRST
                      first <-  paste0("#", i, " ", object@records[[i]]@recordType)

                      ##LAST
                      if (i < length(object@records) &&
                          !is.null(object@records[[i]]@info[["parentID"]]) &&
                          !is.null(object@records[[i + 1]]@info[["parentID"]]) &&
                          (object@records[[i]]@info[["parentID"]] ==
                           object@records[[i+1]]@info[["parentID"]])) {
                        last <- " <> "

                      }else {
                        last <- " | "
                        if (i == length(object@records)) {
                          last <- ""

                        } else if (linebreak) {
                          last <- "\n\t .. .. : "
                          assign(x = "linebreak", value = FALSE, envir = env)
                        }
                      }
                      return(paste0(first,last))
                    }
                    return("")
                  }, FUN.VALUE = character(1))

                 ##print on screen, differentiate between records with many
                 ##curves or just one
                 if(any(grepl(terminal_output, pattern = "<>", fixed = TRUE))){
                   cat("\n\t .. .. : ",
                       gsub(pattern = "|", replacement = "\n\t .. .. :",
                            x = terminal_output, fixed = TRUE), sep = "")

                 } else{
                   cat("\n\t .. .. : ", terminal_output, sep = "")
                 }
              })

            }else{
              cat("\n\t >> This is an empty object, which cannot be used for further analysis! <<")
            }
            cat("\n")
          }
)##end show method

## set_RLum() ---------------------------------------------------------------
#' @describeIn set_RLum
#' Construction method for [RLum.Analysis-class] objects.
#'
#' @param protocol [character] (*optional*):
#' sets protocol type for analysis object. Value may be used by subsequent analysis functions.
#'
#' @param records [list] (*optional*):
#' list of [RLum.Analysis-class] objects
#'
#' @param info [list] (*optional*):
#' a list containing additional info data for the object.
#'
#' @export
setMethod(
  "set_RLum",
  signature = "RLum.Analysis",
  definition = function(
    class,
    originator,
    .uid,
    .pid,
    protocol = NA_character_,
    records = list(),
    info = list()) {

    ##produce empty class object
    newRLumAnalysis <- new(Class = "RLum.Analysis")

    ##allow self set to reset an RLum.Analysis object
    if(inherits(records, "RLum.Analysis")){
      #fill slots (this is much faster than the old code!)
      newRLumAnalysis@protocol <- if(missing(protocol)) records@protocol else protocol
      newRLumAnalysis@originator <- originator
      newRLumAnalysis@records <- records@records
      newRLumAnalysis@info <- if(missing(info)) records@info else c(records@info, info)
      newRLumAnalysis@.uid <- .uid
      newRLumAnalysis@.pid <- if(missing(.pid)) records@.uid else .pid

    }else{
      #fill slots (this is much faster than the old code!)
      newRLumAnalysis@protocol <- protocol
      newRLumAnalysis@originator <- originator
      newRLumAnalysis@records <- records
      newRLumAnalysis@info <- info
      newRLumAnalysis@.uid <- .uid
      newRLumAnalysis@.pid <- .pid
    }

    return(newRLumAnalysis)
  }
)

## get_RLum() ---------------------------------------------------------------
#' @describeIn get_RLum
#' Accessor method for RLum.Analysis objects.
#' The optional arguments `record.id`, `recordType`, `curveType` and `RLum.type`
#' allow to limit records by their id (list index number), their record type
#' (e.g. `recordType = "OSL"`), their curve type (e.g. `curveType = "predefined"`
#' or `curveType ="measured"`), or object type.
#'
#' The selection of a specific RLum.type object superimposes the default selection.
#' Currently supported objects are: RLum.Data.Curve and RLum.Data.Spectrum
#'
#' Returns:
#'
#' 1. [list] of [RLum.Data-class] objects or
#' 2. Single [RLum.Data-class] object, if only one object is contained and `recursive = FALSE` or
#' 3. [RLum.Analysis-class] objects for `drop = FALSE`
#'
#' @param record.id [numeric] or [logical] (*optional*):
#' IDs of specific records. If of type `logical` the entire id range is assumed
#' and `TRUE` and `FALSE` indicates the selection.
#'
#' @param recordType [character] (*optional*):
#' record type (e.g., "OSL"). Can be also a vector, for multiple matching,
#' e.g., `recordType = c("OSL", "IRSL")`
#'
#' @param curveType [character] (*optional*):
#' curve type (e.g. "predefined" or "measured")
#'
#' @param RLum.type [character] (*optional*):
#' RLum object type. Defaults to "RLum.Data.Curve" and "RLum.Data.Spectrum".
#'
#' @param protocol [character] (*optional*):
#' currently ignored.
#'
#' @param get.index [logical] (*optional*):
#' return a numeric vector with the index of each element in the RLum.Analysis
#' object (`FALSE` by default).
#'
#' @param drop [logical] (*with default*):
#' coerce to the next possible layer (which are [RLum.Data-class] objects if
#' `object` is an [RLum.Analysis-class] object). If `drop = FALSE`, an object
#' of the same type as the input is returned.
#'
#' @param recursive [logical] (*with default*):
#' if `TRUE` (default) when the result of the `get_RLum()` request is a single
#' object, the object itself will be returned directly, rather than being
#' wrapped in a list. Mostly this makes things easier, but this might be
#' undesired if this method is used within a loop.
#'
#' @param subset [expression] (*optional*):
#' logical or character masking a logical expression indicating elements or rows to keep:
#' missing values are taken as false. This argument takes precedence over all other arguments,
#' meaning they are not considered when subsetting the object. `subset` works slots and
#' info elements.
#'
#' @param env [environment] (*with default*):
#' An environment passed to [eval] as the enclosure. This argument is only
#' relevant when subsetting the object and should not be used manually.
#'
#' @export
setMethod("get_RLum",
          signature = ("RLum.Analysis"),
          function(object, record.id = NULL, recordType = NULL, curveType = NULL, RLum.type = NULL,
                   protocol = "UNKNOWN", get.index = FALSE, drop = TRUE, recursive = TRUE,
                   info.object = NULL, subset = NULL, env = parent.frame(2)) {
            .set_function_name("get_RLum")
            on.exit(.unset_function_name(), add = TRUE)

            if (!is.null(substitute(subset))) {
              # To account for different lengths and elements in the @info slot we first
              # check all unique elements (in all records)
              info_el <- unique(unlist(lapply(object@records, function(el) names(el@info))))

              envir <- as.data.frame(do.call(rbind,
                lapply(object@records, function(el) {
                  val <- c(curveType = el@curveType, recordType = el@recordType, unlist(el@info))
                  # add missing info elements and set NA
                  if (!all(info_el %in% names(val))) {
                    new <- info_el[!info_el %in% names(val)]
                    val <- c(val, setNames(rep("", length(new)), new))
                  }

                 # order the named char vector by its names so we don't mix up the columns
                 val <- val[order(names(val))]
                 return(val)
               })), stringAsFactors = FALSE)

              ## coerce subset to logical if character to make it compatible
              ## with remove_RLum()
              if(inherits(substitute(subset), "character"))
                subset <- parse(text = subset[1])[[1]]

              ##select relevant rows
              sel <- tryCatch(eval(
                expr = substitute(subset),
                envir = envir,
                enclos = env
              ),
              error = function(e) {
                .throw_error("Invalid subset expression, valid terms are: ",
                             .collapse(names(envir)))
              })

              if (!is.logical(sel)) {
                .throw_error("'subset' must contain a logical expression")
              }

              if (all(is.na(sel)))
                sel <- FALSE

              if (any(sel)) {
                .validate_logical_scalar(get.index)
                if (get.index)
                  return(which(sel))

                object@records <- object@records[sel]
                return(object)
              } else {
                mapply(function(name, op) {
                  message("  ", name, ": ", .collapse(unique(op), quote = FALSE))
                }, names(envir), envir)
                .throw_message("'subset' expression produced an ",
                               "empty selection, NULL returned")
                return(NULL)
              }
            }

            ##if info.object is set, only the info objects are returned
            else if(!is.null(info.object)) {
              if(info.object %in% names(object@info)){
                return(unlist(object@info[info.object]))
              }

              ## check for entries
              if (length(object@info) == 0) {
                .throw_warning("This 'RLum.Analysis' object has no info ",
                               "objects, NULL returned")
              } else {
                .throw_warning("Invalid 'info.object' name, valid names are: ",
                               .collapse(names(object@info)))
              }
              return(NULL)

            } else {

              ##check for records
              if (length(object@records) == 0) {
                .throw_warning("This 'RLum.Analysis' object has no records, ",
                               "NULL returned")
                return(NULL)
              }

              ##record.id
              .validate_class(record.id, c("integer", "numeric", "logical"),
                              null.ok = TRUE)
              if (is.null(record.id)) {
                record.id <- 1:length(object@records)
              }
              ##logical needs a slightly different treatment
              ##Why do we need this? Because a lot of standard R functions work with logical
              ##values instead of numerical indices
              if (is.logical(record.id)) {
                record.id <- (1:length(object@records))[record.id]
              }

              ##check if record.id exists
              if (!all(abs(record.id) %in% (1:length(object@records)))) {
                .throw_message("At least one 'record.id' is invalid, ",
                               "NULL returned")
                return(NULL)
              }

              ##recordType
              .validate_class(recordType, "character", null.ok = TRUE)
              if (is.null(recordType)) {
                recordType <-
                  unique(vapply(object@records, function(x)
                    x@recordType, character(1)))
              }

              ##curveType
              .validate_class(curveType, "character", null.ok = TRUE)
              if (is.null(curveType)) {
                curveType <- unique(vapply(object@records, function(x)
                  x@curveType, character(1)))
              }

              ##RLum.type
              .validate_class(RLum.type, "character", null.ok = TRUE)
              if (is.null(RLum.type)) {
                RLum.type <- c("RLum.Data.Curve", "RLum.Data.Spectrum", "RLum.Data.Image")
              }

              ##get.index
              .validate_logical_scalar(get.index)

              ##-----------------------------------------------------------------##
              ##a pre-selection is necessary to support negative index selection
              object@records <- object@records[record.id]
              record.id <- seq_along(object@records)

              ##select curves according to the chosen parameter
              temp <- lapply(record.id, function(x) {
                  if (inherits(object@records[[x]], RLum.type)) {
                    ## translate input to regular expression and remove ^ $
                    recordType <- glob2rx(recordType)
                    recordType <- substr(recordType,
                                         start = 2, stop = nchar(recordType) - 1)
                    temp <- lapply(recordType, function(type) {
                      ## use format() to handle NA so that it gets turned into
                      ## the "NA" string (as.character() would leave it as NA)
                      recordType_comp <- format(object@records[[x]]@recordType)

                      ## get the results object and if requested, get the index
                      if (grepl(type, recordType_comp) &&
                          object@records[[x]]@curveType %in% curveType) {
                        if (!get.index) object@records[[x]] else x
                      }
                    })

                    ##remove empty entries and select just one to unlist
                    temp <- .rm_NULL_elements(temp)

                    ##if list has length 0 skip entry
                    if (length(temp) != 0) {
                      temp[[1]]
                    }
                  }
                })

                ##remove empty list element
                temp <- .rm_NULL_elements(temp)

                ##check if the produced object is empty and show warning message
                if (length(temp) == 0)
                  .throw_warning("This request produced an empty list of records")

                ##remove list for get.index
                if (get.index) {
                  return(unlist(temp))
                }
                if (!drop) {
                    temp <- set_RLum(
                      class = "RLum.Analysis",
                      originator = object@originator,
                      records = temp,
                      protocol = object@protocol,
                      .pid = object@.pid
                    )
                    return(temp)
                }
                if (length(temp) == 1 && recursive)
                  return(temp[[1]])
                return(temp)
            }
          })


## remove_RLum() ------------------------------------------------------------
#' @describeIn remove_RLum
#' Method to remove records from an [RLum.Analysis-class] object.
#'
#' @param ... parameters to be passed to [get_RLum]. The arguments `get.index` and
#' `drop` are preset and have no effect when provided
#'
#' @export
setMethod("remove_RLum",
      signature= "RLum.Analysis",
      definition = function(object, ...) {
  .set_function_name("remove_RLum")
  on.exit(.unset_function_name(), add = TRUE)

# Treatment of ... --------------------------------------------------------
  ## make settings with preset
  args_set <- list(
    get.index = TRUE,
    drop = FALSE
  )
  ## we do not support all arguments; therefore we make a positive list
  args <- list(...)
  args[!names(args) %in% c(
    "record.id",
    "recordType",
    "curveType",
    "RLum.type",
    "protocol",
    "info.object",
    "subset",
    "recursive"
  )] <- NULL

  ## construct call ... record.id always takes priority
  if(!is.null(args$record.id))
    rm_id <- args$record.id
  else
    rm_id <- suppressWarnings(do.call(get_RLum, args = c(object, args_set, args)))

  ## remove objects
  object@records[rm_id] <- NULL
  return(object)
})

## structure_RLum() ---------------------------------------------------------
#' @describeIn structure_RLum
#' Returns the structure of an [RLum.Analysis-class] object.
#'
#' @param fullExtent [logical] (*with default*):
#' extends the returned `data.frame` to its full extent, i.e. all info elements
#' are part of the return as well. The default value is `FALSE` as the data
#' frame might become rather big.
#'
#' @export
setMethod("structure_RLum",
          signature= "RLum.Analysis",
          definition = function(object, fullExtent = FALSE) {
            .set_function_name("structure_RLum")
            on.exit(.unset_function_name(), add = TRUE)

            ##check if the object containing other elements than allowed
            sapply(object@records, .validate_class, "RLum.Data.Curve",
                   name = "All elements of 'object'")

            ##get length object
            temp.object.length <- length(object@records)

            ##ID
            temp.id <- seq_len(temp.object.length)

            ##recordType
            temp.recordType <-
              vapply(object@records, function(x) x@recordType, character(1))

            ##PROTOCOL STEP
            temp.protocol.step <-rep(NA_character_, temp.object.length)

            ## GET LIMITS
            temp.limits <- t(vapply(object@records, function(x) {
              c(nrow(x@data), min(x@data[, 1]), max(x@data[, 1]), min(x@data[, 2]), max(x@data[, 2]))
            }, numeric(5)))

            temp.n.channels <- temp.limits[, 1]
            temp.x.min <- temp.limits[, 2]
            temp.x.max <- temp.limits[, 3]
            temp.y.min <- temp.limits[, 4]
            temp.y.max <- temp.limits[, 5]

            ##.uid
            temp.uid <- unlist(lapply(object@records, function(x) x@.uid ))

            ##.pid
            temp.pid <- lapply(object@records, function(x) x@.pid )

            ##originator
            temp.originator <- unlist(lapply(object@records, function(x) x@originator ))

            ##curveType
            temp.curveType <- unlist(lapply(object@records, function(x) x@curveType ))

            ##info elements as character value
            if (fullExtent) {
              temp.info.elements <- as.data.frame(
                data.table::rbindlist(lapply(object@records, function(x) {
                x@info
              }), fill = TRUE))

              if (nrow(temp.info.elements) == 0) {
                ## if we are here temp.info.elements 0 rows and 0 columns:
                ## to avoid crashing further down in the data.frame() call,
                ## we create a data frame with the expected number of rows
                temp.info.elements <- data.frame(info = rep(NA, temp.object.length))
              }
            } else {
               temp.info.elements <- lapply(object@records, function(x) {
                 if(is.null(names(x@info)))
                    return(NA)
                  names(x@info)
                 })
            }

            ##combine output to a data.frame
            return(
              data.frame(
                id = temp.id,
                recordType = temp.recordType,
                curveType = temp.curveType,
                protocol.step = temp.protocol.step,
                n.channels = temp.n.channels,
                x.min = temp.x.min,
                x.max = temp.x.max,
                y.min = temp.y.min,
                y.max = temp.y.max,
                originator = temp.originator,
                .uid = temp.uid,
                .pid = I(as.list(temp.pid)),
                info = if(fullExtent) temp.info.elements else I(temp.info.elements),
                stringsAsFactors = FALSE
              )
            )
          })

## length_RLum() ------------------------------------------------------------
#' @describeIn length_RLum
#' Returns the number of records stored in the object.
#'
#' @export
setMethod("length_RLum",
          "RLum.Analysis",
          function(object){
            length(object@records)
          })

## names_RLum() -------------------------------------------------------------
#' @describeIn names_RLum
#' Returns the names of the [RLum.Data-class] objects stored in the object.
#'
#' @export
setMethod("names_RLum",
          "RLum.Analysis",
          function(object){
            sapply(object@records, function(x) x@recordType)
          })


## add_metadata() -----------------------------------------------------------
#' @describeIn metadata
#' Adds metadata to [RLum.Analysis-class] objects.
#'
#' @param info_element [character] (**required**):
#' name of the metadata entry to manipulate.
#'
#' @export
setMethod("add_metadata<-",
          signature= "RLum.Analysis",
          definition = function(object, info_element, value) {
            .set_function_name("add_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## add the metadata to all records
            records <- lapply(object@records, function(x) {
              do.call(`add_metadata<-`,
                      list(x, info_element = info_element, value = value))
            })

            object@records <- records
            assign(x = deparse(substitute(object))[1], object)
          })

## rename_metadata() --------------------------------------------------------
#' @describeIn metadata
#' Renames a metadata entry of [RLum.Analysis-class] objects.
#'
#' @export
setMethod("rename_metadata<-",
          signature= "RLum.Analysis",
          definition = function(object, info_element, value) {
            .set_function_name("rename_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## rename the metadata entry in all records
            records <- lapply(object@records, function(x) {
              do.call(`rename_metadata<-`,
                      list(x, info_element = info_element, value = value))
            })

            object@records <- records
            assign(x = deparse(substitute(object))[1], object)
          })

## replace_metadata() -------------------------------------------------------
#' @describeIn metadata
#' Replaces or removes metadata of [RLum.Analysis-class] objects.
#'
#' @param subset [expression] (*optional*):
#' logical expression to limit the substitution only to the selected subset
#' of elements.
#'
#' @export
setMethod("replace_metadata<-",
          signature= "RLum.Analysis",
          definition = function(object, info_element, subset = NULL, value) {
            .set_function_name("replace_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## this must be evaluated now, as inside the lapply() inner
            ## function it would be evaluated differently
            subset.expr <- substitute(subset)

            ## replace the metadata in all records
            records <- lapply(object@records, function(x) {
              do.call(`replace_metadata<-`,
                      list(x, info_element = info_element,
                           subset = subset.expr,  verbose = FALSE,
                           value = value))
            })

            object@records <- records
            assign(x = deparse(substitute(object))[1], object)
          })


## smooth_RLum() ------------------------------------------------------------
#' @describeIn smooth_RLum
#' Smoothing of `RLum.Data` records contained in the input object.
#'
#' @export
setMethod(
  f = "smooth_RLum",
  signature = "RLum.Analysis",
  function(object, ...) {
    object@records <- lapply(object@records, smooth_RLum, ...)
    return(object)
  }
)

## sort_RLum() --------------------------------------------------------------
#' @describeIn sort_RLum
#' Sorting of `RLum.Data` objects contained in this `RLum.Analysis` object.
#' At least one of `slot` and `info_element` must be provided. If both are
#' given, ordering by `slot` always takes priority over `info_element`.
#' Only the first element in each `slot` and each `info_element` is used
#' for sorting. Example: `.pid` can contain multiple values, however, only the
#' first is taken.
#' Please note that the `show()` method does some structuring, which may
#' lead to the impression that the sorting did not work.
#'
#' @param slot [character] (*optional*): slot name to use in sorting.
#'
#' @param info_element [character] (*optional*): names of the `info` field
#' to use in sorting. The order of the names sets the sorting priority.
#' Regardless of available info elements, the following
#' elements always exist because they are calculated from the record
#' `XY_LENGTH`, `NCOL`, `X_MIN`, `X_MAX`, `Y_MIN`, `Y_MAX`.
#'
#' @param decreasing [logical] (*with default*): whether the sort order should
#' be decreasing (`FALSE` by default). It can be provided as a vector to control
#' the ordering sequence for each sorting element.
#'
#' @examples
#' data(ExampleData.XSYG, envir = environment())
#' sar <- OSL.SARMeasurement$Sequence.Object[1:5]
#' sort_RLum(sar, solt = "recordType", info_element = c("startDate"))
#'
#' @export
setMethod(
  f = "sort_RLum",
  signature = "RLum.Analysis",
  function(object, slot = NULL, info_element = NULL,
           decreasing = FALSE, ...) {
    .set_function_name("sort_RLum")
    on.exit(.unset_function_name(), add = TRUE)

    ## an empty object has nothing to sort
    if (length(object@records) == 0)
      return(object)

    ## input validation
    .validate_class(slot, "character", null.ok = TRUE)
    if (!is.null(slot)) {
      valid.names <- slotNames(object@records[[1]])
      if (!all(slot %in% valid.names)) {
        .throw_error("Invalid 'slot' name, valid names are: ",
                     .collapse(valid.names))
      }
      if (length(slot) > 1) {
        message("[sort_RLum()]: Only the first 'slot' field will be used in sorting")
        slot <- slot[1]
      }
    }

    .validate_class(info_element, "character", null.ok = TRUE)
    if (!is.null(info_element)) {
      valid.names <- c(
        "XY_LENGTH", "NCOL", "X_MIN", "X_MAX", "Y_MIN", "Y_MAX",
        names(object@records[[1]]@info))
      if (!all(info_element %in% valid.names)) {
        .throw_error("Invalid 'info_element' name, valid names are: ",
                     .collapse(valid.names))
      }
    }

    if (is.null(slot) && is.null(info_element)) {
      .throw_error("At least one of 'slot' and 'info_element' should not be NULL")
    }

    .validate_class(decreasing, classes = "logical")
    if (anyNA(decreasing))
      .throw_error("'decreasing' should be of class 'logical'")

    ## recycle decreasing to match selection
    decreasing <- rep(decreasing, length.out = length(info_element) + 1)

    ## translate to -1 and 1 to match data.table requirements
    decreasing[decreasing] <- -1
    decreasing[!decreasing] <- 1

    ## extract the values from the records
    ## (1) extract slot values (should be a character; take only the first)
    SLOT <- if(!is.null(slot)) {
      data.table::as.data.table(
       unlist(lapply(object@records, function(x) slot(x, slot)[[1]])))

    } else {
      data.table::data.table(V1 = NA)
    }

    ## (2) extract info elements; ensure to take only the first element
    ## of the info element is a vector
    INFO <- if (!is.null(info_element)) {
      data.table::rbindlist(
        lapply(object@records, function(x) {
          data.table::as.data.table(lapply(x@info, function(l) l[[1]]))
        }),
        fill = TRUE)
      }

    ## (3) calculate general data parameters we always want to have
    EXTRA <- t(vapply(object@records, function(x) {
      ## ncol > 2 happens for RLum.Data.Spectrum and RLum.Data.Image
      n_col <- ncol(x@data)

      ## NA happens for RLum.Data.Image
      if(is.na(n_col))
        c(rep(NA_real_, 6))
      else
       c(nrow(x@data), n_col, min(x@data[,1]), max(x@data[,1]), min(x@data[,n_col]), max(x@data[,n_col]))

    }, numeric(6)))

    ## add UID and combine information
    ## the UID is required for the ordering index
    vals <- suppressWarnings(cbind(
      UID = seq_len(max(c(nrow(SLOT), nrow(INFO), nrow(EXTRA)))),
      SLOT = SLOT,
      XY_LENGTH = EXTRA[,1],
      NCOL = EXTRA[,2],
      X_MIN = EXTRA[,3],
      X_MAX = EXTRA[,4],
      Y_MIN = EXTRA[,5],
      Y_MAX = EXTRA[,6],
      INFO))

    ## determine the new ordering if possible
    ord <- data.table::setorderv(
      x = vals,
      cols = c("SLOT.V1", info_element),
      order = decreasing)[["UID"]]

    ## return reordered object
    return(object[ord])
  }
)

## melt_RLum() --------------------------------------------------------------
#' @describeIn melt_RLum
#' Melts [RLum.Analysis-class] objects into a flat data.frame with columns
#' `X`, `Y`, `TYPE`, `UID`, to be used in combination with other packages
#' such as `ggplot2`.
#'
#' @export
setMethod(
  f = "melt_RLum",
  signature = "RLum.Analysis",
  function(object) {
    melt_RLum(object@records)
  }
)

## view() -------------------------------------------------------------------
#' @describeIn view
#' View method for [RLum.Analysis-class] objects.
#'
#' @export
setMethod("view",
          signature = "RLum.Analysis",
          definition = function(object, ...) {
    .set_function_name("view")
    on.exit(.unset_function_name(), add = TRUE)

    .validate_not_empty(object)

    ## set title
    name <- list(...)$title
    if (is.null(name))
      name <- deparse(substitute(object))

    ## collect info lists from all records
    info <- lapply(seq_along(object@records),
                   function(x) c(aliquot = x, object@records[[x]]@info))
    info <- rbindlist(info, fill = TRUE)

    ## run view
    .view(x = info, title = name)
})
