#' Class `"RLum.Analysis"`
#'
#' Object class to represent analysis data for protocol analysis, i.e. all curves,
#' spectra etc. from one measurements. Objects from this class are produced,
#' by e.g. [read_XSYG2R], [read_Daybreak2R]
#'
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
#' showClass("RLum.Analysis")
#'
#' ##set empty object
#' set_RLum(class = "RLum.Analysis")
#'
#' ###use example data
#' ##load data
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' ##show curves in object
#' get_RLum(IRSAR.RF.Data)
#'
#' ##show only the first object, but by keeping the object
#' get_RLum(IRSAR.RF.Data, record.id = 1, drop = FALSE)
#'
#' @keywords internal
#'
#' @md
#' @export
setClass("RLum.Analysis",
         slots = list(
           protocol = "character",
           records = "list"
         ),
         contains = "RLum",
         prototype = list (
           protocol = NA_character_,
           records = list()
         )
)


# as() -----------------------------------------------------------------------------------------
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
#' @md
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


# show() --------------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#' Show structure of `RLum.Analysis` object
#'
#' @md
#' @export
setMethod("show",
          signature(object = "RLum.Analysis"),
          function(object){

            ##print
            cat("\n [RLum.Analysis-class]")

            ## show originator, for compatibility reasons with old example data
            if(.hasSlot(object, "originator")){cat("\n\t originator:", paste0(object@originator,"()"))}

            cat("\n\t protocol:", object@protocol)
            cat("\n\t additional info elements: ", if(.hasSlot(object, "info")){length(object@info)}else{0})
            cat("\n\t number of records:", length(object@records))

            #skip this part if nothing is included in the object
            if(length(object@records) > 0){
              ##get object class types
              temp <- vapply(object@records, function(x){
                class(x)[1]

              }, FUN.VALUE = vector(mode = "character", length = 1))

              ##print object class types
              lapply(1:length(table(temp)), function(x){

                ##show RLum class type
                cat("\n\t .. :", names(table(temp)[x]),":",table(temp)[x])

                ##show structure
                ##set width option ... just an implementation for the tutorial output
                if(getOption("width")<=50) temp.width <- 4 else temp.width  <- 7

                ##set line break variable
                linebreak <- FALSE
                env <- environment()

                ##create terminal output
                terminal_output <-
                  vapply(1:length(object@records),  function(i) {
                    if (names(table(temp)[x]) == is(object@records[[i]])[1]) {
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

                    }else{
                      return("")
                    }

              }, FUN.VALUE = vector(mode = "character", length = 1))

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

# set_RLum() ----------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#' Construction method for [RLum.Analysis-class] objects.
#'
#' @param class [`set_RLum`] [character] (**required**):
#' name of the `RLum` class to be created
#'
#' @param originator [`set_RLum`] [character] (*automatic*):
#' contains the name of the calling function (the function that produces this object);
#' can be set manually.
#'
#' @param .uid [`set_RLum`] [character] (*automatic*):
#' sets an unique ID for this object using the internal C++ function `create_UID`.
#'
#' @param .pid [`set_RLum`] [character] (*with default*):
#' option to provide a parent id for nesting at will.
#'
#' @param protocol [`set_RLum`] [character] (*optional*):
#' sets protocol type for analysis object. Value may be used by subsequent analysis functions.
#'
#' @param records [`set_RLum`] [list] (**required**):
#' list of [RLum.Analysis-class] objects
#'
#' @param info [`set_RLum`] [list] (*optional*):
#' a list containing additional info data for the object
#'
#' **`set_RLum`**:
#'
#' Returns an [RLum.Analysis-class] object.
#'
#' @md
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

# get_RLum() ----------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#' Accessor method for RLum.Analysis object.
#'
#' The slots record.id, `@recordType`, `@curveType` and `@RLum.type` are optional to allow for records
#' limited by their id (list index number), their record type (e.g. `recordType = "OSL"`)
#' or object type.
#'
#' Example: curve type (e.g. `curveType = "predefined"` or `curveType ="measured"`)
#'
#' The selection of a specific RLum.type object superimposes the default selection.
#' Currently supported objects are: RLum.Data.Curve and RLum.Data.Spectrum
#'
#' @param object (**required**):
#' an object of class [RLum.Analysis-class]
#'
#' @param record.id [`get_RLum`]: [numeric] or [logical] (*optional*):
#' IDs of specific records. If of type `logical` the entire id range is assumed
#' and `TRUE` and `FALSE` indicates the selection.
#'
#' @param recordType [`get_RLum`]: [character] (*optional*):
#' record type (e.g., "OSL"). Can be also a vector, for multiple matching,
#' e.g., `recordType = c("OSL", "IRSL")`
#'
#' @param curveType [`get_RLum`]: [character] (*optional*):
#' curve type (e.g. "predefined" or "measured")
#'
#' @param RLum.type [`get_RLum`]: [character] (*optional*):
#' RLum object type. Defaults to "RLum.Data.Curve" and "RLum.Data.Spectrum".
#'
#' @param get.index [`get_RLum`]: [logical] (*optional*):
#' return a numeric vector with the index of each element in the RLum.Analysis object.
#'
#' @param recursive [`get_RLum`]: [logical] (*with default*):
#' if `TRUE` (default) when the result of the `get_RLum()` request is a single
#' object, the object itself will be returned directly, rather than being
#' wrapped in a list. Mostly this makes things easier, but this might be
#' undesired if this method is used within a loop.
#'
#' @param drop [`get_RLum`]: [logical] (*with default*):
#' coerce to the next possible layer (which are `RLum.Data`-objects),
#' `drop = FALSE` keeps the original `RLum.Analysis`
#'
#' @param info.object [`get_RLum`]: [character] (*optional*):
#' name of the wanted info element
#'
#' @param subset [`get_RLum`]: [expression] (*optional*):
#' logical expression indicating elements or rows to keep: missing values are
#' taken as false. This argument takes precedence over all other arguments,
#' meaning they are not considered when subsetting the object.
#'
#' @param env [`get_RLum`]: [environment] (*with default*):
#' An environment passed to [eval] as the enclosure. This argument is only
#' relevant when subsetting the object and should not be used manually.
#'
#' @return
#'
#' **`get_RLum`**:
#'
#' Returns:
#'
#' 1. [list] of [RLum.Data-class] objects or
#' 2. Single [RLum.Data-class] object, if only one object is contained and `recursive = FALSE` or
#' 3. [RLum.Analysis-class] objects for `drop = FALSE`
#'
#' @md
#' @export
setMethod("get_RLum",
          signature = ("RLum.Analysis"),
          function(object, record.id = NULL, recordType = NULL, curveType = NULL, RLum.type = NULL,
                   protocol = "UNKNOWN", get.index = NULL, drop = TRUE, recursive = TRUE,
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
                  if (any(!info_el %in% names(val))) {
                    val_new <- setNames(
                      rep("",length(info_el[!info_el %in% names(val)])), info_el[!info_el %in% names(val)])
                    val <- c(val, val_new)
                  }

                 # order the named char vector by its names so we don't mix up the columns
                 val <- val[order(names(val))]
                 return(val)
               })), stringAsFactors = FALSE)

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
                object@records <- object@records[sel]
                return(object)
              } else {
                tmp <- mapply(function(name, op) {
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
                unlist(object@info[info.object])

              }else{
                ##check for entries
                if(length(object@info) == 0){
                  .throw_warning("This 'RLum.Analysis' object has no info ",
                                 "objects, NULL returned")
                }else{
                  .throw_warning("Invalid 'info.object' name, valid names are: ",
                                 .collapse(names(object@info)))
                }
                return(NULL)
              }

            } else {

              ##check for records
              if (length(object@records) == 0) {
                .throw_warning("This 'RLum.Analysis' object has no records, ",
                               "NULL returned")
                return(NULL)
              }

              ##record.id
              if (is.null(record.id)) {
                record.id <- c(1:length(object@records))

              } else {
                .validate_class(record.id, c("integer", "numeric", "logical"))
              }
              ##logical needs a slightly different treatment
              ##Why do we need this? Because a lot of standard R functions work with logical
              ##values instead of numerical indices
              if (is.logical(record.id)) {
                record.id <- c(1:length(object@records))[record.id]
              }

              ##check if record.id exists
              if (!all(abs(record.id) %in% (1:length(object@records)))) {
                .throw_message("At least one 'record.id' is invalid, ",
                               "NULL returned")
                return(NULL)
              }

              ##recordType
              if (is.null(recordType)) {
                recordType <-
                  unique(vapply(object@records, function(x)
                    x@recordType, character(1)))

              } else {
                .validate_class(recordType, "character")
              }

              ##curveType
              if (is.null(curveType)) {
                curveType <- unique(unlist(lapply(1:length(object@records),
                                                  function(x) {
                                                    object@records[[x]]@curveType
                                                  })))

              } else {
                .validate_class(curveType, "character")
              }

              ##RLum.type
              if (is.null(RLum.type)) {
                RLum.type <- c("RLum.Data.Curve", "RLum.Data.Spectrum", "RLum.Data.Image")
              } else {
                .validate_class(RLum.type, "character")
              }

              ##get.index
              if (is.null(get.index)) {
                get.index <- FALSE

              } else {
                .validate_logical_scalar(get.index)
              }

              ##get originator
              originator <- NA_character_
              if (.hasSlot(object, "originator")) {
                originator <- object@originator
              }

              ##-----------------------------------------------------------------##
              ##a pre-selection is necessary to support negative index selection
              object@records <- object@records[record.id]
              record.id <- 1:length(object@records)

              ##select curves according to the chosen parameter
              if (length(record.id) >= 1) {
                temp <- lapply(record.id, function(x) {
                  if (is(object@records[[x]])[1] %in% RLum.type) {
                      ##as input a vector is allowed
                    temp <- lapply(1:length(recordType), function(k) {
                      ##translate input to regular expression
                      recordType[k] <- glob2rx(recordType[k])
                      recordType[k] <- substr(recordType[k], start = 2, stop = nchar(recordType[k]) - 1)

                      ##handle NA
                      if(is.na(object@records[[x]]@recordType))
                        recordType_comp <- "NA"
                      else
                        recordType_comp <- object@records[[x]]@recordType

                      ## get the results object and if requested, get the index
                      if (grepl(recordType[k], recordType_comp) &
                          object@records[[x]]@curveType %in% curveType) {
                        if (!get.index) object@records[[x]] else x
                      }
                    })

                    ##remove empty entries and select just one to unlist
                    temp <- temp[!vapply(temp, is.null,logical(1))]

                    ##if list has length 0 skip entry
                    if (length(temp) != 0) {
                      temp[[1]]
                    } else{
                      temp <- NULL
                    }
                  }
                })

                ##remove empty list element
                temp <- temp[!vapply(temp, is.null, logical(1))]

                ##check if the produced object is empty and show warning message
                if (length(temp) == 0)
                  .throw_warning("This request produced an empty list of records")

                ##remove list for get.index
                if (get.index) {
                  return(unlist(temp))

                } else{
                  if (!drop) {
                    temp <- set_RLum(
                      class = "RLum.Analysis",
                      originator = originator,
                      records = temp,
                      protocol = object@protocol,
                      .pid = object@.pid
                    )
                    return(temp)

                  } else{
                    if (length(temp) == 1 & recursive == TRUE) {
                      return(temp[[1]])

                    } else{
                      return(temp)
                    }
                  }
                }

              } else{
                ## FIXME(mcol): this block seems unreachable : as before the
                ## `if` block we set `record.id <- 1:length(object@records)`,
                ## it can never happen that `length(record.id) < 1`
                # nocov start
                if (!get.index[1]) {
                  if (drop == FALSE) {
                    ##needed to keep the argument drop == TRUE
                    temp <- set_RLum(
                      class = "RLum.Analysis",
                      originator = originator,
                      records = list(object@records[[record.id]]),
                      protocol = object@protocol,
                      .pid = object@.pid
                    )
                    return(temp)

                  } else{
                    return(object@records[[record.id]])
                  }

                } else{
                  return(record.id)
                }
                # nocov end
              }
            }
          })

# structure_RLum() ----------------------------------------------------------------------------
###
#' @describeIn RLum.Analysis
#' Method to show the structure of an [RLum.Analysis-class] object.
#'
#' @param fullExtent [structure_RLum]; [logical] (*with default*):
#' extends the returned `data.frame` to its full extent, i.e. all info elements
#' are part of the return as well. The default value is `FALSE` as the data
#' frame might become rather big.
#'
#' @return
#'
#' **`structure_RLum`**:
#'
#' Returns [data.frame-class] showing the structure.
#'
#' @md
#' @export
setMethod("structure_RLum",
          signature= "RLum.Analysis",
          definition = function(object, fullExtent = FALSE) {
            .set_function_name("structure_RLum")
            on.exit(.unset_function_name(), add = TRUE)

            ##check if the object containing other elements than allowed
            if(!all(vapply(object@records, FUN = class, character(1)) == "RLum.Data.Curve"))
              .throw_error("Only 'RLum.Data.Curve' objects are allowed")

            ##get length object
            temp.object.length <- length(object@records)

            ##ID
            temp.id <- seq_along(object@records)

            ##recordType
            temp.recordType <-
              vapply(object@records, function(x) {
                x@recordType
              }, character(1))

            ##PROTOCOL STEP
            temp.protocol.step <- c(NA)
            length(temp.protocol.step) <- temp.object.length

            ##n.channels
            temp.n.channels <- vapply(object@records, function(x){length(x@data[,1])}, numeric(1))

            ##X.MIN
            temp.x.min <- vapply(object@records, function(x){min(x@data[,1])}, numeric(1))

            ##X.MAX
            temp.x.max <- vapply(object@records, function(x){max(x@data[,1])}, numeric(1))

            ##y.MIN
            temp.y.min <- vapply(object@records, function(x){min(x@data[,2])}, numeric(1))

            ##X.MAX
            temp.y.max <- vapply(object@records, function(x){max(x@data[,2])}, numeric(1))

            ##.uid
            temp.uid <- unlist(lapply(object@records, function(x){x@.uid}))

            ##.pid
            temp.pid <- unlist(lapply(object@records, function(x){x@.pid}))
            if (length(temp.pid) > 1)
              temp.pid <- paste(temp.pid, collapse = ", ")

            ##originator
            temp.originator <- unlist(lapply(object@records, function(x){x@originator}))

            ##curveType
            temp.curveType <- unlist(lapply(object@records, function(x){x@curveType}))

            ##info elements as character value
            if (fullExtent) {
              temp.info.elements <- as.data.frame(data.table::rbindlist(lapply(object@records, function(x) {
                x@info
              }), fill = TRUE))

              if (nrow(temp.info.elements) == 0) {
                ## if we are here temp.info.elements 0 rows and 0 columns:
                ## to avoid crashing further down in the data.frame() call,
                ## we create a data frame with the expected number of rows
                temp.info.elements <- data.frame(info = rep(NA, temp.object.length))
              }
            } else{
              temp.info.elements <-
                unlist(sapply(object@records, function(x) {
                  if (length(x@info) != 0) {
                    paste(names(x@info), collapse = " ")
                  } else{
                    NA
                  }
                }))
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
                .pid = temp.pid,
                info = temp.info.elements,
                stringsAsFactors = FALSE
              )
            )
          })


# length_RLum() -------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#' Returns the length of the object, i.e., number of stored records.
#'
#' @return
#'
#' **`length_RLum`**
#'
#' Returns the number records in this object.
#'
#' @md
#' @export
setMethod("length_RLum",
          "RLum.Analysis",
          function(object){
            length(object@records)
          })

# names_RLum() --------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#' Returns the names of the [RLum.Data-class] objects (same as shown with the
#' `show` method)
#'
#' @return
#'
#' **`names_RLum`**
#'
#' Returns the names of the record types (`recordType`) in this object.
#'
#' @md
#' @export
setMethod("names_RLum",
          "RLum.Analysis",
          function(object){
            sapply(object@records, function(x) x@recordType)
          })


## add_metadata() -----------------------------------------------------------
#' @describeIn RLum.Analysis
#' Adds metadata to [RLum.Analysis-class] objects
#'
#' @param info_element [character] (**required**) name of the metadata field
#' to add
#'
#' @param value (**required**) The value assigned to the selected element
#' of the metadata field.
#'
#' @keywords internal
#'
#' @md
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
#' @describeIn RLum.Analysis
#' Renames a metadata entry of [RLum.Analysis-class] objects
#'
#' @param info_element [character] (**required**) name of the metadata field
#' to rename
#'
#' @param value (**required**) The value assigned to the selected element
#' of the `info` slot.
#'
#' @keywords internal
#'
#' @md
#' @export
setMethod("rename_metadata<-",
          signature= "RLum.Analysis",
          definition = function(object, info_element, value) {
            .set_function_name("rename_metadata")
            on.exit(.unset_function_name(), add = TRUE)

            ## rename the metadata field in all records
            records <- lapply(object@records, function(x) {
              do.call(`rename_metadata<-`,
                      list(x, info_element = info_element, value = value))
            })

            object@records <- records
            assign(x = deparse(substitute(object))[1], object)
          })

## replace_metadata() -------------------------------------------------------
#' @describeIn RLum.Analysis
#' Replaces or removes metadata of [RLum.Analysis-class] objects
#'
#' @param info_element [character] (**required**) name of the metadata field
#' to replace or remove
#'
#' @param value (**required**) The value assigned to the selected elements
#' of the metadata field. If `NULL` the elements named in `info_element`
#' will be removed.
#'
#' @keywords internal
#'
#' @md
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


# smooth_RLum() -------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#'
#' Smoothing of `RLum.Data` objects contained in this `RLum.Analysis` object
#' using the internal function `.smoothing`.
#'
#' @param ... further arguments passed to underlying methods
#'
#' @return
#'
#' **`smooth_RLum`**
#'
#' Same object as input, after smoothing
#'
#' @md
#' @export
setMethod(
  f = "smooth_RLum",
  signature = "RLum.Analysis",
  function(object, ...) {
        object@records <- lapply(object@records, function(x){
          smooth_RLum(x, ...)
        })

    return(object)
  }
)


## sort_RLum() --------------------------------------------------------------
#' @describeIn RLum.Analysis
#'
#' Sorting of `RLum.Data` objects contained in this `RLum.Analysis` object.
#' At least one of `slot` and `info_element` must be provided. If both are
#' given, ordering by `slot` always supersedes ordering by `info_element`.
#'
#' @param slot [character] (*optional*): slot name to use in sorting.
#'
#' @param info_element [character] (*optional*): name of the `info` field
#' to use in sorting.
#'
#' @param decreasing [logical] (*with default*): whether the sort order should
#' be decreasing (`FALSE` by default).
#'
#' @param ... further arguments passed to underlying methods
#'
#' @return
#'
#' **`sort_RLum`**
#'
#' Same object as input, but sorted according to the specified parameters.
#'
#' @md
#' @export
setMethod(
  f = "sort_RLum",
  signature = "RLum.Analysis",
  function(object, slot = NULL, info_element = NULL,
           decreasing = FALSE, ...) {
    .set_function_name("sort_RLum")
    on.exit(.unset_function_name(), add = TRUE)

    ## input validation
    sort.by.slot <- FALSE
    if (!is.null(slot)) {
      .validate_class(slot, "character", extra = "NULL")
      valid.names <- slotNames(object@records[[1]])
      if (!slot %in% valid.names) {
        .throw_error("Invalid 'slot' name, valid names are: ",
                     .collapse(valid.names))
      }
      sort.by.slot <- TRUE
    }
    if (!is.null(info_element)) {
      .validate_class(info_element, "character", extra = "NULL")
      valid.names <- names(object@records[[1]]@info)
      if (!info_element%in% valid.names) {
        .throw_error("Invalid 'info_element' name, valid names are: ",
                     .collapse(valid.names))
      }
    }
    if (is.null(slot) && is.null(info_element)) {
      .throw_error("At least one of 'slot' and 'info_element' should not be NULL")
    }
    .validate_logical_scalar(decreasing)

    ## extract the values from the records according to which the sorting is
    ## done: ordering by slot always supersedes ordering by info_element
    vals <- if (sort.by.slot) {
              sapply(object@records, function(x) slot(x, slot))
            } else {
              sapply(object@records, function(x) slot(x, "info")[[info_element]])
            }

    ## determine the new ordering if possible
    tryCatch(ord <- order(vals, decreasing = decreasing),
             error = function(e) {
               .throw_error("Records could not be sorted according to ",
                            ifelse(sort.by.slot,
                                   paste0("slot = '", slot, "'"),
                                   paste0("info_element = '", info_element, "'")))
             })

    ## return reordered object
    return(object[ord])
  }
)

# melt_RLum() -------------------------------------------------------------------------------
#' @describeIn RLum.Analysis
#' Melts [RLum.Analysis-class] objects into a flat data.frame to be used
#' in combination with other packages such as `ggplot2`.
#'
#' @return
#'
#' **`melt_RLum`**
#'
#' Flat [data.frame] with `X`, `Y`, `TYPE`, `UID`
#'
#' @md
#' @export
setMethod(
  f = "melt_RLum",
  signature = "RLum.Analysis",
  function(object) {
    melt_RLum(object@records)
  }
)

## view() -------------------------------------------------------------------
#' @describeIn RLum.Analysis
#'
#' View method for [RLum.Analysis-class] objects
#'
#' @param ... other arguments that might be passed
#'
#' @keywords internal
#'
#' @md
#' @export
setMethod("view",
          signature = "RLum.Analysis",
          definition = function(object, ...) {

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
