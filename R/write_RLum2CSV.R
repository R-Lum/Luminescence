#' @title Export RLum-objects to CSV
#'
#' @description This function exports [RLum-class]-objects to CSV-files using the R function
#' [utils::write.table]. All [RLum-class]-objects are supported, but the
#' export is lossy, i.e. the pure numerical values are exported only. Information
#' that cannot be coerced to a [data.frame] or a [matrix] are discarded as well as
#' metadata.
#'
#' @details However, in combination with the implemented import functions, nearly every
#' supported import data format can be exported to CSV-files, this gives a great
#' deal of freedom in terms of compatibility with other tools.
#'
#' **Input is a list of objects**
#'
#' If the input is a [list] of objects all explicit function arguments can be provided
#' as [list].
#'
#' @param object [RLum-class] or a [list] of `RLum` objects (**required**):
#' objects to be written. Can be a [data.frame] if needed internally.
#'
#' @param path [character] (*optional*):
#' character string naming folder for the output to be written. If nothing
#' is provided `path` will be set to the working directory.
#' **Note:** this argument is ignored when `export = FALSE`.
#'
#' @param prefix [character] (*with default*):
#' optional prefix to name the files. This prefix is valid for all written files
#'
#' @param export [logical] (*with default*):
#' enable/disable the file export. If set to `FALSE` nothing is written to
#' the file connection, but a list comprising objects of type [data.frame] and [matrix]
#' is returned instead.
#'
#' @param compact [logical] (*with default*): if `TRUE` (default) the output
#' will be simpler but less comprehensive, that is not all elements in the
#' objects will be fully broken down. This is in particular useful for writing
#' `RLum.Results` objects to CSV files, as such objects can be rather complex
#' and not all information are needed in a CSV file or can be meaningfully
#' translated to CSV format.
#'
#' @param ... further arguments that will be passed to the function
#' [utils::write.table]. All arguments except the argument `file` are supported
#'
#' @return
#' The function returns either a CSV-file (or many of them) or for the
#' option `export == FALSE` a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.2.2
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Science, Aberystwyth University (United Kingdom)
#'
#' @seealso [RLum.Analysis-class], [RLum.Data-class], [RLum.Results-class],
#' [utils::write.table]
#'
#' @keywords IO
#'
#' @examples
#'
#' ##transform values to a list (and do not write)
#' data(ExampleData.BINfileData, envir = environment())
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)[[1]]
#' write_RLum2CSV(object, export = FALSE)
#'
#' \dontrun{
#'
#' ##create temporary filepath
#' ##(for usage replace by own path)
#' temp_file <- tempfile(pattern = "output", fileext = ".csv")
#'
#' ##write CSV-file to working directory
#' write_RLum2CSV(temp_file)
#'
#' }
#'
#' @md
#' @export
write_RLum2CSV <- function(
  object,
  path = NULL,
  prefix = "",
  export = TRUE,
  compact = TRUE,
  ...
) {
  .set_function_name("write_RLum2CSV")
  on.exit(.unset_function_name(), add = TRUE)

  # Self-call -----------------------------------------------------------------------------------
  ##this option allows to work on a list of RLum-objects
  if(is.list(object) && !is.data.frame(object)){
    ## expand input arguments
    rep.length <- length(object)
    path <- .listify(path, rep.length)
    export <- .listify(export, rep.length)

      ##prefix ... create automatic prefix if nothing is provided
      prefix <- as.list(paste0(prefix[1], "[[",1:length(object),"]]_"))

      ## write list name to object
      for (i in seq_along(object))
        attr(object[[i]], "list_name") <- names(object)[i]

    ##execute the self-call function
      temp <- lapply(seq_along(object), function(x) {
        write_RLum2CSV(
          object = object[[x]],
          path = path[[x]],
          prefix = prefix[[x]],
          export = export[[x]],
          ...
        )
      })

      ##this prevents that we get a list of NULL
      if(is.null(unlist(temp))){
        return(NULL)

      }else{
        return(temp)
      }
  }

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, c("RLum.Analysis", "RLum.Data.Curve",
                            "RLum.Data.Image", "RLum.Data.Spectrum",
                            "RLum.Results", "data.frame"))
  .validate_not_empty(object)

  ## check export path
  if (export == TRUE) {
    if (is.null(path)) {
      path <- getwd()
      message("[write_RLum2CSV()] Path automatically set to: ", path)
    } else if (!dir.exists(path)) {
      .throw_error("Directory provided via the argument 'path' does not exist")
    }
  }

  ## What do we need at the end of the day is a named list of data.frames or matrices we can export
  ## using the function write.table; the name of the list elements will become the file names

  ## extract all elements ... depending on the input
  if (inherits(object, "RLum.Analysis")) {
        ##tricky, we cannot use get_RLum() as the function lapply calls as.list() for an object!
        object_list <- lapply(object, function(x){get_RLum(x)})

        ##change names of the list and produce the right format straight away
        names(object_list) <- paste0(1:length(object_list),"_",names(object))

  } else if (inherits(object, "RLum.Data")) {

        ##get object and make list
        object_list <- list(get_RLum(object))

        ##set new name
        names(object_list) <- paste0("1_",object@recordType)

  } else if (inherits(object, "RLum.Results")) {
      ##unlist what ever comes, but do not break structures like matrices, numerics and
      names <- names(object@data)

      ##get elements
      object_list <- lapply(object@data, function(e){
        ##only run something on the list of it is worth it and pack it in the list
        if(inherits(e, "matrix") || inherits(e, "numeric") || inherits(e, "data.frame"))
          return(list(e))

        ##unlist the rest until the end
        if(!compact)
          return(unlist(e))

        ##now we return whatever we have
        return(e)
      })

      ##now unlist again one level
      object_list <- unlist(object_list, recursive = FALSE)

      ##sort out objects we do not like and we cannot procede ...
      object_list_rm <- vapply(object_list, function(x) {
         inherits(x, "matrix") || inherits(x, "numeric") || inherits(x, "data.frame")
      }, FUN.VALUE = logical(1))

      ##remove unwanted objects
      object_list <- object_list[object_list_rm]

      ##set warning
      if(any(!object_list_rm))
        .throw_warning(length(which(!object_list_rm)),
                       " elements could not be converted to CSV")

      if (length(object_list) == 0) {
        .throw_error("No valid records in 'object'")
      }

      ##adjust the names
      names(object_list) <- paste0(1:length(object_list),"_",names(object_list))

  } else if (inherits(object, "data.frame")) {
    object_list <- list(object)
    if(!is.null(attr(object, "filename"))) filename <- attr(object, "filename") else  filename <- ""

    names(object_list) <- paste0("conv_", attr(object, "list_name"), filename)
  }

  # Export --------------------------------------------------------------------------------------
  if(export){
    ##set export settings for write.table
    export_settings.default <- list(
      append = FALSE,
      quote = TRUE,
      sep = ";",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = FALSE,
      col.names = FALSE,
      qmethod = c("escape", "double"),
      fileEncoding = ""
    )

    ##modify on demand
    export_settings <- modifyList(x = export_settings.default, val = list(...))

    ##write files to file system
    for(i in 1:length(object_list)){
      utils::write.table(
        x = object_list[[i]],
        file = paste0(path,"/", prefix, names(object_list)[i],".csv"),
        append = export_settings$append,
        quote =  export_settings$quote,
        sep =  export_settings$sep,
        eol =  export_settings$eol,
        na =  export_settings$na,
        dec =  export_settings$dec,
        row.names =  export_settings$row.names,
        col.names =  export_settings$col.names,
        qmethod =  export_settings$qmethod,
        fileEncoding =  export_settings$fileEncoding)
    }

  }else{
    return(object_list)
  }
}
