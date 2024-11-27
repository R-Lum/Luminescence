#' @title Export PSL-file(s) to CSV-files
#'
#' @description This function is a wrapper function around the functions [read_PSL2R] and
#' [write_RLum2CSV] and it imports an PSL-file (SUERC portable OSL reader file format)
#' and directly exports its content to CSV-files.
#' If nothing is set for the argument `path` ([write_RLum2CSV]) the input folder will
#' become the output folder.
#'
#' @param file [character] (**required**):
#' name of the PSL-file to be converted to CSV-files
#'
#' @param extract_raw_data [logical] (*with default*): enable/disable raw data
#' extraction. The PSL files imported into R contain an element `$raw_data`, which
#' provides a few more information (e.g., count errors), sometimes it makes
#' sense to use this data of the more compact standard values created by [read_PSL2R]
#'
#' @param single_table [logical] (*with default*): enable/disable the creation
#' of single table with n rows and n columns, instead of separate [data.frame]
#' objects. Each curve will be represented by two columns for time and counts
#'
#' @param ... further arguments that will be passed to the function
#' [read_PSL2R] and [write_RLum2CSV]
#'
#' @return
#' The function returns either a CSV-file (or many of them) or for the option
#' `export = FALSE` a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.1.2
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class], [RLum.Data-class], [RLum.Results-class],
#' [utils::write.table], [write_RLum2CSV], [read_PSL2R]
#'
#' @keywords IO
#'
#' @examples
#'
#' ## export into single data.frame
#' file <- system.file("extdata/DorNie_0016.psl", package="Luminescence")
#' convert_PSL2CSV(file, export = FALSE, single_table = TRUE)
#'
#'
#' \dontrun{
#' ##select your BIN-file
#' file <- file.choose()
#'
#' ##convert
#' convert_PSL2CSV(file)
#'
#' }
#'
#' @md
#' @export
convert_PSL2CSV <- function(
  file,
  extract_raw_data = FALSE,
  single_table = FALSE,
  ...
) {
  .set_function_name("convert_PSL2CSV")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(file, c("character", "RLum"))
  .validate_not_empty(file)

  ##set input arguments
  convert_PSL2R_settings.default <- list(
    drop_bg = FALSE,
    as_decay_curve = TRUE,
    smooth = FALSE,
    merge = if(single_table) TRUE else FALSE,
    export = TRUE,
    verbose = TRUE
  )

  ##modify list on demand
  convert_PSL2R_settings <- modifyList(x = convert_PSL2R_settings.default, val = list(...))

  # Import file ---------------------------------------------------------------------------------
  if(!inherits(file, "RLum")){
    object <- read_PSL2R(
      file = file,
      drop_bg = convert_PSL2R_settings$drop_bg,
      as_decay_curve = convert_PSL2R_settings$as_decay_curve,
      smooth = convert_PSL2R_settings$smooth,
      merge = convert_PSL2R_settings$merge,
      verbose = convert_PSL2R_settings$verbose
   )
  }else{
    object <- file
  }

  ## try to extract file name from object ... this will be needed later
  filename <- try({
    rev(strsplit(object@info$Datafile_Path, "\\", fixed = TRUE)[[1]])[1]
  }, silent = TRUE)

  # raw data ----------------------------------------------------------------
  ## extract raw data instead of conventional data
  if(extract_raw_data) {
    psl_raw <- lapply(object@records, function(x) x@info$raw_data)

    names(psl_raw) <- names(object)
    object <- psl_raw
  }

  # single_table ------------------------------------------------------------
  ## generate a single table
  if(single_table) {
    ## run the conversion to CSV objects
    if(inherits(object, "RLum")) {
      l <- convert_PSL2CSV(object, export = FALSE, compact = FALSE)

    } else {
      l <- object
    }

    ## get max row number
    nrow_max <- vapply(l, nrow, numeric(1))

    ## create super matrix
    m <- matrix(NA, nrow = max(nrow_max), ncol = length(nrow_max) * ncol(l[[1]]))

    ## fill matrix
    s <- matrix(seq_len(length(l) * ncol(l[[1]])), nrow = ncol(l[[1]]))
    for(i in 1:length(l)) {
      m[1:nrow(l[[i]]), s[1,i]:(rev(s[,i])[1])] <- as.matrix(l[[i]])

    }

    ## set column names
    if(!extract_raw_data) {
      colnames(m) <- paste0(rep(names(l), each = 2), c("_t", "_cts"))

    } else {
      colnames(m) <- paste0(
        rep(seq_along(l), each = ncol(l[[1]])),
        "_" ,
        rep(names(l), each = ncol(l[[1]])),
        "_",
        rep(colnames(l[[1]]), length(l)))
    }

    ## overwrite object
    object <- as.data.frame(m)

    ## if possible, provide the file name as attribute
    if(!inherits(filename, "try-error"))
      attr(object, "filename") <- gsub(".", "_", filename, fixed = TRUE)
  }

  # Export to CSV -------------------------------------------------------------------------------
  ##get all arguments we want to pass and remove the doubled one
  arguments <- c(
    list(
      object = object,
      col.names = if(single_table[1] || extract_raw_data[1]) TRUE else FALSE,
      export = convert_PSL2R_settings$export),
    list(...))
  arguments[duplicated(names(arguments))] <- NULL

  ## now modify list again to ensure that the user input is always respected
  arguments <- modifyList(arguments, val = list(...), keep.null = TRUE)

  ##this if-condition prevents NULL in the terminal
  if(convert_PSL2R_settings$export){
    invisible(do.call("write_RLum2CSV", arguments))

  }else{
    do.call("write_RLum2CSV", arguments)
  }
}
