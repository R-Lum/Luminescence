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

){

  # General tests -------------------------------------------------------------------------------

  ##file is missing?
  if(missing(file)){
    stop("[convert_PSL2CSV()] 'file' is missing!", call. = FALSE)

  }

  ##set input arguments
  convert_PSL2R_settings.default <- list(
    drop_bg = FALSE,
    as_decay_curve = TRUE,
    smooth = FALSE,
    merge = if(single_table) TRUE else FALSE,
    export = TRUE
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
      merge = convert_PSL2R_settings$merge

   )
  }else{
    object <- file

  }


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
      colnames(m) <-  paste0(rep(seq_along(l), each = length(l)), "_" ,names(l), "_", rep(colnames(l[[1]]), length(l)))

    }

    object <- as.data.frame(m)

  }


  # Export to CSV -------------------------------------------------------------------------------
  ##get all arguments we want to pass and remove the doubled one
  arguments <- c(list(object = object, export = convert_PSL2R_settings$export), list(...))
  arguments[duplicated(names(arguments))] <- NULL

  ##this if-condition prevents NULL in the terminal
  if(convert_PSL2R_settings$export == TRUE){
    invisible(do.call("write_RLum2CSV", arguments))

  }else{
    do.call("write_RLum2CSV", arguments)

  }

}
