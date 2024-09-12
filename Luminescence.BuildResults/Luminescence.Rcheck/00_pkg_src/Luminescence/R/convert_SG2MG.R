#' @title Converts Single-Grain Data to Multiple-Grain Data
#'
#' @description Conversion of single-grain data to multiple-grain data by adding signals
#' from grains belonging to one disc (unique pairs of position, set and run).
#'
#' @param object [Risoe.BINfileData-class] [character] (**required**): [Risoe.BINfileData-class]
#' object or BIN/BINX-file name
#'
#' @param write_file [logical] (*with default*): if the input was a path to a file, the
#' output can be written to a file if `TRUE`. The multiple grain file will be written into the
#' same folder and with extension `-SG` to the file name.
#'
#' @param ... further arguments passed down to [read_BIN2R] if input is file path
#'
#' @return [Risoe.BINfileData-class] object and if `write_file = TRUE` and the input
#' was a file path, a file is written to origin folder.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany), Norbert Mercier, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France);
#'
#'
#' @seealso [Risoe.BINfileData-class], [read_BIN2R], [write_R2BIN]
#'
#' @keywords IO
#'
#' @examples
#' ## simple run
#' ## (please not that the example is not using SG data)
#' data(ExampleData.BINfileData, envir = environment())
#' convert_SG2MG(CWOSL.SAR.Data)
#'
#' @md
#' @export
convert_SG2MG <- function(
  object,
  write_file = FALSE,
  ...
  ){

# Check input -------------------------------------------------------------
  if(!is(object, "Risoe.BINfileData")) {
    file_name <- object
    object <- read_BIN2R(object, ...)

  }

# Transform ---------------------------------------------------------------
  ## get unique pairs of position, run and set and then
  upairs_sg_id <- as.numeric(rownames(
    unique(object@METADATA[object@METADATA[["GRAIN"]] != 0,c("POSITION", "RUN", "SET")])))

  for(i in upairs_sg_id){
    ##get IDs of all relevant records
    records_id <- object@METADATA[
      object@METADATA[["POSITION"]] == object@METADATA[["POSITION"]][[i]] &
      object@METADATA[["RUN"]] == object@METADATA[["RUN"]][[i]] &
      object@METADATA[["SET"]] == object@METADATA[["SET"]][[i]],
      "ID"]

    # sum up count values and write it into the first grain record
    object@DATA[[i]] <- matrixStats::rowSums2(
      matrix(
        unlist(object@DATA[records_id]),
        ncol = length(records_id)))

  }

  ## clean dataset and remove all irrelevant data
  upairs_id <- as.numeric(rownames(
    unique(object@METADATA[, c("POSITION", "RUN", "SET")])))

  object@METADATA <- object@METADATA[upairs_id, ]
  object@DATA <- object@DATA[upairs_id]

  ##recalculate IDs and reset GRAIN
  object@METADATA[["ID"]] <- 1:length(object@DATA)
  object@METADATA[["GRAIN"]] <- 0


# Write file --------------------------------------------------------------
  if(write_file[1]){
    if(!inherits(try(file.exists(file_name), silent = FALSE), "try-error")){
      dirname <- dirname(normalizePath(file_name))
      filename <- strsplit(basename(normalizePath(file_name)), ".", fixed = TRUE)[[1]]

      write_R2BIN(object, paste0(dirname,"/",filename[1],"_SG.",filename[2]), ...)
    }

  }

# Return object -----------------------------------------------------------
return(object)

}
