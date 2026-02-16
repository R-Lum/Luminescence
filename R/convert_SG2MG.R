#' @title Converts Single-Grain Data to Multiple-Grain Data
#'
#' @description Conversion of single-grain data to multiple-grain data by adding signals
#' from grains belonging to one disc (unique pairs of position, set and run).
#'
#' @param object [Luminescence::Risoe.BINfileData-class], [character] (**required**):
#' [Luminescence::Risoe.BINfileData-class] object or BIN/BINX-file name.
#'
#' @param write_file [logical] (*with default*):
#' whether the output should be written to a file (only considered if `object`
#' is of type character). The multiple grain file will be written to a file
#' of named after the original one with `-SG` appended to it.
#'
#' @param ... further arguments passed down to [Luminescence::read_BIN2R] and
#' [Luminescence::write_R2BIN] if `object` is a path to a file.
#'
#' @return
#' A [Luminescence::Risoe.BINfileData-class] object. If `write_file = TRUE` an
#' the input was a file path, a file is also written out to origin folder.
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Norbert Mercier, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)
#'
#' @seealso [Luminescence::Risoe.BINfileData-class], [Luminescence::read_BIN2R],
#' [Luminescence::write_R2BIN]
#'
#' @keywords IO
#'
#' @examples
#' ## simple run
#' ## (please not that the example is not using SG data)
#' data(ExampleData.BINfileData, envir = environment())
#' convert_SG2MG(CWOSL.SAR.Data)
#'
#' @export
convert_SG2MG <- function(
  object,
  write_file = FALSE,
  ...
) {
  .set_function_name("convert_SG2MG")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, c("character", "Risoe.BINfileData"))
  .validate_not_empty(object)
  .validate_logical_scalar(write_file)

  file_name <- NULL
  if (!inherits(object, "Risoe.BINfileData")) {
    file_name <- normalizePath(object, mustWork = FALSE)
    object <- read_BIN2R(object, ...)
  }

  ## Transform --------------------------------------------------------------
  ## reset the rownames because they may not correspond to the row indices
  ## (see #1415)
  rownames(object@METADATA) <- NULL

  ## ids of single grain records
  sg_id <- as.numeric(rownames(
      unique(object@METADATA[object@METADATA[["GRAIN"]] != 0, "ID", drop = FALSE])))

  for (i in sg_id) {
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
  if (write_file && !is.null(file_name)) {
    write_R2BIN(object,
                file = paste0(tools::file_path_sans_ext(file_name), "_SG.",
                              tools::file_ext(file_name)),
                ...)
  }

  object
}
