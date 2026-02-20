#' @title Merge Risoe.BINfileData objects or Risoe BIN-files
#'
#' @description
#' The function allows merging Risoe BIN/BINX files or [Luminescence::Risoe.BINfileData-class]
#' objects.
#'
#' @details
#' The function allows merging different measurements to one file or one
#' object. The record IDs are recalculated for the new object. Other values
#' are kept for each object. The number of input objects is not limited.
#'
#' `position.number.append.gap` option
#'
#' If the option `keep.position.number = FALSE` is used, the position
#' numbers of the new data set are recalculated by adding the highest position
#' number of the previous data set to the each position number of the next data
#' set. For example: The highest position number is 48, then this number will
#' be added to all other position numbers of the next data set (e.g. 1 + 48 =
#' 49)
#'
#' However, there might be cases where an additional addend (summand) is needed
#' before the next position starts. Example:
#'
#' - Position number set (A): `1,3,5,7`
#' - Position number set (B): `1,3,5,7`
#'
#' With no additional summand the new position numbers would be:
#' `1,3,5,7,8,9,10,11`. That might be unwanted. Using the argument
#' `position.number.append.gap = 1` it will become:
#' `1,3,5,7,9,11,13,15,17`.
#'
#' @param objects [character] or [Luminescence::Risoe.BINfileData-class] (**required**):
#' Character vector with path and files names with ".bin" or ".binx" extension
#' (e.g. `input.objects = c("path/file1.bin", "path/file2.bin")` or a list of
#' [Luminescence::Risoe.BINfileData-class] objects (e.g. `input.objects = c(object1, object2)`).
#'
#' @param output.file [character] (*optional*):
#' File output path and name. If no value is given, a [Luminescence::Risoe.BINfileData-class]
#' object returned instead of a file.
#'
#' @param keep.position.number [logical] (*with default*):
#' Allows keeping the original position numbers of the input objects.
#' Otherwise the position numbers are recalculated.
#'
#' @param position.number.append.gap [integer] (*with default*):
#' Set the position number gap between merged BIN-file sets, if the option
#' `keep.position.number = FALSE` is used. See details for further
#' information.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param ... currently not used.
#'
#' @return
#' Returns a [Luminescence::Risoe.BINfileData-class] object or writes to the BIN-file
#' specified by `output.file`.
#'
#' @note
#' The validity of the output objects is not further checked.
#'
#' @section Function version: 0.2.11
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::Risoe.BINfileData-class], [Luminescence::read_BIN2R], [Luminescence::write_R2BIN]
#'
#' @references
#' Duller, G.A.T., 2007. Analyst (Version 3.24) (manual). Aberystwyth University, Aberystwyth.
#'
#' @keywords IO manip
#'
#' @examples
#'
#' ##merge two objects
#' data(ExampleData.BINfileData, envir = environment())
#'
#' object1 <- CWOSL.SAR.Data
#' object2 <- CWOSL.SAR.Data
#'
#' object.new <- merge_Risoe.BINfileData(c(object1, object2))
#'
#' @export
merge_Risoe.BINfileData <- function(
  objects,
  output.file,
  keep.position.number = FALSE,
  position.number.append.gap = 0,
  verbose = TRUE,
  ...
) {
  .set_function_name("merge_Risoe.BINfileData")
  on.exit(.unset_function_name(), add = TRUE)

  ## deprecated argument
  if ("input.objects" %in% ...names()) {
    objects <- list(...)$input.objects
    .deprecated(old = "input.objects", new = "objects", since = "1.2.0")
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(objects, c("character", "list"))
  .validate_logical_scalar(keep.position.number)
  .validate_logical_scalar(verbose)
  if (length(objects) < 2) {
    .throw_message("At least two input objects are needed, nothing done",
                   error = FALSE)
    return(objects)
  }

  ## Import files -----------------------------------------------------------
  if (is.character(objects)) {
    for (i in 1:length(objects)) {
      .validate_file(objects[[i]], ext = c("bin", "binx"),
                     scan.dir = FALSE, verbose = verbose)
    }
    temp <- lapply(objects, read_BIN2R, verbose = verbose)

  }else{
    for (i in 1:length(objects)) {
      .validate_class(objects[[i]], "Risoe.BINfileData",
                      name = "All elements of 'objects'")
    }
    temp <- objects
  }

  # Get POSITION values -------------------------------------------------------

  ##grep maximum position value from the first file
  temp.position.max <- max(temp[[1]]@METADATA[["POSITION"]])

  ##grep all position values except from the first file
  temp.position.values <- unlist(sapply(2:length(temp), function(x){
    temp <- temp[[x]]@METADATA[["POSITION"]] +
      temp.position.max +
      position.number.append.gap

    assign(x = "temp.position.max", value = max(temp), envir = parent.env(environment()))

    return(temp)
  }))

  temp.position.values <- c(temp[[1]]@METADATA[["POSITION"]], temp.position.values)

  # Get overall record length -----------------------------------------------
  temp.record.length <- sum(sapply(temp, function(x) nrow(x@METADATA)))

  # Merge Files -------------------------------------------------------------
  temp.new.METADATA <- temp[[1]]@METADATA
  temp.new.DATA <- temp[[1]]@DATA
  temp.new.RESERVED <- if (".RESERVED" %in% slotNames(temp[[1]]))
                         temp[[1]]@.RESERVED else list()

  ## loop over the remaining input objects
  for (i in 2:length(objects)) {
    temp.new.METADATA <- rbind(temp.new.METADATA, temp[[i]]@METADATA)
    temp.new.DATA <- c(temp.new.DATA, temp[[i]]@DATA)
    if (".RESERVED" %in% slotNames(temp[[i]])) {
      temp.new.RESERVED <- c(temp.new.RESERVED, temp[[i]]@.RESERVED)
    }
  }

  ##SET RECORD ID in METADATA
  temp.new.METADATA$ID <- 1:temp.record.length

  ##SET POSITION VALUES
  if (!keep.position.number) {
    temp.new.METADATA$POSITION <- temp.position.values
  }

  ##TODO version number?
  # Produce BIN file object -------------------------------------------------
  temp.new <- set_Risoe.BINfileData(
    METADATA = temp.new.METADATA,
    DATA = temp.new.DATA,
    .RESERVED = temp.new.RESERVED
  )

  # OUTPUT ------------------------------------------------------------------
  if (missing(output.file))
    return(temp.new)

  write_R2BIN(temp.new, output.file, verbose = verbose)
}
