#' @title  Merge Risoe.BINfileData objects or Risoe BIN-files
#'
#' @description Function allows merging Risoe BIN/BINX files or [Risoe.BINfileData-class] objects.
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
#' @param input.objects [character] with [Risoe.BINfileData-class] objects (**required**):
#' Character vector with path and files names
#' (e.g. `input.objects = c("path/file1.bin", "path/file2.bin")` or
#' [Risoe.BINfileData-class] objects (e.g. `input.objects = c(object1, object2)`).
#' Alternatively a `list` is supported.
#'
#'
#' @param output.file [character] (*optional*):
#' File output path and name. If no value is given, a [Risoe.BINfileData-class] is
#' returned instead of a file.
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
#' @return Returns a `file` or a [Risoe.BINfileData-class] object.
#'
#' @note
#' The validity of the output objects is not further checked.
#'
#' @section Function version: 0.2.9
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Risoe.BINfileData-class], [read_BIN2R], [write_R2BIN]
#'
#' @references
#' Duller, G.A.T., 2007. Analyst (Version 3.24) (manual). Aberystwyth University, Aberystwyth.
#'
#'
#' @keywords IO manip
#'
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
#' @md
#' @export
merge_Risoe.BINfileData <- function(
  input.objects,
  output.file,
  keep.position.number = FALSE,
  position.number.append.gap = 0
) {
  .set_function_name("merge_Risoe.BINfileData")
  on.exit(.unset_function_name(), add = TRUE)

  # Integrity Checks --------------------------------------------------------

  .validate_class(input.objects, c("character", "list"))
  if(length(input.objects) < 2){
    message("[merge_Risoe.BINfileData()] Nothing done: at least two input objects are needed!")
    return(input.objects)
  }

  if (is.character(input.objects)) {
    for(i in 1:length(input.objects)){
      if(file.exists(input.objects[i])==FALSE){
        .throw_error("File '", input.objects[i], "' does not exist")
      }
    }

  }else{
    for (i in 1:length(input.objects)) {
      .validate_class(input.objects[[i]], "Risoe.BINfileData",
                      name = "All elements of 'input.objects'")
    }
  }


  # Import Files ------------------------------------------------------------

  ##loop over all files to store the results in a list
  ##or the input is already a list

  if (is.character(input.objects)) {
    temp <- lapply(input.objects, read_BIN2R, txtProgressBar = FALSE)

  }else{
    temp <- input.objects
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
  temp.record.length <- sum(sapply(1:length(temp), function(x){
    length(temp[[x]]@METADATA[,"ID"])
  }))


  # Merge Files -------------------------------------------------------------
  ##loop for similar input objects
  for(i in 1:length(input.objects)){
    if (!exists("temp.new.METADATA")) {

      temp.new.METADATA <- temp[[i]]@METADATA
      temp.new.DATA <- temp[[i]]@DATA
      temp.new.RESERVED <- list()
      if (".RESERVED" %in% slotNames(temp[[i]])) {
        temp.new.RESERVED <- temp[[i]]@.RESERVED
      }

    }else{

      temp.new.METADATA <- rbind(temp.new.METADATA, temp[[i]]@METADATA)
      temp.new.DATA <- c(temp.new.DATA, temp[[i]]@DATA)

      new.reserved <- list()
      if (".RESERVED" %in% slotNames(temp[[i]])) {
        new.reserved <- temp[[i]]@.RESERVED
      }
      temp.new.RESERVED <- c(temp.new.RESERVED, new.reserved)
    }
  }

  ##SET RECORD ID in METADATA
  temp.new.METADATA$ID <- 1:temp.record.length

  ##SET POSITION VALUES
  if(keep.position.number == FALSE){
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
  if(missing(output.file) == FALSE){
    write_R2BIN(temp.new, output.file)

  }else{
    return(temp.new)
  }
}
