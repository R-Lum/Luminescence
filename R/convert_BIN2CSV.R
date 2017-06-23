#' Export Risoe BIN-file(s) to CSV-files
#'
#' This function is a wrapper function around the functions [read_BIN2R] and
#' [write_RLum2CSV] and it imports a Risoe BIN-file and directly exports its 
#' content to CSV-files. If nothing is set for the argument `path` 
#' ([write_RLum2CSV]) the input folder will become the output folder.
#' 
#' @param file [character] (**required**): 
#' name of the BIN-file to be converted to CSV-files
#'
#' @param ... further arguments that will be passed to the function 
#' [read_BIN2R] and [write_RLum2CSV]
#'
#' @return 
#' The function returns either a CSV-file (or many of them) or for the 
#' option `export == FALSE` a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso [RLum.Analysis-class], [RLum.Data-class], [RLum.Results-class],
#' [utils::write.table], [write_RLum2CSV], [read_BIN2R]
#'
#' @keywords IO
#'
#' @examples
#'
#' ##transform Risoe.BINfileData values to a list
#' data(ExampleData.BINfileData, envir = environment())
#' convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1), export = FALSE)
#'
#' \dontrun{
#' ##select your BIN-file
#' file <- file.choose()
#'
#' ##convert
#' convert_BIN2CSV(file)
#'
#' }
#'
#' @md
#' @export
convert_BIN2CSV <- function(
  file,
  ...

){

  # General tests -------------------------------------------------------------------------------

  ##file is missing?
  if(missing(file)){
    stop("[convert_BIN2CSV()] file is missing!", call. = FALSE)

  }


  ##set input arguments
  convert_BIN2CSV_settings.default <- list(
    path = if(!is(file, "Risoe.BINfileData")){dirname(file)}else{NULL},
    show.raw.values = FALSE,
    position = NULL,
    n.records = NULL,
    zero_data.rm = TRUE,
    duplicated.rm = FALSE,
    show.record.number = FALSE,
    txtProgressBar = TRUE,
    forced.VersionNumber = NULL,
    ignore.RECTYPE = FALSE,
    pattern = NULL,
    verbose = TRUE,
    export = TRUE

  )

  ##modify list on demand
  convert_BIN2CSV_settings <- modifyList(x = convert_BIN2CSV_settings.default, val = list(...))

  # Import file ---------------------------------------------------------------------------------
  if(!is(file, "Risoe.BINfileData")){
    object <- read_BIN2R(
      file = file,
      show.raw.values = convert_BIN2CSV_settings$show.raw.values,
      position = convert_BIN2CSV_settings$position,
      n.records =  convert_BIN2CSV_settings$n.records,
      zero_data.rm = convert_BIN2CSV_settings$zero_data.rm,
      duplicated.rm = convert_BIN2CSV_settings$duplicated.rm,
      fastForward = TRUE,
      show.record.number = convert_BIN2CSV_settings$show.record.number,
      txtProgressBar = convert_BIN2CSV_settings$txtProgressBar,
      forced.VersionNumber = convert_BIN2CSV_settings$forced.VersionNumber,
      ignore.RECTYPE = convert_BIN2CSV_settings$ignore.RECTYPE,
      pattern = convert_BIN2CSV_settings$pattern,
      verbose = convert_BIN2CSV_settings$verbose
   )

  }else{
   object <- Risoe.BINfileData2RLum.Analysis(file)


  }

  # Export to CSV -------------------------------------------------------------------------------

  ##get all arguments we want to pass and remove the doubled one
  arguments <- c(list(object = object, export = convert_BIN2CSV_settings$export), list(...))
  arguments[duplicated(names(arguments))] <- NULL

  ##this if-condition prevents NULL in the terminal
  if(convert_BIN2CSV_settings$export == TRUE){
    invisible(do.call("write_RLum2CSV", arguments))

  }else{
    do.call("write_RLum2CSV", arguments)

  }

}
