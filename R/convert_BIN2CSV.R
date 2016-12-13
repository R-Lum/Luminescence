#' Export Risoe BIN-file(s) to CSV-files
#'
#' This function is a wrapper function around the functions \code{\link{read_BIN2R}} and
#' \code{\link{write_RLum2CSV}} and it imports a Risoe BIN-file and directly exports its content to CSV-files.
#' If nothing is set for the argument \code{path} (\code{\link{write_RLum2CSV}}) the input folder will
#' become the output folder.
#'
#' @param file \code{\link{character}} (\bold{required}): name of the BIN-file to be converted to CSV-files
#'
#' @param \dots further arguments that will be passed to the function \code{\link{read_BIN2R}} and \code{\link{write_RLum2CSV}}
#'
#' @return The function returns either a CSV-file (or many of them) or for the option \code{export == FALSE}
#' a list comprising objects of type \code{link{data.frame}} and \code{\link{matrix}}
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Data}}, \code{\linkS4class{RLum.Results}},
#' \code{\link[utils]{write.table}}, \code{\link{write_RLum2CSV}}, \code{\link{read_BIN2R}}
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
#' @export
convert_BIN2CSV <- function(
  file,
  ...

){

  # General tests -------------------------------------------------------------------------------

  ##file is missing?
  if(missing(file)){
    stop("[write_RLum2CSV()] file is missing!", call. = FALSE)

  }


  ##set input arguments
  convert_BIN2R_settings.default <- list(
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
    verbose = TRUE

  )

  ##modify list on demand
  convert_BIN2R_settings <- modifyList(x = convert_BIN2R_settings.default, val = list(...))

  # Import file ---------------------------------------------------------------------------------
  if(!is(file, "Risoe.BINfileData")){
    object <- read_BIN2R(
      file = file,
      show.raw.values = convert_BIN2R_settings$show.raw.values,
      position = convert_BIN2R_settings$position,
      n.records =  convert_BIN2R_settings$n.records,
      zero_data.rm = convert_BIN2R_settings$zero_data.rm,
      duplicated.rm = convert_BIN2R_settings$duplicated.rm,
      fastForward = TRUE,
      show.record.number = convert_BIN2R_settings$show.record.number,
      txtProgressBar = convert_BIN2R_settings$txtProgressBar,
      forced.VersionNumber = convert_BIN2R_settings$forced.VersionNumber,
      ignore.RECTYPE = convert_BIN2R_settings$ignore.RECTYPE,
      pattern = convert_BIN2R_settings$pattern,
      verbose = convert_BIN2R_settings$verbose
   )

  }else{
   object <- Risoe.BINfileData2RLum.Analysis(file)


  }

  # Export to CSV -------------------------------------------------------------------------------
  ##this if-condition prevents NULL in the terminal
  if(list(...)$export == TRUE){
    invisible(write_RLum2CSV(
      object = object,
      ...
    ))

  }else{
    write_RLum2CSV(
      object = object,
      ...
    )

  }

}
