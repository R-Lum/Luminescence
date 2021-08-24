#' Export measurement data produced by a Daybreak luminescence reader to CSV-files
#'
#' This function is a wrapper function around the functions [read_Daybreak2R] and
#' [write_RLum2CSV] and it imports an Daybreak-file (TXT-file, DAT-file)
#' and directly exports its content to CSV-files.  If nothing is set for the 
#' argument `path` ([write_RLum2CSV]) the input folder will become the output folder.
#'
#' @param file [character] (**required**): 
#' name of the Daybreak-file (TXT-file, DAT-file) to be converted to CSV-files
#'
#' @param ... further arguments that will be passed to the function 
#' [read_Daybreak2R] and [write_RLum2CSV]
#'
#' @return 
#' The function returns either a CSV-file (or many of them) or for the option `export = FALSE`
#' a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [RLum.Analysis-class], [RLum.Data-class], [RLum.Results-class],
#' [utils::write.table], [write_RLum2CSV], [read_Daybreak2R]
#'
#' @keywords IO
#'
#' @examples
#'
#' \dontrun{
#' ##select your BIN-file
#' file <- file.choose()
#'
#' ##convert
#' convert_Daybreak2CSV(file)
#'
#' }
#'
#' @md
#' @export
convert_Daybreak2CSV <- function(
  file,
  ...

){

  # General tests -------------------------------------------------------------------------------

  ##file is missing?
  if(missing(file)){
    stop("[convert_Daybreak2R()] file is missing!", call. = FALSE)

  }


  ##set input arguments
  convert_Daybreak2R_settings.default <- list(
    raw = FALSE,
    verbose = TRUE,
    txtProgressBar = TRUE,
    export = TRUE
  )

  ##modify list on demand
  convert_Daybreak2R_settings <- modifyList(x = convert_Daybreak2R_settings.default, val = list(...))

  # Import file ---------------------------------------------------------------------------------
  if(!inherits(file, "RLum")){
    object <- read_Daybreak2R(
      file = file,
      raw = convert_Daybreak2R_settings$raw,
      verbose = convert_Daybreak2R_settings$raw,
      txtProgressBar = convert_Daybreak2R_settings$raw

   )
  }else{
    object <- file

  }

  # Export to CSV -------------------------------------------------------------------------------

  ##get all arguments we want to pass and remove the doubled one
  arguments <- c(list(object = object, export = convert_Daybreak2R_settings$export), list(...))
  arguments[duplicated(names(arguments))] <- NULL

  ##this if-condition prevents NULL in the terminal
  if(convert_Daybreak2R_settings$export == TRUE){
    invisible(do.call("write_RLum2CSV", arguments))

  }else{
    do.call("write_RLum2CSV", arguments)

  }

}
