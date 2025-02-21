#' @title Export XSYG-file(s) to CSV-files
#'
#' @description
#' This function is a wrapper function around the functions [read_XSYG2R] and
#' [write_RLum2CSV] and it imports an XSYG-file and directly exports its content
#' to CSV-files. If nothing is set for the argument `path` ([write_RLum2CSV])
#' the input folder will become the output folder.
#'
#' @param file [character] (**required**):
#' name of the XSYG-file to be converted to CSV-files
#'
#' @param ... further arguments that will be passed to the function
#' [read_XSYG2R] and [write_RLum2CSV]
#'
#' @return
#' The function returns either a CSV-file (or many of them) or for the option `export = FALSE`
#' a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class], [RLum.Data-class], [RLum.Results-class],
#' [utils::write.table], [write_RLum2CSV], [read_XSYG2R]
#'
#' @keywords IO
#'
#' @examples
#'
#' ##transform XSYG-file values to a list
#' data(ExampleData.XSYG, envir = environment())
#' convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10], export = FALSE)
#'
#' \dontrun{
#' ##select your BIN-file
#' file <- file.choose()
#'
#' ##convert
#' convert_XSYG2CSV(file)
#'
#' }
#'
#' @md
#' @export
convert_XSYG2CSV <- function(
  file,
  ...
) {
  .set_function_name("convert_XSYG2CSV")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(file, c("character", "RLum"))
  .validate_not_empty(file)

  ##set input arguments
  convert_XSYG2R_settings.default <- list(
    recalculate.TL.curves = TRUE,
    pattern = ".xsyg",
    txtProgressBar = TRUE,
    export = TRUE
  )

  ##modify list on demand
  convert_XSYG2R_settings <- modifyList(x = convert_XSYG2R_settings.default, val = list(...))

  # Import file ---------------------------------------------------------------------------------
  if(!inherits(file, "RLum")){
    object <- read_XSYG2R(
      file = file,
      fastForward = TRUE,
      recalculate.TL.curves = convert_XSYG2R_settings$recalculate.TL.curves,
      pattern = convert_XSYG2R_settings$pattern,
      txtProgressBar = convert_XSYG2R_settings$txtProgressBar
   )
  }else{
    object <- file
  }

  # Export to CSV -------------------------------------------------------------------------------

  ##get all arguments we want to pass and remove the doubled one
  arguments <- c(list(object = object, export = convert_XSYG2R_settings$export), list(...))
  arguments[duplicated(names(arguments))] <- NULL

  ##this if-condition prevents NULL in the terminal
  if(convert_XSYG2R_settings$export == TRUE){
    invisible(do.call("write_RLum2CSV", arguments))

  }else{
    do.call("write_RLum2CSV", arguments)
  }
}
