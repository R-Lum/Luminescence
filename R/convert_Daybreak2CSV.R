#' @title Export measurement data produced by a Daybreak luminescence reader to CSV-files
#'
#' @description
#' This function is a wrapper function around the functions [Luminescence::read_Daybreak2R] and
#' [Luminescence::write_RLum2CSV] and it imports a Daybreak-file (TXT-file, DAT-file)
#' and directly exports its content to CSV-files.  If nothing is set for the
#' argument `path` ([Luminescence::write_RLum2CSV]) the input folder will become the output folder.
#'
#' @param file [character] (**required**):
#' name of the Daybreak-file (TXT-file, DAT-file) to be converted to CSV-files
#'
#' @param ... further arguments that will be passed to the function
#' [Luminescence::read_Daybreak2R] and [Luminescence::write_RLum2CSV]
#'
#' @return
#' The function returns either a CSV-file (or many of them) or for the option `export = FALSE`
#' a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Analysis-class], [Luminescence::RLum.Data-class], [Luminescence::RLum.Results-class],
#' [utils::write.table], [Luminescence::write_RLum2CSV], [Luminescence::read_Daybreak2R]
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
#' }
#'
#' @export
convert_Daybreak2CSV <- function(
  file,
  ...
) {
  .set_function_name("convert_Daybreak2CSV")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(file, c("character", "RLum"))
  .validate_not_empty(file)

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
    .validate_length(file, 1)
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
  if (convert_Daybreak2R_settings$export)
    return(invisible(do.call("write_RLum2CSV", arguments)))
  do.call("write_RLum2CSV", arguments)
}
