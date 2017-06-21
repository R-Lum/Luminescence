#' Export XSYG-file(s) to CSV-files
#'
#' This function is a wrapper function around the functions \code{\link{read_XSYG2R}} and
#' \code{\link{write_RLum2CSV}} and it imports an XSYG-file and directly exports its content to CSV-files.
#' If nothing is set for the argument \code{path} (\code{\link{write_RLum2CSV}}) the input folder will
#' become the output folder.
#'
#' @param file \code{\link{character}} (\bold{required}): name of the XSYG-file to be converted to CSV-files
#'
#' @param \dots further arguments that will be passed to the function \code{\link{read_XSYG2R}} and \code{\link{write_RLum2CSV}}
#'
#' @return The function returns either a CSV-file (or many of them) or for the option \code{export = FALSE}
#' a list comprising objects of type \code{link{data.frame}} and \code{\link{matrix}}
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Data}}, \code{\linkS4class{RLum.Results}},
#' \code{\link[utils]{write.table}}, \code{\link{write_RLum2CSV}}, \code{\link{read_XSYG2R}}
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
#' @export
convert_XSYG2CSV <- function(
  file,
  ...

){

  # General tests -------------------------------------------------------------------------------

  ##file is missing?
  if(missing(file)){
    stop("[convert_XSYG2R()] file is missing!", call. = FALSE)

  }


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
