#'@title Import TIFF Image Data into R
#'
#'@description Simple wrapper around [tiff::readTIFF] to import TIFF images
#'and TIFF image stacks to be further processed within the package `'Luminescence'`
#'
#'@param file [character] (**required**): file name
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#'@param ... not in use, for compatibility reasons only
#'
#'@return [RLum.Data.Image-class] object
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@section Function version: 0.1.2
#'
#'@seealso [tiff::readTIFF], [RLum.Data.Image-class]
#'
#'@keywords IO
#'
#'@examples
#'
#'\dontrun{
#'file <- file.choose()
#'image <- read_TIFF2R(file)
#'
#'}
#'
#'@md
#'@export
read_TIFF2R <- function(
  file,
  verbose = TRUE,
  ...
) {
  .set_function_name("read_TIFF2R")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(file, "character")
  .validate_not_empty(file)
  .require_suggested_package("tiff", "Importing TIFF files")

  if(!file.exists(file))
    .throw_error("File does not exist or is not readable")

  ## Import -----------------------------------------------------------------

  if (verbose) {
    cat("\n[read_TIFF2R()] Importing ...")
    cat("\n path: ", dirname(file))
    cat("\n file: ", .shorten_filename(basename(file)))
    cat("\n")
  }

  ## import
  temp <- tiff::readTIFF(file, all = TRUE, as.is = TRUE)

  if(is(temp, "list"))
    temp <- as(temp, "RLum.Data.Image")

# Return ------------------------------------------------------------------
  set_RLum(class = "RLum.Data.Image", data = temp@data)
}
