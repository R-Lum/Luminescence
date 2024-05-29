#'@title Import TIFF Image Data into R
#'
#'@description Simple wrapper around [tiff::readTIFF] to import TIFF images
#'and TIFF image stacks to be further processed within the package `'Luminescence'`
#'
#'@param file [character] (**required**): file name
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
  ...
){
# Integrity ---------------------------------------------------------------
  ## most of the users don't need this import, no need to bother them
  ## with required libraries
  if (!requireNamespace("tiff", quietly = TRUE))
    stop("Importing TIFF files requires the package tiff.\n",
         "To install this package run 'install.packages('tiff')' in your R console.",
         call. = FALSE)

  if(!file.exists(file))
    stop("[read_TIFF2R()] File does not exist or is not readable!", call. = FALSE)

# Import ------------------------------------------------------------------
  ## import
  temp <- tiff::readTIFF(file, all = TRUE, as.is = TRUE)

  if(is(temp, "list"))
    temp <- as(temp, "RLum.Data.Image")

# Return ------------------------------------------------------------------
  set_RLum(class = "RLum.Data.Image", data = temp@data)

}

