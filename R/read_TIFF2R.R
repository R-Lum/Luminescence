#'@title Import TIFF Image Data into R
#'
#'@description Simple wrapper around [tiff::readTIFF] to import TIFF images
#'and TIFF image stacks to be further processed within the package 'Luminescence'
#'
#'@param file [character] (**required**): file name
#'
#'@return [RLum.Image.Data-class] object
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@section Function version: 0.1.0
#'
#'@seealso [tiff::readTIFF], [RLum.Data.Image-class], [raster::raster]
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
#'@export
read_TIFF2R <- function(
  file
){
# Integrity ---------------------------------------------------------------
  ## most of the users don't need this import, no need to bother them
  ## with required libraries
  if (!requireNamespace("tiff", quietly = TRUE))
    stop("Importing TIFF files require the package tiff.\n",
         "To install this package run 'install.packages('tiff')' in your R console.",
         call. = FALSE)

  if(!file.exists(file))
    stop("[read_TIFF2R()] File does not exist or is not readable!", call. = FALSE)

# Import ------------------------------------------------------------------
  ## import
  temp <- tiff::readTIFF(file, all = TRUE, as.is = TRUE)

  ##transform to raster brick
  raster_brick <- raster::brick(lapply(temp, function(x){
    raster::raster(x, xmx = nrow(x), ymx = ncol(x))
  }))

# Return ------------------------------------------------------------------
  set_RLum(class = "RLum.Data.Image", data = raster_brick)

}

