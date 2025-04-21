#'@title Import TIFF Image Data into R
#'
#'@description Simple wrapper around [tiff::readTIFF] to import TIFF images
#'and TIFF image stacks to be further processed within the package `'Luminescence'`
#'
#'@param file [character] (**required**): file name, can be of type [list] for automated
#'processing of many images
#'
#'@param merge2stack [logical] (*with default*): if `file` is a [list] it merges
#'the individual images into one image stack. Please note that the smallest image
#'dimension determines pixel dimension of the output stack.
#'
#'@param verbose [logical] (*with default*): enable/disable output to the
#'terminal
#'
#'@param ... not in use, for compatibility reasons only
#'
#'@return [RLum.Data.Image-class] object
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@section Function version: 0.2.0
#'
#'@seealso [tiff::readTIFF], [RLum.Data.Image-class]
#'
#'@keywords IO
#'
#'@examples
#'
#'## use system file
#'file <- system.file("extdata", "TIFFfile.tif", package = "Luminescence")
#'
#'## import image
#'image <- read_TIFF2R(file)
#'
#'@md
#'@export
read_TIFF2R <- function(
  file,
  merge2stack = FALSE,
  verbose = TRUE,
  ...
) {
  .set_function_name("read_TIFF2R")
  on.exit(.unset_function_name(), add = TRUE)

  # Self call ---------------------------------------------------------------
  if(inherits(file, "list") || length(file) > 1) {
    .validate_class(merge2stack, "logical")

    ## read list
    tmp <- lapply(file, read_TIFF2R, verbose = verbose)

    if(merge2stack[1]) {
      ## because we don't know what we get, we determine the minimal dimensions
      t_dim <- vapply(tmp, function(x) dim(x@data), numeric(3))
      t_range <- matrixStats::rowMins(t_dim)

      ## the image stack cannot be bigger than the smallest image
      tmp <- lapply(tmp, function(x) x@data[1:t_range[1],1:t_range[2],])
      tmp <- array(unlist(tmp), dim = c(t_range[1], t_range[2], length(tmp)))

      return(set_RLum(class = "RLum.Data.Image", data = tmp))
    }

    ## return the results
    return(tmp)
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(file, "character")
  .validate_length(file, 1)
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
