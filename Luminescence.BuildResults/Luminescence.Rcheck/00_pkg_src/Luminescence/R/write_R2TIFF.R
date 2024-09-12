#'@title Export RLum.Data.Image and RLum.Data.Spectrum objects to TIFF Images
#'
#'@description Simple wrapper around [tiff::writeTIFF] to export suitable
#' RLum-class objects to TIFF images. Per default 16-bit TIFF files are exported.
#'
#'@param object [RLum.Data.Image-class] or [RLum.Data.Spectrum-class] object (**required**):
#'input object, can be a [list] of such objects
#'
#'@param file [character] (**required**): the file name and path
#'
#'@param norm [numeric] (*with default*): normalisation values. Values in TIFF files must range between 0-1, however, usually
#'in imaging applications the pixel values are real integer count values. The normalisation to the
#'to the highest 16-bit integer values -1 ensures that the numerical values are retained in the exported
#'image. If `1` nothing is normalised.
#'
#'@param ... further arguments to be passed to [tiff::writeTIFF].
#'
#'@return A TIFF file
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@section Function version: 0.1.0
#'
#'@seealso [tiff::writeTIFF], [RLum.Data.Image-class], [RLum.Data.Spectrum-class]
#'
#'@keywords IO
#'
#'@examples
#'data(ExampleData.RLum.Data.Image, envir = environment())
#'write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile())
#'
#'@md
#'@export
write_R2TIFF <- function(
  object,
  file = tempfile(),
  norm = 65535,
  ...
){
# Integrity ---------------------------------------------------------------
  ## most of the users don't need this import, no need to bother them
  ## with required libraries
  if (!requireNamespace("tiff", quietly = TRUE))
    # nocov start
    stop("Exporting objects to TIFF files requires the package tiff.\n",
         "To install this package run 'install.packages('tiff')' in your R console.",
         call. = FALSE)
    # nocov end

# Transform  --------------------------------------------------------------
  ## make a list ... it is just easier
  if(!is(object, "list"))
    object <- list(object)

  ## check list input
  if(!any(vapply(object, function(x) class(x)[1], character(1)) %in% c("RLum.Data.Image", "RLum.Data.Spectrum")))
    stop("[write_R2TIFF()] Only RLum.Data.Image and RLum.Data.Spectrum objects are supported!", call. = FALSE)

  ## check path
  if(!dir.exists(dirname(file)))
    stop("[write_R2TIFF()] Path does not exist!", call. = FALSE)

  ## create file names
  file <- normalizePath(file, mustWork = FALSE)
  file_dir <- dirname(file)
  file_base <- strsplit(basename(file), split = ".", fixed = TRUE)[[1]][1]

  ## expand if longer than 1
  if(length(object) > 1)
    file <- normalizePath(paste0(file_dir,"/",file_base,"_",1:length(object),".tiff"), mustWork = FALSE)

# Export to TIFF ----------------------------------------------------------
  ## remove arguments we already use
  args <- list(...)[!list(...) %in% c("what", "where")]

  ## modify arguments
  args <- modifyList(x = list(
    bits.per.sample = 16L
  ), args)


  for(i in 1:length(object)){
    object[[i]]@data[] <- as.numeric(object[[i]]@data)
    object[[i]]@data[] <- object[[i]]@data / norm[1]
    do.call(what = tiff::writeTIFF, args = c(list(object[[i]]@data, where = file[i]), args))

  }

}
