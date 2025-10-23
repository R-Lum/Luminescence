#' @title Export RLum.Data.Image and RLum.Data.Spectrum objects to TIFF Images
#'
#' @description
#' Simple wrapper around [tiff::writeTIFF] to export suitable [RLum-class]
#' objects to TIFF images. Per default 16-bit TIFF files are exported.
#'
#' @param object [RLum.Data.Image-class] or [RLum.Data.Spectrum-class] object (**required**):
#' input object, can be a [list] of such objects.
#'
#' @param file [character] (**required**):
#' name of the output file.
#'
#' @param norm [numeric] (*with default*):
#' normalisation value. Usually, in imaging applications the pixel values are
#' integer count values, but values in TIFF files must be in the 0-1 range.
#' Normalising to the to the highest 16-bit integer values - 1 ensures that
#' the numerical values are retained in the exported image. If `1` nothing is
#' normalised.
#'
#' @param ... further arguments to be passed to [tiff::writeTIFF].
#'
#' @return A TIFF file
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @section Function version: 0.1.2
#'
#' @seealso [tiff::writeTIFF], [RLum.Data.Image-class], [RLum.Data.Spectrum-class]
#'
#' @keywords IO
#'
#' @examples
#' data(ExampleData.RLum.Data.Image, envir = environment())
#' write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile(fileext = ".tiff"))
#'
#' @export
write_R2TIFF <- function(
  object,
  file = tempfile(),
  norm = 65535,
  ...
) {
  .set_function_name("write_R2TIFF")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  ## most of the users don't need this import, no need to bother them
  ## with required libraries
  .require_suggested_package("tiff", "Exporting objects to TIFF files")

  ## make a list ... it is just easier
  if (!inherits(object, "list"))
    object <- list(object)

  ## check list input
  sapply(object, function(x) {
    .validate_class(x, c("RLum.Data.Image", "RLum.Data.Spectrum"),
                    extra = "a 'list' of such objects",
                    name = "'object'")
    if (inherits(x, "RLum.Data.Image") && length(dim(x@data)) != 3)
      .throw_error("Empty RLum.Data.Image object detected")
  })

  .validate_class(file, "character")
  .validate_positive_scalar(norm)

  ## Prepare filenames ------------------------------------------------------

  file <- normalizePath(file, mustWork = FALSE)
  file_dir <- dirname(file)
  if (!dir.exists(file_dir))
    .throw_error("Path '", file_dir, "' does not exist")

  ## expand if longer than 1
  if(length(object) > 1)
    file <- paste0(tools::file_path_sans_ext(file), "_", 1:length(object), ".tiff")

  ## Export to TIFF ---------------------------------------------------------

  ## modify arguments
  args <- modifyList(x = list(
    bits.per.sample = 16L
  ), list(...))

  ## remove arguments we already use
  args[c("what", "where")] <- NULL

  for (i in seq_along(object)) {
    data <- object[[i]]@data

    ## consider the case that we have already an image stack
    if (length(dim(data)) > 2 && dim(data)[3] > 1) {
      img_list <- lapply(1:dim(data)[3], function(x) {
        m <- data[, , x]
        storage.mode(m) <- "numeric"
        m / norm
      })
    } else {
      img_list <- data / norm
    }

    ## write file
    do.call(what = tiff::writeTIFF, args = c(list(img_list, where = file[i]), args))
  }
}
