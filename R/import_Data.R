#'@title Import Luminescence Data into R
#'
#'@description
#'Convenience wrapper function to provide a quicker and more standardised way of
#'reading data into R by looping through all in the package available data import functions starting with `read_`.
#'
#'@param file [character] (**required**): file to be imported, can be a [list]
#'
#'@param ... arguments to be further passed down to supported functions (please check the functions
#'to determine the correct arguments)
#'
#'@param fastForward [logical] (*with default*): option to create [RLum-class] objects
#'during import or a [list] of such objects
#'
#'@param verbose [logical] (*with default*): enable/disable verbose mode
#'
#'@section Function version: 0.1.2
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso `r paste0("[", grep("^read_", getNamespaceExports("Luminescence"), value = TRUE), "]", collapse = ", ")`
#'
#'@keywords datagen
#'
#'@examples
#'
#' ## import BINX/BIN
#' file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
#' temp <- import_Data(file)
#'
#' ## RF data
#' file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
#' temp <- import_Data(file)
#'
#'@md
#'@export
import_Data <- function (
  file,
  ...,
  fastForward = TRUE,
  verbose = FALSE
) {
  .set_function_name("import_Data")
  on.exit(.unset_function_name(), add = TRUE)

  ## supported functions are extracted automatically from the package
  ## namespace so that we don't have to maintain this list manually
  fun <- grep("^read_", getNamespaceExports("Luminescence"), value = TRUE)

  ## get arguments of functions
  args <- c(list(file = file, fastForward = fastForward, verbose = verbose), list(...))

  ## just try all functions
  for (i in fun) {
    ## get arguments and remove non-supported arguments
    t <- suppressWarnings(suppressMessages(try(do.call(what = i, args = args), silent = TRUE)))
    if (!is.null(t) && !inherits(t, "try-error"))
      return(t)

  }
  .throw_message("Unknown file format, nothing imported")
}
