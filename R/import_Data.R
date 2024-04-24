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
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [read_BIN2R], [read_XSYG2R], [read_PSL2R], [read_SPE2R], [read_TIFF2R], [read_RF2R],
#'[read_Daybreak2R]
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
  ## supported functions
  fun <- c(
    "read_BIN2R",
    "read_XSYG2R",
    "read_PSL2R",
    "read_Daybreak2R",
    "read_RF2R",
    "read_SPE2R",
    "read_TIFF2R")

  ## get arguments of functions
  args <- c(list(file = file, fastForward = fastForward, verbose = verbose), list(...))

  ## just try all functions
  for (i in fun) {
    ## get arguments and remove non-supported arguments
    t <- suppressWarnings(suppressMessages(try(do.call(what = i, args = args), silent = TRUE)))
    if (!is.null(t) && !inherits(t, "try-error"))
      return(t)

  }
  message("[import_Data()] Unknown file format, nothing imported!")

}
