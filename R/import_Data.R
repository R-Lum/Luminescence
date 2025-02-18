#'@title Import Luminescence Data into R
#'
#'@description
#'Convenience wrapper function to provide a quicker and more standardised way of
#'reading data into R by looping through all in the package available data import
#'functions starting with `read_`. Import data types can be mixed.
#'
#'@param file [character] (**required**): file to be imported, can be a [list] or a [character] vector
#'
#'@param ... arguments to be further passed down to supported functions (please check the functions
#'to determine the correct arguments)
#'
#'@param fastForward [logical] (*with default*): option to create [RLum-class] objects
#'during import or a [list] of such objects
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#'@section Function version: 0.1.5
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@returns Always returns a [list]; empty or filled with [RLum.Analysis-class] objects
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

  .validate_class(file, c("character", "list"))

  ## supported functions are extracted automatically from the package
  ## namespace so that we don't have to maintain this list manually
  fun <- grep("^read_", getNamespaceExports("Luminescence"), value = TRUE)

  ## make the file handling just a little bit easier
  if(length(file) > 1)
    file <- as.list(file)

  ## get arguments of functions
  args <- c(list(file = file, fastForward = fastForward, verbose = verbose), list(...))

  ## set empty output list
  out <- list()

  ## just try all functions and all files
  for (i in fun) {
      ## get arguments and remove non-supported arguments
      t <- suppressWarnings(suppressMessages(try(do.call(what = i, args = args), silent = TRUE)))

      if (!is.null(t) && !inherits(t, "try-error"))
        out <- c(out, t)

  }

  ## return if output is not empty
  if (length(out) == 0)
   .throw_message("Unknown file format, nothing imported")

  return(out)

}
