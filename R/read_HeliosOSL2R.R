#'@title Import Luminescence Data from Helios Luminescence Reader
#'
#'@description Straightforward import of files with the ending `.osl` produced
#'by the zero rad Helios luminescence reader and conversion to [RLum.Analysis-class] objects.
#'
#'@param file [character] (**required**): path to file to be imported. Can be a [list]
#'for further processing
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#'@param ... not in use, for compatibility reasons only
#'
#'@note Thanks to Krzysztof Maternicki for providing example data.
#'
#'@return [RLum.Analysis-class] object
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@section Function version: 0.1.0
#'
#'@seealso [RLum.Data.Curve-class], [RLum.Analysis-class]
#'
#'@keywords IO
#'
#'@examples
#'file <- system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence")
#'read_HeliosOSL2R(file)
#'
#'@md
#'@export
read_HeliosOSL2R <- function(
  file,
  verbose = TRUE,
  ...
) {
  .set_function_name("read_HeliosOSL2R")
  on.exit(.unset_function_name(), add = TRUE)

# Self-call ---------------------------------------------------------------
  if(inherits(file, "list")) {
    out <- lapply(file, function(x) {
      tmp <- try(read_HeliosOSL2R(x, verbose = verbose), silent = TRUE)
      if(inherits(tmp, "try-error")) {
        .throw_message(strsplit(tmp, ":", fixed = TRUE)[[1]][2], "->... record skipped!")
        return(NULL)
      }
      tmp

    })

    ## remove NULL
    out <- .rm_NULL_elements(out)

    if(length(out) == 0)
      out <- NULL

    return(out)
  }


  ## Integrity checks -------------------------------------------------------

  .validate_class(file, c("character", "list"))
  .validate_length(file, 1)

  ## check file format
  if (tolower(ext <- tools::file_ext(file)) != "osl")
    .throw_error("File extension '", ext, "' not supported")

  ## fix path
  file <- normalizePath(file)

  ## Import -----------------------------------------------------------------

  if (verbose) {
    cat("\n[read_HeliosOSL2R()] Importing ...")
    cat("\n path: ", dirname(file))
    cat("\n file: ", .shorten_filename(basename(file)))
    cat("\n")
  }

  ## read entire file
  lines <- readLines(file)

    ## get footer lines id, which is printed in quotes but not the first
    footer_id <- which(grepl(pattern = '\\"', x = lines, fixed = FALSE))[2]

    ## import data measurement data
    df <- read.table(
      file = textConnection(lines[1:(footer_id - 1)]),
      header = TRUE,
      sep = ",")

    ## extract metadata
    ## remove quotes and split at : + whitespace
    meta <- strsplit(x = gsub(
      pattern = '\\"',
      replacement = "",
      x = lines[footer_id:length(lines)]),
      split = ": ",
      fixed = TRUE)

    ## drop lines object
    rm(lines)

    ## create info object
    ## extract names
    info_names <- vapply(seq_along(meta), function(x){
      if(length(meta[[x]]) == 2)
        meta[[x]][1]
      else
        paste0("unnamed_", x)

    }, character(1))
    info_names <- gsub(pattern = " ", "_", x = info_names, fixed = TRUE)

    ## extra elements
    info <- lapply(meta, function(x){
      if(length(x) == 2)
        x[2]
      else
        x[1]

    })
    names(info) <- info_names

# Create object -----------------------------------------------------------
  ## this needs to be checked for the moment
  ## we extract only three curves
  pid <- create_UID()

  ## get records
  records <- lapply(3:ncol(df), function(x){
    set_RLum(
      class = "RLum.Data.Curve",
      originator = "read_HeliosOSL2R",
      curveType = "measured",
      recordType = paste0("OSL (", colnames(df)[x], ")"),
      data = as.matrix(df[,c(2,x)]),
      info = c(
        xlab = colnames(df)[2],
        ylab = colnames(df)[x],
        parentID = pid,
        info))
  })

  ## create RLum.Analysis as output
  object <- set_RLum(
    class = "RLum.Analysis",
    originator = "read_HeliosOSL2R",
    records = records)

  return(object)
}
