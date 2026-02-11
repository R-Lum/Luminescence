#'@title Import Luminescence Data from Helios Luminescence Reader
#'
#' @description
#' Import of files with `.osl` extension produced by the zero rad Helios
#' luminescence reader and conversion to [Luminescence::RLum.Analysis-class]
#' objects.
#'
#' @param file [character], [list] (**required**):
#' name of one or multiple `.osl` files (URLs are supported); it can be the
#' path to a directory, in which case the function tries to detect and import
#' all `.osl` files found in the directory.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#'@param ... not in use, for compatibility reasons only
#'
#'@note Thanks to Krzysztof Maternicki for providing example data.
#'
#' @return
#' A [Luminescence::RLum.Analysis-class] object. Results are returned as a
#' list when multiple files are processed or `file` is a list.
#'
#'@author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @section Function version: 0.1.1
#'
#'@seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Analysis-class]
#'
#'@keywords IO
#'
#'@examples
#'file <- system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence")
#'read_HeliosOSL2R(file)
#'
#'@export
read_HeliosOSL2R <- function(
  file,
  verbose = TRUE,
  ...
) {
  .set_function_name("read_HeliosOSL2R")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_logical_scalar(verbose)
  file <- .validate_file(file, ext = "osl", pattern = "\\.osl$",
                         verbose = verbose)

  ## Self-call --------------------------------------------------------------
  if (inherits(file, "list")) {
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
    footer_id <- grep(pattern = '\\"', x = lines)[2]

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
  set_RLum(
    class = "RLum.Analysis",
    originator = "read_HeliosOSL2R",
    records = records)
}
