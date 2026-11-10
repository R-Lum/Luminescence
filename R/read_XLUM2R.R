#' @title Import XLUM files into R
#'
#' @description
#' Imports XLUM files produced into R.
#'
#' @param file [character] or [list] (**required**):
#' name of one or multiple XLUM files (URLs are supported); it can be the path
#' to a directory, in which case the function tries to detect and import all
#' XLUM files found recursively from the given directory.
#'
#' @param fastForward [logical] (*with default*):
#' if `TRUE` a list of [Luminescence::RLum.Analysis-class] objects is returned.
#'
#' @param import [logical] (*with default*):
#' whether to read and import the file content (`TRUE` by default). If set to
#' `FALSE`, only the XLUM file structure is returned.
#'
#' @param pattern [character] (*with default*):
#' regular expression pattern passed to [list.files] to construct a list of
#' files to read. This is used only when `file` is a path, otherwise it's
#' ignored.
#'
#' @param n_sequences [numeric] (*with default*):
#' number of sequences to be imported; by default all sequences are imported.
#' It is ignored if `import = FALSE`.
#'
#' @param n_records [numeric] (*with default*):
#' number of records to be imported; by default all records are imported. Note
#' that since a single record can contain multiple curves (the common case for
#' XSYG files), the `@records` slot in the object returned may hold more
#' elements than specified in `n_records`. It is ignored if `import = FALSE`.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar during import. Ignored if `verbose = FALSE`.
#'
#' @return
#' **Using the option `import = TRUE` (default)**
#'
#' By default, a list consisting of two elements is returned:
#' \item{Sequence.Header}{[data.frame] with information on the
#' sequence.}
#' \item{Sequence.Object}{[Luminescence::RLum.Analysis-class]
#' containing the curves.}
#'
#' If `fastForward = TRUE`, an [Luminescence::RLum.Analysis-class] object is
#' returned.
#'
#' **Using the option `import = FALSE`**
#'
#' A list consisting of two elements is shown:
#' - [data.frame] with information on the samples.
#' - [data.frame] with information on the sequences stored in the XLUM file.
#'
#' If `fastForward = TRUE`, the same information is returned as a single
#' data frame.
#'
#' @note
#' This function is in alpha status.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @seealso [Luminescence::RLum.Analysis-class], [Luminescence::RLum.Data.Curve-class]
#'
#' @references
#' Kreutzer, S., Grehl, S., Höhne, M., Simmank, O., Dornich, K., Adamiec, G.,
#' Burow, C., Roberts, H. M., Duller, G. A. T., 2023.
#' XLUM: an open data format for exchange and long-term preservation of luminescence data.
#' Geochronology, 5, 271-284, \doi{10.5194/gchron-5-271-2023}
#'
#' XLUM format specification, [https://r-lum.github.io/xlum_specification/specification/]()
#'
#' @keywords IO
#'
#' @examples
#' file <- system.file("extdata/XLUM_file.xlum", package = "Luminescence")
#' xlum <- read_XLUM2R(file)
#'
#' @export
read_XLUM2R <- function(
  file,
  fastForward = FALSE,
  import = TRUE,
  pattern = "\\.xlum",
  n_sequences = NULL,
  n_records = NULL,
  verbose = TRUE,
  txtProgressBar = TRUE
) {
  .set_function_name("read_XLUM2R")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_logical_scalar(verbose)
  file <- .validate_file(file, pattern = pattern, recursive = TRUE,
                         throw.error = FALSE, verbose = verbose)
  if (length(file) == 0)
    return(NULL)

  .validate_logical_scalar(fastForward)
  .validate_logical_scalar(import)
  .validate_positive_scalar(n_sequences, int = TRUE, null.ok = TRUE)
  .validate_positive_scalar(n_records, int = TRUE, null.ok = TRUE)
  if (!is.null(n_sequences) && !is.null(n_records))
    .throw_error("Only one of 'n_sequences' and 'n_records' can be specified")
  .validate_logical_scalar(txtProgressBar)
  check_base64 <- .require_suggested_package("base64enc", throw.error = FALSE,
                                             "Reading base64-encoded XLUM files")

  ## Self-call --------------------------------------------------------------
  if (inherits(file, "list")) {
    output <- lapply(seq_along(file), function(x) {
      read_XLUM2R(
          file = file[[x]],
          fastForward = fastForward,
          import = import,
          pattern = pattern,
          n_sequences = n_sequences,
          n_records = n_records,
          verbose = verbose,
          txtProgressBar = txtProgressBar
      )
    })

    if (!fastForward)
      return(output)
    if (import)
      return(unlist(output, recursive = FALSE))

    return(as.data.frame(data.table::rbindlist(output)))
  }

  # (0) config --------------------------------------------------------------

  ## don't show the progress bar if not verbose
  if (!verbose)
    txtProgressBar <- FALSE


  ## set HUGE for larger nodes
  HUGE <- 524288

  ## parse the entire XML tree
  tree <- try(XML::xmlRoot(XML::xmlTreeParse(file, useInternalNodes = TRUE,
                                             options = HUGE, error = NULL)),
              silent = TRUE)
  if (inherits(tree, "try-error")) {
    if (verbose) {
      msg <- trimws(as.character(conditionCall(attr(tree, "condition"))[[2]]))
      .throw_message("XML file cannot be parsed (", msg, "), NULL returned")
    }
    return(NULL)
  }
  if (XML::xmlName(XML::xmlRoot(tree)) != "xlum") {
    .throw_error("<xlum> root node not found in '", file, "'")
  }

  ## only show the file structure
  if (!import) {
    ## sample information
    info.sample <- as.data.frame(t(sapply(XML::xpathSApply(tree, "//sample"),
                                          XML::xmlAttrs)))
    info.sample[info.sample == "NA"] <- NA

    ## sequence information
    info.sequence <- as.data.frame(t(sapply(XML::xpathSApply(tree, "//sequence"),
                                            XML::xmlAttrs)))
    info.sequence[info.sequence == "NA"] <- NA

    ## additional option for fastForward == TRUE
    if (fastForward) {
      colnames(info.sample) <- paste0("sample::", colnames(info.sample))
      output <- cbind(info.sequence, info.sample)
    } else {
      output <- list(Samples = info.sample, Sequences = info.sequence)
    }

    return(output)
  }

  ## ========================================================================
  ## IMPORT XLUM FILE

  ## start the import
  .output_import_message(file, verbose)

  ## number of records in each sequence
  recs <- XML::xpathSApply(tree, "//sequence",
                           function(s) length(XML::getNodeSet(s, ".//record")))
  if (length(recs) == 0)
    return(list())

  ## count how many sequences must be read in order to read n_records
  if (!is.null(n_records)) {
    n_records <- min(n_records, sum(recs))
    n_sequences <- which(cumsum(recs) >= n_records)[1]
  } else {
    n_records <- sum(recs)
    n_sequences <- min(length(recs), n_sequences)
  }

  ## initialize the progress bar
  if (txtProgressBar) {
    pb <- txtProgressBar(min = 0, max = n_sequences, char = "=", style = 3)
  }

  ## attributes on the <xlum> node
  xlum.header <- as.list(XML::xmlAttrs(tree))
  xlum.header[xlum.header == "NA"] <- NA

  ## loop over the <sample> nodes
  all.output <- NULL
  n_sequences_to_read <- n_sequences
  n_records_to_read <- n_records
  for (sample.idx in seq_len(XML::xmlSize(tree))) {
    sample <- tree[[sample.idx]]

    ## attributes on the <sample> node
    sample.header <- as.list(XML::xmlAttrs(sample))
    sample.header[sample.header == "NA"] <- NA

    ## loop over the <sequence> nodes up to the number of records to be read
    output <- lapply(seq_len(min(XML::xmlSize(sample), n_sequences_to_read)), function(x) {
      sequence <- sample[[x]]
      sequence.header <- as.data.frame(XML::xmlAttrs(sequence))
      colnames(sequence.header) <- ""

      sequence.position <- as.integer(sequence.header["position", ])
      sequence.name <- sequence.header["name", ]
      if (sequence.name == "NA")
        sequence.name <- "not_set"

      ###--------------------------------------------------------------------
      ## loop over the <record> nodes inside this sequence
      idx_records <- seq_len(min(XML::xmlSize(sequence), n_records_to_read))
      sequence.object <- unlist(lapply(idx_records, function(i) {
        record <- sequence[[i]]

        ## get all record attributes
        attrs_record <- XML::xmlAttrs(record)
        recordType <- attrs_record["recordType"]

        ## create a fallback, the function should not fail
        if (is.na(recordType))
          recordType <- "not_set"

        ## loop over the <curve> nodes inside this record
        lapply(seq_len(XML::xmlSize(record)), function(j) {
          curve <- record[[j]]
          if (XML::xmlSize(curve) == 0)
            return(NULL)

          ## extract attributes
          attrs <- XML::xmlAttrs(curve)
          xValues <- .string2numeric(attrs["xValues"])
          yValues <- .string2numeric(attrs["yValues"])
          tValues <- .string2numeric(attrs["tValues"])
          vValues <- .string2numeric(trimws(XML::xmlValue(curve)),
                                     check_base64 = check_base64)
          component <- attrs["component"]

          ## combine attributes
          attrs_comb <- as.list(c(attrs, attrs_record))
          attrs_comb <- attrs_comb[!duplicated(names(attrs_comb))]
          attrs_comb[c("xValues", "yValues", "tValues", "curveType")] <- NULL
          attrs_comb[attrs_comb == "NA"] <- NULL

          ## get additional information
          info <- modifyList(attrs_comb,
                             list(position = sequence.position,
                                  sequenceName = sequence.name))


          ## decide what type of object is stored in this node
          values.given <- !c(anyNA(tValues), anyNA(xValues), anyNA(yValues))
          sum.values.given <- sum(values.given)
          ## is.image <- identical(values.given, c(TRUE, TRUE,  TRUE))
          ## is.spect <- identical(values.given, c(TRUE, TRUE,  FALSE))
          ## is.curve <- identical(values.given, c(TRUE, FALSE, FALSE))

          ## set RLum.Data objects
          if (sum.values.given == 3) {
            out.class <- "RLum.Data.Image"
            data <- array(vValues,
                          dim = c(length(yValues), length(xValues), length(tValues)))
          } else if (sum.values.given == 2 && !values.given[3]) {
            out.class <- "RLum.Data.Spectrum"
            data <- matrix(vValues,
                           nrow = length(xValues),
                           ncol = length(tValues),
                           dimnames = list(xValues, tValues))
          } else if (sum.values.given == 1 && values.given[1]) {
            out.class <- "RLum.Data.Curve"
            data <- cbind(tValues, vValues, deparse.level = 0)
          } else {
            .throw_warning("Invalid value specification, <curve> node skipped")
            return(NULL)
          }

          set_RLum(
              class = out.class,
              originator = "read_XLUM2R",
              recordType = paste0(recordType, " (", component, ")"),
              curveType = attrs["curveType"],
              data = data,
              info = info)
        })
      }), use.names = FALSE)

      n_records_to_read <<- n_records_to_read - length(idx_records)

      ## if the XLUM file is broken we get NULL as list element
      if (!is.null(sequence.object)) {
        sequence.object <- set_RLum(
            class = "RLum.Analysis",
            originator = "read_XLUM2R",
            records = sequence.object,
            protocol = as.character(sequence.header["protocol", 1]),
            info = c(list(file = file),
                     xlum.header,
                     sample = sample.header)
        )

        ## set parent uid of RLum.Analysis as parent ID of the records
        sequence.object <- .set_pid(sequence.object)

        ## update progress bar
        if (txtProgressBar) {
          setTxtProgressBar(pb, x)
        }

        ## merge output and return values
        if (fastForward) {
          return(sequence.object)
        }
        return(list(Sequence.Header = sequence.header,
                    Sequence.Object = sequence.object))
      }
    }) ## end loop for sequence list

    all.output <- c(all.output, output)

    n_sequences_to_read <- n_sequences_to_read - XML::xmlSize(sample)
    if (n_sequences_to_read <= 0)
      break
  } ## end loop over samples

  ## close ProgressBar
  if (txtProgressBar)
    close(pb)

  ## show output information
  num.removed <- length(output[sapply(all.output, is.null)])
  if (num.removed > 0)
    .throw_warning(num.removed, " incomplete sequence(s) removed")

  if (verbose) {
    cat(sprintf("\t >> %d of %d sequence(s) loaded successfully\n",
                n_sequences - num.removed, n_sequences))
  }

  ## remove NULL elements and return
  .rm_NULL_elements(all.output)
}

.string2numeric <- function(str, check_base64 = FALSE) {
  if (str == "0")
    return(NA_character_)

  ## check for base64 encoding and convert if needed
  if (isTRUE(check_base64)) {
    ## if it contains white space it is not base64
    if (!grepl(" ", str, fixed = TRUE))
      str <- rawToChar(base64enc::base64decode(str), multiple = FALSE)
  }

  ## convert values
  as.numeric(unlist(lapply(str, strsplit, split = " ", fixed = TRUE)))
}
