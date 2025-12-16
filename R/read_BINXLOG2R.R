#'@title Import Risø BINX-data from the BINX Log File
#'
#'@description
#' The function attempts to extract meaningful measurement data from the
#' log file (`.txt`) stored along with a BINX file if this option was chosen.
#' This is particularly helpful if the BINX file is broken and the data would
#' be lost otherwise.
#'
#'@details
#' This function is basically a hack trying to make the most out of a broken file in case
#' at least the log file is salvageable. Please do not expect black magic; the extract is
#' very basic, and the function was written to work for TL, OSL, IRSL curves only.
#' If it breaks, it breaks.
#'
#' **TL curve extraction**
#' Since the BIN/BINX do not contain x-values, they also cannot be created magically
#' here. To avoid problems with the conversion, we assume always BIN-file version `3` for
#' the conversion. This should be okay in most cases, however, it may become problematic
#' if you have data for dedicated curve analysis.
#'
#'@param file [character] (**required**): ASCII log file to read. There is no check
#'the file extension; just see whether it works or not.
#'
#'@param verbose [logical] (*with default*): enable/disable output to the terminal.
#'
#'@param ... further arguments that will be passed to the function (currently not used)
#'
#'@seealso [Luminescence::read_BIN2R], [Luminescence::write_R2BIN], [Luminescence::Risoe.BINfileData-class],
#' [base::readLines], [Luminescence::RLum.Analysis-class], [list.files]
#'
#'@return
#' Returns an S4 [Luminescence::RLum.Analysis-class] object or a [list] of it.
#'
#'@note
#' This function tries to extract data from a log file produced for debugging
#' purposes. There is no guarantee regarding the format stability, and the
#' function is meant as a last resort if your BIN/BINX files are unusable.
#'
#'@section Function version: 0.1.0
#'
#'@author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@examples
#'## read log file (RLum.Analysis)
#'file <-  system.file("extdata/BINX_IRSL_LOG.TXT", package = "Luminescence")
#'object <-  read_BINXLOG2R(file = file)
#'
#'## convert to RisoeBINFile-class object
#'object <- convert_RLum2Risoe.BINfileData(object)
#'
#'## export as BINX
#'\dontrun{
#'  write_R2BIN(t, file = tempfile(), version = "04")
#'}
#'
#'@keywords IO
#'@md
#'@export
read_BINXLOG2R <- function(
  file,
  verbose = TRUE,
  ...
  ){
  .set_function_name("read_BINXLOG2R")
  on.exit(.unset_function_name(), add = TRUE)

# Incoming integrity ------------------------------------------------------
  .validate_class(file, c("character", "list"))
  .validate_logical_scalar(verbose)

# Self-call ---------------------------------------------------------------
  if(inherits(file, "list")) {
    out <- lapply(file, read_BINXLOG2R, verbose = verbose)
    return(unlist(out, recursive = FALSE))
  }

  .validate_length(file, 1)

# Function core -----------------------------------------------------------
  ## check whether the file is real
  if(!file.exists(file))
    .throw_error("File does not exist!")

  ## open file connection and ensure that it properly closed
  con <- file(file, "rb")
  on.exit(close(con), add = TRUE)

  if (verbose) {
    cat("\n[read_BINXLOG2R()] Importing ...")
    cat("\n path: ", dirname(file))
    cat("\n file: ", .shorten_filename(basename(file[1])))
    cat("\n size: ", round(file.size(file[1]) / 1000, 2), "kB")
    cat("\n")
  }

  ## reminder
  ## All those suppressed warnings should avoid all kind of errors
  ## when we have different encodings, we then just try.

  ## read entire file
  lines <- readLines(con, skipNul = TRUE, warn = FALSE)

  ## check first 20 lines for keywords, this should reduce the number of failures
  if(sum(grepl("Sequence Name|Datafile Name|Error trapping is active", lines[1:20])) != 3)
     .throw_error("The file does not seem to be a valid debugging log-file")

  ## extra meta header
  FNAME <- trimws(
    regmatches(
      x = lines[1:5],
      regexpr(pattern = "(?<=Datafile Name\\:).+", lines[1:5], perl = TRUE)))
  SYSTEMID <- as.numeric(
    sub(".*: *([0-9]+).*", "\\1", lines[3:8][grepl("System ID", lines[3:8])]))

  ## remove all unwanted lines
  clean_lines <- suppressWarnings(
    lines[!grepl("Wait", lines, fixed = TRUE)])

  ## get all current operation lines
  co <- suppressWarnings(grep("Current operation",
       clean_lines, fixed = TRUE, value = TRUE, useBytes = TRUE))

      ## extract the DATE, TIME, RUN, SET, SAMPLE (POSITION)
      ## we extract the rest laster in the super loop
      pattern <- "^([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})\\s+([0-9]{2}:[0-9]{2}:[0-9]{2}).*Set:\\s*(\\d+)\\s+Run:\\s*(\\d+)\\s+Sample:\\s*(\\d+)"
      header <- regmatches(co,  regexec(pattern, co, useBytes = TRUE))

        ## we extract only the position here, because this we will use
        ## to sort the dataset
        POSITION <- as.integer(vapply(header, \(x) x[6], character(1)))
        POSITION_ORDER <- order(POSITION)

        COMMENT <-  vapply(header, \(x) x[1], character(1))
        DATE <-  as.Date(vapply(header, \(x) x[2], character(1)), format = "%d.%m.%Y")
        TIME <-  vapply(header, \(x) x[3], character(1))
        SET <- as.integer(vapply(header, \(x) x[4], character(1)))
        RUN <- as.integer(vapply(header, \(x) x[5], character(1)))

  ## get start boundaries for blocks and build start and stop matrix of the blocks
  start_id <- suppressWarnings(
    grep(pattern = "Current operation:", x = clean_lines, fixed = TRUE, useBytes = TRUE))
  end_id <- c(start_id[-1] - 1, length(clean_lines))
  groups  <- rep(seq_along(start_id), times = end_id - start_id + 1)

  ## split the blocks
  data_blocks <- split(clean_lines[-(1:start_id[1] - 1)], groups)

  ## now iterate over the current operation blocks; we don't
  ## extract in one go an match later because we don't know
  ## what is stored and whether there was an error
  out <- lapply(sort(unique(POSITION)), \(i) {
    records <- lapply(which(i == POSITION), \(j) {
      ## check for any data in the data, if not return empty object
      if(suppressWarnings(!any(grepl(pattern = "Data", x = data_blocks[[j]], fixed = TRUE, useBytes = TRUE))))
        return(set_RLum("RLum.Data.Curve"))

      ## get record Type and extract depending on the TYPE
      ## preset (you never know)
      recordType <- list(
        LTYPE = NA_character_,
        HIGH = 0,
        LOW = 0,
        RATE = 0,
        NPOINTS = 0,
        AN_TEMP = 0)

      ## TL
      recordType <- if(suppressWarnings(any(grepl("TL", data_blocks[[j]], fixed = TRUE, useBytes = TRUE)))) {
        match <- suppressWarnings(strsplit(unlist(regmatches(
          x = data_blocks[[j]],
          m = regexec(
            pattern = "TL\\s[0-9]+\\s[0-9]+.[0-9]+\\s[0-9]+\\s[0-9]+",
            text = data_blocks[[j]], perl = TRUE, useBytes = TRUE))), split = " ", fixed = TRUE,
          useBytes = TRUE))

        ## get elements and combine those
        LTYPE <- unique(vapply(match, \(x) x[[1]], character(1)))
        HIGH <- max(as.numeric(vapply(match, \(x) x[[2]], character(1))))
        RATE <- mean(as.numeric(vapply(match, \(x) x[[3]], character(1))))
        AN_TEMP <- sum(as.numeric(vapply(match, \(x) x[[5]], character(1))))

        ## return
        list(LTYPE = LTYPE, HIGH = HIGH, RATE = RATE, NPOINTS = 0, AN_TEMP = AN_TEMP, LOW = 0)

      ## IRSL and OSL
      } else if (suppressWarnings(any(grepl("IR SET", data_blocks[[j]], fixed = TRUE, useBytes = TRUE)))){
        ## get relevant data
        LTYPE <- "IRSL"
        match <- suppressWarnings(unlist(regmatches(
          x = data_blocks[[j]],
          m = regexec(
            pattern = "(?<=PO\\sI).+",
            text = data_blocks[[j]],
            useBytes = TRUE,
            perl = TRUE))))

        ## if it is not IRSL, it is OSL
        if(length(match) == 0) {
          LTYPE <- "OSL"
          match <- suppressWarnings(unlist(regmatches(
            x = data_blocks[[j]],
            m = regexec(
              pattern = "(?<=OS\\sB).+",
              text = data_blocks[[j]],
              useBytes = TRUE,
              perl = TRUE))))
         }

        ## get HIGH and NPOINTS
        match <- scan(text = match[nzchar(match)], what = numeric(), quiet = TRUE)
        HIGH <- match[1]

        ## get rate and temperature (the first)
        match <- suppressMessages(unlist(regmatches(
          x = data_blocks[[j]],
          m = regexec(
            pattern = "(?<=ST\\s).+",
            text = data_blocks[[j]], perl = TRUE,
            useBytes = TRUE))))

        ## get AN_TEMP and RATE
        match <- scan(text = match, what = numeric(), quiet = TRUE)
        AN_TEMP <- match[1]
        RATE <- match[2]

        ## return
        list(
          LTYPE = LTYPE,
          HIGH = HIGH,
          RATE = RATE,
          NPOINTS = 0,
          AN_TEMP = AN_TEMP,
          LOW = 0)
      }

      ## determine how many data blocks we have
      n_data <- suppressWarnings(
        grep(pattern = "Data downloaded", x = data_blocks[[j]], fixed = TRUE, useBytes = TRUE))

      ## loop over data blocks
      data <- unlist(lapply(n_data, \(x) {
          ## get data blocks boundaries
          header <- data_blocks[[j]][x]
          n_points <- suppressWarnings(as.numeric(sub(".*\\s", "", header)))
          n_lines <- floor(n_points / 25)

          ## get block
          block <- data_blocks[[j]][seq.int(x + 1, length.out = n_lines)]

          ## get everything after two spaces
          values <- suppressWarnings(
            regmatches(block, regexpr("(?<=\\s\\s).*", block, perl = TRUE, useBytes = TRUE)))

          ## parse numbers
          scan(text = values, what = numeric(), quiet = TRUE)
      }))

      ## create data using internal xy constructor
      DATA <- matrix(NA, ncol = 2)
      if (!is.na(recordType[["LTYPE"]])) {
        DATA <- src_create_RLumDataCurve_matrix(
          DATA = data,
          VERSION = 3, ## we hard code this to V3 to prevent error messages
          NPOINTS = length(data), # this helps to prevent errors if NPOINTS is wrong
          LOW = recordType[["LOW"]],
          HIGH = recordType[["HIGH"]],
          LTYPE = recordType[["LTYPE"]],
          AN_TEMP = recordType[["AN_TEMP"]],
          TOLDELAY = 0L, TOLON = 0L, TOLOFF = 0L)
      }

      ## create curve object
      set_RLum(
        class = "RLum.Data.Curve",
        originator = "read_BINXLOG2R",
        recordType = recordType[["LTYPE"]],
        data = DATA,
        info = list(
          DATE = DATE[j],
          TIME = TIME[j],
          POSITION = POSITION[j],
          RUN = RUN[j],
          SET = SET[j],
          FNAME = FNAME,
          SYSTEMID = SYSTEMID,
          COMMENT = substr(COMMENT[j], 1, 70))
        )
    })

    ## remove recordType NA
    records[sapply(records, \(x) is.na(x@recordType))] <- NULL

    ## create RLum.Analysis
    set_RLum("RLum.Analysis", records = records, originator = "read_BINXLOG2R")

  })

  ## return
  return(out)
}
