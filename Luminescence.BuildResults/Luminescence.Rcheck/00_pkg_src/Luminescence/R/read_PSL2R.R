#' @title Import PSL files to R
#'
#' @description Imports PSL files produced by a SUERC portable OSL reader into R.
#'
#' @details This function provides an import routine for the SUERC portable OSL Reader PSL
#' format (measurement data and sequence). PSL files are just plain text and can be
#' viewed with any text editor.  Due to the formatting of PSL files this import
#' function relies heavily on regular expression to find and extract all relevant information. See **note**.
#'
#' @param file [character] (**required**):
#' path and file name of the PSL file. If input is a `vector` it should comprise
#' only `character`s representing valid paths and PSL file names.
#' Alternatively the input character can be just a directory (path). In this case the
#' the function tries to detect and import all PSL files found in the directory.
#'
#' @param drop_bg [logical] (*with default*):
#' `TRUE` to automatically remove all non-OSL/IRSL curves.
#'
#' @param as_decay_curve  [logical] (*with default*):
#' Portable OSL Reader curves are often given as cumulative light sum curves.
#' Use `TRUE` (default) to convert the curves to the more usual decay form.
#'
#' @param smooth [logical] (*with default*):
#' `TRUE` to apply Tukey's Running Median Smoothing for OSL and IRSL decay curves.
#' Smoothing is encouraged if you see random signal drops within the decay curves related
#' to hardware errors.
#'
#' @param merge [logical] (*with default*):
#' `TRUE` to merge all `RLum.Analysis` objects. Only applicable if multiple
#' files are imported.
#'
#' @param ... currently not used.
#'
#' @return
#' Returns an S4 [RLum.Analysis-class] object containing
#' [RLum.Data.Curve-class] objects for each curve.
#'
#' @seealso [RLum.Analysis-class], [RLum.Data.Curve-class], [RLum.Data.Curve-class]
#'
#' @author Christoph Burow, University of Cologne (Germany),
#'  Sebastian Kreutzer, Institut of Geography, Heidelberg University (Germany)
#'
#' @section Function version: 0.1.1
#'
#' @note
#' Because this function relies heavily on regular expressions to parse
#' PSL files it is currently only in beta status. If the routine fails to import
#' a specific PSL file please report to `<christoph.burow@@gmx.net>` so the
#' function can be updated.
#'
#' @keywords IO
#'
#' @examples
#'
#' # (1) Import PSL file to R
#'
#' file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
#' psl <- read_PSL2R(file, drop_bg = FALSE, as_decay_curve = TRUE, smooth = TRUE, merge = FALSE)
#' print(str(psl, max.level = 3))
#' plot(psl, combine = TRUE)
#'
#' @md
#' @export
read_PSL2R <- function(file, drop_bg = FALSE, as_decay_curve = TRUE, smooth = FALSE, merge = FALSE, ...) {
  ## INPUT VALIDATION ----
  if (length(file) == 1) {
    if (!grepl(".psl$", file, ignore.case = TRUE)) {
      file <- list.files(file, pattern = ".psl$", full.names = TRUE, ignore.case = TRUE)
      if (length(file) == 0)
        stop("[read_PSL2R()]: No .psl files found", call. = FALSE)
      message("[read_PSL2R()]: The following files were found and imported: \n", paste(" ..", file, collapse = "\n"))
    }
  }
  if (!all(file.exists(file)))
    stop("The following files do not exist, please check: \n",
         paste(file[!file.exists(file)], collapse = "\n"), call. = FALSE)

  ## MAIN ----
  results <- vector("list", length(file))
  for (i in 1:length(file)) {
    ## Read in file ----
    doc <- readLines(file[i])

    ## Document formatting ----
    # remove lines with i) blanks only, ii) dashes, iii) equal signs
    doc <- gsub("^[ ]*$", "", doc)
    doc <- gsub("^[ -]*$", "", doc)
    doc <- gsub("^[ =]*$", "", doc)

    # the header ends with date and time with the previous line starting with a single slash
    lines_with_slashes <- doc[grepl("\\", doc, fixed = TRUE)]

    ## OFFENDING LINE: this deletes the line with sample name and time and date
    sample_and_date <- lines_with_slashes[length(lines_with_slashes)]
    sample <- trimws(gsub("\\\\", "", strsplit(sample_and_date, "@")[[1]][1]))
    date_and_time <- strsplit(strsplit(sample_and_date, "@")[[1]][2], " ")[[1]]
    date_and_time_clean <- date_and_time[date_and_time != "" & date_and_time != "/" & date_and_time != "PM" & date_and_time != "AM"]
    date <- as.Date(date_and_time_clean[1], "%m/%d/%Y")
    time <- format(date_and_time_clean[2], format = "%h:%M:%S")
    doc <- gsub(lines_with_slashes[length(lines_with_slashes)],
                "", fixed = TRUE, doc)

    # last delimiting line before measurements are only apostrophes and dashes
    lines_with_apostrophes <-doc[grepl("'", doc, fixed = TRUE)]
    doc <- gsub(lines_with_apostrophes[length(lines_with_apostrophes)],
                "", fixed = TRUE, doc)

    # finally remove all empty lines
    doc <- doc[doc != ""]

    ## Split document ----
    begin_of_measurements <- grep("Measurement :", doc, fixed = TRUE)
    number_of_measurements <- length(begin_of_measurements)

    # Parse and format header
    header <- doc[1:(begin_of_measurements[1]-1)]
    header <- format_Header(header)

    # add sample name, date and time to header list
    header$Date <- date
    header$Time <- time
    header$Sample <- sample

    # Parse and format the measurement values
    measurements_split <- vector("list", number_of_measurements)

    # save lines of each measurement to individual list elements
    for (j in seq_len(number_of_measurements)) {
      if (j != max(number_of_measurements))
        measurements_split[[j]] <- doc[begin_of_measurements[j]:(begin_of_measurements[j+1] - 1)]
      else
        measurements_split[[j]] <- doc[begin_of_measurements[j]:length(doc)]
    }

    # format each measurement; this will return a list of RLum.Data.Curve objects
    measurements_formatted <- lapply(measurements_split, function(x) {
      format_Measurements(x, convert = as_decay_curve, header = header)
    })

    # drop dark count measurements if needed
    if (drop_bg) {
      measurements_formatted <- lapply(measurements_formatted, function(x) {
        if (x@recordType != "USER")
          return(x)
      })
      measurements_formatted <- measurements_formatted[!sapply(measurements_formatted, is.null)]
    }

    # decay curve smoothing using Tukey's Running Median Smoothing (?smooth)
    if (smooth) {
      measurements_formatted <- lapply(measurements_formatted, function(x) {
        if (x@recordType != "USER")
          x@data[,2] <- smooth(x@data[ ,2])
        return(x)
      })
    }

    ## get measurement sequence
    measurement_sequence <- data.table::rbindlist(
      lapply(seq_along(measurements_split), function(x) {
      ## remove measurement
      tmp <- gsub(
        pattern = "Measurement : ",
        replacement = "",
        x = measurements_split[[x]][1],
        fixed = TRUE)

    ## split entries
     tmp <- strsplit(x = tmp, split = " | ", fixed = TRUE)[[1]]

     ## data.frame
     data.frame(
       RUN = x,
       NAME = trimws(tmp[1]),
       STIM = strsplit(tmp[2], split = " ", fixed = TRUE)[[1]][2],
       ON_OFF = strsplit(tmp[3], split = "(us)", fixed = TRUE)[[1]][2],
       CYCLE = strsplit(tmp[4], split = "(ms),", fixed = TRUE)[[1]][2])

    }))

    ## RETURN ----
    results[[i]] <- set_RLum(
      "RLum.Analysis",
       protocol = "portable OSL",
       info = c(
         header,
         list(Sequence = measurement_sequence)),
       records = measurements_formatted)
  }#Eof::Loop

  ## MERGE ----
  if (length(results) > 1 && merge)
    results <- merge_RLum(results)

  ## RETURN ----
  if (length(results) == 1)
    results <- results[[1]]

  return(results)
}

################################################################################
## HELPER FUNCTIONS
################################################################################


## ------------------------- FORMAT MEASUREMENT ----------------------------- ##
format_Measurements <- function(x, convert, header) {
  ## measurement parameters are given in the first line
  settings <- x[1]

  settings_split <- unlist(strsplit(settings, "|", fixed = TRUE))

  # welcome to regex/strsplit hell
  settings_measurement <- trimws(gsub(".*: ", "", settings_split[which(grepl("Measure", settings_split))]))
  settings_stimulation_unit <- gsub("[^0-9]", "", settings_split[which(grepl("Stim", settings_split))])
  settings_on_time <- as.integer(unlist(strsplit(gsub("[^0-9,]", "", settings_split[which(grepl("Off", settings_split))]), ","))[1])
  settings_off_time <- as.integer(unlist(strsplit(gsub("[^0-9,]", "", settings_split[which(grepl("Off", settings_split))]), ","))[2])
  settings_cycle <- na.omit(as.integer(unlist(strsplit(gsub("[^0-9,]", "", settings_split[which(grepl("No", settings_split))]), ","))))[1]
  settings_stimulation_time <- na.omit(as.integer(unlist(strsplit(gsub("[^0-9,]", "", settings_split[which(grepl("No", settings_split))]), ","))))[2]

  settings_list <- list("measurement" = settings_measurement,
                        "stimulation_unit" = switch(settings_stimulation_unit, "0" = "USER", "1" = "IRSL", "2" = "OSL"),
                        "on_time" = settings_on_time,
                        "off_time" = settings_off_time,
                        "cycle" = settings_cycle,
                        "stimulation_time" = settings_stimulation_time)

  ## terminal counts are given in the last line
  terminal_count_text <- x[length(x)]

  terminal_count_text_formatted <- gsub("[^0-9]", "",
                                        unlist(strsplit(terminal_count_text, "/")))

  terminal_count <- as.numeric(terminal_count_text_formatted[1])
  terminal_count_error <- as.numeric(terminal_count_text_formatted[2])


  ## parse values and create a data frame
  x_stripped <- x[-c(1, 2, length(x))]

  df <- data.frame(matrix(NA, ncol = 5, nrow = length(x_stripped)))

  for (i in 1:length(x_stripped)) {
    x_split <- unlist(strsplit(x_stripped[i], " "))
    x_split <- x_split[x_split != ""]
    x_split_clean <- gsub("[^0-9\\-]", "", x_split)
    x_split_cleaner <- x_split_clean[x_split_clean != "-"]

    df[i, ] <- as.numeric(x_split_cleaner)
  }

  names(df) <- c("time", "counts", "counts_error",
                 "counts_per_cycle", "counts_per_cycle_error")


  # shape of the curve: decay or cumulative
  if (convert)
    data <- matrix(c(df$time, df$counts_per_cycle), ncol = 2)
  else
    data <- matrix(c(df$time, df$counts), ncol = 2)

  # determine the stimulation type
  if (grepl("Stim 0", settings)) {
    recordType <- "USER"
  }
  if (grepl("Stim 1", settings)) {
    recordType <- "IRSL"
  }
  if (grepl("Stim 2", settings)) {
    recordType <- "OSL"
  }

  object <- set_RLum(
    class = "RLum.Data.Curve",
    originator = "read_PSL2R",
    recordType = recordType,
    curveType = "measured",
    data = data,
    info = list(settings = c(settings_list, header),
    raw_data = df))

  return(object)

}

## ---------------------------- FORMAT HEADER ------------------------------- ##
format_Header <- function(x) {
  header_formatted <- list()

  # split by double blanks
  header_split <- strsplit(x, "  ", fixed = TRUE)

  # check whether there are twice as many values
  # as colons; if there is an equal amount, the previous split was not sufficient
  # and we need to further split by a colon (that is followed by a blank)
  header_split_clean <- lapply(header_split, function(x) {
    x <- x[x != ""]
    n_elements <- length(x)
    n_properties <- length(grep(":", x, fixed = TRUE))

    if (n_elements / n_properties == 1)
      x <- unlist(strsplit(x, ": ", fixed = TRUE))

    return(x)
  })

  # format parameter/settings names and corresponding values
  values <- vector(mode = "character")
  names <- vector(mode = "character")

  for (i in 1:length(header_split_clean)) {
    for (j in seq(1, length(header_split_clean[[i]]), 2)) {
      names <- c(names, header_split_clean[[i]][j])
      values <- c(values, header_split_clean[[i]][j + 1])
    }
  }

  # some RegExing for nice reading
  names <- gsub("[: ]$", "", names, perl = TRUE)
  names <- gsub("^ ", "", names)
  names <- gsub(" $", "", names)
  # for some weird reason "offset subtract" starts with '256 '
  names <- gsub("256 ", "", names)
  # finally, replace all blanks with underscores
  names <- gsub(" ", "_", names)

  values <- gsub("[: ]$", "", values, perl = TRUE)
  values <- gsub("^ ", "", values)
  values <- gsub(" $", "", values)

  # return header as list
  header <- as.list(values)
  names(header) <- names

  return(header)
}
