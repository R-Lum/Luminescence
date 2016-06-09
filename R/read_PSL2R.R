#' Import PSL files to R
#' 
#' Imports PSL files produced by a SUERC portable OSL reader into R
#'
#' <placeholder>
#'
#' @param file \code{\link{character}} or \code{\link{list}} (\bold{required}): path and file name of the
#' PSL file. If input is a \code{list} it should comprise only \code{character}s representing
#' valid paths and PSL file names.
#' Alternatively the input character can be just a directory (path). In this case the
#' the function tries to detect and import all PSL files found in the directory.
#'
#' @param drop.bg \code{\link{logical}} (with default):  \code{TRUE} to automatically 
#' remove all non-OSL/IRSL curves.
#'
#' @param as.decay.curve  \code{\link{logical}} (with default): Portable OSL Reader curves
#' are often given as cumulative light sum curves. Use \code{TRUE} (default) to convert
#' the curves to the more usual decay form.
#' 
#' @param ... currently not used.
#'
#' @return Returns an S4 \code{\linkS4class{RLum.Analysis}} object.
#' 
#' @seealso \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Data.Curve}}
#' 
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @section Function version: 0.0.1
#'
#' @keywords IO
#' 
#' @examples
#' # none available yet
#' 
#' @export
read_PSL2R <- function(file, drop.bg = FALSE, as.decay.curve = TRUE, ...) {
  
  ## Read in file ----
  doc <- readLines(file)
  
  ## Document formatting ----
  # remove lines with i) blanks only, ii) dashes, iii) equal signs
  doc <- gsub("^[ ]*$", "", doc)
  doc <- gsub("^[ -]*$", "", doc)
  doc <- gsub("^[ =]*$", "", doc)
  
  # the header ends with date and time with the previous line starting with a single slash
  lines_with_slashes <- doc[grepl("\\", doc, fixed = TRUE)]
  doc <- gsub(lines_with_slashes[length(lines_with_slashes)],
              "", fixed = TRUE, doc)
  
  # last delimiting line before measurements are only apostrophes and dashes
  lines_with_apostrophes <- doc[grepl("'", doc, fixed = TRUE)]
  doc <- gsub(lines_with_apostrophes[length(lines_with_apostrophes)],
              "", fixed = TRUE, doc)
  
  # finally remove all empty lines
  doc <- doc[doc != ""]
  
  ## Split document ----
  begin_of_measurements <- grep("Measurement :", doc, fixed = TRUE)
  number_of_measurements <- length(begin_of_measurements)
  
  
  # Header
  header <- doc[1:(begin_of_measurements[1]-1)]
  header <- format_Header(header)
  
  # Measurements
  measurements_split <- vector("list", number_of_measurements)
  
  for (i in seq_len(number_of_measurements)) {
    if (i != max(number_of_measurements))
      measurements_split[[i]] <- doc[begin_of_measurements[i]:(begin_of_measurements[i+1] - 1)]
    else 
      measurements_split[[i]] <- doc[begin_of_measurements[i]:length(doc)]
  }
  
  measurements_formatted <- lapply(measurements_split, function(x) { 
    format_Measurements(x, convert = as.decay.curve)
    })
  
  if (drop.bg) {
    measurements_formatted <- lapply(measurements_formatted, function(x) {
      if (x@recordType != "BG")
        return(x)
    })
    
    measurements_formatted <- measurements_formatted[!sapply(measurements_formatted, is.null)]
  }
  
  ## RETURN ----
  object <- set_RLum("RLum.Analysis",
                     protocol = "portable OSL",
                     info = header,
                     records = measurements_formatted)
  
  return(object)
}  

################################################################################
## HELPER FUNCTIONS
################################################################################


## -------------------------------------------------------------------------- ##

format_Measurements <- function(x, convert) {
  
  
  ## measurement parameters are given in the first line
  settings <- x[1]
  # settings_split <- unlist(strsplit(settings, "|", fixed = TRUE))
  
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
    recordType <- "BG"
  } 
  if (grepl("Stim 1", settings)) {
    recordType <- "IRSL"
  }
  if (grepl("Stim 2", settings)) {
    recordType <- "OSL"
  }
  
  object <- set_RLum(class = "RLum.Data.Curve",
                     recordType = recordType,
                     curveType = "measured",
                     data = data,
                     info = list(settings = settings,
                                 raw_data = df)
                     )
  
  return(object)
  
}

## -------------------------------------------------------------------------- ##
format_Header <- function(x) {
  
  header_formatted <- list()
  
  header_split <- strsplit(x, "  ", fixed = TRUE)
  
  header_split_clean <- lapply(header_split, function(x) {
    
    x <- x[x != ""]
    n_elements <- length(x)
    n_properties <- length(grep(":", x, fixed = TRUE))
    
    if (n_elements / n_properties == 1)
      x <- unlist(strsplit(x, ": ", fixed = TRUE))
    
    return(x)
  })
  
  values <- vector(mode = "character")
  names <- vector(mode = "character")
  
  for (i in 1:length(header_split_clean)) {
    for (j in seq(1, length(header_split_clean[[i]]), 2)) {
      names <- c(names, header_split_clean[[i]][j])
      values <- c(values, header_split_clean[[i]][j + 1])
    }
  }
  
  names <- gsub("[: ]$", "", names, perl = TRUE)
  names <- gsub("^ ", "", names)
  names <- gsub(" $", "", names)
  # for some weird reason "offset subtract" starts with a "256 "
  names <- gsub("256 ", "", names)
  # finally, replace all blanks with underscores
  names <- gsub(" ", "_", names)
  
  values <- gsub("[: ]$", "", values, perl = TRUE)
  values <- gsub("^ ", "", values)
  values <- gsub(" $", "", values)
  
  header <- as.list(values)
  names(header) <- names
 
  return(header) 
}