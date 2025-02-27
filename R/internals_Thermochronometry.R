#' @title Import Thermochronometry Data
#'
#' @description
#' Import data from thermochronometry experiments into R.
#' This function is an adaption of the script `STAGE1, ExcelToStructure` by
#' Benny Guralnik, 2014, modified to accept CSV files with the same structure
#' as the original Excel files.
#'
#'@param file [character] (**required**): path to a CSV file; alternatively a
#' [vector] of paths
#'
#'@param output_type [character] (*with default*): defines the output for the function,
#'which can be either `"RLum.Results"` (the default) or a plain R list (`"list"`)
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'
#'@returns Depending on the setting of `output_type` it will be either a plain R [list]
#'or an [RLum.Results-class] object with the following structure data elements
#'
#'`$data: `
#'`.. $ITL` : a [data.frame] with five columns, `SAMPLE`, `TEMP`, `TIME`, `LxTx`, `LxTx_ERROR`
#'`.. $DRC` : a [data.frame] with five columns, `SAMPLE`, `ALQ`, `TIME`, `LxTx`, `LxTx_ERROR`
#'`.. $FAD` : a [data.frame] with five columns, `SAMPLE`, `ALQ`, `TIME`, `LxTx`, `LxTx_ERROR`
#'
#'This refers to `$ITL`: Isothermal curves, `$DRC`: Dose-response curve, `$FAD`: Fading
#'
#'@md
#'@noRd
.import_ThermochronometryData <- function(
  file,
  output_type = "RLum.Results"
) {
  .set_function_name(".import_ThermochronometryData")
  on.exit(.unset_function_name(), add = TRUE)

# Helper functions -------------------------------------------------------
  ## consistently extract numerical data
  .extract_numerics <- function(x) {
    tmp <- suppressWarnings(as.numeric(na.exclude(as.numeric(x))))
    if(length(tmp) == 0)
      tmp <- NA

    tmp
  }

  .validate_args(output_type, c("RLum.Results", "list"))

  ## define variable
  ka <- 1e+3 * 365 * 24 * 3600 # ka in seconds

# Import ------------------------------------------------------------------
  ## preset records
  records <- file[1]
  if (inherits(file, "character")) {

    if (grepl("xlsx?", tools::file_ext(file[1]), ignore.case = TRUE)) {
      .throw_error("XLS/XLSX format is not supported, use CSV instead")
    }

    ## import data from all files ... separate header and body
    tmp_records <- lapply(file, function(x) {
      if (!file.exists(x))
        .throw_error("File does not exist")
      header <- data.table::fread(x, nrows = 3, select = c(1:5))
      body <- data.table::fread(x, skip = 3, header = TRUE)
      list(as.data.frame(header), as.data.frame(body))
    })
    names(tmp_records) <- basename(tools::file_path_sans_ext(file))

  ## compile records
  records <- lapply(tmp_records, function(x){
    list(
      id = colnames(x[[1]][-1])[!grepl(pattern = "\\.\\.\\.[0-9]+", x = colnames(x[[1]])[-1])],
      params = list(
        natT = .extract_numerics(x[[1]][1,-1]),          #natural temperature
        natDdot = .extract_numerics(x[[1]][2,-1]) / ka,  #natural dose rate
      rawdata = lapply(seq(1,nrow(x[[2]]),2), function(y) {
        list(
          T = x[[2]][y, 2], # Temperature
          Ddot = x[[2]][y + 1, 2], # Instrument dose rate
          t = .extract_numerics(x[[2]][y, -c(2:4)]) * 1e+3, # Measurement time (irradiation or delay time)
          L = .extract_numerics(x[[2]][y + 1, -c(2:4)])/max(.extract_numerics(x[[2]][y + 1, -c(2:4)])) # normalise the luminescence signal data to the maximum
        )
      })
    ))
  })

  ## assign originator to this list
  attr(records, "originator") <- ".import_ThermochronometryData "

  } # end CSV import

  ## if input is a list check what is coming in
  if(!inherits(records, "list") ||
     is.null(attr(records, "originator")) ||
     attr(records, "originator") != ".import_ThermochronometryData ")
    .throw_error("Input type not supported")

  # Create output -----------------------------------------------------------
  if (output_type == "RLum.Results") {
    ## create data frame for each data type

    ## we will use the temperature to discriminate the records; everything
    ## with temperature < 15 is either for DRC or FAD, the rest ITL.
    ## here we safe the list index of each record type so that we can access those
    ## data later
    ## index --------
    id_l <- lapply(records, function(x) {
        tmp <- cumsum(unlist(.get_named_list_element(x, "T")) > 15)
        names(tmp) <- NULL

        ## create index list
        list(
          DRC = which(tmp == 0),
          ITL = which(!duplicated(tmp))[-1],
          FAD = which(tmp == max(tmp))[-1])

    })

    ## now we create for each data type a data.frame in the ggplot2 accessible
    ## way
    ## DRC ---------
    DRC <- as.data.frame(data.table::rbindlist(lapply(seq_along(records), function(x) {
      ## extract variables
      ALQ <- seq_along(id_l[[x]]$DRC)
      TIME <- .get_named_list_element(records[[x]], "t")[id_l[[x]]$DRC]
      LxTx <- .get_named_list_element(records[[x]], "L")[id_l[[x]]$DRC]

      ## get length of each record
      n_length <- lengths(TIME)

      ## the number of rows are determined automatically
      data.frame(
        SAMPLE = names(records)[x],
        ALQ = as.numeric(mapply(rep, ALQ, n_length)),
        TIME = unlist(.get_named_list_element(records[[x]], "t")[id_l[[x]]$DRC]),
        LxTx = unlist(.get_named_list_element(records[[x]], "L")[id_l[[x]]$DRC]),
        LxTx_ERROR = NA)

    })))

    ## ITL ---------
    ITL <- as.data.frame(data.table::rbindlist(lapply(seq_along(records), function(x) {
      ## extract variables
      TEMP <- .get_named_list_element(records[[x]], "T")[id_l[[x]]$ITL]
      TIME <- .get_named_list_element(records[[x]], "t")[id_l[[x]]$ITL]
      LxTx <- .get_named_list_element(records[[x]], "L")[id_l[[x]]$ITL]

      ## get length of each record
      n_length <- lengths(TIME)

      ## the number of rows are determined automatically
      data.frame(
        SAMPLE = names(records)[x],
        TEMP = unlist(mapply(rep, TEMP, n_length, SIMPLIFY = FALSE)),
        TIME = unlist(TIME),
        LxTx = unlist(LxTx),
        LxTx_ERROR = NA)

    })))

    ## FAD ---------
    FAD <- as.data.frame(data.table::rbindlist(lapply(seq_along(records), function(x) {
      ## extract variables
      ALQ <- seq_along(id_l[[x]]$FAD)
      TIME <- .get_named_list_element(records[[x]], "t")[id_l[[x]]$FAD]
      LxTx <- .get_named_list_element(records[[x]], "L")[id_l[[x]]$FAD]

      ## get length of each record
      n_length <- lengths(TIME)

      ## the number of rows are determined automatically
      data.frame(
        SAMPLE = names(records)[x],
        ALQ = unlist(mapply(rep, ALQ, n_length)),
        TIME = unlist(.get_named_list_element(records[[x]], "t")[id_l[[x]]$FAD]),
        LxTx = unlist(.get_named_list_element(records[[x]], "L")[id_l[[x]]$FAD]),
        LxTx_ERROR = NA)

    })))

    ## Ddot ----------
    ## Ddot is only relevant for DRC data
    Ddot_DRC <- lapply(seq_along(records), function(x) {
      unlist(.get_named_list_element(records[[x]], "Ddot"))[id_l[[x]]$DRC]
    })

    ##natDdot
    nat_Ddot <- unlist(
      lapply(records, .get_named_list_element, "natDdot"),
      recursive = FALSE)

    ## create RLum.Results object
    records <- set_RLum(
      class = "RLum.Results",
      data = list(
        DRC = DRC,
        ITL = ITL,
        FAD = FAD),
      info = list(
        call = sys.call(),
        sample_names = unique(names(records)),
        Ddot_DRC = Ddot_DRC,
        nat_Ddot = nat_Ddot)
      )

  }

  ## always return records
  return(records)
}
