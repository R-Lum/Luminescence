#' @title Import Thermochronometry Data
#'
#' @description
#' Import data from thermochronometry experiments into R.
#' This function is an adaption of the script `STAGE1, ExcelToStructure` by
#' Benny Guralnik, 2014, modified to accept CSV files with the same structure
#' as the original Excel files.
#'
#' @param file [character] (**required**):
#' path to a CSV file; alternatively a [vector] of paths.
#'
#' @param output_type [character] (*with default*):
#' return type for the function, either [Luminescence::RLum.Results-class] (default) or `"list"`
#' (for a plain R list).
#'
#' @returns
#' Depending on the setting of `output_type` it will be either a plain R [list]
#' or an [Luminescence::RLum.Results-class] object with the following structure data elements:
#'
#' `$data:`
#' `.. $ITL`: data frame with columns `SAMPLE`, `TEMP`, `TIME`, `LxTx`, `LxTx_ERROR`
#' `.. $DRC`: data frame with columns `SAMPLE`, `ALQ`, `TIME`, `LxTx`, `LxTx_ERROR`
#' `.. $FAD`: data frame with columns `SAMPLE`, `ALQ`, `TIME`, `LxTx`, `LxTx_ERROR`
#'
#' This refers to `$ITL`: Isothermal curves, `$DRC`: Dose-response curve,
#' `$FAD`: Fading.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @noRd
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
      return(NA)
    tmp
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(file, "character")
  .validate_args(output_type, c("RLum.Results", "list"))

  exists <- file.exists(file)
  if (!all(exists)) {
    .throw_error("File '", file[!exists][1], "' does not exist")
  }
  if (any(grepl("xlsx?", tools::file_ext(file), ignore.case = TRUE))) {
    .throw_error("XLS/XLSX format is not supported, use CSV instead")
  }

  ## define variable
  ka <- 1e+3 * .const$year_s # ka in seconds

  ## Import -----------------------------------------------------------------
  records <- lapply(file, function(x) {
    ## read the files separating header and body
    head <- data.table::fread(x, nrows = 3, select = 1:5)
    body <- data.table::fread(x, skip = 3, header = TRUE)

    list(
        id = grep("\\.\\.\\.[0-9]+", colnames(head)[-1], invert = TRUE,
                  value = TRUE),
        natT = .extract_numerics(head[1, -1]),         # natural temperature
        natDdot = .extract_numerics(head[2, -1]) / ka, # natural dose rate
        T = body[seq(1, nrow(body), 2)][[2]],          # temperature
        Ddot = body[seq(2, nrow(body), 2)][[2]],       # instrument dose rate
        rawdata = lapply(seq(1, nrow(body), 2), function(y) {
          data.table(SAMPLE = basename(tools::file_path_sans_ext(x)),
                     ALQ = ceiling(y / 2),
                     TEMP = body[y][[2]],
                     ## measurement time (irradiation or delay time)
                     TIME = .extract_numerics(body[y, -(1:4)]) * 1e+3,
                     LxTx = .extract_numerics(body[y + 1, -(1:4)]))
        })
    )
  })
  names(records) <- basename(tools::file_path_sans_ext(file))

  # Create output -----------------------------------------------------------
  if (output_type == "RLum.Results") {
    ## create data frame for each data type

    ## we will use the temperature to discriminate the records; everything
    ## with temperature < 15 is either for DRC or FAD, the rest ITL.
    ## here we save the list index of each record type so that we can access
    ## them later
    res <- lapply(seq_along(records), function(idx) {
      x <- records[[idx]]
      tmp <- cumsum(x$T > 15)
      idx_DRC <- which(tmp == 0)
      idx_ITL <- setdiff(which(!duplicated(tmp)), idx_DRC)
      idx_FAD <- setdiff(which(tmp == max(tmp)), idx_ITL)

      ## extract variables
      DRC <- data.table::rbindlist(x$rawdata[idx_DRC])
      ITL <- data.table::rbindlist(x$rawdata[idx_ITL])
      FAD <- data.table::rbindlist(x$rawdata[idx_FAD])

      ## adjust aliquot numbers to start from 1 in each block
      DRC$ALQ <- DRC$ALQ - DRC$ALQ[1] + 1
      FAD$ALQ <- FAD$ALQ - FAD$ALQ[1] + 1

      list(DRC = data.frame(DRC[, -3], # drop TEMP
                            LxTx_ERROR = NA),
           ITL = data.frame(ITL[, -2], # drop ALQ
                            LxTx_ERROR = NA),
           FAD = data.frame(FAD[, -3], # drop TEMP
                            LxTx_ERROR = NA),
           ## Ddot is only relevant for DRC data
           Ddot_DRC = x$Ddot[idx_DRC]
           )
    })

    DRC <- as.data.frame(data.table::rbindlist(lapply(res, function(x) x$DRC)))
    ITL <- as.data.frame(data.table::rbindlist(lapply(res, function(x) x$ITL)))
    FAD <- as.data.frame(data.table::rbindlist(lapply(res, function(x) x$FAD)))
    Ddot_DRC <- lapply(res, function(x) x$Ddot_DRC)
    nat_Ddot <- lapply(records, function(x) x$natDdot)

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
