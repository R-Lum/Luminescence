#'@title Import Thermochronometry Data to List
#'
#'@description Import Excel Data from Thermochronometry Experiments into R.
#'This function is an adaption of the script `STAGE1, ExcelToStructure` by
#'Benny Guralnik, 2014
#'
#'@param file [character] (**required**): path to XLS file
#'
#'@returns Returns a [list] with a very particular format
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [readxl::read_excel]
#'
#'@md
#'@noRd
.ExcelToStruct <- function(file) {
# Helper functions -------------------------------------------------------
  ## consistently extract numerical data
  .extract_numerics <- function(x) {
    tmp <- suppressWarnings(as.numeric(na.exclude(as.numeric(x))))

    if(length(tmp) == 0)
      tmp <- NA

    tmp
  }

  ## define variable
  ka <- 1e+3 * 365 * 24 * 3600 # ka in seconds

# Import ------------------------------------------------------------------
  ## get number of sheets in the file
  sheets <- readxl::excel_sheets(file)

  ## import data from all sheets ... separate header and body
  tmp_records <- lapply(sheets, function(x) {
    header <- readxl::read_excel(file, sheet = x, .name_repair = "unique_quiet", n_max = 3)
    body <- readxl::read_excel(file, sheet = x, .name_repair = "unique_quiet", skip = 3)
    list(as.data.frame(header), as.data.frame(body))
  })
  names(tmp_records) <- sheets

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

  return(records)
}
