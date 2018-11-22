#' @title Import RF files to R
#'
#' @description TODO
#'
#' @details TODO
#'
#' @param file [character] (**required**): path and file name of the RF file
#'
#' @return
#' Returns an S4 [RLum.Analysis-class] object containing
#' [RLum.Data.Curve-class] objects for each curve.
#'
#' @seealso [RLum.Analysis-class], [RLum.Data.Curve-class]
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)
#'
#' @section Function version: 0.0.1
#'
#' @keywords IO
#'
#' @examples
#'
#' ##Import
#' file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
#' temp <- read_RF2R(file)
#'
#' @md
#' @export
read_RF2R <- function(file) {

  ##TODO: Add integrity tests

  # Integrity check -----------------------------------------------------------------------------

  ##read first line to ensure the format
  if(!grepl(pattern = "macro_version=17-10-2018", x = readLines(file, n = 1), fixed = TRUE))
    stop("[read_RF2R()] File format not supported!", call. = FALSE)

  # Import --------------------------------------------------------------------------------------

  ##import the entire file
  temp <- readLines(file, warn = FALSE)

  ##TODO: Extract header

    ##extract boundaries framed by tags +++++++++++++++++++
    ##statistics
    id_statistics <- grep(pattern = "Statistics>", x = temp, fixed = TRUE)

    ##RF_nat
    id_RF_nat <- grep(pattern = "Natural>", x = temp, fixed = TRUE)

    ##RF_reg
    id_RF_reg <- grep(pattern = "Bleached>", x = temp, fixed = TRUE)

    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##Extract matrices
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##Statistics
    statistics_header <- strsplit(x = temp[id_statistics[1]+1], split = "\t", fixed = TRUE)[[1]][-1]

    ##data
    m_statistics <- as.data.frame(lapply((id_statistics[1]+2):(id_statistics[2]-1), function(x){
        strsplit(x = temp[x], split = "\t", fixed = TRUE)[[1]]

    }), stringsAsFactors = FALSE)

    ##extract colnames
    colnames(m_statistics) <- unlist(strsplit(x = as.character(m_statistics[1,]), split = ":", fixed = TRUE))

    ##remove first
    df_statistics <- cbind(ROI = statistics_header, m_statistics[-1,], stringsAsFactors = FALSE)

    ##RF_nat
    ##header
    RF_nat_header <- strsplit(x = temp[id_RF_nat[1]+1], split = "\t", fixed = TRUE)[[1]]

    ##data
    m_RF_nat <- matrix(
      data = as.numeric(strsplit(
        x = paste(temp[(id_RF_nat[1] + 2):(id_RF_nat[2] - 1)], collapse = "\t"),
        split = "\t",
        fixed = TRUE
      )[[1]]),
      ncol = length(RF_nat_header),
      byrow = TRUE
    )

    ##set colnames
    colnames(m_RF_nat) <- RF_nat_header

    ##RF_reg
    ##header
    RF_reg_header <- strsplit(x = temp[id_RF_reg[1]+1], split = "\t", fixed = TRUE)[[1]]

    ##data
    m_RF_reg <- matrix(
      data = as.numeric(strsplit(
        x = paste(temp[(id_RF_reg[1] + 2):(id_RF_reg[2] - 1)], collapse = "\t"),
        split = "\t",
        fixed = TRUE
      )[[1]]),
      ncol = length(RF_reg_header),
      byrow = TRUE
    )

    ##set colnames
    colnames(m_RF_reg) <- RF_reg_header

    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##Create RLum.Analysis objects
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    temp <- lapply(1:ncol(df_statistics), function(a){

      ##set records
      records <- lapply(1:2, function(o) {
        if(o == 1){
          temp_curve <- m_RF_nat[,c(2,2 + a)]

        }else{
          temp_curve <- m_RF_reg[,c(2,2 + a)]

        }

        ##write curve
        set_RLum(
          class = "RLum.Data.Curve",
          originator = "read_RF2R",
          curveType = "measured",
          recordType = "RF",
          data = temp_curve
        )


      })

      ##create RLum.Analysis object
      set_RLum(class = "RLum.Analysis",
               originator = "read_RF2R",
               records = records,
               info = as.list(df_statistics[1,]))




    })


  # Return --------------------------------------------------------------------------------------
  return(temp)

}
