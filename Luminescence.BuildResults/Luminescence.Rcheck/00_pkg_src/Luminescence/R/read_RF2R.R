#' @title Import RF-files to R
#'
#' @description Import files produced by the IR-RF 'ImageJ' macro (`SR-RF.ijm`; Mittelstraß and Kreutzer, 2021) into R and create a list of [RLum.Analysis-class]
#' objects
#'
#' @details The results of spatially resolved IR-RF data are summarised in so-called RF-files ((Mittelstraß and Kreutzer, 2021).
#' This functions provides an easy import to process the data seamlessly with the R package 'Luminescence'.
#' The output of the function can be passed to the function [analyse_IRSAR.RF]
#'
#' @param file [character] (**required**): path and file name of the RF file. Alternatively a list of file
#' names can be provided.
#'
#' @return Returns an S4 [RLum.Analysis-class] object containing
#' [RLum.Data.Curve-class] objects for each curve.
#'
#' @seealso [RLum.Analysis-class], [RLum.Data.Curve-class], [analyse_IRSAR.RF]
#'
#' @author Sebastian Kreutzer, Geography & Earth Science, Aberystwyth University (United Kingdom)
#'
#' @section Function version: 0.1.0
#'
#' @keywords IO
#'
#' @references Mittelstraß, D., Kreutzer, S., 2021. Spatially resolved infrared radiofluorescence:
#' single-grain K-feldspar dating using CCD imaging. Geochronology 3, 299–319. \doi{10.5194/gchron-3-299-2021}
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

# Self-call -----------------------------------------------------------------------------------
  if(inherits(file, "list")){
    results_list <- lapply(file, function(f){
      temp <- try(read_RF2R(file = f), silent = TRUE)

      ##check whether it worked
      if(inherits(temp, "try-error")){
        try(
          stop("[read_RF2R()] Import for file ", f, " failed. NULL returned!", call. = FALSE))
        return(NULL)
      }else{
        return(temp)
      }

    })

    return(unlist(results_list, recursive = FALSE))

  }


# Integrity check -----------------------------------------------------------------------------
  ##throw warning if we have a vector
  if(length(file) > 1){
    warning("[read_RF2R()] 'file' has a length > 1. Only the first element was taken!
            If you want to import multiple files, 'file' has to be of type 'list'.", call. = TRUE)
    file <- file[1]
  }

  ##check input
  if(!inherits(file, "character"))
    stop("[read_RF2R()] 'file' needs to be of type character!", call. = FALSE)

  ##check whether file is available
  if(!file.exists(file))
    stop("[read_RF2R()] File '", file, "' does not exist!", call. = FALSE)

  ##read first line to ensure the format
  vers_str <-  readLines(file, 1)
  version_supported <- c("17-10-2018", "27-11-2018", "0.1.0")
  version_found <- regmatches(vers_str,
                              regexpr("(?<=macro\\_version=)[0-9-.]+", vers_str, perl = TRUE))

  if (!any(version_found %in% version_supported))
    stop("[read_RF2R()] File format not supported!", call. = FALSE)

# Import --------------------------------------------------------------------------------------

  ##import the entire file
  temp <- readLines(file, warn = FALSE)

# Extract information -------------------------------------------------------------------------

    ##extract header (here as function; that might be useful in future)
    .extract_header <- function(x){
      x <- gsub(pattern = "<", replacement = "", fixed = TRUE, x = x)
      x <- gsub(pattern = ">", replacement = "", fixed = TRUE, x = x)
      header <- strsplit(x = x, split = " ", fixed = TRUE)[[1]]
      header <- unlist(strsplit(x = header, split = "=", fixed = TRUE))

      header_names <- header[seq(1, length(header), by = 2)]
      header <-  as.list(header[seq(2, length(header), by = 2)])
      names(header) <- header_names
      return(header)
    }

    header <- try(.extract_header(temp[1]), silent = TRUE)

    ##test the header
    if(inherits(header, 'try-error')){
      try(stop("[read_RF2R()] Header extraction failed, try to continue without ... ", call. = FALSE))
      header <- NA
    }

    ##extract tag boundaries framed by tags +++++++++++++++++++
    ##the 2nd line corrects the inner boundaries
    ##(1) statistics
    id_statistics <- grep(pattern = "Statistics>", x = temp, fixed = TRUE)
    id_statistics <- c(id_statistics[1] + 1, id_statistics[2] - 1)

    ##(2) Natural (henceforth: RF_nat)
    id_RF_nat <- grep(pattern = "Natural>", x = temp, fixed = TRUE)
    id_RF_nat <- c(id_RF_nat[1] + 1, id_RF_nat[2] - 1)

    ##(3) Bleached (henceforth: RF_reg)
    id_RF_reg <- grep(pattern = "Bleached>", x = temp, fixed = TRUE)
    id_RF_reg <- c(id_RF_reg[1] + 1, id_RF_reg[2] - 1)

    ##extract content within the tags +++++++++++++++++++
    ##(1) statistics
    ##
    ####header
      statistics_header <- strsplit(x = temp[id_statistics[1]], split = "\t", fixed = TRUE)[[1]][-1]

      ##data
      m_statistics <- as.data.frame(lapply((id_statistics[1]+1):(id_statistics[2]), function(x){
          strsplit(x = temp[x], split = "\t", fixed = TRUE)[[1]]

      }), stringsAsFactors = FALSE)

      ##extract colnames
      colnames(m_statistics) <-
        unlist(strsplit(
          x = as.character(m_statistics[1, ]),
          split = ":",
          fixed = TRUE
        ))

      ##remove first
      df_statistics <-
        cbind(ROI = statistics_header, m_statistics[-1, ], stringsAsFactors = FALSE)


    ##(2) RF_nat
    ##
    ####header
      RF_nat_header <-
        strsplit(x = temp[id_RF_nat[1]], split = "\t", fixed = TRUE)[[1]]

      ##data
      m_RF_nat <- matrix(
        data = as.numeric(strsplit(
          x = paste(temp[(id_RF_nat[1] + 1):(id_RF_nat[2])], collapse = "\t"),
          split = "\t",
          fixed = TRUE
        )[[1]]),
        ncol = length(RF_nat_header),
        byrow = TRUE
      )

      ##set colnames
      colnames(m_RF_nat) <- RF_nat_header

    ##(3) RF_reg
    ##
    ####header
      RF_reg_header <-
        strsplit(x = temp[id_RF_reg[1]], split = "\t", fixed = TRUE)[[1]]

      ##data
      m_RF_reg <- matrix(
        data = as.numeric(strsplit(
          x = paste(temp[(id_RF_reg[1] + 1):(id_RF_reg[2])], collapse = "\t"),
          split = "\t",
          fixed = TRUE
        )[[1]]),
        ncol = length(RF_reg_header),
        byrow = TRUE
      )

      ##set colnames
      colnames(m_RF_reg) <- RF_reg_header


# Create RLum.Analysis objects ----------------------------------------------------------------
    object_list <- lapply(1:nrow(df_statistics), function(a){

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
               info = c(
                 as.list(df_statistics[a,]),
                 header
                 ))

    })

# Return --------------------------------------------------------------------------------------
return(object_list)

}
