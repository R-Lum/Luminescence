#' @title Create Theta Matrix
#'
#' @description Create the \eqn{\Theta} matrix with the shared uncertainties
#' that can used as input in, e.g., [AgeS_Computation] and [Age_OSLC14] which is used for the
#' covariance matrix \eqn{\Sigma} (Combès \& Philippe, 2017)
#'
#' @details The function intends to ease the creation of the \eqn{Theta} matrix, which cannot be
#' created straight forward, e.g., base R functions such as `stats::cov`.
#' The relationship between the covariance matrix  \eqn{Sigma} and \eqn{Theta} is given with
#'
#' \deqn{\Sigma_ij = A_i * A_j * \Theta_ij}
#'
#' For details see Combès \& Philippe, 2017 and Guérin et al. (under review).
#'
#' **Input modes**
#'
#' The function supports two different operation modes:
#'
#' 1. `input` is left empty: the function returns a [data.frame] template that can be used as input (the option `output_file` works as well)
#' 2. `input` is fed with a [data.frame] or a `character` (file path), the \eqn{\Theta} matrix is returned
#'
#' **Input format**
#'
#' The function expects either a CSV-file or a [data.frame] as input. To create template you can
#' run the function leaving the argument `input` empty (see example). Please note the format
#' of the input table ([data.frame]) needs to kept as specified in the template.
#'
#' The following table lists the meaning of the columns:
#'
#' \tabular{lll}{
#' COLUMN \tab DESCRIPTION \tab UNIT \cr
#' `SAMPLE_ID` \tab sample name \tab - \cr
#' `DR_BETA_K` \tab  standard error beta-dose rate K \tab Gy/ka \cr
#' `DR_BETA_U` \tab  standard error beta-dose rate U \tab Gy/ka \cr
#' `DR_BETA_Th` \tab  standard error beta-dose rate Th \tab Gy/ka \cr
#' `DR_GAMMA_K` \tab  standard error gamma-dose rate K \tab Gy/ka \cr
#' `DR_GAMMA_U` \tab  standard error gamma-dose rate U \tab Gy/ka \cr
#' `DR_GAMMA_Th` \tab  standard error gamma-dose rate Th \tab Gy/ka \cr
#' `DR_GAMMA_TOTAL` \tab  standard error total gamma-dose rate \tab Gy/ka \cr
#' `DR_TOTAL` \tab total dose rate  \tab Gy/ka \cr
#' `DR_TOTAL_X` \tab standard error total dose rate  \tab Gy/ka \cr
#' }
#'
#' *Note: All columns can be set to 0 or NA, no column must be left empty! If a value > 0 is provided
#' for `DR_GAMMA_TOTAL` this value is taken and values in, e.g., `DR_GAMMA_K` are discarded (set to 0)!*
#'
#' **Systematic uncertainties**
#'
#' The following table provides informaton on the named argument
#' that can be provided via the argument `sigma_s`
#'
#' \tabular{lll}{
#' ARGUMENT \tab DESCRIPTION \tab UNIT \cr
#' `s_betaK` \tab relative uncertainty K concentration \tab - \cr
#' `s_betaU` \tab relative uncertainty U concentration \tab - \cr
#' `s_betaTh` \tab relative uncertainty Th concentration \tab - \cr
#' `s_gammaK` \tab relative uncertainty K concentration \tab - \cr
#' `s_gammaU` \tab relative uncertainty U concentration \tab - \cr
#' `s_gammaTh` \tab relative uncertainty Th concentration \tab - \cr
#' `s_gammaDR` \tab relative uncertainty gamma-dose rate  \tab - \cr
#' `s_CAL` \tab relative uncertainty beta-source calibration \tab - \cr
#' `s_intDR` \tab absolute uncertainty internal dose rate \tab Gy/ka \cr
#' }
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France), based
#' on an 'MS Excel' sheet by Guillaume Guérin, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)
#'
#' @section Function version: 0.1.0
#'
#' @param input [character] or [data.frame] (*optional*): input data frame or file connection
#' to import a CSV-file with the needed information. If nothing is provided the function returns
#' an input template. The argument `output_file` can be used to write this input template to the file
#' system
#'
#' @param output_file [character] (optional): file path for the output CSV-file, the field separator
#' is hard set to `","`. Please use [utils::write.table] for more flexibility.
#'
#' @param sigma_s [numeric] (**required**): named character with values for systematic uncertainties. Those values
#' are lab-specific. Can be set to `NULL` to remove systematic uncertainties. The order of the *named*
#' vector is not important, but the naming!
#' **Note**: some of the uncertainties have a unit, please check details.
#'
#' @param ... further arguments that can be passed to [utils::read.table] (for the CSV-file import)
#'
#' @seealso [AgeS_Computation], [Age_OSLC14], [utils::read.table], [utils::write.table]
#'
#' @return A symetric \eqn{Theta} matrix or if `input` is missing, a [data.frame] with an input
#' template
#'
#' @references
#'
#' Combès, B., Philippe, A., 2017. Bayesian analysis of individual and systematic multiplicative errors
#' for estimating ages with stratigraphic constraints in optically stimulated luminescence dating.
#' Quaternary Geochronology 39, 24–34. \doi{10.1016/j.quageo.2017.02.003}
#'
#' @examples
#' ##(1) return template data.frame (no file output)
#' create_ThetaMatrix()
#'
#' \dontrun{
#' ##(2) return template as data.frame + file
#' file_path <- tempfile(fileext = ".csv")
#' create_ThetaMatrix(output_file = file_path )
#'
#' ##NOT RUNNING EXAMPLE for sigma_s
#' calc_ThetaMatrix(...,
#' sigma_s =  c(
#'  s_betaK = 0.010,
#'  s_betaU = 0.007,
#'  s_betaTh = 0.006,
#'  s_gammaK = 0.010,
#'  s_gammaU = 0.007,
#'  s_gammaTh = 0.006,
#'  s_gammaDR = 0.05,
#'  s_CAL = 0.020,
#'  s_intDR = 0.030))
#'
#' }
#'
#'
#'
#' @keywords datagen IO
#'
#' @md
#' @export
create_ThetaMatrix <- function(
  input,
  output_file = NULL,
  sigma_s,
  ...
  ){


  # Configuration -------------------------------------------------------------------------------
  ## set reference for data.frame
  df_colnames <-  c(
    "SAMPLE_ID",
    "DR_BETA_K",
    "DR_BETA_U",
    "DR_BETA_TH",
    "DR_GAMMA_K",
    "DR_GAMMA_U",
    "DR_GAMMA_TH",
    "DR_GAMMA_TOTAL",
    "DR_TOTAL",
    "DR_TOTAL_X"
  )

  ##set reference for sigma_s
  sigma_s_ref <- c(
    "s_betaK",
    "s_betaU",
    "s_betaTh",
    "s_gammaK",
    "s_gammaU",
    "s_gammaTh",
    "s_gammaDR",
    "s_CAL",
    "s_intDR"
  )

  # Verify basic input --------------------------------------------------------------------------------
  # basic input
  if(missing(input)){

    #set data.frame
    df <- as.data.frame(matrix(NA_real_, ncol = length(df_colnames)))
    colnames(df) <- df_colnames

    #return
    if(!is.null(output_file)){
      ##more comfort for output_file
      if(any(!grepl(basename(output_file), pattern = ".csv", fixed = TRUE)))
        output_file <- paste0(output_file, "_template.csv")

      ##write template table
      write.table(x = df, file = output_file, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
      message("[create_ThetaMatrix()] 'input' missing, template data.frame returned and exported to ", output_file)

    }else{
      message("[create_ThetaMatrix()] 'input' missing, input template returned!")

    }

    ##return
    return(df)

  }else if(class(input) == "data.frame"){
     df <- input

  }else if(class(input) == "character"){
    if(!file.exists(input))
      stop(paste0("[create_ThetaMatrix()] File ", input, " does not exist!"), call. = FALSE)

    ##extract extra parameters
    import_settings <- modifyList(
      x = list(
        header = TRUE,
        skip = 0,
        sep = ","
        ),
      val = list(...))

    ##Import
    df <- read.table(
      file = input,
      header = import_settings$header,
      sep = import_settings$sep,
      skip = import_settings$skip,
      row.names = NULL)

  }else{
    stop("[create_ThetaMatrix()] 'input' accepts file paths (character) or data frames only!", call. = FALSE)

  }

  ##check sigma_s
  if(!is.null(sigma_s) && !all(names(sigma_s) %in% sigma_s_ref))
      stop("[create_ThetaMatrix()] Value names do not match in 'sigma_s', please check the manual!", call. = FALSE)

  ##set NULL case
  if(is.null(sigma_s)){
    sigma_s <- numeric(length(sigma_s_ref))
    names(sigma_s) <- sigma_s_ref

  }

# Verify data.frame ---------------------------------------------------------------------------

  #verify data.frame, we hard stop here
  if(!all(colnames(df) %in% df_colnames))
    stop("[create_ThetaMatrix()] The input data.frame has not the expected columns, please check the manual!",call. = FALSE)

  #if data.frame has only one row, it cannot work either
  if(nrow(df) < 2)
    stop("[create_ThetaMatrix()] The input data.frame needs at least 2 rows!", call. = FALSE)

  #set NA values to 0
  if(any(is.na(df))){
    df[is.na(df)] <- 0
    warning("[create_ThetaMatrix()] NA values found and set to 0.", call. = FALSE)

  }



  # Create matrix -------------------------------------------------------------------------------
  ##to avoid a miscalculation, we remove columns not used due to the gamma dose rate setting
  if(any(df[["DR_GAMMA_TOTAL"]] != 0)){
     ##identify affected rows
     temp_id <- which(df[["DR_GAMMA_TOTAL"]] != 0)

     ##set the other three other columns to 0
     df[temp_id, c("DR_GAMMA_K", "DR_GAMMA_U", "DR_GAMMA_TH")] <- 0

     ##show warning to inform the user of what was done
     ##Why a warning: If the user did everything right, the other values should have been set
     ##to 0 to avoid a double calculation
     warning(
       paste0(
         "[create_ThetaMatrix()] Found values > 0 in column 'DR_GAMMA_TOTAL' in record(s): ",
         paste(temp_id, collapse = ","), ". Only 'DR_GAMMA_TOTAL' was used for the calculation!"
       ), call. = FALSE)
  }


  ##create and set names
  m <- matrix(data = NA, nrow = nrow(df), ncol = nrow(df))
  rownames(m) <- df[[1]]
  colnames(m) <- df[[1]]

  ##transform to matrix
  df <- as.matrix(df[,-1])

  ##get possible combinations and add the 1:1 combination
  cmb <- cbind(
    combn(x = 1:nrow(df), m = 2),
    matrix(rep(1:nrow(df),each = 2), nrow = 2))

  ##fill matrix - if you want to add further uncertainties this calculations needs to be
  ##modified
  for(j in 1:ncol(cmb)){
    ##fill the diagonal
    if(cmb[,j][1] == cmb[,j][2]){
      m[cmb[, j][1], cmb[, j][2]] <- df[cmb[,j][1],"DR_TOTAL_X"]^2 + (df[cmb[,j][1],"DR_TOTAL"] * sigma_s["s_CAL"])^2

    }else{
      ##fill the rest of the matrix
      m[cmb[, j][1], cmb[, j][2]] <-
        sum(df[cmb[, j][1], -ncol(df)] * df[cmb[, j][2], -ncol(df)] *
              sigma_s[-which(names(sigma_s) == "s_intDR")] ^ 2) +
        sigma_s["s_intDR"] ^ 2

    }

  }

  ##mirror data along diagonal to get a symmetric matrix
  m[lower.tri(m)] <- t(m)[lower.tri(m)]


  # Return --------------------------------------------------------------------------------------
  if(!is.null(output_file)){
    message("[create_ThetaMatrix()] Theta matrix exported to ", output_file)
    write.table(x = m, file = output_file, append = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")

  }

  ##return the Theta matrix
  return(m)

}
