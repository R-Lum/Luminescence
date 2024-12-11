#' @title Analyse Thermochronometry Data
#'
#' @description Analyse thermochronometry data based on MatLab code written
#' by XXX and XXX and XXX
#'
#' @details ##TODO
#'
#' @param object [character] or [RLum.Results] (**required**): file path to XLSX file with ... ##TODO
#'
#' @param plot [logical] (*with default*): enable/disable plot output
#'
#' @param verbose [logical] (*with default*): enable/disable terminal output
#'
#' @param ... further arguments passed to ...
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany), Svenja Riedesel, DTU Risø (Denmark)
#'
#' @keywords datagen
#'
#' @return
#' An [RLum.Results-class] object is returned: ##TODO
#'
#' @seealso ##TODO
#'
#' @examples
#' # example code ##TOD
#'
#' @md
#'
#' @md
#' @export
analyse_ThermochronometryData <- function(
  object,
  plot = TRUE,
  verbose = TRUE,
  ...

){

  ##TODO --- general
  ## - output of this function follows the suggestions from the MatLab code by Benny Gurlanik; however
  ## the implementation is R based
  ## - Excepted output
  ## - proposed a new format for a CSV file with data ... or even better ... in the long-term
  ## run the entire analysis in R only
  ## - later we have to think about two different modes of analysis
  ## - we want to pass certain parameters over to functions

  # Import and prepare data -------------------------------------------------
  ## for a start with only allow data coming in in the format proposed by the MatLab script
  if(inherits(object, "character")) {
    object <- Luminescence:::.import_ThermochronometryData(object, output_type = "RLum.Results")
    sample_names <- object@info$sample_names

  } else {
    stop("[analyse_ThermochronometryData] Input for 'object' not supported!", call. = FALSE)

  }

  ## prepare plot and reset to default on exit
  if(plot) {
    par_default <- par(no.readonly = TRUE)
    par(mfrow = c(1,3))
    on.exit(par(par_default))

  }

  ## Reminder: We have n samples in one Excel sheet ... each set will be analysed
  ## separately
  results_combined <- lapply(seq_along(sample_names), function(i) {
    # (1) Fading data ------------------------------------------------------------
    ## get fading object for this particular sample
    FAD <- object@data$FAD[object@data$FAD$SAMPLE == sample_names[i],]

    ## extract the data we need (three columns)
    df_FAD <- FAD[,c("LxTx", "LxTx_ERROR", "TIME")]

    ## set NA values in LxTx error to 0
    df_FAD[is.na(df_FAD[["LxTx_ERROR"]]),"LxTx_ERROR"] <- 0

    results_FAD <- analyse_FadingMeasurement(
      object = df_FAD,
      verbose = FALSE,
      plot = plot,
      plot.single = 3,
      plot.trend = FALSE)

    # (2) ITL Data ------------------------------------------------------------
    df_ITL <- object@data$ITL[object@data$ITL$SAMPLE == sample_names[i],]
    results_ITL <- fit_IsoThermalHolding(
      data = df_ITL,
      rhop = results_FAD,
      plot = plot)

    # (3) DRC Data ------------------------------------------------------------
    ## get DRC data
    df_DRC <- object@data$DRC[object@data$DRC$SAMPLE == sample_names[i],]

    ## get DRC data in correct form
    df_DRC <- df_DRC[,c("TIME", "LxTx", "LxTx_ERROR", "ALQ")]
    df_DRC[is.na(df_DRC[["LxTx_ERROR"]]),"LxTx_ERROR"] <- 0

    ## add dose rate if available
    if (!is.null(object@info$Ddot_DRC)) {
      Ddot_DRC <- object@info$Ddot_DRC[[i]]
      ## we have to do this aliquot wise
      for (d in unique(df_DRC[["ALQ"]])) {
        df_DRC[df_DRC[["ALQ"]] == d,"TIME"] <- df_DRC[df_DRC[["ALQ"]] == d, "TIME"] * Ddot_DRC[d]

      }

      ## adjust column names
      colnames(df_DRC) <- c("DOSE", colnames(df_DRC[-1]))
    }

    ## DRC fitting
    results_DRC <- suppressWarnings(plot_GrowthCurve(
      sample = df_DRC,
      mode = "alternate",
      xlab = if (any("DOSE" %in% colnames(df_DRC))) "Dose [Gy]" else "Dose [s]",
      cex.global = 0.65,
      output.plot = plot,
      main = sample_names[i],
      output.plotExtended = FALSE
    ))

    ## return single lists
    return(list(results_FAD, results_ITL, results_DRC))

  })

  ## merge results for each
    ## flatten list
    results_combined <- unlist(results_combined, recursive = TRUE)

    ## get originator (we will merge accordingly)
    org <- vapply(results_combined, function(x) x@originator, character(1))

    ## get list with merged results
    results_combined <- lapply(unique(org), function(x) {
      merge_RLum(
        results_combined[org == x]

      )

    })

# Results -----------------------------------------------------------------
  results <- set_RLum(
    class = "RLum.Results",
    data = list(
      FAD = results_combined[[1]],
      ITL = results_combined[[2]],
      DRC = results_combined[[3]]),
    info = list(
      call = sys.call()
    ))

  return(results)


}#EOF
