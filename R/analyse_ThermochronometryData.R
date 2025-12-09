#' @title Analyse Thermochronometry Data
#'
#' @description Analyse thermochronometry data based on MatLab code written
#' by XXX and XXX and XXX
#'
#' @details ##TODO
#'
#' @param object [character] (**required**):
#' path to a CSV file ##TODO;
#' alternatively a [vector] of paths
#'
#' @param ITL_model [character] (*with default*):
#' type of model to fit, either `"GOK"` (default) or `"BTS"`.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param ... further parameters passed to [fit_IsothermalHolding]
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany),
#' Svenja Riedesel, DTU Risø (Denmark)
#'
#' @keywords datagen
#'
#' @return
#' An [RLum.Results-class] object is returned:
#'
#' Slot: **@data**
#'
#' \tabular{lll}{
#'  **OBJECT** \tab **TYPE** \tab **COMMENT**\cr
#' `FAD` \tab `RLum.Results` \tab results of the fading measurement \cr
#' `ITL` \tab `RLum.Results` \tab results of the isothermal holding \cr
#' `DRC` \tab `RLum.Results` \tab results of the dose response curve \cr
#' }
#'
#' Slot: **@info**
#'
#' \tabular{lll}{
#' **OBJECT** \tab `TYPE` \tab `COMMENT`\cr
#' `call` \tab `call` \tab the original function call\cr
#' }
#'
#' @references
#'
#' Bouscary, C., King, G.E, Kranz-Bartz, M., Bernard, M., Biswas, R.H., Bossin,
#' L., Duverger, A., Guralnik, B., Herman, F., Nanni, U., Stalder, N., Valla,
#' P.G., Visnjevic, V., Wen, X., 2025.
#' OSLThermo and ESRThermo: Libraries of code for trapped-charge thermochronometry.
#'
#' @seealso [analyse_FadingMeasurement], [fit_IsothermalHolding],
#' [fit_DoseResponseCurve]
#'
#' @examples
#' # example code ##TODO
#'
#' @export
analyse_ThermochronometryData <- function(
  object,
  ITL_model = c("GOK", "BTS"),
  plot = TRUE,
  verbose = TRUE,
  ...
) {
  .set_function_name("analyse_ThermochronometryData")
  on.exit(.unset_function_name(), add = TRUE)

  ##TODO --- general
  ## - output of this function follows the suggestions from the MatLab code by Benny Gurlanik; however
  ## the implementation is R based
  ## - Excepted output
  ## - proposed a new format for a CSV file with data ... or even better ... in the long-term
  ## run the entire analysis in R only
  ## - later we have to think about two different modes of analysis
  ## - we want to pass certain parameters over to functions

  ## Integrity checks -------------------------------------------------------
  ## for a start we only allow data in the format proposed by the MatLab script
  .validate_class(object, "character")
  .validate_args(ITL_model, c("GOK", "BTS"))
  .validate_logical_scalar(plot)
  .validate_logical_scalar(verbose)

  object <- .import_ThermochronometryData(object, output_type = "RLum.Results")
  sample_names <- object@info$sample_names

  ## prepare plot and reset to default on exit
  if(plot) {
    par.default <- .par_defaults()
    on.exit(par(par.default), add = TRUE)
    par(mfrow = c(1,3))
  }

  ## Reminder: We have n samples in one Excel sheet ... each set will be analysed
  ## separately
  results_combined <- lapply(seq_along(sample_names), function(i) {
    # (1) Fading data ------------------------------------------------------------
    ## get fading object for this particular sample
    df_FAD <- object@data$FAD[object@data$FAD$SAMPLE == sample_names[i],
                              c("LxTx", "LxTx_ERROR", "TIME")]

    ## set NA values in LxTx error to 0
    df_FAD$LxTx_ERROR[is.na(df_FAD$LxTx_ERROR)] <- 0

    results_FAD <- analyse_FadingMeasurement(
      object = df_FAD,
      verbose = FALSE,
      plot = plot,
      plot_singlePanels = 3,
      plot.trend = FALSE)

    # (2) ITL Data ------------------------------------------------------------
    df_ITL <- object@data$ITL[object@data$ITL$SAMPLE == sample_names[i],]
    results_ITL <- fit_IsothermalHolding(
      data = df_ITL,
      ITL_model = ITL_model,
      rhop = results_FAD,
      plot = plot,
      verbose = verbose,
      ...)

    # (3) DRC Data ------------------------------------------------------------
    ## get DRC data
    df_DRC <- object@data$DRC[object@data$DRC$SAMPLE == sample_names[i],]

    ## get DRC data in correct form
    df_DRC <- df_DRC[,c("TIME", "LxTx", "LxTx_ERROR", "ALQ")]
    df_DRC$LxTx_ERROR[is.na(df_DRC$LxTx_ERROR)] <- 0

    ## add dose rate if available
    if (!is.null(object@info$Ddot_DRC)) {
      Ddot_DRC <- object@info$Ddot_DRC[[i]]
      ## we have to do this aliquot wise
      for (d in unique(df_DRC[["ALQ"]])) {
        aliquot <- df_DRC$ALQ == d
        df_DRC$TIME[aliquot] <- df_DRC$TIME[aliquot] * Ddot_DRC[d]
      }

      ## adjust column names
      colnames(df_DRC) <- c("DOSE", colnames(df_DRC[-1]))
    }

    ## DRC fitting
    results_DRC <- suppressWarnings(
        fit_DoseResponseCurve(
            object = df_DRC,
            mode = "alternate",
            verbose = verbose))

    ## DRC plotting
    plot_DoseResponseCurve(
      object = results_DRC,
      xlab = if ("DOSE" %in% colnames(df_DRC)) "Dose [Gy]" else "Dose [s]",
      log = "x",
      plot_extended = FALSE,
      mar = c(5.1, 4.1, 4.1, 2.1),
      mgp = c(3, 1, 0),
      tcl = -0.5,
      main = sample_names[i])

    ## return single lists
    return(list(results_FAD, results_ITL, results_DRC))
  })

  ## merge results for each
    ## flatten list
    results_combined <- unlist(results_combined, recursive = TRUE)

  ## get originators (we will merge accordingly)
  originator <- vapply(results_combined, function(x) x@originator, character(1))

  ## get list with merged results
  results_combined <- lapply(unique(originator), function(x) {
    merge_RLum(results_combined[originator == x])
  })
  names(results_combined) <- unique(originator)

# Results -----------------------------------------------------------------
  set_RLum(
    class = "RLum.Results",
    data = list(
      FAD = results_combined[["analyse_FadingMeasurement"]],
      ITL = results_combined[["fit_IsothermalHolding"]],
      DRC = results_combined[["fit_DoseResponseCurve"]]),
    info = list(
      call = sys.call()
    ))
}
