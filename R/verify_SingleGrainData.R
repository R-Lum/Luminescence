#' @title Verify single grain data sets and check for invalid grains, i.e.
#' zero-light level grains
#'
#' @description
#' This function tries to identify automatically zero-light level curves (grains)
#' from single grain data measurements.
#'
#' @details
#'
#' **How does the method work?**
#'
#' The function compares the expected values (\eqn{E(X)}) and the variance
#' (\eqn{Var(X)}) of the count values for each curve. Assuming that the
#' background roughly follows a Poisson distribution, the absolute difference
#' of both values should be zero or at least around zero as
#'
#' \deqn{E(x) = Var(x) = \lambda}
#'
#' Thus the function checks for:
#'
#' \deqn{abs(E(x) - Var(x)) >= \Theta}
#'
#' With \eqn{\Theta} an arbitrary, user defined, threshold. Values above the
#' threshold indicate curves comprising a signal.
#'
#' Note: the absolute difference of \eqn{E(X)} and \eqn{Var(x)} instead of the
#' ratio was chosen as both terms can become 0 which would result in 0 or `Inf`,
#' if the ratio is calculated.
#'
#' @param object [Luminescence::Risoe.BINfileData-class] or [Luminescence::RLum.Analysis-class] (**required**):
#' input object. The function also accepts a list with objects of allowed type.
#'
#' @param threshold [numeric] (*with default*):
#' numeric threshold value for the allowed difference between the `mean` and
#' the `var` of the count values (see details).
#'
#' @param use_fft [logical] (*with default*): applies an additional approach based on [stats::fft].
#' The threshold is fixed and cannot be changed.
#'
#' @param cleanup [logical] (*with default*):
#' if set to `TRUE`, curves/aliquots identified as zero light level curves/aliquots are
#' automatically removed. Output is an object as same type as the input, i.e.
#' either [Luminescence::Risoe.BINfileData-class] or [Luminescence::RLum.Analysis-class]
#'
#' @param cleanup_level [character] (*with default*):
#' selects the level for the clean-up of the input data sets.
#' Two options are allowed: `"curve"` or `"aliquot"`:
#'
#' - If  `"curve"` is selected, every single curve marked as `invalid` is removed.
#' - If `"aliquot"` is selected, curves of one aliquot (grain or disc) can be
#' marked as invalid, but will not be removed. An aliquot will be only removed
#' if all curves of this aliquot are marked as invalid.
#'
#' @param verbose [logical] (*with default*):
#' enable/disables output to the terminal.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... further parameters to control the plot output; if selected.
#' Supported arguments `main`, `ylim`
#'
#' @return
#' The function returns
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** `@data`
#'
#' \tabular{lll}{
#' **Element** \tab **Type** \tab **Description**\cr
#'  `$unique_pairs` \tab `data.frame` \tab the unique position and grain pairs \cr
#'  `$selection_id` \tab `numeric` \tab the selection as record ID \cr
#'  `$selection_full` \tab `data.frame` \tab implemented models used in the baSAR-model core \cr
#' }
#'
#' **slot:** `@info`
#'
#' The original function call
#'
#' **Output variation**
#'
#' For `cleanup = TRUE` the same object as the input is returned, but cleaned up
#' (invalid curves were removed). This means: Either a [Luminescence::Risoe.BINfileData-class]
#' or an [Luminescence::RLum.Analysis-class] object is returned in such cases.
#' A [Luminescence::Risoe.BINfileData-class] object can be exported to a BINX-file by
#' using the function [Luminescence::write_R2BIN].
#'
#' @note
#' This function can work with [Luminescence::Risoe.BINfileData-class] objects or
#' [Luminescence::RLum.Analysis-class] objects (or a list of it). However, the function is
#' highly optimised for [Luminescence::Risoe.BINfileData-class] objects as it make sense to
#' remove identify invalid grains before the conversion to an
#' [Luminescence::RLum.Analysis-class] object.
#'
#' The function checking for invalid curves works rather robust and it is likely
#' that Reg0 curves within a SAR cycle are removed as well. Therefore it is
#' strongly recommended to use the argument `cleanup = TRUE` carefully if
#' the cleanup works only on curves.
#'
#' @section Function version: 0.2.7
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::Risoe.BINfileData-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::write_R2BIN], [Luminescence::read_BIN2R]
#'
#' @keywords manip datagen
#'
#' @examples
#'
#' ##01 - basic example I
#' ##just show how to apply the function
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##verify and get data.frame out of it
#' verify_SingleGrainData(OSL.SARMeasurement$Sequence.Object)$selection_full
#'
#' ##02 - basic example II
#' data(ExampleData.BINfileData, envir = environment())
#' id <- verify_SingleGrainData(object = CWOSL.SAR.Data,
#' cleanup_level = "aliquot")$selection_id
#'
#' \dontrun{
#' ##03 - advanced example I
#' ##importing and exporting a BIN-file
#'
#' ##select and import file
#' file <- file.choose()
#' object <- read_BIN2R(file)
#'
#' ##remove invalid aliquots(!)
#' object <- verify_SingleGrainData(object, cleanup = TRUE)
#'
#' ##export to new BIN-file
#' write_R2BIN(object, paste0(dirname(file),"/", basename(file), "_CLEANED.BIN"))
#' }
#'
#' @export
verify_SingleGrainData <- function(
    object,
    threshold = 10,
    use_fft = FALSE,
    cleanup = FALSE,
    cleanup_level = c("aliquot", "curve"),
    verbose = TRUE,
    plot = FALSE,
    ...
) {
  .set_function_name("verify_SingleGrainData")
  on.exit(.unset_function_name(), add = TRUE)

  ##three types of input are allowed:
  ##(1) RisoeBINfileData
  ##(2) RLum.Analysis
  ##(3) List of RLum.Analysis

  # Self Call -----------------------------------------------------------------------------------
  if (inherits(object, "list")) {
    if (length(object) == 0)
      return(set_RLum(class = if (isTRUE(cleanup)) "RLum.Analysis" else "RLum.Results"))

    results <- .warningCatcher(lapply(seq_along(object), function(x) {
      verify_SingleGrainData(
        object = object[[x]],
        threshold = threshold,
        use_fft = use_fft,
        cleanup = cleanup,
        cleanup_level = cleanup_level,
        verbose = verbose,
        plot = plot,
        main = paste0("Record #",x)
      )
    }))

    ##account for cleanup
    if (!cleanup)
      return(merge_RLum(results))

    results <- .rm_NULL_elements(.rm_nonRLum(results))
    if (length(results) == 0)
      return(NULL)

    return(results)
  }

  ## ------------------------------------------------------------------------
  ## input validation

  .validate_class(object, c("Risoe.BINfileData", "RLum.Analysis"),
                  extra = "a 'list' of such objects")
  .validate_positive_scalar(threshold)
  .validate_logical_scalar(use_fft)
  .validate_logical_scalar(cleanup)
  cleanup_level <- .validate_args(cleanup_level, c("aliquot", "curve"))
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(plot)

  ## implement Fourier Transform for Frequency Analysis
  ## inspired by ChatGPT (OpenAI, 2024)
  .calc_FFT_selection <- function(l, tmp_threshold = threshold/2) {
    vapply(l, function(x){
      x <- x[x>0]
      tmp_power_spectrum <- Mod(stats::fft(x)^2)
      tmp_mean_power <- mean(tmp_power_spectrum[-1])
      tmp_dominant_power <- max(tmp_power_spectrum[2:(length(tmp_power_spectrum)/2)])
      tmp_dominant_power > tmp_threshold * tmp_mean_power
    }, logical(1))
  }

  ## helper to compute mean and variance for a list of curves
  .mean_var_matrix <- function(l) t(vapply(l, function(x) c(mean(x), stats::var(x)),
                                           numeric(2)))

  ## helper to build the selection data.frame
  .build_selection_df <- function(position, grain, data) {
    ratio <- temp.results_matrix[, 2] / temp.results_matrix[, 1]
    data.frame(
      POSITION = position,
      GRAIN = grain,
      MEAN = temp.results_matrix[, 1],
      VAR = temp.results_matrix[, 2],
      RATIO = ratio,
      THRESHOLD = threshold,
      VALID = ratio > threshold & if (use_fft) .calc_FFT_selection(data) else TRUE
    )
  }

  ## helper for aliquot-level selection by one or two columns
  .aliquot_selection_id <- function(cols) {
    if (nrow(unique_pairs) == 0) return(integer(0))
    if (all(is.na(unique_pairs))) return(NA)
    sort(unlist(lapply(1:nrow(unique_pairs), function(x) {
      matches <- .subset2(selection, cols[1]) == .subset2(unique_pairs, cols[1])[x]
      if (length(cols) > 1)
        matches <- matches & .subset2(selection, cols[2]) == .subset2(unique_pairs, cols[2])[x]
      which(matches)
    })))
  }

  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##RisoeBINfileData
  if(inherits(object, "Risoe.BINfileData")){
    ##run test on DATA slot
    ##MEAN + SD
    temp.results_matrix <- .mean_var_matrix(object@DATA)

    ## combine everything into a data.frame
    selection <- .build_selection_df(object@METADATA$POSITION,
                                     object@METADATA$GRAIN,
                                     object@DATA)

    ##get unique pairs for POSITION and GRAIN for VALID == TRUE
    unique_pairs <- unique(
      selection[selection[["VALID"]], c("POSITION", "GRAIN")])

    selection_id <- if (cleanup_level == "aliquot") .aliquot_selection_id(1:2)
                    else which(selection[["VALID"]])

    ##select output on the chosen input
    if(cleanup){
      ##selected wanted elements
      object@DATA <- object@DATA[selection_id]
      if(length(object@DATA) > 0) {
        object@METADATA <- object@METADATA[selection_id,]
        object@METADATA$ID <- 1:length(object@DATA)

        ##print message
        if(verbose){
          cat("\n[verify_SingleGrainData()] Risoe.BINfileData object reduced to records:\n",
              .collapse(selection_id, quote = FALSE))
          cat("\n\n[verify_SingleGrainData()] Risoe.BINfileData object record index reset.\n")
        }
      } else {
        object <- NULL
      }

      ##return
      return_object <- object

    }else{
      return_object <- set_RLum(
        class = "RLum.Results",
        data = list(
          unique_pairs =  unique_pairs,
          selection_id = selection_id,
          selection_full = selection),
        info = list(call = sys.call())
      )
    }


    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##RLum.Analysis and list with RLum.Analysis objects
    ## ... and yes it make sense not to mix that up with the code above
  } else if (inherits(object,"RLum.Analysis")) {
    ## check for empty records
    if(length(object@records) == 0) {
      .throw_warning("Cannot process empty RLum.Analysis objects, NULL returned")
      return(NULL)
    }

    .validate_originator(object,
                         c("Risoe.BINfileData2RLum.Analysis", "read_XSYG2R"))
    is_risoe_origin <- .check_originator(object, "Risoe.BINfileData2RLum.Analysis")

    ##first extract all count values from all curves
    object_list <- lapply(object@records, function(x){
      ##yes, would work differently, but it is faster
      x@data[,2]
    })

    ##MEAN + SD
    temp.results_matrix <- .mean_var_matrix(object_list)

    ##get structure for the RLum.Analysis object
    temp_structure <- structure_RLum(object, fullExtent = TRUE)

    ##now we have two cases, depending on where measurement is coming from
    if (is_risoe_origin) {
      selection <- .build_selection_df(temp_structure$info.POSITION,
                                       temp_structure$info.GRAIN,
                                       object_list)
      sel.cols <- c("POSITION", "GRAIN")

    } else { ## read_XSYG2R case
      selection <- .build_selection_df(
          if (any(grepl("position", names(temp_structure))))
            temp_structure$info.position else NA,
          NA,
          object_list)
      sel.cols <- "POSITION"
    }

    ## get unique POSITION and GRAIN pairs where VALID == TRUE
    unique_pairs <- unique(selection[selection[["VALID"]], sel.cols, drop = FALSE])

    ##set up cleanup
    if(cleanup_level == "aliquot") {
      selection_id <- if (is_risoe_origin) .aliquot_selection_id(1:2)
                      else .aliquot_selection_id(1)

      ##make sure that we do not break subsequent code
      if(length(selection_id) == 0) selection_id <- NA

    } else{
      ##reduce data to TRUE selection
      selection_id <- which(selection[["VALID"]])
    }

    ##return value
    ##select output on the chosen input
    if (cleanup && !anyNA(selection_id)) {
      ##print message
      if(verbose && cleanup_level == "curve"){
        sid <- .collapse(selection_id, quote = FALSE)
        .throw_message("RLum.Analysis object reduced to records: ",
                       if (!nzchar(sid)) "<none>" else sid, error = FALSE)
      }

      ##selected wanted elements
      return_object <- set_RLum(
          class = "RLum.Analysis",
          records = suppressWarnings(get_RLum(object, record.id = selection_id,
                                              drop = FALSE)),
          info = list(
            unique_pairs = unique_pairs,
            selection_id = selection_id,
            selection_full = selection)
        )
      if (length(selection_id) == 0) {
        return_object@originator <- object@originator
        return_object@protocol <- object@protocol
      }

    }else{
      if (anyNA(selection_id))
        .throw_warning("'selection_id' is NA, everything tagged for removal")

      return_object <- set_RLum(
        class = "RLum.Results",
        data = list(
          unique_pairs = unique_pairs,
          selection_id = selection_id,
          selection_full = selection),
        info = list(call = sys.call())
      )

      ## cleanup means cleanup
      if (cleanup)
        return_object <- NULL
    }
  }

  # Plot ----------------------------------------------------------------------------------------
  if(plot){
    ##set plot settings
    plot_settings <-
      modifyList(x = list(
        main = "Record selection",
        ylim = range(c(selection[["RATIO"]], threshold * 1.1))
      ),
      val = list(...))

    ##plot area
    plot(
      NA,
      NA,
      xlim = c(1,nrow(selection)),
      ylim = plot_settings$ylim,
      log = "y",
      xlab = "Record index",
      ylab = "Calculated ratio [a.u.]",
      main = plot_settings$main
    )

    sel_valid <- which(selection[["VALID"]])
    sel_invalid <- which(!selection[["VALID"]])

    ##plot points above the threshold
    points(x = sel_valid,
           y = selection[["RATIO"]][sel_valid], pch = 20, col = "darkgreen")
    points(x = sel_invalid,
           y = selection[["RATIO"]][sel_invalid], pch = 20, col = rgb(0,0,0,0.5))

    abline(h = threshold, col = "red", lty = 1, lwd = 2)

    mtext(
      side = 3,
      text = paste0(
        "(total: ", nrow(selection),
        " | valid: ", length(sel_valid),
        " | invalid: ", length(sel_invalid), ")"),
      cex = 0.9 * par()$cex)
  }

  # Return --------------------------------------------------------------------------------------
  if(is.null(return_object))
    .throw_warning("Verification and cleanup removed all records, NULL returned")

  return(return_object)
}
