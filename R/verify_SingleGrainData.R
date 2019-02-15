#' Verify single grain data sets and check for invalid grains, i.e.
#' zero-light level grains
#'
#' This function tries to identify automatically zero-light level curves (grains)
#' from single grain data measurements.
#'
#' **How does the method work?**
#'
#' The function compares the expected values (\eqn{E(X)}) and the variance
#' (\eqn{Var(X)}) of the count values for each curve. Assuming that the
#' background roughly follows a poisson distribution the absolute difference
#' of both values should be zero or at least around zero as
#'
#' \deqn{E(x) = Var(x) = \lambda}
#'
#' Thus the function checks for:
#'
#' \deqn{abs(E(x) - Var(x)) >= \Theta}
#'
#' With \eqn{\Theta} an arbitray, user defined, threshold. Values above the
#' threshold indicating curves comprising a signal.
#'
#' Note: the absolute difference of \eqn{E(X)} and \eqn{Var(x)} instead of the
#' ratio was chosen as both terms can become 0 which would result in 0 or `Inf`,
#' if the ratio is calculated.
#'
#' @param object [Risoe.BINfileData-class] or [RLum.Analysis-class] (**required**):
#' input object. The function also accepts a list with objects of allowed type.
#'
#' @param threshold [numeric] (*with default*):
#' numeric threshold value for the allowed difference between the `mean` and
#' the `var` of the count values (see details)
#'
#' @param cleanup [logical] (*with default*):
#' if set to `TRUE` curves indentified as zero light level curves are
#' automatically removed. Ouput is an object as same type as the input, i.e.
#' either [Risoe.BINfileData-class] or [RLum.Analysis-class]
#'
#' @param cleanup_level [character] (*with default*):
#' selects the level for the cleanup of the input data sets.
#' Two options are allowed: `"curve"` or `"aliquot"`:
#'
#' - If  `"curve"` is selected every single curve marked as `invalid` is removed.
#' - If `"aliquot"` is selected, curves of one aliquot (grain or disc) can be
#' marked as invalid, but will not be removed. An aliquot will be only removed
#' if all curves of this aliquot are marked as invalid.
#'
#' @param verbose [logical] (*with default*):
#' enables or disables the terminal feedback
#'
#' @param plot [logical] (*with default*):
#' enables or disables the graphical feedback
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
#' **slot:****`@data`**
#'
#' \tabular{lll}{
#' **Element** \tab **Type** \tab **Description**\cr
#'  `$unique_pairs` \tab `data.frame` \tab the unique position and grain pairs \cr
#'  `$selection_id` \tab `numeric` \tab the selection as record ID \cr
#'  `$selection_full` \tab `data.frame` \tab implemented models used in the baSAR-model core \cr
#' }
#'
#' **slot:****`@info`**
#'
#' The original function call
#'
#' **Output variation**
#'
#' For `cleanup = TRUE` the same object as the input is returned, but cleaned up
#' (invalid curves were removed). This means: Either an [Risoe.BINfileData-class]
#' or an [RLum.Analysis-class] object is returned in such cases.
#' An [Risoe.BINfileData-class] object can be exported to a BIN-file by
#' using the function [write_R2BIN].
#'
#' @note
#' This function can work with [Risoe.BINfileData-class] objects or
#' [RLum.Analysis-class] objects (or a list of it). However, the function is
#' highly optimised for [Risoe.BINfileData-class] objects as it make sense to
#' remove identify invalid grains before the conversion to an
#' [RLum.Analysis-class] object.
#'
#' The function checking for invalid curves works rather robust and it is likely
#' that Reg0 curves within a SAR cycle are removed as well. Therefore it is
#' strongly recommended to use the argument `cleanup = TRUE` carefully.
#'
#' @section Function version: 0.2.1
#'
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#'
#' @seealso [Risoe.BINfileData-class], [RLum.Analysis-class], [write_R2BIN],
#' [read_BIN2R]
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
#' @md
#' @export
verify_SingleGrainData <- function(
  object,
  threshold = 10,
  cleanup = FALSE,
  cleanup_level = 'aliquot',
  verbose = TRUE,
  plot = FALSE
){


  ##three types of input are allowed:
  ##(1) RisoeBINfileData
  ##(2) RLum.Analysis
  ##(3) List of RLum.Analysis

  # Self Call -----------------------------------------------------------------------------------
  if(is(object, "list")){

    results <- lapply(1:length(object), function(x) {
      verify_SingleGrainData(
        object = object[[x]],
        threshold = threshold,
        cleanup = cleanup,
        cleanup_level = cleanup_level,
        verbose = verbose
      )
    })

      ##account for cleanup
      if(cleanup){
        return(results)

      }else{
        return(merge_RLum(results))

      }

  }

  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##RisoeBINfileData
  if(is(object, "Risoe.BINfileData")){

      ##run test on DATA slot
        ##MEAN + SD
        temp.results_matrix <- lapply(X = object@DATA, FUN = function(x){
            c(mean(x), var(x))

        })

        temp.results_matrix <- do.call(rbind,  temp.results_matrix)

        ##DIFF
        temp.results_matrix_RATIO <- temp.results_matrix[,2]/temp.results_matrix[,1]

        ##SEL
        temp.results_matrix_VALID <- temp.results_matrix_RATIO > threshold

      ##combine everything to in a data.frame
        selection <- data.frame(
          POSITION = object@METADATA$POSITION,
          GRAIN = object@METADATA$GRAIN,
          MEAN = temp.results_matrix[, 1],
          VAR = temp.results_matrix[, 2],
          RATIO = temp.results_matrix_RATIO,
          THRESHOLD = rep_len(threshold, length(object@DATA)),
          VALID = temp.results_matrix_VALID
        )

        ##get unique pairs for POSITION and GRAIN for VALID == TRUE
        unique_pairs <- unique(
          selection[selection[["VALID"]], c("POSITION", "GRAIN")])


        if(cleanup_level == "aliquot"){

          selection_id <- sort(unlist(lapply(1:nrow(unique_pairs), function(x) {
            which(
              .subset2(selection, 1) == .subset2(unique_pairs, 1)[x] &
                .subset2(selection, 2) == .subset2(unique_pairs, 2)[x]
            )


          })))


        }else{

         ##reduce data to TRUE selection
         selection_id <- which(selection[["VALID"]])

        }


      ##select output on the chosen input
      if(cleanup){

        ##selected wanted elements
        object@DATA <- object@DATA[selection_id]
        object@METADATA <- object@METADATA[selection_id,]
        object@METADATA$ID <- 1:length(object@DATA)


        ##print message
        selection_id <- paste(selection_id, collapse = ", ")
        if(verbose){
          cat(paste0("\n[verify_SingleGrainData()] Risoe.BINfileData object reduced to records: \n", selection_id))
          cat("\n\n[verify_SingleGrainData()] Risoe.BINfileData object record index reset.")

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
  }else if(is(object,"RLum.Analysis")){

    ##first extract all count values from all curves
    object_list <- lapply(get_RLum(object), function(x){
        ##yes, would work differently, but it is faster
        x@data[,2]

    })

    ##MEAN + SD
    temp.results_matrix <- lapply(X = object_list, FUN = function(x){
      c(mean(x), var(x))

     })

    temp.results_matrix <- do.call(rbind,  temp.results_matrix)

    ##DIFF
    temp.results_matrix_RATIO <- temp.results_matrix[,2]/temp.results_matrix[,1]

    ##SEL
    temp.results_matrix_VALID <- temp.results_matrix_RATIO > threshold

    ##get structure for the RLum.Anlaysis object
    temp_structure <- structure_RLum(object, fullExtent = TRUE)

      ##now we have two cases, depending on where measurement is coming from
      if (object@originator == "Risoe.BINfileData2RLum.Analysis") {

        ##combine everything to in a data.frame
        selection <- data.frame(
          POSITION = temp_structure$info.POSITION,
          GRAIN = temp_structure$info.GRAIN,
          MEAN = temp.results_matrix[, 1],
          VAR = temp.results_matrix[, 2],
          RATIO = temp.results_matrix_RATIO,
          THRESHOLD = rep_len(threshold, length(object_list)),
          VALID = temp.results_matrix_VALID
        )

        ##get unique pairs for POSITION and GRAIN for VALID == TRUE
        unique_pairs <- unique(
          selection[selection[["VALID"]], c("POSITION", "GRAIN")])


      } else if (object@originator == "read_XSYG2R") {

        ##combine everything to in a data.frame
        selection <- data.frame(
          POSITION = if(any(grepl(pattern = "position", names(temp_structure)))){
            temp_structure$info.position}else{
              NA
            },
          GRAIN = NA,
          MEAN = temp.results_matrix[, 1],
          VAR = temp.results_matrix[, 2],
          RATIO = temp.results_matrix_RATIO,
          THRESHOLD = rep_len(threshold, length(object_list)),
          VALID = temp.results_matrix_VALID
        )

        ##get unique pairs for POSITION for VALID == TRUE
        unique_pairs <- unique(
          selection[["POSITION"]][selection[["VALID"]]])

      } else{

        stop("[verify_SingleGrainData()] I don't know what to do object 'originator' not supported!",
             call. = FALSE)
      }


      ##set up cleanup
      if(cleanup_level == "aliquot") {
        if (object@originator == "read_XSYG2R") {

          if(!is.na(unique_pairs)){

          selection_id <-
            sort(unlist(lapply(1:nrow(unique_pairs), function(x) {
              which(.subset2(selection, 1) == .subset2(unique_pairs, 1)[x])


            })))

          }else{
           selection_id <- NA

          }


        } else if (object@originator == "Risoe.BINfileData2RLum.Analysis") {
          selection_id <-
            sort(unlist(lapply(1:nrow(unique_pairs), function(x) {
              which(
                .subset2(selection, 1) == .subset2(unique_pairs, 1)[x] &
                  .subset2(selection, 2) == .subset2(unique_pairs, 2)[x]
              )


            })))

        }

        ##make sure that we do not break subsequent code
        if(length(selection_id) == 0) selection_id <- NA


      } else{
        ##reduce data to TRUE selection
        selection_id <- which(selection[["VALID"]])

      }


    ##return value
    ##select output on the chosen input
    if(cleanup && !any(is.na(selection_id))){

      ##print message
      if(verbose){
        selection_id_text <- paste(selection_id, collapse = ", ")
        cat(paste0("\n[verify_SingleGrainData()] RLum.Analysis object reduced to records: ",
                   selection_id_text))

      }

      ##selected wanted elements
      if (length(selection_id) == 0) {
        object <- set_RLum(
          class = "RLum.Analysis",
          originator = object@originator,
          protocol = object@protocol,
          records = list(),
          info = list(
            unique_pairs = unique_pairs,
            selection_id = selection_id,
            selection_full = selection)
        )

      } else{

        object <- set_RLum(
          class = "RLum.Analysis",
          records = get_RLum(object, record.id = selection_id, drop = FALSE),
          info = list(
            unique_pairs = unique_pairs,
            selection_id = selection_id,
            selection_full = selection)
        )

     }

      ##return
      return_object <- object

    }else{
      if(any(is.na(selection_id))){
        warning("[verify_SingleGrainData()] selection_id is NA, nothing removed, everything selected!",
                call. = FALSE)

      }

      return_object <- set_RLum(
        class = "RLum.Results",
        data = list(
          unique_pairs = unique_pairs,
          selection_id = selection_id,
          selection_full = selection),
        info = list(call = sys.call())
      )

    }


  }else{
    stop(paste0("[verify_SingleGrainData()] Input type '", is(object)[1], "' is not allowed for this function!"), call. = FALSE)

  }

  # Plot ----------------------------------------------------------------------------------------
  if(plot){

    ##plot area
    plot(
      NA,
      NA,
      xlim = c(1,nrow(selection)),
      ylim = range(selection[["RATIO"]]),
      log = "y",
      xlab = "Record index",
      ylab = "Calculated ratio [a.u.]",
      main = "Record selection"
    )

    ##plot points above the threshold
    points(x = which(selection[["VALID"]]),
           y = selection[["RATIO"]][selection[["VALID"]]], pch = 20, col = "darkgreen")
    points(x = which(!selection[["VALID"]]),
           y = selection[["RATIO"]][!selection[["VALID"]]], pch = 20, col = rgb(0,0,0,0.5))

    abline(h = threshold, col = "red", lty = 1, lwd = 2)

    mtext(
      side = 3,
      text = paste0(
        "(total: ", nrow(selection),
        " | valid: ", length(which(selection[["VALID"]])),
        " | invalid: ", length(which(!selection[["VALID"]])), ")"),
      cex = 0.9 * par()$cex)

  }

  # Return --------------------------------------------------------------------------------------
  return(return_object)


}

