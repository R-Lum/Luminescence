#' Verify single grain data sets and check for invalid grains, i.e. zero light level grains
#'
#' This function tries to identify automatically zero light level curves (grains) from single grain data
#' measurements. \cr
#'
#' \bold{method}\cr
#'
#' The function compares the mean and the variance of the count values for each curve. Assuming that
#' the background roughly follows a poission distribution the absolute difference of both values
#' should be zero or at least around zero. Values significantly above the threshold indicate that the curves
#' comprises a signal. The threshold can be freely chosen by the user.\cr
#'
#' Note: the difference instead of the ratio was chosen as the mean and the variance can both become
#' 0 which would result in \code{Inf} values.
#'
#'
#' @param object \code{\linkS4class{Risoe.BINfileData}} or \code{\linkS4class{RLum.Analysis}}
#' (\bold{required}): input object. The function also accepts a list with objects of allowed type.
#'
#' @param threshold \code{\link{numeric}} (with default): numeric threshold value for the allowed difference between
#' the \code{mean} and the \code{var} of the count values (see details)
#'
#' @param cleanup \code{\link{logical}} (with default): if set to \code{TRUE} curves indentified as
#' zero light level curves are automatically removed. Ouput is an object as same type as the input, i.e.
#' either \code{\linkS4class{Risoe.BINfileData}} or \code{\linkS4class{RLum.Analysis}}
#'
#' @param cleanup_level \code{\link{character}} (with default): selects the level for the cleanup
#' of the input data sets. Two options are allowed: \code{"curve"} or \code{"aliquot"}. If  \code{"curve"}
#' is selected every single curve marked as \code{invalid} is removed. If \code{"aliquot"} is selected,
#' curves of one aliquot (grain or disc) can be marked as invalid, but will not be removed. An aliquot
#' will be only removed if all curves of this aliquot are marked as invalid.
#'
#' @param verbose \code{\link{logical}} (with default): enables or disables terminal feedback
#'
#' @return Returns either an S4 object of type \code{\linkS4class{RLum.Results}} and the slot
#' \code{data} contains a \code{\link{list}} with the following structure:\cr
#' $ unique_paris (\code{data.frame}): the unique position and grain pairs \cr
#' .. $ POSITION \cr
#' .. $ GRAIN \cr
#'
#' $ selection_id (\code{numeric}): the made selection by record ID\cr
#'
#' $ selection_full (\code{data.frame}): the full set with all criteria\cr
#' .. $ POSITION \cr
#' .. $ GRAIN \cr
#' .. $ MEAN \cr
#' .. $ VAR \cr
#' .. $ RATIO \cr
#' .. $ THRESHOLD \cr
#' .. $ VALID \cr
#'
#' or for \code{cleanup = TRUE} the same object as the input, but with cleaned up (invalid curves removed).
#' This means: Either an \code{\linkS4class{Risoe.BINfileData}} or an \code{\linkS4class{RLum.Analysis}} object is returned.
#' An \code{\linkS4class{Risoe.BINfileData}} object can be exported to a BIN-file by using the function
#' \code{\link{write_R2BIN}}.
#'
#' @note This function can work with \code{\linkS4class{Risoe.BINfileData}} objects or
#' \code{\linkS4class{RLum.Analysis}} objects (or a list of it). However, the function is highly optimised
#' for \code{\linkS4class{Risoe.BINfileData}} objects as it make sense to remove identify invalid
#' grains before the conversion to an \code{\linkS4class{RLum.Analysis}} object.\cr
#'
#' The function checking for invalid curves works rather robust and it is likely that Reg0 curves
#' within a SAR cycle are removed as well. Therefore it is strongly recommended to use the argument
#' \code{cleanup = TRUE} carefully.
#'
#' @section Function version: 0.1.0
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#'
#' @seealso \code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum.Analysis}},
#' \code{\link{write_R2BIN}}, \code{\link{read_BIN2R}}
#'
#' @references -
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
  cleanup = FALSE,
  cleanup_level = 'aliquot',
  verbose = TRUE
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
          message(paste0("[verify_SingleGrainData()] Risoe.BINfileData object reduced to records: \n", selection_id))
          message("\n\n[verify_SingleGrainData()] Risoe.BINfileData object record index reset.")

        }


        ##return
        return(object)

      }else{
        return(set_RLum(
          class = "RLum.Results",
          data = list(
            unique_pairs =  unique_pairs,
            selection_id = selection_id,
            selection_full = selection),
          info = list(call = sys.call())
        ))

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

        stop("[verify_SingleGrainData()] I don't know what to do object 'originator' not supported!")
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

      } else{
        ##reduce data to TRUE selection
        selection_id <- which(selection[["VALID"]])

      }

    ##return value
    ##select output on the chosen input
    if(cleanup && !is.na(selection_id)){

      ##print message
      if(verbose){
        selection_id <- paste(selection_id, collapse = ", ")
        message(paste0("[verify_SingleGrainData()] RLum.Analysis object reduced to records: ", selection_id))

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
      return(object)

    }else{
      if(is.na(selection_id)){
        warning("[verify_SingleGrainData()] selection_id is NA, nothing removed, everything selected!")

      }

      return(set_RLum(
        class = "RLum.Results",
        data = list(
          unique_pairs = unique_pairs,
          selection_id = selection_id,
          selection_full = selection),
        info = list(call = sys.call())
      ))

    }


  }else{

    stop(paste0("[verify_SingleGrainData()] Input type '", is(object)[1], "' is not allowed for this function!"), call. = FALSE)

  }


}
