#' Verify single grain data sets and check for invalid grains, i.e. zero light level grains
#'
#' This function tries to identify automatically zero light level curves (grains) from single grain data
#' measurements. \cr
#'
#' \bold{method}\cr
#'
#' The function compares the mean and the variance of the count values for each curve. Assuming that
#' the background roughly follows a poission distribution the absolute difference of both values
#' should be zero or at least around zero. Values significantly above indicates that the curves
#' contain a signal. The threshold can be freely chosen by the user.\cr
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
#' zero light level curves are automatically removed. Ouput is an object as same type as the input.
#'
#' @return Returns either an S4 object of type \code{\linkS4class{RLum.Results}} and the slot
#' \code{data} contains a \code{\link{list}} with the following structure:\cr
#' $ selection (data.frame) \cr
#' .. $ POSITION \cr
#' .. $ GRAIN \cr
#' .. $ MEAN \cr
#' .. $ VAR \cr
#' .. $ DIFF \cr
#' .. $ THRESHOLD \cr
#' .. $ VALID \cr
#'
#' or for \code{cleanup = TRUE} the same object as the input, but with cleaned up (invalid curves
#' removed)
#'
#'
#' @note This function can work with \code{\linkS4class{Risoe.BINfileData}} objects or
#' \code{\linkS4class{RLum.Analysis}} objects (or a list of it). However, the function is highly optimised
#' for \code{\linkS4class{Risoe.BINfileData}} objects as it make sense to remove identify invalid
#' grains before the conversion to an \code{\linkS4class{RLum.Analysis}} object.\cr
#'
#' Currently the function just check for invalid curves and work quite robost. Within a SAR cycle
#' Reg0 curves are likely to removed as well. Therefore it is strongly recommended to use the argument
#' \code{cleanup = TRUE} carefully.
#'
#' @section Function version: 0.1.0
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#'
#' @seealso \code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum.Analysis}}
#'
#' @references -
#'
#' @keywords manip
#'
#' @examples
#'
#' ##just show how to apply the function
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##verify and get data.frame out of it
#' verify_SingleGrainData(OSL.SARMeasurement$Sequence.Object)$selection
#'
#' @export
verify_SingleGrainData <- function(
  object,
  threshold = 20,
  cleanup = FALSE
){

  ##TODO
  ##The fucntion should better remove only grains indentified as invalid entirely ... a single
  ##curve from a grain position should not set all grain as invalid, however, in the data frame
  ##another column would be needed
  ##Consider to use the ratio instead of the diff

  ##three types of input are allowed:
  ##(1) RisoeBINfileData
  ##(2) RLum.Analysis
  ##(3) List of RLum.Analysis

  # Self Call -----------------------------------------------------------------------------------
  if(is(object, "list")){

      results <- lapply(object, function(x){

        verify_SingleGrainData(x, threshold = threshold, cleanup = cleanup)

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
        temp.results_matrix_DIFF <- abs(matrixStats::rowDiffs(temp.results_matrix))

        ##SEL
        temp.results_matrix_VALID <- temp.results_matrix_DIFF > threshold

      ##combine everything to in a data.frame
        selection <- data.frame(
          POSITION = object@METADATA$POSITION,
          GRAIN = object@METADATA$GRAIN,
          MEAN = temp.results_matrix[, 1],
          VAR = temp.results_matrix[, 2],
          DIFF = temp.results_matrix_DIFF,
          THRESHOLD = rep_len(threshold, length(object@DATA)),
          VALID = temp.results_matrix_VALID
        )

      ##select output on the chosen input
      if(cleanup){

        ##reduce data to TRUE selection
        selection_id <- which(selection$VALID)

          ##selected wanted elemennts
          object@DATA <- object@DATA[selection_id]
          object@METADATA <- object@METADATA[selection_id,]
          object@METADATA$ID <- 1:length(object@DATA)


        ##print message
        selection_id <- paste(selection_id, collapse = ", ")
        message(paste0("[verify_SingleGrainData()] Risoe.BINfileData object reduced to records: ", selection_id))
        message("[verify_SingleGrainData()] Risoe.BINfileData object record index reset.")

        ##return
        return(object)

      }else{
        return(set_RLum(
          class = "RLum.Results",
          data = list(selection = selection),
          info = list(call = sys.call())
        ))

      }


  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##RLum.Analysis and list with RLum.Analysis objects
  ## ... and yes it make sense not to mix that up with the code above
  }else if(is(object,"RLum.Analysis")){

    ##first extract all count values from all curves
    object_list <- lapply(get_RLum(object), function(x){

        ##yes, would work differently, but it faster
        x@data[,2]

    })

    ##MEAN + SD
    temp.results_matrix <- lapply(X = object_list, FUN = function(x){
      c(mean(x), var(x))

     })

    temp.results_matrix <- do.call(rbind,  temp.results_matrix)

    ##DIFF
    temp.results_matrix_DIFF <- abs(matrixStats::rowDiffs(temp.results_matrix))

    ##SEL
    temp.results_matrix_VALID <- temp.results_matrix_DIFF > threshold

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
          DIFF = temp.results_matrix_DIFF,
          THRESHOLD = rep_len(threshold, length(object_list)),
          VALID = temp.results_matrix_VALID
        )

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
          DIFF = temp.results_matrix_DIFF,
          THRESHOLD = rep_len(threshold, length(object_list)),
          VALID = temp.results_matrix_VALID
        )


      } else{

        stop("[verify_SingleGrainData()] I don't know what to do object 'originator' not supported!")
      }

     ##return value
    ##select output on the chosen input
    if(cleanup){

      ##reduce data to TRUE selection
      selection_id <- which(selection$VALID)

      ##selected wanted elemennts
      if (length(selection_id) == 0) {
        object <- set_RLum(
          class = "RLum.Analysis",
          originator = object@originator,
          protocol = object@protocol,
          records = list(),
          info = list(selection = selection)
        )

      } else{

        object <- set_RLum(
          class = "RLum.Analysis",
          records = get_RLum(object, record.id = selection_id, drop = FALSE),
          info = list(selection = selection)
        )

     }

      ##print message
      selection_id <- paste(selection_id, collapse = ", ")
      message(paste0("[verify_SingleGrainData()] RLum.Analysis object reduced to records: ", selection_id))

      ##return
      return(object)

    }else{
      return(set_RLum(
        class = "RLum.Results",
        data = list(selection = selection),
        info = list(call = sys.call())
      ))

    }


  }else{

    stop(paste0("[verify_SingleGrainData()] Input type '", is(object)[1], "' is not allowed for this function!"), call. = FALSE)

  }


}
