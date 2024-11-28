#'@title Trim Channels of RLum.Data-class Objects
#'
#'@description Trim off the number of channels of [RLum.Data-class] objects of similar record type
#' on the time domain. This function is useful in cases where objects have different lengths (short/longer
#'measurement time) but should be analysed jointly by other functions.
#'
#'@details
#'The function has two modes of operation:
#'
#' 1. Single [RLum.Data-class] objects or a [list] of such objects:
#' the function is applied separately over each object.
#'
#' 2. Multiple curves via [RLum.Analysis-class] or a [list] of such objects:
#' in this mode, the function first determines the minimum number of channels
#' for each category of records and then jointly processes them. For instance,
#' if the object contains one TL curve with 100 channels and two OSL curves
#' with 100 and 99 channels, respectively, then the minimum would be set to
#' 100 channels for the TL curve and to 99 for the OSL curves. If no further
#' parameters are applied, the function will shorten all OSL curves to 99
#' channels, but leave the TL curve untouched.
#'
#'@param object [RLum.Data-class] [RLum.Analysis-class] (**required**): input object,
#'can be a [list] of objects. Please note that in the latter case the function works
#'only isolated on each element of the [list].
#'
#'@param recordType [character] (*optional*): type of the record where the trim
#'should be applied. If not set, the types are determined automatically and applied
#'for each record type classes. Can be provided as [list].
#'
#' @param trim_range [numeric] (*optional*): sets the range of indices to
#' keep. If only one value is given, this is taken to be the minimum; if two
#' values are given, then the range is defined between the two values
#' (inclusive). Any value beyond the second is silently ignored. If nothing
#' is set (default), then all curves are trimmed to the same maximum length.
#'
#'@returns A trimmed object or [list] of such objects similar to the input objects
#'
#'@section Function version: 0.2
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [RLum.Data-class], [RLum.Analysis-class]
#'
#'@keywords manip
#'
#'@examples
#'## trim all TL curves in the object to channels 10 to 20
#'data(ExampleData.BINfileData, envir = environment())
#'temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
#'
#'c <- trim_RLum.Data(
#' object = temp,
#' recordType = "TL",
#' trim_range = c(10,20))
#'
#'plot_RLum.Analysis(
#'object = c,
#'combine = TRUE,
#'subset = list(recordType = "TL"))
#'
#'## simulate a situation where one OSL curve
#'## in the dataset has only 999 channels instead of 1000
#'## all curves should be limited to 999
#'temp@records[[2]]@data <- temp@records[[2]]@data[-nrow(temp[[2]]@data),]
#'
#'c <- trim_RLum.Data(object = temp)
#'nrow(c@records[[4]]@data)
#'
#'
#'@md
#'@export
trim_RLum.Data <- function(
    object,
    recordType = NULL,
    trim_range = NULL
) {
  .set_function_name("trim_RLum.Data")
  on.exit(.unset_function_name(), add = TRUE)

# Self-call ---------------------------------------------------------------
  if(inherits(object, "list")) {
    ## expand parameters
    parm <- .expand_parameters(length(object))

    l <- lapply(seq_along(object), function(x){
      trim_RLum.Data(
        object = object[[x]],
        recordType = parm$recordType[[x]],
        trim_range = parm$trim_range[[x]])
    })

    return(l)
  }

# Work horse functions ----------------------------------------------------
  ## RLum.Data.Curve
  .trim_RLum.Data.Curve <- function(object, type, range){
    ## only if type is matched
    if  (any(object@recordType[1] %in% type)) {
      range[2] <- min(nrow(object@data), range[2])
      object@data <- object@data[range[1]:range[2], , drop = FALSE]
    }
    object
  }

  ## RLum.Data.Spectrum
  .trim_RLum.Data.Spectrum <- function(object, type, range){
    ## only if type is matched
    if (any(object@recordType[1] %in% type)) {
      range[2] <- min(ncol(object@data), range[2])
      object@data <- object@data[, range[1]:range[2], drop = FALSE]
    }
    object
  }

  ## RLum.Data.Image
  .trim_RLum.Data.Image <- function(object, type, range){
    ## only if type is matched and data is not empty
    if (any(object@recordType[1] %in% type && length(dim(object@data)) == 3)) {
      range <- pmin(range, dim(object@data)[3])
      object@data <- object@data[, , range[1]:range[2], drop = FALSE]
    }
    object
  }

  ## RLum.Analysis (which calls the functions above)
  .trim_RLum.Analysis <- function(object, type, range) {
    ## determine lengths of objects
    ln <- unlist(lapply(object@records, function(x) {
      ln <- switch(
        class(x)[1],
        "RLum.Data.Curve" = dim(x@data)[1],
        "RLum.Data.Spectrum" = dim(x@data)[2],
        "RLum.Data.Image" = dim(x@data)[3]
      )
      names(ln) <- x@recordType
      ln
    }))

    ## run over single objects
    object@records <- lapply(object@records, function(x){
      ## determine max and min of the particular record Types compared to the global information
      tmp_max <- min(c(ln[names(ln) == x@recordType], range[2]))
      tmp_min <- range[1] ## cannot be smaller than 1

      ## call sub-function to process and return
      .trim_object(x, recordType, c(tmp_min, tmp_max))
    })

    return(object)
  }

  ## function dispatcher
  .trim_object <- function(obj, type, range) {
    trim.fun <- switch(class(obj)[1],
                       "RLum.Analysis"      = .trim_RLum.Analysis,
                       "RLum.Data.Curve"    = .trim_RLum.Data.Curve,
                       "RLum.Data.Image"    = .trim_RLum.Data.Image,
                       "RLum.Data.Spectrum" = .trim_RLum.Data.Spectrum)
    trim.fun(obj, type, range)
  }


  ## Integrity checks -------------------------------------------------------

  .validate_class(object, c("RLum.Data", "RLum.Analysis"))

  ## determine classes for record Types
  if(is.null(recordType)) {
    recordType <- switch(
      class(object)[1],
      "RLum.Analysis" = unique(vapply(object@records, function(x) x@recordType, character(1))),
      object@recordType
    )
  }
  .validate_class(recordType, "character")

  ## set trim_range if not provided
  if (is.null(trim_range))
    trim_range <- c(1,Inf)
  .validate_class(trim_range, c("integer", "numeric"))

  ## silently sanitize `trim_range` to ensure that it has length 2,
  ## it contains no elements smaller than 1, and is sorted
  if (length(trim_range) == 1)
    trim_range <- c(1, trim_range)
  trim_range <- sort(pmax(abs(trim_range[1:2]), 1))

  ## Dispatch and return ----------------------------------------------------
  .trim_object(object, type = recordType, range = trim_range)
}
