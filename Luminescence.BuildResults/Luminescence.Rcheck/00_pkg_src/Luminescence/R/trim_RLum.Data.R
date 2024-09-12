#'@title Trim Channels of RLum.Data-class Objects
#'
#'@description Trim off the number of channels of [RLum.Data-class] objects of similar record type
#' on the time domain. This function is useful in cases where objects have different lengths (short/longer
#'measurement time) but should be analysed jointly by other functions.
#'
#'@details
#'The function has two modes of operation:
#'
#'1. Single [RLum.Data-class] objects or a [list] of such objects
#'The function is applied separately over each object.
#'
#'2. Multiple curves via [RLum.Analysis-class] or a [list] of such objects
#'In this mode, the function first determines the minimum number of channels for
#'each category of records and then jointly processes them.
#'For instance, the object contains one TL curve with 100 channels and two
#'OSL curves with 100 and 99 channels, respectively. Than the minimum for TL would be set
#'to 100 channels and 99 for the OSL curves. If no further parameters are applied, the
#'function will shorten all OSL curves to 99 channels, but leave the TL curve untouched.
#'
#'@param object [RLum.Data-class] [RLum.Analysis-class] (**required**): input object,
#'can be a [list] of objects. Please note that in the latter case the function works
#'only isolated on each element of the [list].
#'
#'@param recordType [character] (*optional*): type of the record where the trim
#'should be applied. If not set, the types are determined automatically and applied
#'for each record type classes. Can be provided as [list].
#'
#'@param trim_range [numeric] (*optional*): sets the trim range (everything
#'within the range + 1 is kept). If nothing is set all curves are trimmed to a similar
#'maximum length. Can be provided as [list].
#'
#'@returns A trimmed object or [list] of such objects similar to the input objects
#'
#'@section Function version: 0.1.0
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
    if(any(object@recordType[1] %in% type))
      object@data <- object@data[max(c(1,range[1])):min(c(nrow(object@data),range[2])),, drop = FALSE]

    object
  }

  ## RLum.Data.Spectrum
  .trim_RLum.Data.Spectrum <- function(object, type, range){
    ## only if type is matched
    if(any(object@recordType[1] %in% type))
      object@data <- object@data[,max(c(1,range[1])):min(c(ncol(object@data),range[2])), drop = FALSE]

    object
  }

  ## RLum.Data.Image
  .trim_RLum.Data.Image <- function(object, type, range){
    ## only if type is matched
    if(any(object@recordType[1] %in% type))
      object@data <- object@data[,,max(c(1,range[1])):min(c(ncol(object@data),range[2])), drop = FALSE]

    object
  }

  ## RLum.Analysis (which calls the functions above)
  .trim_RLum.Analysis <- function(object, type, range) {
    ## determine lengths of objects
    ln <- unlist(lapply(object@records, function(x) {
      ln <- switch(
        class(x)[1],
        "RLum.Data.Curve" = dim(x@data)[1],
        "RLum.Data.Spectrum" = dim(x@dsta)[2],
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
      switch(
        class(x)[1],
        "RLum.Data.Curve" = .trim_RLum.Data.Curve(object = x, type = recordType, range = c(tmp_min, tmp_max)),
        "RLum.Data.Spectrum" = .trim_RLum.Data.Spectrum(object = x, type = recordType, range = c(tmp_min, tmp_max)),
        "RLum.Data.Image" = .trim_RLum.Data.Image(object = x, type = recordType, range = c(tmp_min, tmp_max)),
        x
      )
    })

    return(object)

  }

# Dispatcher -------------------------------------------------------------------
  ## stop for wrong input
  if(!inherits(object, "RLum.Data") && !inherits(object, "RLum.Analysis"))
    stop("[trim_RLum.Data()] Unsupported input class!", call. = FALSE)

  ## determine classes for record Types
  if(is.null(recordType)) {
    recordType <- switch(
      class(object)[1],
      "RLum.Analysis" = unique(vapply(object@records, function(x) x@recordType, character(1))),
      object@recordType
    )

  }

  ## silently sanitize trim_range input
  if(all(is.null(trim_range)))
    trim_range <- c(1,Inf)
  else if(length(trim_range) == 1)
    trim_range <- c(1, abs(trim_range))
  else if(length(trim_range) > 2)
    trim_range <- abs(trim_range[1:2])

# Dispatch and return -----------------------------------------------------
 switch(
   class(object)[1],
   "RLum.Data.Curve" = .trim_RLum.Data.Curve(object, type = recordType, range = trim_range),
   "RLum.Data.Spectrum" = .trim_RLum.Data.Spectrum(object, type = recordType, range = trim_range),
   "RLum.Data.Image" = .trim_RLum.Data.Image(object, type = recordType, range = trim_range),
   "RLum.Analysis" = .trim_RLum.Analysis(object, type = recordType, range = trim_range)
 )

}
