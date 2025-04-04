#'@title Remove Signal Background from `RLum.Data.Curve` Objects
#'
#'@description Convenient (background) of luminescence curves using [merge_RLum]
#'
#'@details
#'
#'The function aims to simplify a frequently encountered task: subtracting curves
#'or backgrounds from luminescence curves, such as OSL, TL, or RF. The function
#'presumes that if curves are presented in pairs, for instance, TL - TL,
#'the second curve represents a background signal that needs to be removed
#'from the first curve. Following the removal, the background curve is discarded from
#'the dataset. Alternatively, custom background curves can be provided,
#'which are then utilised for the subtraction. In essence, the function
#'utilises the [merge_RLum] function but simplifies the selection of pairs and curves.
#'
#'@param object [RLum.Analysis-class] (**required**): A non-empty [RLum.Analysis-class] object
#'with, e.g., OSL/IRSL/TL curves or a [list] of such object. If a list is provided
#'non-conform list elements are silently removed from the [list]
#'
#'@param object_bg [RLum.Data.Curve-class], a [list] of such objects, a [matrix] or [numeric] (*optional*):
#'Sets the background as a curve that is subtracted from the record types set with `recordType`.
#'If you provide a [matrix] or [numeric] internally, everything is coerced to a [RLum.Data.Curve-class] object.
#'If you desire full freedom, you can construct a [list] of [RLum.Data.Curve-class] objects.
#'
#'@param recordType [character] (*optional*): provide the `recordType` subject to the
#'background subtraction. Subsequent curve selection uses [get_RLum].
#'If set to `NULL` the record type of highest occurrence will be used. Example: `recordType = "TL (UVVIS)"`
#'
#'@param clean_up [logical] (*with default*): enable/disable background curve removal
#'after background subtraction. If `object_bg` is set, nothing is removed from
#'the input object as the background is already stored separately.
#'
#'@returns Returns an [RLum.Analysis-class] object or a [list] of such objects.
#'
#'@section Function version: 0.1.0
#'
#'@author
#'Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [get_RLum], [merge_RLum], [RLum.Analysis-class],[RLum.Data.Curve-class]
#'
#'@keywords datagen
#'
#'@examples
#'
#'## load example dataset
#'xsyg <- read_XSYG2R(
#'system.file("extdata/XSYG_file.xsyg", package = "Luminescence"),
#'fastForward = TRUE,
#'verbose = FALSE)
#'
#'## remove constant background from OSL curves
#'remove_SignalBackground(
#' object = xsyg,
#' object_bg = 100,
#' recordType = "OSL (UVVIS)")
#'
#' ## use a more elaborate examples
#' ## with two TL curves (2nd is background)
#' xsyg_v1 <- set_RLum("RLum.Analysis", records = c(
#'  rep(xsyg[[1]]@records[[1]], 2),
#'  xsyg[[1]]@records[[4]],
#'  xsyg[[1]]@records[[4]],
#'  rep(xsyg[[1]]@records[[10]], 2),
#'  xsyg[[1]]@records[[4]],
#'  xsyg[[1]]@records[[4]]))
#'
#' ## remove background and strip background
#' ## curves from the object
#' o <- remove_SignalBackground(
#'  object = xsyg_v1,
#'  recordType = "TL (UVVIS)")
#'
#'@md
#'@export
remove_SignalBackground <- function(
  object,
  object_bg = NULL,
  recordType = NULL,
  clean_up = TRUE

){
    # Internals (DO NOT TOUCH THIS PART) --------------------------------------
  .set_function_name("remove_SignalBackground")
  on.exit(.unset_function_name(), add = TRUE)

  # Self-call ---------------------------------------------------------------
  if(inherits(object, "list")) {
    ## no expansion not special treatment except for silent object removal
    return({
      lapply(
        X = .rm_nonRLum(object, "RLum.Analysis"),
        FUN = remove_SignalBackground,
        object_bg = object_bg,
        recordType = recordType,
        clean_up = clean_up)

    })

  }

  # Integrity tests ---------------------------------------------------------
  .validate_class(object, "RLum.Analysis")
  if(!is.null(object_bg)) .validate_class(object_bg, c("RLum.Data.Curve","list", "matrix", "numeric", "integer"))
  if(!is.null(recordType)) .validate_class(recordType, "character")
  .validate_class(clean_up, "logical")

  # Find curves for removal -------------------------------------------------
  ## if nothing is set, we do quick and dirty recordType guess based on the
  ## occurrence
  if(is.null(recordType))
    recordType <- names_RLum(object)|> table() |> which.max() |> names() |> unique()

  ## get index of relevant curves; these are the only record types of concern
  id_pairs <- suppressWarnings(
    get_RLum(object, recordType = recordType[1], get.index = TRUE))

    ## bet gentle if the recordType does not exist, this behaviour
    ## should make it easier in case we process a list
    if(is.null(id_pairs)) {
      .throw_warning("'recordType' setting invalid, nothing removed.")
      return(object)
    }

  if(!is.null(object_bg)) {
    ## if it is a vector recycle if required using the first curve
    if(inherits(object_bg, c("matrix", "numeric", "integer"))) {
      m_ref <- object@records[[id_pairs[1]]]
      object_bg <- as.matrix(object_bg)

      object_bg <- set_RLum(
        "RLum.Data.Curve",
        recordType = recordType,
        data = matrix(
          c(m_ref[,1],
            rep(object_bg[,max(ncol(object_bg))], length.out = nrow(m_ref))),
          ncol = 2))

    }

    ## check for list
    if(inherits(object_bg, "RLum.Data.Curve"))
      object_bg <- list(object_bg)

    ## if we have an object for bg, we take this an recycle it to the length
    object_bg <- rep(object_bg, length.out = length(id_pairs))

    ## set id signal
    id_signal <- id_bg <- id_pairs
    object_bg[id_bg] <- object_bg

  } else {
    ## create matrix with pairs; we recycle on purpose
    m_pairs <- suppressWarnings(matrix(id_pairs, ncol = 2, byrow = TRUE))

    ## calculate difference between pairs ... if 1 we have a pair,
    ## if negative it is invalid because we recycle
    m_pairs <- cbind(m_pairs, matrixStats::rowDiffs(m_pairs))

    ## get signal id and background id
    id_signal <- m_pairs[m_pairs[,3] == 1,1]
    id_bg <- m_pairs[m_pairs[,3] == 1,2]

    ## set object_bg to have it consistent
    ## this creates an empty list elements, which we will keep for the index
    object_bg[id_bg] <- object@records[id_bg]

  }

  # Subtract ----------------------------------------------------------------
  ## now we perform the background subtraction element by element using our
  ## merge function, this makes things a lot easier
  object@records[id_signal] <- lapply(seq_along(id_signal), function(x) {
    merge_RLum.Data.Curve(
      object = list(object@records[id_signal[x]][[1]],object_bg[id_bg[x]][[1]]),
      merge.method = "-")
  })

  # Return ------------------------------------------------------------------
  if (clean_up[1] && !all(id_signal == id_bg))
    object@records[id_bg] <- NULL


  ## return whatever is left
  return(object)
}

