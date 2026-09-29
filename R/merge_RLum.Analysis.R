#' @rdname merge_RLum
setMethod("merge_RLum", signature(object = "list", .class = "RLum.Analysis"),
function(
  object,
  .class
) {
  .set_function_name("merge_RLum.Analysis")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  lapply(object, .validate_class, classes = c("RLum.Analysis", "RLum.Data"),
         name = "All elements of 'object'")

  ## Merge objects ----------------------------------------------------------

  ##(0) get recent environment to later set variable temp.meta.data.first
  temp.environment  <- environment()
  temp.meta.data.first <- NULL

  ##(1) collect all elements in a list
  temp.element.list <- unlist(lapply(object, function(x) {
    if (inherits(x, "RLum.Data"))
      return(x)

    ## x is an RLum.Analysis object
    ## extract meta data from the first RLum.Analysis object
    if (is.null(temp.meta.data.first)) {
      assign("temp.meta.data.first", x@protocol, envir = temp.environment)
    }

    get_RLum(x)
  }))

  ## return new RLum.Analysis object
  set_RLum(
    class = "RLum.Analysis",
    originator = "merge_RLum.Analysis",
    records = temp.element.list %||% list(),
    protocol = temp.meta.data.first,
    info = unlist(lapply(object, function(x) {
      x@info
    }), recursive = FALSE),
    .pid = unlist(lapply(object, function(x) {
      x@.uid
    }))
  )
})
