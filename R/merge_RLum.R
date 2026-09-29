#' @rdname merge_RLum
#' @export
setMethod("merge_RLum", signature = "list", function(object, ..., .class) {
  .set_function_name("merge_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  ##we are friendly and remove all empty list elements, this helps a lot if we place things
  ##we DO NOT provide a warning as this lowers the computation speed in particular cases.
  object <- .rm_NULL_elements(object)

  ## if the list is empty we do nothing
  if (length(object) < 1) {
    .throw_warning("Nothing was merged as the object list was found ",
                   "to be empty or contains only one object")
    return(NULL)
  }

  ## check if objects are of class RLum
  temp.class.test <- unique(sapply(object, function(x) {
        is(x)[1]
      }))

  ## objects must be consistent, unless there's an RLum.Analysis object
  if (length(temp.class.test) > 1 && !"RLum.Analysis" %in% temp.class.test) {
    .throw_error("Objects cannot be merged, different classes found: ",
                 .collapse(temp.class.test))
  }

  ## determine the output class
  objects.class <-
        ifelse("RLum.Analysis" %in% temp.class.test, "RLum.Analysis", temp.class.test)
  if (!startsWith(objects.class, "RLum") || objects.class == "RLum.Data.Image")
    .throw_error("Merging of '", objects.class, "' objects is not supported")

  ## select which merge method should be used based on the determined class
  merge_RLum(object, ..., .class = new(objects.class))
})
