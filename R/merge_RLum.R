#' @title General merge function for RLum S4 class objects
#'
#' @description
#' Function calls object-specific merge functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for merging specific
#' [RLum-class] objects. Depending on the input object, the
#' corresponding merge function will be selected.  Allowed arguments can be
#' found in the documentation of each merge function.
#' Empty list elements (`NULL`) are automatically removed from the input `list`.
#'
#' \tabular{lll}{
#' **object** \tab \tab **corresponding merge function** \cr
#' [RLum.Data.Curve-class] \tab -> \tab [merge_RLum.Data.Curve] \cr
#' [RLum.Data.Spectrum-class] \tab -> \tab [merge_RLum.Data.Spectrum] \cr
#' [RLum.Analysis-class] \tab -> \tab [merge_RLum.Analysis] \cr
#' [RLum.Results-class] \tab -> \tab [merge_RLum.Results]
#' }
#'
#' @param objects [list] of [RLum-class] (**required**):
#' list of S4 object of class `RLum`
#'
#' @param ... further arguments that one might want to pass to the specific merge function
#'
#' @return Return is the same as input objects as provided in the list.
#'
#' @note So far not for every `RLum` object a merging function exists.
#'
#' @section Function version: 0.1.3
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#'
#' @keywords utilities
#'
#' @examples
#'
#'
#' ##Example based using data and from the calc_CentralDose() function
#'
#' ##load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ##apply the central dose model 1st time
#' temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)
#'
#' ##apply the central dose model 2nd time
#' temp2 <- calc_CentralDose(ExampleData.DeValues$CA1)
#'
#' ##merge the results and store them in a new object
#' temp.merged <- get_RLum(merge_RLum(objects = list(temp1, temp2)))
#'
#' @export
merge_RLum<- function(
  objects,
  ...
) {
  .set_function_name("merge_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests --------------------------------------------------------
  .validate_class(objects, "list")

    ##we are friendly and remove all empty list elements, this helps a lot if we place things
    ##we DO NOT provide a warning as this lowers the computation speed in particular cases.
    objects <- .rm_NULL_elements(objects)

  ##if list is empty afterwards we do nothing
  if(length(objects) >= 1) {
      ##check if objects are of class RLum
     temp.class.test <- unique(sapply(objects, function(x) {
       .validate_class(x, "RLum",
                       name = "All elements of 'objects'")
        is(x)[1]
      }))

      ##check if objects are consistent
      if (length(temp.class.test) > 1) {
        ##This is not valid for RLum.Analysis objects
        if (!"RLum.Analysis" %in% temp.class.test) {
          .throw_error("Only similar input objects in the list are supported")
        }
      }

      ##grep object class
      objects.class <-
        ifelse("RLum.Analysis" %in% temp.class.test, "RLum.Analysis", temp.class.test)

      ##select which merge function should be used
      switch (
        objects.class,
        RLum.Data.Image = .throw_error("Merging of 'RLum.Data.Image' objects is currently not supported"),
        RLum.Data.Spectrum = merge_RLum.Data.Spectrum(objects, ...),
        RLum.Data.Curve = merge_RLum.Data.Curve(objects, ...),
        RLum.Analysis = merge_RLum.Analysis(objects, ...),
        RLum.Results = merge_RLum.Results(objects, ...)
      )

    }else{
      .throw_warning("Nothing was merged as the object list was found ",
                     "to be empty or contains only one object")
      return(NULL)
    }
}
