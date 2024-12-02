#' @title Convenient View Function
#'
#' @description Invokes the [utils::View] function tailored to objects
#' in the package. If started from RStudio, it uses the RStudio view.
#'
#' @param object (**required**) object to view
#'
#' @param ... further arguments passed to the function
#'
#' @seealso [utils::View()]
#'
#' @returns `NULL` and opens the data viewer
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("view", function (object, ... ) standardGeneric("view"))

## ensure that we can use the internal RStudio view function
## https://stackoverflow.com/questions/48234850/how-to-use-r-studio-view-function-programatically-in-a-package
#' @md
#' @noRd
.view <- function(x, title) {
  get("View", envir = as.environment("package:utils"))(x, title) # nocov
}
