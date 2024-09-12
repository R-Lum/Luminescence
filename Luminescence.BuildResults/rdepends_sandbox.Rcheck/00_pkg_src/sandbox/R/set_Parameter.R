#' @title Set Profile- and Grain-Specific Model Parameters.
#'
#' @description The function defines one model parameter used to generate a set of
#' virtual grains. A parameter is defined in a probabilistic way, as parametric
#' distribution function. Each parameter of the distribution function can be
#' changed through time using [set_Rule].
#'
#' @details The following parameter types are available:
#' * `exact`: parameter does not vary at all. No additional 
#' parameters needed except for vector `value`, defining the 
#' constant values for corresponding depths.
#' * `uniform`: parameter varies following a uniform distribution. 
#' The following additional parameter vectors are required: `min` 
#' (minimum) and `max` (maximum)
#' * `normal`: parameter varies following a normal distribution,
#' which is defined by mean and standard deviation
#' * `gamma`: parameter varies following a gamma distribution, 
#' defined by shape parameter, scale parameter) 
#' and offset (defining constant offset of values)
#'
#' @param book [list] object, rule book to be edited.
#' 
#' @param parameter [character] scalar, keyword defining the parameter to 
#'        be defined. Some
#'        parameters can be described by more than one function, see details.
#'        
#' @param type [character] scalar, keyword defining the distribution
#'        function used to describe the parameter. See details for available
#'        keywords, default is `"exact"`.
#'        
#' @return A [list] object.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @examples
#' ## get empty rule book
#' book_1 <- get_RuleBook(book = "empty")
#'
#' ## set density from default "normal" to "exact"
#' book_2 <- set_Parameter(book = book_1,
#'                         parameter = "density",
#'                         type = "exact")
#'
#' book_1$density$density_1$type
#' book_2$density$density_1$type
#'
#' @md
#' @export set_Parameter
set_Parameter <- function(
  book,
  parameter,
  type
) {

  ## check input parameters ---------------------------------------------------
  ## needs to be programmed, fist. Should be a check if both keywords are
  ## listed in the two parameter lists.
  output_flag <- TRUE

  ## create parameter definition ----------------------------------------------

  ## define dummy function/closure
  fun_dummy <- splinefun(x = c(0, 1),
                        y = c(0, 1))

  ## constant definition
  if (type == "exact") {
    parameter_out <- list(type = "exact",
                          value = fun_dummy)
  }

  ## rnorm definition
  if (type == "normal") {
    parameter_out <- list(type = "normal",
                          mean = fun_dummy,
                          sd = fun_dummy)
  }
  
  ## runif definition
  if (type == "uniform") {
    parameter_out <- list(type = "uniform",
                          min = fun_dummy,
                          max = fun_dummy)
  }
  
  ## runif definition
  if (type == "gamma") {
    parameter_out <- list(type = "gamma",
                          shape = fun_dummy,
                          scale = fun_dummy,
                          offset = fun_dummy)
  }
  
  ## extract book content
  book_content <- names(book)

  ## isolate chapter to edit
  book_edit <- book[book_content == parameter]

  ## set parameters
  for (i in 2:length(book_edit[[1]][[2]])) {
    book_edit[[1]][[i]] <- parameter_out
  }

  ## update input book
  book[book_content == parameter] <- book_edit

  ## return output ------------------------------------------------------------
  if (output_flag) 
    return(book)
  
}
