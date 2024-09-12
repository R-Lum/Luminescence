#' @title Set depth-dependent rule for model parameter.
#'
#' @description The function defines how the specified model parameter varies with depth.
#' The transfer function uses different interpolation functions to create a
#' continuous representation of a parameter value with depth.
#'
#' @details To assign standard OSL model parameters, one of the available keywords of 
#' the R package [RLumModel-package] can be used. The function will then set 
#' all rules of the rule book with the standard values associated with these 
#' models, and setting the standard deviation to zero. The keyword can be 
#' one out of `"Bailey2001"`, `"Bailey2004"`, `"Pagonis2008"`, 
#' `"Pagonis2007"`, `"Bailey2002"` and `"Friedrich2017"`. 
#' This will fill the rule book with the standard parameters independent of 
#' depth. Note that a dose rate (parameter name `osl_doserate`) needs to 
#' be set separately!
#'
#' @param book [list] object, rule book to be edited.
#' 
#' @param parameter [character] scalar, parameter name to be edited. 
#'        Can also be the keyword for an OSL model. See details.
#' 
#' @param value [numeric] [list], specifying the
#'        parameter values at the corresponding depth points. If a parameter
#'        is defined by more than one argument (e.g., mean and standard
#'        deviation), all the relevant arguments must be defined for each
#'        corresponding depth as separate list element.
#' 
#' @param depth [numeric] [list], specifying the depths used for the
#'        interpolation. All elements must be of the same lengths as the
#'        corresponding data in `value`.
#' 
#' @param type [character] scalar, interpolation method. One out of
#'        `spline`, default is `spline`.
#' 
#' @return A [list] object with all created formula objects.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, Geography 
#' & Earth Sciences, Aberystwyth University (United Kingdom)
#' 
#' @examples
#'
#' ## create empty rule book
#' book_01 <- get_RuleBook()
#' 
#' ## assign rule definitions to lists
#' depth <- list(c(0, 10))
#' age <- list(c(0, 1000))
#' 
#' ## add age definition
#' book_01 <- set_Rule(
#'  book = book_01, 
#'  parameter = "age", 
#'  value = age, 
#'  depth = depth)
#'
#' @md
#' @export set_Rule
set_Rule <- function(
  book,
  parameter,
  value,
  depth,
  type = "spline"
) {
## check/adjust input parameters ---------------------------------------------------
  ## check input
  if (!all(c("sandbox", "book") %in% attributes(book)[c("package", "medium")]))
    stop("[set_Rule()] 'book' is not an object created by sandbox!", call. = FALSE)
  
  ## check data format of interpolation type
  if (!any(type[1] %in% c("spline"))) 
    warning("Interpolation method unavailable. Spline is used!", call. = FALSE)
  
## create function ----------------------------------------------------------
  ## defined keywords (the models from RLumModel)
  keywords <- c(
    "Bailey2001", 
    "Bailey2004", 
    "Pagonis2008", 
    "Pagonis2007", 
    "Bailey2002", 
    "Friedrich2017")
  
  ## option 1 - parameter name specified and not part of RLumModel
  if (!parameter[1] %in% keywords) {
    ## account for errors
    param_names <- names(book)[-1]
    if (!any(parameter[1] %in% param_names)) {
      stop(paste("[set_Rule()] Parameter name not present in rule book!\n Supported parameters are:", 
                 paste(param_names, collapse = ", ")), 
           call. = FALSE)
    }
    
    ## isolate chapter to edit
    book_edit <- book[parameter[1]]
    
    ## adjust parameter length
    if (length(book_edit[[1]][-1]) != length(value)) {
      book_edit_new <- vector("list", length(value) + 1)
      book_edit_new[[1]] <- book_edit[[1]][[1]]
      
      for (i in 2:length(book_edit_new)) 
        book_edit_new[[i]] <- book_edit[[1]][[2]]
      
      names(book_edit_new) <- c(names(book_edit[[1]])[1],
                                paste(names(book_edit),
                                      1:length(value),
                                      sep = "_"))
      
      book_edit[[1]] <- book_edit_new
    }
    
    ## infer group name
    group <- book_edit[[1]]$group
    
    ## process according to group name
    if (group == "general") {
      ## define rule for general parameter type
      
      ## spline interpolation
      if (type == "spline") {
        for (i in 1:length(value)) {
          ## update book_edit object
          book_edit[[1]][[2]][[i + 1]] <- splinefun(x = depth[[1]], y = value[[i]])
        }
      }
      
    } else if (group == "specific") {
      ## define rule for specific parameter type
      for (i in 1:length(value)) {
        for (j in 1:length(value[[i]])) {
          
          # ## update book_edit object
          # book_edit[[1]][[i + 1]][[j + 1]] <- splinefun(x = depth[[i]][[j]],
          #                                 y = value[[i]][[j]])
          
          ## update book_edit object
          
          ## CHANGED, DEPTH LIST HAS NOT SUB LIST I THINK! 2017-10-18
          book_edit[[1]][[i + 1]][[j + 1]] <- splinefun(
            x = depth[[1]], y = value[[i]][[j]])
          
        }
      }
    }
    
    ## return output ------------------------------------------------------------
      ## update input book
      book[parameter[1]] <- book_edit
      
      ## return output
      return(book)
    
  } else {
    
    ## assign keyword book
    book_key <- book
    
    ## get OSL parameters of keyword model
    osl_parameters <- RLumModel::.set_pars(model = parameter)[1:7]
    
    ## create corresponding parameter names
    osl_names <- osl_parameters
    
    for (i in 1:length(osl_names)) {
      osl_names[[i]] <- 
        paste0("osl_", rep(x = names(osl_parameters)[i], 
                          times = length(osl_parameters[[i]])),
              1:length(osl_parameters[[i]]))
    }
    
    ## convert lists to vectors
    osl_names <- c(as.character(unlist(osl_names)), "osl_R")
    osl_parameters <- c(as.numeric(unlist(osl_parameters)), 5e7)
  
    ## update rule book with parameters
    for (i in 1:length(osl_names)) {
      value_i <- lapply(X = 1:(length(book_key$population) - 1), 
                        FUN = function(x, osl_parameters, depth) {
                          
                          list(mean = rep(x = osl_parameters[i], 
                                          times = length(depth[[1]])),
                               sd = rep(x = 0, 
                                        times = length(depth[[1]])))
                        }, osl_parameters, depth)
      
      
      book_key <- set_Rule(book = book_key, 
               parameter = osl_names[i],
               value = value_i, 
               depth = depth)
    }
    
    ## return output ------------------------------------------------------------
    return(book_key)
    
  }
  
}
