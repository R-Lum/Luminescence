#' @title Add a Rule to a Rule Book
#'
#' @description The function adds a new rule to an existing rule book. The specified rule 
#' will be appended to the rule book.
#'
#' @param book [character] value, name of the rule book to be modified.
#' 
#' @param name [character] value, name of the rule to be added.
#' 
#' @param group [character] value, group to which the rule belongs. One 
#' out of `"general"` (covering the sediment section properties) and 
#' `"specific"` (relevant for a single grain).
#' 
#' @param type [character] value, generic type of the rule. One out of 
#' `"exact"` (defined by exact value, changing with depth), 
#' `"normal"` (normal distribution, defined by mean and standard 
#' deviation, changing with depth), `"uniform"` (defined by minimum and 
#' maximum values, changing with depth) and `"gamma"` (gamma distribution, 
#' defined by shape and scale parameter and constant offset, all changing 
#' with depth)
#' 
#' @param populations [numeric] value, number of populations to create.
#' The number of populations to add should match the existing number of 
#' populations.
#' 
#' @return A [list] object with all rules for a model run.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, Geography 
#' & Earth Sciences, Aberystwyth University (United Kingdom)
#' 
#' @examples
#'
#' ## create simple true age-depth-relationship
#' book_1 <- get_RuleBook()
#' 
#' book_2 <- add_Rule(
#'  book = book_1, 
#'  name = "extrarule", 
#'  group = "general", 
#'  type = "normal", 
#'  populations = 1)
#'                 
#' @md                           
#' @export add_Rule
add_Rule <- function(
  book,
  name,
  group,
  type,
  populations = 1
) {
  
# Input checks ------------------------------------------------------------
  ## check input
  if (!all(c("sandbox", "book") %in% attributes(book)[c("package", "medium")]))
    stop("[add_Rule()] 'book' is not an object created by sandbox!", call. = FALSE)
  
# Set dummy function ------------------------------------------------------
  ## define dummy function/closure
  fun_dummy <- splinefun(x = c(0, 1), y = c(0, 1))
  
# Add rule ----------------------------------------------------------------
  ##maintain all attributes except names
  attr_default <- attributes(book)[names(attributes(book)) != "names"]
  
  ## define rule to add
  if (type == "exact") {
    rule_add = list(group = group)
    
    for (i in 1:populations) {
      rule_add[[length(rule_add) + 1]] <- list(
        type = "exact", value = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }

  } else if (type == "normal") {
      rule_add = list(
      group = group)
    for (i in 1:populations) {
      rule_add[[length(rule_add) + 1]] <- list(
        type = "normal", mean = fun_dummy, sd = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }
  } else if (type == "uniform") {
      rule_add = list(group = group)
    
    for (i in 1:populations) {
      rule_add[[length(rule_add) + 1]] <- list(type = "uniform",
                                               min = fun_dummy,
                                               max = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }
  } else if (type == "gamma") {
      rule_add = list(
      group = group)
    
    for (i in 1:populations) {
      rule_add[[length(rule_add) + 1]] <- list(
        type = "gamma",
        shape = fun_dummy,
        scale = fun_dummy,
        offset = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }
  }
  
  ## append new rule
  book[[length(book) + 1]] <- rule_add

  ## remove first book entry
  book_body <- book
  book_body[[1]] <- NULL
  
  ## add name of new rule
  names(book_body)[length(book_body)] <- name

  ## extract book types in raw format  
  book_groups <- lapply(X = book_body, FUN = function(book_body) {
    book_body$group
  })
  
  ## assign book types
  book_groups <- as.character(book_groups)
  
  ## get index of groups
  book_index <- match(x = book_groups, 
                      table = c("general", 
                                "specific"))
  
  ## amalgamate book parts
  book_new <- c(book[[1]], 
                book_body[book_index == 1], 
                book_body[book_index == 2])
  


# Return ------------------------------------------------------------------
  ## set names and restore original attributes
  names(book_new) <- c(
    "book", names(book_body[book_index == 1]), names(book_body[book_index == 2]))
  attributes(book_new) <- c(attributes(book_new), attr_default)
  
  ## return output
  return(book_new)
}

