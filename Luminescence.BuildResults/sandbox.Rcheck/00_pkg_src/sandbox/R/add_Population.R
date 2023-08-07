#' @title Add a Population to a Rule Book
#'
#' @description The function adds a further population element to all rules or a rule book.
#'
#' @param book [character] value, name of the rule book to be modified.
#' 
#' @param populations [numeric] value, number of additional populations
#' to create.
#' 
#' @return A [list] object with all rules for a model run.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @examples
#'
#' ## create simple true age-depth-relationship
#' book_1 <- get_RuleBook()
#' 
#' book_2 <- add_Population(
#'  book = book_1,
#'  populations = 1)
#'  
#' @md                            
#' @export add_Population
add_Population <- function(
  book,
  populations = 1
) {
  
# Input checks ------------------------------------------------------------
  ## check input
  if (!all(c("sandbox", "book") %in% attributes(book)[c("package", "medium")]))
    stop("[add_Population()] 'book' is not an object created by sandbox!", call. = FALSE)

# Process -----------------------------------------------------------------
  ##maintain all attributes except names
  attr_default <- attributes(book)
  
  ## remove first book entry
  book_body <- book
  book_body[[1]] <- NULL
  
  ## work through all rule definitions
  book_body <- lapply(X = book_body, FUN = function(book_body, populations) {
    
    ## check if rule is of group specific
    if (book_body$group == "specific") {
      ## extract base name of rule
      name_base <- strsplit(x = names(book_body)[2], 
                            split = "_")[[1]]
      
      name_base <- paste(name_base[-length(name_base)], 
                         collapse = "_")
      
      ## append all populations
      for (i in 1:populations) {
        ## get existing number of populations
        n <- length(book_body)
        
        ## append population
        book_body[[length(book_body) + 1]] <- book_body[[length(book_body)]]
        
        ## re-assign rule names for each population
        names(book_body)[2:(n + 1)] <- paste(name_base, 
                                             1:(n), 
                                             sep = "_")
      }      
    }
    
    ## return output
    return(book_body)
    
  }, populations)
  

  ## amalgamate book parts
  book_new <- c(book[1], book_body)
  
  ## restore attributes
  attributes(book_new) <-  attr_default
  
  ## return output
  return(book_new)

}
