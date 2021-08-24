##
set_selected <- function(x, pos = NULL, curve = NULL) {
  
  if (!is.list(x))
    stop("\n[set_selected] 'x' must be a list.", call. = FALSE)
  if (is.list(curve))
    if (length(pos) != length(curve))
      stop("\n[set_selected] 'x' and 'curve' must be of same length.", call. = FALSE)
  
  # Set everything to false
  for (i in 1:length(x)) {
    x[[i]]@info$selected <- FALSE
    for (j in 1:length(x[[i]]@records))
      x[[i]]@records[[j]]@info$selected <- FALSE
  }
  
  # Case 3: set selected curves
  if (!is.null(pos)) {
    for (i in 1:length(pos)) {
      x[[pos[i]]]@info$selected <- TRUE
      
      if (!is.null(curve)) {
        for (j in curve[[i]]) {
          if (is.na(j))
            next
          if (j == 0)
            next
          x[[pos[i]]]@records[[j]]@info$selected <- TRUE
        }
        
      } else {
        for (j in 1:length(x[[pos[i]]]@records))
          x[[pos[i]]]@records[[j]]@info$selected <- TRUE
      }
    }
  }
  
  return(x)
}

get_selected <- function(x) {
  
  # selected aliquots
  sel_al <- sapply(x, function(x) x@info$selected)
  is_null <- which(sapply(sel_al, is.null))
  
  if (length(is_null) != 0)
    sel_al[is_null] <- FALSE
  
  if (is.list(sel_al))
    sel_al <- unlist(sel_al)
  
  x <- x[sel_al]
  
  # selected curves
  for (i in 1:length(x)) {
    is_selected <- sapply(x[[i]], function(y) y@info$selected)
    x[[i]]@records <- x[[i]]@records[is_selected]
    
    if (length(x[[i]]@records) == 0)
      x[[i]] <- NULL
  }
  
  return(x)
}
