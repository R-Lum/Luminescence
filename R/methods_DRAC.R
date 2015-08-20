##################################################################################
##                      METHODS FOR S3 GENERICS                                 ##
##################################################################################
 
## ---------------------------------------------------------------------------##
## DATA FRAME COERCION METHOD

## This is a method for the as.data.frame S3 generic. We need this to intercept the
## DRAC list object after it hast passed the actual list-method. After it was 
## coerced to a data.frame we assign new column names (DRAC ID keys) and 
## make sure that all columns are either of class 'character' or 'numeric'.
## Finally, we attach a further class name to identify it as a valid DRAC object 
## when passed to use_DRAC

#' @export
as.data.frame.DRAC.list <- function(x, row.names = NULL, optional = FALSE, ...) {
  DF <- as.data.frame.list(x)
  colnames(DF) <- paste0("TI:", 1:ncol(DF))
  for (i in 1:ncol(DF)) {
    if (is.factor(DF[ ,i])) 
      DF[ ,i] <- as.character(DF[, i])
  }
  class(DF) <- c("data.frame", "DRAC.data.frame")
  return(DF)
}


## ---------------------------------------------------------------------------##
## PRINT METHOD

#' @export
print.DRAC.list <- function(x, ...) {
  
  limit <- 80
  
  for (i in 1:length(x)) {
    
    # for pretty printing we insert newlines and tabs at specified lengths
    ls <- attributes(x[[i]])$description
    ls.n <- nchar(ls)
    ls.block <- floor(ls.n / limit)
    strStarts <- seq(0, ls.n, limit)
    strEnds <- seq(limit-1, ls.n + limit, limit)
    blockString <- paste(mapply(function(start, end) { 
      trimmedString <- paste(substr(ls, start, end), "\n\t\t\t")
      if (substr(trimmedString, 1, 1) == " ")
        trimmedString <- gsub("^[ ]*", "", trimmedString)
      return(trimmedString)
    }, strStarts, strEnds), collapse="")
    
    msg <- paste(attributes(x[[i]])$key, "=>",names(x)[i], "\n",
                 "\t VALUES =", paste(x[[i]], collapse = ", "), "\n",
                 "\t ALLOWS 'X' = ", attributes(x[[i]])$allowsX, "\n",
                 "\t REQUIRED =", attributes(x[[i]])$required, "\n",
                 "\t DESCRIPTION = ", blockString, "\n"
    )
    
    if (!is.null(levels(x[[i]]))) {
      msg <- paste(msg,
                   "\t OPTIONS = ", paste(levels(x[[i]]), collapse = ", "),
                   "\n\n")
    } else {
      msg <- paste(msg, "\n")
    }
    
    cat(msg)
  }
}


## ---------------------------------------------------------------------------##
## DOUBLE SQUARE BRACKETS METHOD

#' @export
`[[<-.DRAC.list` <- function(x, i, value) {
  
  ## CHECK INPUT LENGTH ----
  length.old <- length(x[[i]])
  length.new <- length(value)
  
  if (length.old != length.new) {
    warning(paste(names(x)[i], ": Input must be of length", length.old), 
            call. = FALSE)
    return(x)
  }
  
  ## CHECK INPUT CLASS ----
  class.old <- class(x[[i]])
  class.new <- class(value)
  
  # some input fields allow 'X' as input, so in terms of R can be of class
  # "character" or "numeric/integer". hence, we check if input is "X" and 
  # if the filed allows it. If so, we change the old class to "character".
  if (any(value == "X") && attributes(x[[i]])$allowsX) {
    class.old <- "character"
  }
  # where the input field is alreay "X" we have to check whether the new
  # non-character input is allowed
  if (any(x[[i]] == "X") && attributes(x[[i]])$allowsX) {
    class.new <- "character"
    value <- as.character(value)
  }
  
  # numeric input can be both of class 'integer' or 'numeric'. We will
  # allow any combination and reject only non-numeric/integer input
  if (class.old == "numeric" || class.old == "integer") {
    if (class.new != "numeric" && class.new != "integer") {
      warning(paste(names(x)[i], ": Input must be of class", class.old),
              call. = FALSE)
    }
  }
  
  # for 'factor' and 'character' elements only 'character' input is allowed 
  if (class.old == "factor" || class.old == "character") {
    if (class.new != "character") {
      warning(paste(names(x)[i], ": Input must be of class", "character"),
              call. = FALSE)
      return(x)
    }
  }
  
  ## CHECK IF VALID OPTION ----
  # in case of 'factor's the user is only allowed input that matches one of 
  # the options specified by the factor levels. if it is a valid option,
  # the input is converted to a factor to keep the information.
  if (class.old == "factor") {
    levels <- levels(x[[i]])
    if (any(`%in%`(value, levels)) == FALSE) {
      warning(paste(names(x)[i], ": Invalid option. Valid options are:", paste(levels, collapse = ", ")),
              call. = FALSE)
      return(x)
    } else {
      value <- factor(value, levels)
    }
  }
  
  ## WRITE NEW VALUES ----
  # we strip our custom class and the attributes, pass the object to the default generic and 
  # finally re-attach our class and attributes
  tmp.attributes <- attributes(x[[i]])[names(attributes(x[[i]])) != "class"]
  class(x) <- "list"
  x <- `[[<-`(x, i, value)
  attributes(x[[i]]) <- tmp.attributes
  if (class.old == "factor")
    class(x[[i]]) <- "factor"
  class(x) <- c("DRAC.list", "list")
  return(x)
}

## ---------------------------------------------------------------------------##
## SINGLE SQUARE BRACKET METHOD

#' @export
`[<-.DRAC.list` <- function(x, i, value) {
  return(`[[<-`(x, i, value))
}

## ---------------------------------------------------------------------------##
## DOLLAR SIGN METHOD

#' @export
`$<-.DRAC.list`<- function(x, name, value) {
  # this is straightforward; retrieve the index and pass the object
  # to the custom [[<- function, which does the data verification
  index <- which(names(x) == name)
  x[[index]] <- value
  return(x)
}