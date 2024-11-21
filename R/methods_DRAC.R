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
print.DRAC.highlights <- function(x, ...) {
  x <- as.list(x)
  names <- names(x)
  mapply(function(el, name) {
    cat(paste0(attributes(el)$key, " = ", name,":\n  ", paste(el, collapse = ",\n  "), "\n"))
    }, x, names)
}

#' @export
print.DRAC.list <- function(x, blueprint = FALSE, ...) {

  ## CASE 1: Pretty print the structure of the DRAC list
  if (!blueprint) {
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
                   "\t VALUES =", .collapse(x[[i]], quote = FALSE), "\n",
                   "\t ALLOWS 'X' = ", attributes(x[[i]])$allowsX, "\n",
                   "\t REQUIRED =", attributes(x[[i]])$required, "\n",
                   "\t DESCRIPTION = ", blockString, "\n"
      )
      if (!is.null(levels(x[[i]]))) {
        msg <- paste(msg,
                     "\t OPTIONS = ", .collapse(levels(x[[i]]), quote = FALSE),
                     "\n\n")
      } else {
        msg <- paste(msg, "\n")
      }
      cat(msg)
    }
  }

  ## CASE 2: Return a 'blueprint' that can be copied from the console to a
  ## script so the user does not need to write down all >50 fields by hand
  if (blueprint) {
    var <- as.list(sys.call())[[2]]
    names <- names(x)

    for (i in 1:length(x)) {

      # in case of factors also show available levels as comments so you don't
      # have to look it up
      if (is.factor(x[[i]]))
        options <- paste("# OPTIONS:", .collapse(levels(x[[i]]), quote = FALSE))
      else
        options <- ""

      ## decide if values should be quoted
      use.quotes <- is.character(x[[i]]) || is.factor(x[[i]])
      cat(paste0(var, "$`", names[i], "` <- c(",
                 .collapse(x[[i]], quote = use.quotes), ") ", options , "\n"))
    }
    message("\n\t You can copy all lines above to your script and fill in the data.")
  }
}


## ---------------------------------------------------------------------------##
## DOUBLE SQUARE BRACKETS METHOD

#' @export
`[[<-.DRAC.list` <- function(x, i, value) {
  .set_function_name("[[<-.DRAC.list]]")
  on.exit(.unset_function_name(), add = TRUE)

  ## REJECT ALL INADEQUATE CLASSES ----
  acceptedClasses <- c("integer", "character", "numeric", "factor")
  if (!.validate_class(value, acceptedClasses,
                       throw.error = FALSE)) {
    return(x)
  }

  ## CHECK INPUT LENGTH ----
  if (!.validate_length(value, length(x[[i]]), throw.error = FALSE,
                        name = names(x)[i])) {
    return(x)
  }

  ## CHECK INPUT CLASS ----
  class.old <- attr(x[[i]], "default_class")

  class.new <- class(value)

  ## CHECK INPUT FIELDS THAT ALLOW 'X' -----
  # the following checks apply to fields that are normally numeric, but also
  # accept 'X' as input. this EXCLUDES factors!
  if (class.old != "factor") {
    # some input fields allow 'X' as input, so in terms of R can be of class
    # "character" or "numeric/integer". hence, we check if input is "X" and
    # if the field allows it. If so, we change the old class to "character".
    if (any(value == "X") && attributes(x[[i]])$allowsX) {
      if (anyNA(as.numeric(value[which(value != "X")]))) {
        .throw_warning("Cannot coerce '", value[which(value != "X")],
                       "' to a numeric value, input must be numeric or 'X'")
        return(x)
      }
      class.old <- "character"
    }

    # where the input field is already "X" we have to check whether the new
    # non-character input is allowed
    if (!all(is.na(x[[i]]))) {
      if (any(x[[i]] == "X") && attributes(x[[i]])$allowsX) {
        if (anyNA(as.numeric(value[which(value != "X")]))) {
          .throw_warning("Cannot coerce '", value[which(value != "X")],
                         "' to a numeric value, input must be numeric or 'X'\n")
          return(x)
        }
        class.new <- "character"
        value <- as.character(value)
      }
    }
  }

  # numeric input can be both of class 'integer' or 'numeric'. We will
  # allow any combination and reject only non-numeric/integer input
  if (class.old == "numeric" || class.old == "integer") {
    if (class.new != class.old) {
      ## check if coercion is possible
      if (anyNA(suppressWarnings(as.numeric(value)))) {
        .throw_warning(names(x)[i], ": found ", class.new, ", expected ", class.old, " -> cannot coerce, set NAs")
         if(class.old == "integer")
           value <- NA_integer_
         else
           value <- NA_real_

      } else {
      ## try coercion
      .throw_warning(names(x)[i], ": found ", class.new, ", expected ", class.old, " -> coercing to ", class.old)
        if(class.old == "integer")
          value <- as.integer(value)
        else
          value <- as.numeric(value)
      }
    }
  }

  # for 'factor' and 'character' elements only 'character' input is allowed
  if (class.old == "factor" || class.old == "character") {
    if (class.new != "character") {
      .throw_warning(names(x)[i], ": Input must be of class 'character'")
      return(x)
    }
  }

  ## CHECK IF VALID OPTION ----
  # in case of 'factor's the user is only allowed input that matches one of
  # the options specified by the factor levels. if it is a valid option,
  # the input is converted to a factor to keep the information.
  if (class.old == "factor") {
    levels <- levels(x[[i]])
    if (any(`%in%`(value, levels) == FALSE)) {
      .throw_warning(names(x)[i], ": Invalid option, valid options are: ",
                     .collapse(levels))
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
