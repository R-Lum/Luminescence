####################################################################################################
##                     INTERNAL HELPER FUNCTIONS                                                  ##
####################################################################################################

#+++++++++++++++++++++
#+ .set_pid()        +
#+++++++++++++++++++++

#' Set unique id of the RLum.Analysis object as parent id for each RLum.Data object in the record list
#'
#' This function only applies on RLum.Analysis objects and was written for performance not
#' usability, means the functions runs without any checks and is for internal usage only.
#'
#' @param [RLum.Analysis-class] (**required**):
#' input object where the function should be applied on
#'
#' @return
#' Returns the same object as the input
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @examples
#'
#' ##example using self created data
#' object <- set_RLum(
#' "RLum.Analysis",
#' records = list(
#'  set_RLum("RLum.Data.Curve"),
#'  set_RLum("RLum.Data.Curve")))
#'
#' object <- .set_pid(object)
#'
#' @md
#' @noRd
.set_pid <- function(object){

  object@records <-
    lapply(object@records, function(x) {
      x@.pid  <- object@.uid
      return(x)
    })

  return(object)
}

#+++++++++++++++++++++
#+ .warningCatcher()        +
#+++++++++++++++++++++

#' Catches warning returned by a function and merges them.
#' The original return of the function is returned. This function is in particular
#' helpful if a function returns a lot of warnings with the same content.
#'
#' @param expr [expression] (**required**):
#' the R expression, usually a function
#'
#' @return
#' Returns the same object as the input and a warning table
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @examples
#'
#' f <- function() {
#'  warning("warning 1")
#'  warning("warning 1")
#'  warning("warnigs 2")
#'  1:10
#' }
#' print(.warningCatcher(f()))
#'
#' @md
#' @noRd
.warningCatcher <- function(expr) {
  ##set variables
  warning_collector <- list()
  env <-  environment()

  ##run function and catch warnings
  results <- withCallingHandlers(
    expr = expr,
    warning = function(c) {
      assign(x = "warning_collector",
             value = c,
             envir = env)
      invokeRestart("muffleWarning")
    }
  )

  ##set new warning messages with merged results
  if (length(warning_collector) > 0) {
    w_table <- table(as.character(unlist(warning_collector)))
    w_table_names <- names(w_table)

    for (w in 1:length(w_table)) {
      warning(paste(
        w_table_names[w],
        "This warning occurred",
        w_table[w],
        "times!"
      ),
      call. = FALSE)

    }

  }
  return(results)

}

#+++++++++++++++++++++
#+ .smoothing()      +
#+++++++++++++++++++++

#' Allows smmoothing of data based on the function zoo::rollmean
#'
#' The function just allows a direct and meaningfull access to the functionality of the zoo::rollmean()
#' function. Arguments of the function are only partly valid.
#'
#' @param x [numeric] (**required**):
#' the object for which the smoothing should be applied.
#'
#' @param k [integer] (*with default*):
#' window for the rolling mean; must be odd for rollmedian.
#' If nothing is set k is set automatically
#'
#' @param fill [numeric] (*with default*):
#' a vector defining the left and the right hand data
#'
#' @param align [character] (*with default*):
#' specifying whether the index of the result should be
#' left- or right-aligned or centered (default) compared to the rolling window of observations,
#' allowed `"right"`, `"center"` and `left`
#'
#' @param method [method] (*with default*):
#' defines which method should be applied for the smoothing: `"mean"` or `"median"`
#'
#' @return
#' Returns the same object as the input and a warning table
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @examples
#'
#' v <- 1:100
#' .smoothing(v)
#'
#' @md
#' @noRd
.smoothing <- function(
  x,
  k = NULL,
  fill = NA,
  align = "right",
  method = "mean") {

  ##set k
  if (is.null(k)){
   k <- ceiling(length(x) / 100)
   if(method == "median" && k %%2 ==0)
     k <- k + 1
  }

  ##smooth data
  if(method == "mean"){
    zoo::rollmean(x, k = k, fill = fill, align = align)

  }else if(method == "median"){
    zoo::rollmedian(x, k = k, fill = fill, align = align)

  }else{
    stop("[Luminescence:::.smoothing()] Unvalid input for 'method'!")

  }

}


#++++++++++++++++++++++++++++++
#+ Scientific axis annotation +
#++++++++++++++++++++++++++++++

#' Bored of the 1e10 notation of large numbers in R? Already tried to force
#' R to produce more fancy labels? Worry not, fancy_scientific() (written by
#' Jack Aidley) is at your help!
#'
#' Source:
#' [http://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot]()
#'
#' @param l [numeric] (**required**):
#' a numeric vector, i.e. the labels that you want to add to your plot
#'
#' @return
#' Returns an expression
#'
#' @section Function version: 0.1.0
#'
#' @author Jack Aidley
#'
#' @examples
#'
#' plot(seq(1e10, 1e20, length.out = 10),
#'      1:10,
#'      xaxt = "n")
#'
#' axis(1, at = axTicks(1),
#'      labels = fancy_scientific(axTicks(1)))
#'
#' @md
#' @noRd
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # remove plus sign
  l <- gsub("\\+", "", l)
  # return this as an expression
  parse(text=l)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Statistical Summary for Plot functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Create Statistical Summary Character Vector for Plot functions
#'
#' This function automatically generates the statistical summary for the plot functions within
#' the package. This should unify the approach how such things are created and support, theoretically
#' all keywords for all plot functions in a similar way.
#'
#'@param x [data.frame] (optional): output from the function `calc_Statistics()`. If nothing is
#'provided a list of prefix keyword combinations supported by the function `calc_Statistics()` is returned.
#'
#'@param keywords[character] (with default): keywords supported by the function `calc_Statistics()`
#'
#'@param digits [numeric] (with default): modifiy the digits independently for the plot output
#'
#'@param sep [character] (with default): a separator used for the creation of the output of the plot
#'
#'@param prefix [character] (with default): allows to add a leading prefix to the string
#'
#'@param suffix [character] (with default): allows to add a suffix to the entire string
#'
#'@author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#'@section Version: 0.1.0
#'
#'
#'@md
#'@noRd
.create_StatisticalSummaryText <- function(
  x = NULL, #insert the output of calc_Statistics
  keywords = NULL,
  digits = 2, #allow for different digts
  sep = " \n ",
  prefix = "",
  suffix = ""
){


  # Grep keyword information --------------------------------------------------------------------
  if(is.null(x)){
    summary <- calc_Statistics(data.frame(x = 1:10, y = 1:10))

  }else{
    summary <- x

  }

  #all allowed combinations
  keywords_allowed <- unlist(lapply(names(summary), function(x){
    paste0(x, "$", names(summary[[x]]))

  }))

  ##return if for x == NULL
  if(is.null(x))
    return(keywords_allowed)

  # Create call ---------------------------------------------------------------------------------
  #create list
  l <- lapply(keywords, function(k){
    ##strip keyword if necessary
    if(grepl(pattern = "$", x = k, fixed = TRUE)[1]){
      strip <- strsplit(k, split = "$", fixed = TRUE)[[1]]
      keywords_prefix <- strip[1]
      k_strip <- strip[2]
    }else{
      keywords_prefix <- "unweighted"
      k_strip <- k

    }

    ##construct string
    if(!is.null(summary[[keywords_prefix]][[k_strip]])){
      if(keywords_prefix == "unweighted"){
        paste0(k_strip, " = ", round(summary[[keywords_prefix]][[k_strip]], digits))

      }else{
        paste0(k, " = ", round(summary[[keywords_prefix]][[k_strip]], digits))

      }

    }else{
      return(NULL)

    }

  })

  ##remove NULL entries
  l <- l[!sapply(l, is.null)]

  ##construct final call
  return(paste0(prefix, paste(unlist(l), collapse = sep), suffix))

}

#++++++++++++++++++++++++++++++
#+ Unlist RLum               +
#++++++++++++++++++++++++++++++
#'
#' Recursive unlisting of lists until the first element in the list
#' is something, but not a list. This funktion helps
#' to get rid of nested lists. The function stops running if a single
#' level list is reached.
#'
#' @param x [list] (**required**): list with lists
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @example
#' a <- list(b = list(c = list("test")))
#' .unlist_RLum(a)
#'
#' @return [list] with only one level left
#' @md
#' @noRd
.unlist_RLum <- function(x){
  stopifnot(class(x) == "list")

  if(length(x) > 0 && class(x[[1]]) == "list"){
    x <- unlist(x, recursive = FALSE)
    .unlist_RLum(x)
  }else{
    return(x)

  }

}


