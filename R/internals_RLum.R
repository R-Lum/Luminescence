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
#' @param \code{\linkS4class{RLum.Analysis}} (\bold{required}): input object where the function
#' should be applied on
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
#' @param expr \code{\link{expression}} (\bold{required}): the R expression, usually a
#' function
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
#' @param x \code{\link{numeric}} (\bold{required}): the object for which the smoothing should be
#' applied.
#'
#' @param k \code{\link{integer}} (with default): window for the rolling mean; must be odd for rollmedian.
#' If nothing is set k is set automatically
#'
#' @param fill \code{\link{numeric}} (with default): a vector defining the left and the right hand data
#'
#' @param align \code{\link{character}} (with default): specifying whether the index of the result should be
#' left- or right-aligned or centered (default) compared to the rolling window of observations, allowed
#' \code{"right"}, \code{"center"} and \code{left}
#'
#' @param method \code{\link{method}} (with default): defines which method should be applied for the
#' smoothing: \code{"mean"} or \code{"median"}
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
#' v <- 1:100
#' .smoothing(v)
#'
#' @noRd
.smoothing <- function(
  x,
  k = NULL,
  fill = NA,
  align = "right",
  method = "mean") {

  ##set k
  if (is.null(k)) k <- ceiling(length(x) / 100)
    if(method == "median" && k %%2 !=0) k <- k + 1

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
#' http://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot
#'
#' @param l \code{\link{numeric}} (\bold{required}): a numeric vector, i.e. the
#' labels that you want to add to your plot
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
