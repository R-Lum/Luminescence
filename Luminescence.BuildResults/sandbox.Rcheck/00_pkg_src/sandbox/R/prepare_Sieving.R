#' @title Sieve a Sample
#'
#' @description The function removes grains that are not within the provided sieve
#' interval.
#'
#' @param sample [data.frame] sample object to be sieved.
#' 
#' @param interval [numeric] vector, sieve interval, in phi units.
#' 
#' @return [data.frame] with grains that are within the sieve interval.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @examples
#' ## load example data set
#' data(sample, envir = environment())
#' 
#' ## sieve sample (in phi units)
#' sample_sieved <- prepare_Sieving(
#'   sample = sample,
#'   interval = c(5, 6))
#'                                  
#' ## plot results
#' plot(density(
#'   x = sample$grainsize, 
#'   from = -1, 
#'   to = 11))
#' lines(density(
#'   x = sample_sieved$grainsize, 
#'   from = -1, 
#'   to = 11), 
#'   col = 2)
#' 
#' @md
#' @export
prepare_Sieving <- function(
  sample,
  interval
) {
  
# Check incoming ----------------------------------------------------------
  if (is.null(attributes(sample)$package) || attributes(sample)$package != "sandbox")
    stop("[prepare_Sieving()] the input for sample is not an object created by 'sandbox'!", 
         call. = FALSE)
  
  if (!is(sample, "data.frame"))
    stop("[prepare_Sieving()] the input for sample is not of type data.frame!", 
         call. = FALSE)  
  
  ## check/adjust input parameters --------------------------------------------
  ## check data format and structure
  if (!is(interval,"numeric") | length(interval) != 2) {
    warning("Parameter interval must be numeric of length two, full interval taken!")
    interval <- range(sample$grainsize, na.rm = TRUE)
  }
  
  ## sieve sample -------------------------------------------------------------
  sample <- sample[sample$grainsize > min(interval) & sample$grainsize <= max(interval),]
  
  ## return output ------------------------------------------------------------
  return(sample)

}
