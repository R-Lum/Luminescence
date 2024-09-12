#' @title Prepare Subsamples from a Sample Dataset
#' 
#' @description The function splits the master sample in a set of subsamples. The
#' step can be done by creating equally large subsamples in terms of 
#' contained grains (parameter `number`), by volume (parameter 
#' `volume`) or by weight (parameter `weight`).
#' 
#' @param sample [data.frame], sample object to be distributed to 
#' aliquots.
#' 
#' @param number [numeric] value, number of evenly large subsamples to 
#' be created
#' 
#' @param volume [numeric] value, volume of subsamples. Remainder 
#' of the master sample that is too small for the last subsample is 
#' removed. Volume must be given in m^3 and takes packing density of 
#' the sample into account.
#' 
#' @param weight [numeric] value, weight of the subsamples. Remainder 
#' of the master sample that is too small for the last subsample is 
#' removed. Weight is calculated based on density of each grain. Weight 
#' must be given in kg.
#' 
#' @return [list] object with grains organised as aliquots, i.e. list 
#' elements.
#' 
#' @author Michael Dietze, GFZ Postdam (Germany)
#' 
#' @examples
#' ## load example data set
#' data(sample, envir = environment())
#' 
#' ## create 10 subsamples
#' prepare_Subsample(sample, 10) 
#'
#' @md
#' @export prepare_Subsample
prepare_Subsample <- function(
  sample,
  number, 
  volume,
  weight
) {
  
# Check incoming ----------------------------------------------------------
  if (is.null(attributes(sample)$package) || attributes(sample)$package != "sandbox")
    stop("[prepare_Subsample()] the input for sample is not an object created by 'sandbox'!", 
         call. = FALSE)
  
  if (!is(sample, "data.frame"))
    stop("[prepare_Subsample()] the input for sample is not of type data.frame!", 
         call. = FALSE)  
  
  ## check if only one subsampling criterion is given
  if (sum(c(!missing(number), !missing(volume), !missing(weight))) > 1) {
    stop("[prepare_Subsample()] Only one criterion for subsampling is allowed!", 
         call. = FALSE)
  }
  
# Sub sample --------------------------------------------------------------
  ## option 1, subsampling by number of grains
  if (!missing(number)) {
    
    ## get number of grains in sample
    n_grains <- length(sample$grains)
    
    ## get number of grains per sample
    n_sub <- floor(n_grains / number[1])
    
    ## create subsampling vector
    i_sub <- seq(from = 1, to = n_grains, by = n_sub)
    
    ## create and fill slice object with grain depths  
    sample_sub <- vector(mode = "list", length = length(i_sub) - 1)
    
    for (i in 1:length(sample_sub)) 
      sample_sub[[i]] <- sample[(i_sub[i]):(i_sub[i + 1] - 1),]
    
  }
  
  ## option 2, subsampling by volume
  if (!missing(volume)) {
    ## get grain radius
    r_grains <- convert_units(phi = sample$grainsize) / (2 * 1e+06)
    
    ## get grain volumes
    v_grains <- cumsum(4 / 3 * pi * r_grains^3 * 1/sample$packing)
    
    ## get class boundaries of individual volumes
    v_subset <- c(seq(from = volume, to = max(v_grains), by = volume))
    
    ## assign grains to subsamples
    sample_sub <- lapply(X = v_subset,
                         FUN = function(x, v_grains, volume, sample) {
                           sample[v_grains > x - volume & v_grains <= x,]
                         }, v_grains, volume,sample)
  }
  
  ## option 2, subsampling by weight
  if (!missing(weight)) {
    ## get grain radius
    r_grains <- convert_units(phi = sample$grainsize) / (2 * 1e+06)
    
    ## get grain volumes
    v_grains <- 4 / 3 * pi * r_grains^3
    
    ## get grain masses
    m_grains <- cumsum(sample$density * v_grains)
    
    ## get class boundaries of individual volumes
    m_subset <- c(seq(from = weight, to = max(m_grains), by = weight))
    
    ## assign grains to subsamples
    sample_sub <- lapply(X = m_subset,
                         FUN = function(x, m_grains, weight, sample) {
                           sample[m_grains > x - weight & m_grains <= x,]
                         }, m_grains, weight, sample)
  }
  
# Create output -----------------------------------------------------------
  ## set list names
  names(sample_sub) <- paste0("subsample_", 1:length(sample_sub))
  
  ## set package attributes
  attr(sample_sub, "package") <- "sandbox"
  
  ## return output
  return(sample_sub)
}