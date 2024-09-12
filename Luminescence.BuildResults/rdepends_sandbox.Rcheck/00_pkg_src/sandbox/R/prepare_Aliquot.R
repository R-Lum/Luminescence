#' @title Prepare Aliquots from Sample Dataset
#' 
#' @description The function consecutively fills aliquots (i.e., subsamples distributed on 
#' round carrier discs) with grains from an input sample. Remaining grains that 
#' are not enough to fill a further aliquot are discarded.
#' 
#' @param sample [data.frame], sample object to be distributed to 
#' aliquots.
#' 
#' @param diameter [numeric] value, diameter of the aliquot sample 
#' carriers in mm.
#' 
#' @param density [numeric] value, packing density of the grains on
#' the sample carrier. Default is `0.65`. The packing density is unitless.
#' 
#' @return [list] of [data.frame] objects with grains organised as aliquots, i.e. list 
#' elements.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany), 
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#' 
#' @examples
#' ## load example data set
#' data(sample, envir = environment())
#' 
#' A <- prepare_Aliquot(
#'  sample = sample, 
#'  diameter = 0.1)
#' 
#' B <- prepare_Aliquot(
#'  sample = sample, 
#'  diameter = 1, 
#'  density = 0.6)
#'  
#' @md
#' @export prepare_Aliquot
prepare_Aliquot <- function(
  sample,
  diameter, # mm
  density = 0.65
) {
  
# Check incoming ----------------------------------------------------------
  if (is.null(attributes(sample)$package) || attributes(sample)$package != "sandbox")
    stop("[prepare_Aliquot()] the input for sample is not an object created by 'sandbox'!", 
         call. = FALSE)
  
  if (!is(sample, "data.frame"))
    stop("[prepare_Aliquot()] the input for sample is not of type data.frame!", 
         call. = FALSE)

# Prepare aliquots ----------------------------------------------------------
  ## reminder: the code below works with the areas of grains and aliquots

  ## calculate area of an aliquot in m 
  area_aliqout <- pi * (diameter[1] / 1000)^2 / 4

  ## calculate diameter grains and convert to m (instead of µm)
  diameter_grains <- convert_units(phi = sample[["grainsize"]]) / 1e+06
  
  ## calculate the area of each spherical grain (2D) normalised to the  
  ## selected packing density ... this gives basically the number of 
  ## available grains
  area_grains <- pi * diameter_grains^2 / 4 / density[1]
  
  ## get the cumulative area of the grains 
  area_grains_cum <- cumsum(area_grains)
  
  ## check whether settings make sense
  if (max(area_grains_cum) <= area_aliqout)
    stop("[prepare_Aliquot()] chosen aliquot diameter too large; exceeding area sum of all grains!", 
         call. = FALSE)
  
  ## determine grain area **limits** each aliquot can eat-up given the area of
  ## one aliquot and the total available grain surface area
  ## (one aliquot contains multiple grains)
  ## note the number of aliquots is this length -1 
  aliquot_n <- seq(
    from = 0, 
    to = max(area_grains_cum), 
    by = area_aliqout)

  ## create an index vector for the grain areas
  aliquot_i <- seq(from = 1, to = length(area_grains)) 
  
  aliquot_cut <- vapply(aliquot_n, function(a) {
    aliquot_i[which.min(abs(area_grains_cum - a))]
  }, numeric(1))

  ## create list vector of aliquots
  aliquots <- lapply(2:length(aliquot_n), function(i) {
    sample[aliquot_cut[i - 1]:aliquot_cut[i], ]
  })
  
# Return ------------------------------------------------------------------
  ## set list names
  names(aliquots) <- paste0("aliquot_", 1:length(aliquots))
  
  ## set package attributes
  attr(aliquots, "package") <- "sandbox"
  
  return(aliquots)
}
