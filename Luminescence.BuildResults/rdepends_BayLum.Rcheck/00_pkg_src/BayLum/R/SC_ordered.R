#' @title Create Stratigraphically Ordered Sample Matrix
#'
#' @description Construct the stratigraphic matrix used in the functions [AgeS_Computation] and [AgeC14_Computation]
#' for samples that are all ordered by increasing age.
#'
#' @param Nb_sample [integer] (**required**): the number of samples; alternatively
#' an object of class `BayLum.list` can be provided as input (such as produced by [create_DataFile])
#'
#' @return Stratigraphic matrix where each sample are ordered by increasing order.
#' This matrix can be integrated in the function [AgeS_Computation].
#' Please see [AgeS_Computation] for more information on this matrix.
#'
#' @seealso [AgeS_Computation], [SCMatrix]
#'
#' @author Claire Christophe, Anne Philippe, Sebastian Kreutzer, Guillaume Guérin
#'
#' @examples
#' SC <- SC_Ordered(Nb_sample = 3)
#'
#' @keywords datagen
#'
#' @md
#' @export
SC_Ordered <- function(Nb_sample) {
  ## treat input if DATA is provided
  if (!is.null(attr(Nb_sample, which = "originator")) && attr(Nb_sample, which = "originator") == "create_DataFile")
      Nb_sample <- Nb_sample$Nb_sample

  ## set matrix
  SC <- matrix(
    data = 0,
    ncol = Nb_sample[1],
    nrow = (Nb_sample[1] + 1))
  SC[1, ] <- rep(1, Nb_sample[1])

  for (i in 1:Nb_sample[1]) {
    SC[i + 1, ] <- c(rep(0, i), rep(1, (Nb_sample[1] - i)))
  }

  return(SC)
}
