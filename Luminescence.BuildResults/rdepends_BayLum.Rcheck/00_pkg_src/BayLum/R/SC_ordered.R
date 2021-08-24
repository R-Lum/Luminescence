#' Create stratigraphically ordered sample matrix
#'
#' Construct the stratigraphic matrix used in the functions [AgeS_Computation] and [AgeC14_Computation]
#' for samples that are all ordered by increasing age.
#'
#' @param Nb_sample [integer] (**required**): the number of samples
#'
#' @return Stratigraphic matrix where each sample are ordered by increasing order.
#' This matrix can be intergrated in the function [AgeS_Computation].
#' Please see [AgeS_Computation] for more information on this matrix.
#'
#' @seealso [AgeS_Computation], [SCMatrix]
#'
#' @author Claire Christophe, Anne Philippe, Sebastian Kreutzer, Guillaume Guérin
#'
#' @examples
#' # compute the stratigraphic matrix for 3 samples such that the first sample is younger
#' # than the second, and the second is younger than the third
#' SC <- SC_Ordered(Nb_sample = 3)
#'
#' @keywords datagen
#'
#' @md
#' @export
SC_Ordered <- function(Nb_sample) {
  SC <- matrix(data = 0,
               ncol = Nb_sample,
               nrow = (Nb_sample + 1))
  SC[1, ] <- rep(1, Nb_sample)
  for (i in 1:Nb_sample) {
    SC[i + 1, ] <- c(rep(0, i), rep(1, (Nb_sample - i)))
  }
  return(SC)
}

