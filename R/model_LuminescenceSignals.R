#' Model Luminescence Signals (wrapper)
#'
#' Wrapper for the function \code{\link[RLumModel]{model_LuminescenceSignals}} from the package
#' \link[RLumModel]{RLumModel-package}. For the further details and examples please
#' see the manual of this package.
#'
#' @inheritParams RLumModel::model_LuminescenceSignals
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),\cr
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaige (France), \cr
#'
#'
#' @section Function version: 0.1.0
#'
#' @export
model_LuminescenceSignals <-
  function(model,
           sequence,
           lab.dose_rate = 1,
           simulate_sample_history = FALSE,
           plot = TRUE,
           verbose = TRUE,
           show.structure = FALSE,
           ...) {
    RLumModel::model_LuminescenceSignals (
      model = model,
      sequence = sequence,
      lab.dose_rate = lab.dose_rate,
      simulate_sample_history = simulate_sample_history ,
      plot = plot,
      verbose = verbose,
      show.structure = show.structure,
      ...
    )

  }
