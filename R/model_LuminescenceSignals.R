#' Model Luminescence Signals (wrapper)
#'
#' Wrapper for the function \code{\link[RLumModel]{model_LuminescenceSignals}} from the package
#' \code{RLumModel}. For the further details please see the help page of this package.
#'
#' @inheritParams RLumModel::model_LuminescenceSignals
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaige (France), \cr orignal function:
#' Johannes Friedrich, University of Bayreuth (Germany)
#' @section Function version: 0.1.0
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
