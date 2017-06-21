#' Run Luminescence shiny apps (wrapper)
#'
#' Wrapper for the function \code{\link[RLumShiny]{app_RLum}} from the package
#' \link[RLumShiny]{RLumShiny-package}. For further details and examples please
#' see the manual of this package.
#'
#' @param app \code{\link{character}} (required): name of the application to start. See details for a list
#' of available apps.
#' @param ... further arguments to pass to \code{\link[shiny]{runApp}}
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @section Function version: 0.1.1
#'
#' @export
app_RLum <- function(app = NULL, ...) {
  
  if (!requireNamespace("RLumShiny", quietly = TRUE))
    stop("Shiny applications require the 'RLumShiny' package. To install",
         " this package run 'install.packages('RLumShiny')' in your R console.", 
         call. = FALSE) 
  
  RLumShiny::app_RLum(app, ...)
}