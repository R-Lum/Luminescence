#' Run Luminescence shiny apps
#' 
#' A wrapper for [`runApp`] to start interactive shiny apps for the R package Luminescence.
#' 
#' The RLumShiny package provides a single function from which all shiny apps can be started: `app_RLum()`. 
#' It essentially only takes one argument, which is a unique keyword specifying which application to start. 
#' See the table below for a list of available shiny apps and which keywords to use. If no keyword is used
#' a dashboard will be started instead, from which an application can be started.
#' 
#' \tabular{lcl}{
#' **Application name:** \tab  **Keyword:**  \tab **Function:** \cr
#' Abanico Plot \tab *abanico* \tab [`plot_AbanicoPlot`] \cr
#' Histogram \tab *histogram* \tab [`plot_Histogram`] \cr
#' Kernel Density Estimate Plot \tab *KDE* \tab [`plot_KDE`] \cr
#' Radial Plot \tab *radialplot* \tab [`plot_RadialPlot`] \cr
#' Dose Recovery Test \tab *doserecovery* \tab [`plot_DRTResults`] \cr
#' Cosmic Dose Rate \tab *cosmicdose*  \tab [`calc_CosmicDoseRate`] \cr
#' CW Curve Transformation \tab *transformCW* \tab [`CW2pHMi`], [`CW2pLM`], [`CW2pLMi`], [`CW2pPMi`] \cr
#' Filter Combinations \tab *filter* \tab [`plot_FilterCombinations`]
#' }
#' 
#' The `app_RLum()` function is just a wrapper for [`runApp`]. 
#' Via the `...` argument further arguments can be directly passed to [`runApp`]. 
#' See `?shiny::runApp` for further details on valid arguments.
#' 
#' 
#' @param app [`character`] (**required**): 
#' name of the application to start. See details for a list of available apps.
#' 
#' @param ... further arguments to pass to [`runApp`]
#' 
#' @author Christoph Burow, University of Cologne (Germany)
#' 
#' @seealso [`runApp`]
#' 
#' @examples 
#' 
#' \dontrun{
#' # Dashboard
#' app_RLum()
#' 
#' # Plotting apps
#' app_RLum("abanico")
#' app_RLum("histogram")
#' app_RLum("KDE")
#' app_RLum("radialplot")
#' app_RLum("doserecovery")
#' 
#' # Further apps
#' app_RLum("cosmicdose")
#' app_RLum("transformCW")
#' app_RLum("filter")
#' }
#' 
#' @md
#' @export app_RLum
app_RLum <- function(app = NULL, ...) {
  
  valid_apps <- c("abanico",
                  "cosmicdose",
                  "doserecovery",
                  "histogram",
                  "KDE",
                  "radialplot",
                  "transformCW",
                  "filter")
  
  if (is.null(app)) {
    
    # start the RLumShiny Dashboard Addin
    RLumShinyAddin()
    
  } else {
    
    # check if keyword is valid
    if (!any(grepl(app, valid_apps, ignore.case = TRUE))) 
      return(message(paste0("Invalid app name: ", app, " \n Valid options are: ", paste(valid_apps, collapse = ", "))))
    
    # start application
    app <- shiny::runApp(system.file(paste0("shiny/", app), package = "RLumShiny"), launch.browser = TRUE,  ...)
  }
  
}