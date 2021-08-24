## global.R ##
library(Luminescence)
library(RLumShiny)
library(shiny)
library(data.table)
library(rhandsontable)

if (utils::packageVersion("Luminescence") < "0.9.0")
  stop(
    "\n\n", rep("#", 30), "\n",
    "This application requires 'Luminescence' version >=0.9.0.\n",
    "See ?Luminescence::install_DevelopmentVersion() to get the ",
    "latest version of the package.",
    "\n", rep("#", 30), "\n\n",
    call. = FALSE)

data("ExampleData.ScaleGammaDose")

example_data <- ExampleData.ScaleGammaDose

f <- function(x, d = 3) formatC(x, digits = d, format = "f")

enableBookmarking(store = "server")