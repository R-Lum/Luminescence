## global.R ##
library(Luminescence)
library(RLumShiny)
library(shiny)
library(data.table)
library(rhandsontable)

# load example data
data(ExampleData.DeValues)

enableBookmarking(store = "server")