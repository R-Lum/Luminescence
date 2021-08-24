## global.R ##
library(Luminescence)
library(shiny)
library(RLumShiny)
library(rhandsontable)
library(data.table)

# load example data
data(ExampleData.DeValues)
data <- ExampleData.DeValues$CA1

enableBookmarking(store = "server")