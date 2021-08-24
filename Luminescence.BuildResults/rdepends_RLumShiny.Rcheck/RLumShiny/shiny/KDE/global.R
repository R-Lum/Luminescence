## Server.R
library(Luminescence)
library(shiny)
library(RLumShiny)
library(data.table)
library(rhandsontable)

# load example data
data(ExampleData.DeValues)

enableBookmarking(store = "server")