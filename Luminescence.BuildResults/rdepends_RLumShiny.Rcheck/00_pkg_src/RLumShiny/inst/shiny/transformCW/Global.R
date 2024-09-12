## global.R ##
library(Luminescence)
library(RLumShiny)
library(shiny)
library(data.table)
library(rhandsontable)

data("ExampleData.CW_OSL_Curve", envir = environment())

enableBookmarking(store = "server")