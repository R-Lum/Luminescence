## global.R ##
library(Luminescence)
library(RLumShiny)
library(shiny)
library(data.table)
library(rhandsontable)

data("ExampleData.SurfaceExposure")
tmp <- Map(function(x, i) {
  x$error <- 0.001
  x$group <- i
  return(x)
}, ExampleData.SurfaceExposure$set_1, LETTERS[1:4])

example_data <- do.call(rbind, tmp)
rm(tmp)

enableBookmarking(store = "server")