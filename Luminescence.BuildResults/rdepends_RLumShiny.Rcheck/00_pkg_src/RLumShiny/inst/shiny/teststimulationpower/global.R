## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Test Stimulation Power App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Date:    2017-11-22
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##load needed packages
library(shiny)
library(Luminescence)
library(DT)
library(knitr)

##Shiny settings
options(shiny.maxRequestSize=100*1024^2)
enableBookmarking(store = "server")

##initialise data
file_data <- NULL
file_info <- NULL
xrange <- c(0,0)
yrange <- c(0,0)
