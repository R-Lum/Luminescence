## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   RCarb Shiny App - global.R
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date: 2018-10-14
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##load needed packages
require(shiny)
require(RCarb)
require(rhandsontable)
require(knitr)

##Shiny settings
##
##increase upload size
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

##RCarb
##
##load reference and example data
data("Example_Data")
data("Reference_Data")
temp_files <<- NULL
