## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Filter_app
## Authors: Urs Tilmann Wolpert, Department of Geography, Justus-Liebig-University Giessen
##          Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: urs.t.wolpert@geogr.uni-giessen.de
## Date:    Thu June 22 2017
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(shiny)
library(Luminescence)
library(readxl)
library(RLumShiny)

##check whether a real database exists or the template should be loaded
if(dir.exists("Data")){
  ##set to first file
  database_path <- list.files("Data/", full.names = TRUE)[1]

  ##check whether this is a real XLSX file
  if(rev(strsplit(database_path, split = ".", fixed = TRUE)[[1]])[1] != "xlsx"){
    stop("The filter database file needs to be of type 'xlsx'!")

  }

}else{
  database_path <- "template/template.xlsx"

}

# ##load data and cleanup filter list
filters <- readxl::excel_sheets(database_path)
filters <- filters[!grepl(pattern = "Main List", x = filters, fixed = TRUE)]

