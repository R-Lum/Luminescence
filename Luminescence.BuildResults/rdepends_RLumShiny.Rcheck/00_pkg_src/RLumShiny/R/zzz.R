# add libraries to ressource path
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("RLumShiny", system.file("www", package = "RLumShiny"))
}

# Dependencies in the shiny apps are currently not registered by R CMD check --as-cran
.satisfyCheck <- function() {
  x <- TRUE
  if (x) return(x)
  Luminescence::sTeve()
  googleVis::renderGvis()
  tmp <- DT::datatable(data.frame(1))
  knitr::normal_print("")
  rm(tmp)
}