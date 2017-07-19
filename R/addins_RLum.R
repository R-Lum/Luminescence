##RLum RStudio AddIns
#' Search for TODOs in the source code and list them
#'
#'
#'@noRD
listTODO <- function(){

  ##parse code
  code <- rstudioapi::getActiveDocumentContext()$contents

  ##get lines with ##TODO
  id <- grep(pattern = "#TODO", x = code, fixed = TRUE)

  ##list lines
  cat("\n", "[", length(id), "issue(s)]\n")
   for(i in id){
    cat(" line ", i, ": ->", code[i], "\n", sep = "")

  }

}
