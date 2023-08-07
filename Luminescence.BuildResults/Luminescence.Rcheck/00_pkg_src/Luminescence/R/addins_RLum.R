##################################################################################
##                     Luminescence - RStudio Add Ins                           ##
##################################################################################
##<<Prerequisite>>
##
## - Add-ins should support more expierenced users. For all others we have the package 'RLumShiny'
##
## - Add-ins should be provided as non-exported function only, having the a name with a leading dot, e.g., .addin.
##   This prevents further chaos in the manuel.
##
## - Interative add-ins are not desired, except they are implemented in the package 'RLumShiny' or they
##   are only available if the package 'RLumShiny' is available.
##
##
##<<FAQ>>
##
## Q. Why are the add-ins non-interactive ... had you been too lazy?
## A. No, but interactivity would require the installation of 'shiny" by default, which is not
##    desired.
##
## Q. The add-ins are not shown in the 'Addin' menu?
## A. Well, if you read this information you are an advanced used, so please install the
##    package 'rstudioapi', 'devtools' and get happy.



#'Install package development version
#'
#'The function uses the GitHub APconnection provided by Christoph Burow
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@noRd
.installDevelopmentVersion <- function(){
  install_DevelopmentVersion(force_install = TRUE)

}

#'Search for TODOs in the source code and list them in the terminal
#'
#'This add-in is a tool developers may want to use to briefly list all open
#'issues in the terminal, instead of using search and stepping through the results.
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@noRd
.listTODO <- function(){

  ##check if package is installed
  if(!requireNamespace("rstudioapi", quietly = TRUE)){
    message("Package 'rstudioapi' is not installed but needed to search for TODOs, do you want to install it?\n\n",
            " [n/N]: No (default)\n",
            " [y/Y]: Yes\n")

    ##parse answer
    answer <- readline()

    if(tolower(answer) == "y"){
      utils::install.packages("rstudioapi", dependencies = TRUE)
    }

  }else{

  ##parse code
  code <- rstudioapi::getActiveDocumentContext()$contents

  ##get lines with ##TODO
  id <- grep(pattern = "#\\s*TODO", x = code, fixed = FALSE)

  ##list lines
  cat("\n", "[", length(id), " issue(s)]\n", sep = "")
   for(i in id){
    cat(" line ", i, ": ->", code[i], "\n", sep = "")

   }

 }

}
