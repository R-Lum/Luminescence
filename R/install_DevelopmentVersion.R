install_DevelopmentVersion <- function(force_install = FALSE) {
  
  message("\n[install_DevelopmentVersion]\n")
  
  # check which branches are currently available
  # see ?github_branches for GitHub API implementation
  branches <- github_branches()
  
  index <-  NULL
  
  # let user pick which branch he wants to install
  while(is.null(index)) {
    message(paste0("Which development branch do you want to install? \n",
                   paste0(" [", 1:length(branches$BRANCH), "]: ", branches$BRANCH, collapse = "\n")))
    message("\n [0]: <Exit>")
    
    index <- readline()
    
    if (index == 0)
      return(NULL)
    if (!index %in% seq_len(length(branches$BRANCH)))
      index <- NULL
    
    cat("\n")
  }

  # select the correct branch
  branch <- branches$BRANCH[as.numeric(index)]
  
  if (!force_install) {
    
    message("----\n",
            "Are all prerequisites installed? Make sure to have read\n", 
            "https://github.com/R-Lum/Luminescence/blob/master/README.md\n",
            "----\n")
    
    message("Please copy and run the following code in your R command-line:\n")
    if (!requireNamespace("devtools", quietly = TRUE))
      message("install.packages('devtools')")
    
    message(branches$INSTALL[as.numeric(index)], "\n")
    
  } else {
    
    reply <- NULL
    while(is.null(reply)) {
      message("Are all prerequisites installed?",
              " (https://github.com/R-Lum/Luminescence/blob/master/README.md)\n",
              " [n/N]: No\n",
              " [y/Y]: Yes\n")
      reply <- readline()
      
      if (reply == "n" || reply == "N")
        return(NULL)
      if (reply != "y" && reply != "Y")
        reply <- NULL
    }
    
    # check if 'devtools' is available and install if not
    if (!requireNamespace("devtools", quietly = TRUE)) {
      message("Please install the 'devtools' package first by running the following command:\n",
              "install.packages('devtools')")
      return(NULL)
    }

    # detach the 'Luminescence' package
    try(detach(name = "package:Luminescence", unload = TRUE, force = TRUE), 
        silent = TRUE)
    
    # try to unload the dynamic library
    dynLibs <- sapply(.dynLibs(), function(x) x[["path"]] )
    try(dyn.unload(dynLibs[grep("Luminescence", dynLibs)]), silent = TRUE)

    # install the development version
    devtools::install_github(paste0("r-lum/luminescence@", branch))
    
  }
  
}
