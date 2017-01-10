install_DevelopmentVersion <- function(force_install = FALSE) {
  
  message("\n[install_DevelopmentVersion]\n")
  
  # check which branches are currently available
  branches <- github_branches()
  
  index <-  NULL
  
  # let user pick which branch he wants to install
  while(is.null(index)) {
    message(paste0("Which version do you want to install? \n",
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
    
    message("Please copy and run the following code in your R command-line:\n")
    if (!requireNamespace("devtools", quietly = TRUE))
      message("install.packages('devtools')")
    message(branches$INSTALL[as.numeric(index)])
    
  } else {
    
    # check if 'devtools' is available and install if not
    if (!requireNamespace("devtools", quietly = TRUE))
      install.packages("devtools")
    
    # detach the 'Luminescence' package
    try(detach(name = "package:Luminescence", unload = TRUE, force = TRUE), 
        silent = TRUE)
    
    # try to unload the dynamic library
    dynLibs <- sapply(.dynLibs(), function(x) x[[2]] )
    try(dyn.unload(dynLibs[grep("Luminescence", dynLibs)]), silent = TRUE)

    # install the development version
    devtools::install_github(paste0("r-lum/luminescence@", branch))
    
  }
  
}
