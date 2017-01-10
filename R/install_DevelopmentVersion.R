install_DevelopmentVersion <- function(force_install = FALSE) {
  
  message("\n[install_DevelopmentVersion]\n")
  
  # branches <- github_branches()
  
  branches <- c("master", "dev_0.7.0", "dev_0.7.0_CB")
  
  branch <-  NULL
  
  while(is.null(branch)) {
    message(paste0("Which version do you want to install?  \n",
                   paste0(" [", 1:length(branches), "]: ", branches, collapse = "\n")))
    branch <- readline()
    if (!branch %in% seq_len(length(branches)))
      branch <- NULL
    cat("\n")
  }

  branch <- branches[as.numeric(branch)]
  
  if (!force_install) {
    
    message("Please copy and run the following code in your R command-line:\n")
    if (!requireNamespace("devtools", quietly = TRUE))
      message("install.packages('devtools')")
    message(paste0("devtools::install_github('r-lum/luminescence@", branch,"')"))
    
  } else {
    
    if (!requireNamespace("devtools", quietly = TRUE))
      install.packages("devtools")
    
    try(detach(name = "package:Luminescence", unload = TRUE, force = TRUE), 
        silent = TRUE)
    
    dynLibs <- sapply(.dynLibs(), function(x) x[[2]] )
    try(dyn.unload(dynLibs[grep("Luminescence", dynLibs)]), silent = TRUE)

    devtools::install_github(paste0("r-lum/luminescence@", branch))
  }
  
}
