#' @title Attempts to install the development version of the 'Luminescence' package
#'
#' @description
#' This function provides a convenient method for installing the development
#' version of the R package 'Luminescence' directly from GitHub.
#'
#' @details
#' This function checks whether the 'devtools' package is currently installed
#' on the system. If `force_install = TRUE` the functions attempts to install
#' the chosen development branch via [devtools::install_github].
#'
#' @param force_install [logical] (*optional*):
#' If `FALSE` (the default) the function produces and prints the required
#' code to the console for the user to run manually afterwards. When `TRUE`
#' and all requirements are fulfilled (see details) this function attempts to install
#' the package itself.
#'
#' @param branch [character] (*with default*):
#' Name of the branch to install. The default value ("master") corresponds to
#' the main development branch.
#'
#' @return
#' This function requires user input at the command prompt to choose the
#' desired development branch to be installed. The required R code to install
#' the package is then printed to the console.
#'
#' @examples
#' \dontrun{
#' install_DevelopmentVersion()
#' }
#'
#' @export
install_DevelopmentVersion <- function(force_install = FALSE, branch = "master") {
  # nocov start
  message("\n[install_DevelopmentVersion]\n")
  message("----\n",
          "For package prerequisites, make sure to have read the following:\n",
          "https://github.com/R-Lum/Luminescence/blob/master/README.md\n",
          "----\n")

  ## check if 'devtools' is available and install if not
  if (!requireNamespace("devtools", quietly = TRUE)) {
    message("Please install the 'devtools' package first by running ",
            "the following command:\n\n install.packages('devtools')\n")
    return(invisible())
  }

  if (!force_install) {
    message("Please copy and run the following code in your R command-line:\n")
    message("devtools::install_github('R-Lum/luminescence@", branch, "')\n")
    return(invisible())
  }

  reply <- NULL
  while (is.null(reply)) {
    message("Proceed with the installation?\n",
              " [n/N]: No\n",
              " [y/Y]: Yes\n")
    reply <- tolower(readline())

    if (reply == "n") {
      message("Nothing done")
      return(invisible())
    }
    if (reply != "y")
        reply <- NULL
  }

    # detach the 'Luminescence' package
    try(detach(name = "package:Luminescence", unload = TRUE, force = TRUE),
        silent = TRUE)

    # try to unload the dynamic library
    dynLibs <- sapply(.dynLibs(), function(x) x[["path"]] )
    try(dyn.unload(grep("Luminescence", dynLibs, value = TRUE, fixed = TRUE)),
        silent = TRUE)

    # install the development version
    devtools::install_github(paste0("r-lum/luminescence@", branch))
  # nocov end
}
