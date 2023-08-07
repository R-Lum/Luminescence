#' @title Create Folder Templates
#'
#' @description Create file and folder structure templates on the user hard drive
#' as expected by [Generate_DataFile] and [Generate_DataFile_MG]. Files and data in the
#' folders must then be overwritten manually with user data. The function intends to
#' minimise the errors going along with the creation of these folder structures.
#' The function uses the example data of `BayLum` to create the templates.
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University
#' (United Kingdom)
#'
#' @section Function version: 0.1.0
#'
#' @param path [character] (**required**): path to the folder where the templates
#' should be created
#'
#' @param mode [character] (*with default*): depending on the dataset you can
#' create templates or single grain (`SG`) or multi-grain (`MG`) data
#'
#' @param n_folders [numeric] (*with default*): number of template folders
#' to be created
#'
#' @param names [character] (*optional*): allows give own names to the subfolders.
#'
#' @param verbose [logical] (*with default*): enables/disables verbose mode
#'
#' @seealso [Generate_DataFile], [Generate_DataFile_MG]
#'
#' @return If the templates were created successfully on the hard drive, the function
#' returns nothing.
#'
#' @examples
#' create_FolderTemplates(tempdir())
#'
#'
#' @keywords datagen IO
#'
#' @md
#' @export
create_FolderTemplates <- function(
  path,
  mode = "SG",
  n_folders = 1,
  names = paste("Sample_", 1:n_folders),
  verbose = TRUE
){

  # Check input -------------------------------------------------------------
  ## missing
  if(missing(path))
    stop("[create_FolderTemplates()] Input for 'path' is missing!", call. = FALSE)

  ## check path
  if(!dir.exists(normalizePath(path, mustWork = FALSE)))
    stop("[create_FolderTemplates()] ",path, " does not exist!", call. = FALSE)

  ## check mode
  if(!toupper(mode) %in% c("SG","MG"))
    stop("[create_FolderTemplates()] mode = '",mode,"' not supported!", call. = FALSE)

  ## remove whitespace from string
  names <-
    gsub(
      pattern = " ",
      replacement = "_",
      x = names,
      fixed = TRUE
    )

  ##expand names
  names <- rep(names, length.out = n_folders[1])

  # Create templates --------------------------------------------------------
  switch (toupper(mode),
    "SG" = extdata <- "extdata/samp1",
    "MG" = extdata <- "extdata/FER1"
  )

  ## get example data
  extdata_path <- list.files(
    path = system.file(extdata, "", package="BayLum"),
    full.names = TRUE,
    recursive = TRUE)

  ##output
  if(verbose) cat("\n[create_FolderTemplates()]\n-|\n")

  for(i in 1:n_folders[1]){
    dir.create(path = paste0(normalizePath(path),"/",names[i]), showWarnings = FALSE)
    file.copy(
      from = extdata_path,
      to = paste0(normalizePath(path),"/",names[i]),
      recursive = TRUE,
      overwrite = TRUE)

    if(verbose) cat(" |__(dir created:)", path,"\n")

  }

  ## final warning
  if(verbose) message("All templates created. Please modify the parameters according to your data!")

}
