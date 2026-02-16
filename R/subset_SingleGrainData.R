#' @title Simple Subsetting of Single Grain Data from Risø BIN/BINX files
#'
#' @description
#' Most measured single grains do not exhibit light and it usually makes sense
#' to subset single grain datasets using a table of position and grain pairs.
#'
#' @param object [Luminescence::Risoe.BINfileData-class] (**required**):
#' input object with the data to subset.
#'
#' @param selection [data.frame], [matrix] (**required**):
#' selection table with two columns for position and grain, respectively.
#'
#' @return A subset [Luminescence::Risoe.BINfileData-class] object
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#'@keywords manip datagen
#'
#'@seealso [Luminescence::Risoe.BINfileData-class], [Luminescence::read_BIN2R],
#'[Luminescence::verify_SingleGrainData]
#'
#'@examples
#'
#'## load example data
#'data(ExampleData.BINfileData, envir = environment())
#'
#'## set POSITION/GRAIN pair dataset
#'selection <- data.frame(POSITION = c(1,5,7), GRAIN = c(0,0,0))
#'
#'##subset
#'subset_SingleGrainData(object = CWOSL.SAR.Data, selection = selection)
#'
#'@export
subset_SingleGrainData <- function(
    object,
    selection
) {
  .set_function_name("subset_SingleGrainData")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "Risoe.BINfileData")
  .validate_class(selection, c("data.frame", "matrix"))
  if (ncol(selection) < 2)
    .throw_error("'selection' should have 2 columns")

  ## try to work with selection
  selection <- as.data.frame(selection)[,1:2]
  colnames(selection) <- c("POSITION", "GRAIN")

# Subset ------------------------------------------------------------------
  ## select ids for subsetting
  sel_id <-sort(merge(object@METADATA[,c("POSITION", "GRAIN", "ID")], selection)[["ID"]])
  if (length(sel_id) == 0)
    .throw_error("No matching records, check POSITION and GRAIN in 'selection'")

  ## pick data
  object@METADATA <- object@METADATA[sel_id,]
  object@DATA <- object@DATA[sel_id]
  object@METADATA[["ID"]] <- 1:nrow(object@METADATA)

# Return ------------------------------------------------------------------
  return(object)
}
