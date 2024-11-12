#'@title Simple Subsetting of Single Grain Data from Risø BIN/BINX files
#'
#'@description Most measured single grains do not exhibit light and it makes
#'usually sense to subset single grain datasets using a table of
#'position and grain pairs
#'
#'@param object [Risoe.BINfileData-class] (**required**): input object with the
#'data to subset
#'
#'@param selection [data.frame] (**required**): selection table with two columns
#'for position (1st column) and grain (2nd column) (columns names do not matter)
#'
#'@return A subset [Risoe.BINfileData-class] object
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@keywords manip datagen
#'
#'@seealso [Risoe.BINfileData-class], [read_BIN2R], [verify_SingleGrainData]
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
#'@md
#'@export
subset_SingleGrainData <- function (
    object,
    selection
) {
  .set_function_name("subset_SingleGrainData")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests --------------------------------------------------------
  .validate_class(object, "Risoe.BINfileData")

  ## try to work with selection
  selection <- as.data.frame(selection)[,1:2]
  colnames(selection) <- c("POSITION", "GRAIN")

# Subset ------------------------------------------------------------------
  ## select ids for subsetting
  sel_id <-sort(merge(object@METADATA[,c("POSITION", "GRAIN", "ID")], selection)[["ID"]])

  ## pick data
  object@METADATA <- object@METADATA[sel_id,]
  object@DATA <- object@DATA[sel_id]
  object@METADATA[["ID"]] <- 1:nrow(object@METADATA)

# Return ------------------------------------------------------------------
  return(object)
}
