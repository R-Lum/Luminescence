#' @title Write BayLum .csv-files
#'
#' @description This function allows the user to write all .csv files expected by
#' [Generate_DataFile] and [Generate_DataFile_MG]. Unlike [create_FolderTemplates],
#' this function makes it possible to write .csv files with all information directly from R.
#' No further modification of .csv files are required. The purpose of this function is (i) to reduce tedious manual editing of .csv-files and the errors that result
#' (ii) to introduce an easy way to review information inside .csv-files (by revisiting code rather than opening individual .csv-files) and (iii) to streamline folder
#' and file creation when preparing data to run BayLum's modelling functions. Note: the user will still need to move the appropriate .bin-files into all the sample folders.
#'
#' @author Frederik Baumgarten, RadPhys, DTU Physics, Technical University of Denmark (Denmark)
#'
#' @section Function version: 0.1.0
#'
#' @param folder [character] (*required**): The name of the main folder in which all subsequent BayLum files and folders will be located. This could be a path to an already existing folder, or the path/name of a folder to be created.
#'
#' @param SampleNames [character] (*required*): Vector of sample names.
#'
#' @param BinPerSample [numeric] (*with default*): Vector of numbers indicating the number of .bin-files per sample.
#'
#' @param SubSampleNames [character] (*optional*): Vector of names to give each subfolder within a sample when the number of .bin-files in a sample counts more than one. If omitted or NULL, the subfolders are named by the subfolder count number.
#'
#' @param DiscPos [numeric] (*with default*): List of data frames with each data frame having one or two columns to identify aliquots/grains to be included in the analysis. The first column corresponds to the position number, and the second column corresponds to the grain number. If the data frame has only one column, a Disc.csv will be written. If the data.frame has two columns, a DiscPos.csv will be written. The length of the list should be the number of .bin-files included.
#'
#' @param DRenv [numeric] (*with default*): Vector where `DRenv[i]` corresponds to environmental dose rate for `.bin-file[i]`. Length should be one or the number of .bin-files included in the analysis.
#'
#' @param DRenv.error [numeric] (*with default*): Vector where `DRenv.error[i]` corresponds to environmental dose rate error for `.bin-file[i]`. Length should be one or the number of .bin-files included in the analysis.
#'
#' @param DRsource [numeric] (*with default*): Vector where `DRsource[i]` corresponds to source dose rate for `.bin-file[i]`. Length should be one or the number of .bin-files included in the analysis.
#'
#' @param DRsource.error [numeric] (*with default*): Vector where `DRsource.error[i]` corresponds to source dose rate error for `.bin-file[i]`. Length should be one or the number of .bin-files included in the analysis.
#'
#' @param signal.integral.min [numeric] (*with default*): Vector where `signal.integral.min[i]` corresponds to the channel number where the OSL signal should be summed from for`bin-file[i]` Length should be one or the number of .bin-files included in the analysis.
#'
#' @param signal.integral.max [numeric] (*with default*): Vector where `signal.integral.max[i]` corresponds to the channel number where the OSL signal should be summed from to for `bin-file[i]` Length should be one or the number of .bin-files included in the analysis.
#'
#' @param background.integral.min [numeric] (*with default*): Vector where `background.integral.min[i]` corresponds to the channel number where the OSL background signal should be summed from for`bin-file[i]` Length should be one or the number of .bin-files included in the analysis.
#'
#' @param background.integral.max [numeric] (*with default*): Vector where `background.integral.max[i]` corresponds to the channel number where the OSL background signal should be summed to for `.bin-file[i]`. Length should be one or the number of .bin-files included in the analysis.
#'
#' @param inflatePercent [numeric] (*with default*): Vector where `inflatePercent[i]` corresponds to uncertainty due to instrumental reproducibility to`bin-file[i]` Length should be one or the number of .bin-files included in the analysis.
#'
#' @param nbOfLastCycleToRemove [numeric] (*with default*): Vector where `nbOfLastCycleToRemove[i]` corresponds to the number of regeneration points to remove in analysis for `bin-file[i]` Length should be one or the number of .bin-files included in the analysis.

#' @seealso [Generate_DataFile], [Generate_DataFile_MG]
#'
#' @return The function returns nothing, but writes the folder structure.
#'
#' @examples
#' # example samples
#' SampleNames <- c("OSL-1-MG","OSL-2-SG")
#'
#' # number of .bin-files for each sample
#' BinPerSample <- c(1,3)
#'
#' # List of data.frames of accepted aliquot/grain to be included
#' # in the analysis for each .bin-file.
#' DiscPos <- list(
#' data.frame("position" = 1:48),
#' data.frame("position" = c(1,1,1,1), "grain" = c(4,67,92,99)),
#' data.frame("position" = c(2,2,2,2), "grain" = c(7,13,41,72)),
#' data.frame("position" = c(3,3,3,3), "grain" = c(7,52,67,88)))
#'
#' # example 1: write to disk (all together)
#' write_BayLumFiles(
#' folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
#' SampleNames = SampleNames,
#' BinPerSample = BinPerSample,
#' DiscPos = DiscPos,
#' DRenv = c(1.75, 1.52, 1.52, 1.52),
#' DRenv.error = c(0.04, 0.03, 0.03, 0.03),
#' DRsource = c(0.2075, 0.1501, 0.1501, 0.1501),
#' DRsource.error = c(0.0010, 0.0008, 0.0008, 0.0008))
#'
#' # example 2: write to disk (by sample)
#' write_BayLumFiles(
#' folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
#' SampleNames = "OSL-1-MG",
#' BinPerSample = 1,
#' DiscPos = DiscPos[[1]],
#' DRenv = 1.75,
#' DRenv.error = 0.04,
#' DRsource = 0.2075,
#' DRsource.error = 0.0010)
#'
#' write_BayLumFiles(
#' folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
#' SampleNames = "OSL-2-SG",
#' BinPerSample = 3,
#' DiscPos = DiscPos[2:4],
#' DRenv = 1.75,
#' DRenv.error = 0.04,
#' DRsource = 0.2075,
#' DRsource.error = 0.0010)
#'
#' @keywords datagen IO
#'
#' @md
#' @export
write_BayLumFiles <- function(
    folder,
    SampleNames = "Sample_1",
    BinPerSample = rep(1, length(SampleNames)),
    SubSampleNames = NULL,
    DiscPos = NULL,
    DRenv = 1.00,
    DRenv.error = 0.04,
    DRsource = 0.1,
    DRsource.error = 0.0020,
    signal.integral.min = 6,
    signal.integral.max = 10,
    background.integral.min = 346,
    background.integral.max = 395,
    inflatePercent = 0.025,
    nbOfLastCycleToRemove = 2
) {
  ##expand SampleNames to reflect all eventual directories
  SampleNames_expanded <- rep(SampleNames, BinPerSample)

  ## make list of arguments
  arg.list <- mget(
    c(
      "DRenv",
      "DRenv.error",
      "DRsource",
      "DRsource.error",
      "signal.integral.min",
      "signal.integral.max",
      "background.integral.min",
      "background.integral.max",
      "inflatePercent",
      "nbOfLastCycleToRemove"
    )
  )

  ##make 1 element inputs the length of the number of total samples/subsamples (to enable just one element input if all are the same) ####
  for (i in 1:length(arg.list)) {
    if (length(arg.list[[i]]) == 1) {
      arg.list[[i]] <- rep(arg.list[[i]], length(SampleNames_expanded))
    }
  }

  ##make the vector of subsamplenames when subsamples exist, but are not named ####
  if (is.null(SubSampleNames))
    SubSampleNames <- unlist(lapply(BinPerSample, seq_len))

  ##check for correct input and stop if missing inputs exist
  arg.fail <- vapply(1:length(arg.list), function(x) {
    length(arg.list[[x]]) != length(SampleNames_expanded)
  }, logical(1))

  if (sum(arg.fail) > 0) {
    err <- paste0(c(
      "[write_BayLumFiles()] It seems that input is missing. The following arguments(s) have more than 1 entry,
      but does not agree with the specified number of samples/subsamples:\n\t -> ",
      paste0(names(arg.list)[arg.fail], collapse = ", "),
      "\n\nAll arguments passed to \"rule.csv\" must be either 1 or match the number of specified samples!"))
    stop(err, call. = FALSE)
  }

  ##create main folder and sample folder ####
  for (i in 1:length(SampleNames)) {
    dir.create(folder, showWarnings = FALSE)
    dir.create(paste(folder, SampleNames[i], sep = "/"), showWarnings = FALSE)
  }

  ##find which samples have only one bin-file to it ####
  duplicate_SampleNames = SampleNames_expanded %in% SampleNames_expanded[duplicated(SampleNames_expanded)]

  ##make vector of sample paths ####
  sub.dir = paste(folder, SampleNames_expanded, sep = "/")

  ##if sample has 2 or more subsamples then add those extra subsample path lines to vector of all sample paths ####
  for (i in 1:length(SampleNames_expanded)) {
    if (duplicate_SampleNames[i]) {
      sub.dir[i] <- paste(folder, SampleNames_expanded[i], SubSampleNames[i], sep = "/")
    }
  }

  ##create the last line of folders, which will be the subsample (if > 1) folders
  for (n in 1:length(SampleNames_expanded)) {
    dir.create(sub.dir[n], showWarnings = FALSE)
  }

  ##create necessary csv files for BayLum ####
  for (i in 1:length(sub.dir)) {

    # check if DiscPos info was given (and write template DiscPos.csv if not given)
    if(is.null(DiscPos)) {
      write.csv(
        data.frame("position" = c(1,1,1), "grain" = c(1,2,3)),
        paste(sub.dir[i], "DiscPos.csv", sep = "/"),
        row.names = FALSE
      )
    } else {
      # check if DiscPos is a data frame and convert if not (useful when multi-grain and user did not bother to input vector of aliquots as data.frame)
      if (!inherits(DiscPos[[i]], "data.frame")) {
        DiscPos[[i]] <- as.data.frame(DiscPos[[i]])
      }

      # check if DiscPos has one column (this would indicate multi-grain data)
      if (ncol(DiscPos[[i]]) == 1) {
        write.csv(
          data.frame("position" = DiscPos[[i]][, 1]),
          paste(sub.dir[i], "Disc.csv", sep = "/"),
          row.names = FALSE
        )
      }

      # check if DiscPos has two columns (this would indicate single-grain data)
      if (ncol(DiscPos[[i]]) == 2) {
        write.csv(
          data.frame("position" = DiscPos[[i]][, 1], "grain" = DiscPos[[i]][, 2]),
          paste(sub.dir[i], "DiscPos.csv", sep = "/"),
          row.names = FALSE
        )
      }
    }

    ##write dose source ####
    # DoseSource
    write.csv(
      data.frame(
        "obs" = arg.list$DRsource[i] ,
        "var" = arg.list$DRsource.error[i] ^ 2
      ),
      paste(sub.dir[i], "DoseSource.csv", sep = "/"),
      row.names = FALSE
    )

    ##write dose env ####
    write.csv(
      data.frame(
        "obs" = arg.list$DRenv[i] ,
        "var" = arg.list$DRenv.error[i] ^ 2
      ),
      paste(sub.dir[i], "DoseEnv.csv", sep = "/"),
      row.names = FALSE
    )

    ##write rule ####
    # rule
    write.csv(
      data.frame(
        "[Param]" = c(
          paste("beginSignal=", arg.list$signal.integral.min[i], sep = " "),
          paste("endSignal=", arg.list$signal.integral.max[i], sep = " "),
          paste(
            "beginBackground=",
            arg.list$background.integral.min[i],
            sep = " "
          ),
          paste(
            "endBackground=",
            arg.list$background.integral.max[i],
            sep = " "
          ),
          paste("beginTest=", arg.list$signal.integral.min[i], sep = " "),
          paste("endTest=", arg.list$signal.integral.max[i], sep = " "),
          paste(
            "beginTestBackground=",
            arg.list$background.integral.min[i],
            sep = " "
          ),
          paste(
            "endTestBackground=",
            arg.list$background.integral.max[i],
            sep = " "
          ),
          paste("inflatePercent=", arg.list$inflatePercent[i], sep = " "),
          paste(
            "nbOfLastCycleToRemove=",
            arg.list$nbOfLastCycleToRemove[i],
            sep = " "
          )
        ),
        check.names = FALSE
      ),
      paste(sub.dir[i], "rule.csv", sep = "/"),
      row.names = FALSE,
      quote = FALSE
    )

  }
}

SampleNames <- c("OSL-1-MG","OSL-2-SG")

# number of .bin-files for each sample
BinPerSample <- c(1,3)

# List of data.frames of accepted aliquot/grain to be included
# in the analysis for each .bin-file.
DiscPos <- list(
data.frame("position" = 1:48),
data.frame("position" = c(1,1,1,1), "grain" = c(4,67,92,99)),
data.frame("position" = c(2,2,2,2), "grain" = c(7,13,41,72)),
data.frame("position" = c(3,3,3,3), "grain" = c(7,52,67,88)))

# example 1: write to disk (all together)
write_BayLumFiles(
folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
SampleNames = SampleNames,
BinPerSample = BinPerSample,
DiscPos = DiscPos,
DRenv = c(1.75, 1.52, 1.52, 1.52),
DRenv.error = c(0.04, 0.03, 0.03, 0.03),
DRsource = c(0.2075, 0.1501, 0.1501, 0.1501),
DRsource.error = c(0.0010, 0.0008, 0.0008, 0.0008))
