#' Import Daybreak ASCII dato into R
#'
#' Import a *.txt (ASCII) file produced by a Daybreak reader into R.
#'
#' @param file \code{\link{character}} (\bold{required}): path and file name of the
#' file to import
#'
#' @param txtProgressBar \code{\link{logical}} (with default): enables or disables
#' \code{\link{txtProgressBar}}.
#'
#' @return  A list of \code{\linkS4class{RLum.Analysis}} objects (each per position) is provided.
#'
#' @note \bold{[BETA VERSION]} This function version still needs to be properly tested.
#'
#' @section Function version: 0.1.0
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)\cr Based on suggestion by Willian Amidon and Andrew Louis Gorin.
#'
#' @seealso \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Data.Curve}}
#'
#' @references -
#'
#' @keywords IO
#'
#'
#' @examples
#'
#' ## This function has no example yet.
#' 
#' @export
read_Daybreak2R <- function(
  file,
  txtProgressBar = TRUE
){

  ##TODO
  ## - run tests
  ## - check where the warning messages are comming from
  ## - implement further integretiy tests

  # Integrity checks ----------------------------------------------------------------------------

  ##check if file exists
  assertive::is_existing_file(file)


  # Read ASCII file -----------------------------------------------------------------------------

  ##read file
  file2read <- readLines(file)

  ##(0) get rid off all the empty lines
  file2read <- file2read[file2read != ""]

  ##(1)
  ##get all rows with the term "[NewRecord]" - that's what we are interested in and it defines
  ##the number of elements we need
  records.row_number <- grep(pattern = "\\[NewRecord\\]", x = file2read)

  ##(1)
  ##make a list ... this is not essentially needed but it makes things easier
  data.list <- lapply(1:length(records.row_number), function(x) {

    ##grep each element
    if (!is.na(records.row_number[x + 1])) {
      return(file2read[records.row_number[x]:(records.row_number[x + 1] - 1)])

    }else{
      return(file2read[records.row_number[x]:length(file2read)])

    }

  })

    ##clear memory
    rm(file2read)


  ##PROGRESS BAR
  if(txtProgressBar == TRUE){
    pb <- txtProgressBar(min=0,max=length(data.list), char = "=", style=3)
  }


  ##(2)
  ##Loop over the list to create RLum.Data.Curve objects
  RLum.Data.Curve.list <- lapply(1:length(data.list), function(x){


    ##get length of record
    record.length <- length(data.list[[x]])

    ##get header length until the argument 'Points'
    header.length <- grep(pattern = "Points", x = data.list[[x]])

    if(length(header.length)>0){
      temp.meta_data <- unlist(strsplit(data.list[[x]][2:header.length], split = "="))

    }else{
      temp.meta_data <- unlist(strsplit(data.list[[x]][2:length(data.list[[x]])], split = "="))

    }

    ##get list names for the info element list
    info.names <- temp.meta_data[seq(1,length(temp.meta_data), by = 2)]

    ##info elements
    info <- as.list(temp.meta_data[seq(2,length(temp.meta_data), by = 2)])
    names(info) <- info.names

    ##add position, which is 'Disk'
    info <- c(info, position = as.integer(info$Disk))

    if(length(header.length)>0){
      ##get measurement data
      temp.data <- unlist(strsplit(unlist(strsplit(
        data.list[[x]][12:length(data.list[[x]])], split = "="
      )), split = ";"))

      ##grep only data of interest
      point.x <-
        as.numeric(gsub("^\\s+|\\s+$", "", temp.data[seq(2,length(temp.data), by = 4)]))
      point.y <-
        as.numeric(gsub("^\\s+|\\s+$", "", temp.data[seq(3,length(temp.data), by = 4)]))


      ##combine it into a matrix
      data <- matrix(c(point.x,point.y), ncol = 2)

    }else{

      ##we presume this should be irradiation ...
      if ("IrradTime" %in% names(info)) {

        point.x <- 1:as.numeric(info$IrradTime)
        point.y <- rep(1, length(point.x))

        data <- matrix(c(point.x,point.y), ncol = 2)

      }

    }

    ##update progress bar
    if (txtProgressBar == TRUE) {
      setTxtProgressBar(pb, x)
    }

    ##return RLum object
    return(
      set_RLum(
        class = "RLum.Data.Curve",
        recordType = sub(" ", replacement = "_", x = info$DataType),
        curveType = "measured",
        data = data,
        info = info
      )
    )

  })

  ##close ProgressBar
  if(txtProgressBar == TRUE){close(pb)}

  ##(3)
  ##Now we have to find out how many aliquots we do have
  positions.id <-  sapply(RLum.Data.Curve.list, function(x){

    get_RLum(x, info.object = "position")

  })

  ##(4)
  ##now combine everyting in an RLum.Analysis object in accordance to the position number
  RLum.Analysis.list <- lapply(unique(positions.id), function(x){

    ##get list ids for position number
    n <- which(positions.id == x)

    ##make list
    temp.list <- lapply(n, function(x){
      RLum.Data.Curve.list[[x]]

    })

    ##put in RLum.Analysis obect
    return(set_RLum(
      class = "RLum.Analysis",
      protocol = "Custom",
      records = temp.list
    )
    )

  })
}
