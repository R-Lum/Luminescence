#'@title Converts RLum.Analysis and RLum.Data.Curve objects to RLum2Risoe.BINfileData objects
#'
#' @description The functions converts [RLum.Analysis-class] and
#' [RLum.Data.Curve-class] objects (or a [list] of such objects) to
#' [Risoe.BINfileData-class] objects. The function intends to provide a
#' minimum of compatibility between both formats. The created
#' [RLum.Analysis-class] object can be later exported to a BIN-file using
#' function [write_R2BIN].
#'
#'@param object [RLum.Analysis-class] or [RLum.Data.Curve-class] (**required**): input object to
#'be converted
#'
#'@param keep.position.number [logical] (*with default*): keeps the original
#' position number or re-calculate the numbers to avoid doubling
#'
#'@section Function version: 0.1.4
#'
#'@author  Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [RLum.Analysis-class], [RLum.Data.Curve-class], [write_R2BIN]
#'
#'@note The conversion can be never perfect. The `RLum` objects may contain information which are
#'not part of the [Risoe.BINfileData-class] definition.
#'
#'@keywords IO
#'
#'@examples
#'
#'##simple conversion using the example dataset
#'data(ExampleData.RLum.Analysis, envir = environment())
#'convert_RLum2Risoe.BINfileData(IRSAR.RF.Data)
#'
#'@return The function returns a [Risoe.BINfileData-class] object.
#'
#'@md
#'@export
convert_RLum2Risoe.BINfileData <- function(
  object,
  keep.position.number = FALSE
) {
  .set_function_name("convert_RLum2Risoe.BINfileData")
  on.exit(.unset_function_name(), add = TRUE)

  # Self call -----------------------------------------------------------------------------------
  if(is(object, "list")){
    ##call function
    object_list <-
      lapply(object, function(x) {
        convert_RLum2Risoe.BINfileData(x)
      })

    ##merge objects
    if(length(object_list) == 1){
      return(object_list[[1]])

    }else{
      return(merge_Risoe.BINfileData(object_list, keep.position.number = keep.position.number))
    }
  }

  ## Integrity tests --------------------------------------------------------

  .validate_class(object, c("RLum.Analysis", "RLum.Data.Curve"),
                  extra = "a 'list' of such objects")
  .validate_not_empty(object)

  ##RLum.Data.Curve
  if(inherits(object, "RLum.Data.Curve"))
    object <- set_RLum(class = "RLum.Analysis", records = list(object))

  # Set PROTOTYPE & DATA --------------------------------------------------------------------------

  ##set Risoe.BINfiledata prototype
  prototype <- set_Risoe.BINfileData()

    ##grep allowed names
    allowed_names <- names(prototype@METADATA)

  ##grep records (this will avoid further the subsetting)
  records <- object@records

  ##write DATA
  prototype@DATA <- lapply(records, function(x) {x@data[,2]})

  # Create METADATA -----------------------------------------------------------------------------

  ##create METADATA list
  METADATA_list <- lapply(records, function(x){
    ##grep matching arguments only
    temp <- x@info[toupper(names(x@info)) %in% allowed_names]

    ##account for the case that no matching name was found
    if(length(temp) != 0){
      ##correct names
      names(temp) <- toupper(names(temp))
      return(temp)

    }else{
      return(list(ID = NA))
    }
  })

  ##make data.frame out of it
  METADATA_df <- as.data.frame(data.table::rbindlist(METADATA_list, fill = TRUE))

  ## sanitize version for change if it comes in wrongly because this
  ## cannot be handled by merge
  if(!is.null(METADATA_df$VERSION))
    METADATA_df$VERSION <- as.character(METADATA_df$VERSION)

  ##write METADATA
  prototype@METADATA <- merge(prototype@METADATA, METADATA_df, all = TRUE)

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##fill various missing values

  ##helper function ...otherwise the code gets too nasty ... only used for NA values!
  .replace <- function(field, value){
    ## get class
    tmp_class <- class(prototype@METADATA[[field]])[1]

    ## convert
    value <- switch(
      tmp_class,
      "factor" = as.factor(prototype@METADATA[[field]]),
      as(prototype@METADATA[[field]], tmp_class)
    )

    prototype@METADATA[[field]][is.na(prototype@METADATA[[field]])] <<- value
  }

  ## >> ID << ##
  prototype@METADATA[["ID"]] <- seq_along(records)

  ## >> SEL << ##
  prototype@METADATA[["SEL"]] <- TRUE

  ## >> VERSION << ##
  .replace("VERSION", "08")

  ## >> RECTYPE << ##
  .replace("RECTYPE", 0)

  ## >> NPOINTS << ##
  if (anyNA(prototype@METADATA[["NPOINTS"]])) {
    prototype@METADATA[["NPOINTS"]] <- vapply(records, function(x){
      length(x@data)/2

    }, FUN.VALUE = numeric(1))
  }

  ## >> LENGTH <<  + PREVIOUS
  if (anyNA(prototype@METADATA[["LENGTH"]])) {
    ##even we have values here before, it will make no difference
    prototype@METADATA[["LENGTH"]] <- (prototype@METADATA[["NPOINTS"]] * 4) + 507
    prototype@METADATA[["PREVIOUS"]] <- c(0,prototype@METADATA[["LENGTH"]][1:length(records) - 1])
  }

  ## >> RUN << ##
  ##if there is only one NA, we should overwrite it, to be consistent
  if (anyNA(prototype@METADATA[["RUN"]]))
    prototype@METADATA[["RUN"]] <- 1:length(records)

  ## >> SET << ##
  .replace("SET", 1)

  ## >> GRAIN << ##
  .replace("GRAIN", 0)

  ## >> GRAINNUMBER << ##
  .replace("GRAINNUMBER", 0)

  ## >> USER << ##
  .replace("USER", "RLum.Data")

  ## >> DTYPE << ##
  .replace("DTYPE", "Natural")

  ## >> LIGHTSOURCE << ##
  .replace("LIGHTSOURCE", "None")

  ## >> SAMPLE << ##
  if (anyNA(prototype@METADATA[["SAMPLE"]])) {
    ##get only the id's to change
    temp_id <- which(is.na(prototype@METADATA[["SAMPLE"]]))

    ##set name
    prototype@METADATA[["SAMPLE"]] <- vapply(temp_id, function(x){
      if(any(names(records[[x]]@info) == "name")){
          records[[x]]@info$name

      }else{
        "unknown"
      }

    }, character(length = 1))
  }

  ## >> COMMENT << ##
  .replace("COMMENT", "convert_RLum2Risoe.BINfileData()")

  ## >> FNAME << ##
  .replace("FNAME", " ")

  ## >> DATE << ## + TIME
  if (anyNA(prototype@METADATA[["DATE"]])) {
    ##get only the id's to change
    temp_id <- which(is.na(prototype@METADATA[["DATE"]]))

    ##set date
    prototype@METADATA[["DATE"]] <- vapply(temp_id, function(x){
      if(any(names(records[[x]]@info) == "startDate")){
        strtrim(records[[x]]@info[["startDate"]], width = 8)

      }else{
        as.character(format(Sys.Date(),"%Y%m%d"))

      }
    }, character(length = 1))

    ##set time
    prototype@METADATA[["TIME"]] <- vapply(temp_id, function(x){
      if(any(names(records[[x]]@info) == "startDate")){
        substr(records[[x]]@info[["startDate"]], start = 9, stop = 14)

      }else{
        as.character(format(Sys.time(),"%H%m%S"))

      }

    }, character(length = 1))

  }

  ## >> LOW << ##
  if (anyNA(prototype@METADATA[["LOW"]])) {
    ##get only the id's to change
    temp_id <- which(is.na(prototype@METADATA[["LOW"]]))

    ##set date
    prototype@METADATA[["LOW"]] <- vapply(temp_id, function(x){
       min(records[[x]]@data[,1])

    }, numeric(length = 1))
  }

  ## >> HIGH << ##
  if (anyNA(prototype@METADATA[["HIGH"]])) {
    ##get only the id's to change
    temp_id <- which(is.na(prototype@METADATA[["HIGH"]]))

    ##set date
    prototype@METADATA[["HIGH"]] <- vapply(temp_id, function(x){
      max(records[[x]]@data[,1])

    }, numeric(length = 1))
  }

  ## >> SEQUENCE << ##
  .replace("SEQUENCE", "")


  # METADA >> correct information -------------------------------------------------------------------------
  ##we have to correct the LTYPE, the format is rather strict
    ##(a) create LTYPE from names of objects
    LTYPE <- vapply(names(object), function(s){
      if(grepl(pattern = " (", x = s, fixed = TRUE)){
        strsplit(s, split = " (", fixed = TRUE)[[1]][1]

      }else{
        s
      }

    }, FUN.VALUE = character(1))

    ##(b) replace characters
    ##(b.1) irradiation
    LTYPE <-  gsub(pattern = "irradiation", replacement = "USER", fixed = TRUE, x = LTYPE)

    ##(b.2 RF
    LTYPE <-  gsub(pattern = "RF", replacement = "RL", fixed = TRUE, x = LTYPE)

    ##set value
    prototype@METADATA[["LTYPE"]] <- LTYPE


  ##correct USER
  ##limit user to 8 characters
  prototype@METADATA[["USER"]] <- strtrim(prototype@METADATA[["USER"]], 8)


  ##correct SAMPLE
  ##limit user to 21 characters
  prototype@METADATA[["SAMPLE"]] <- strtrim(prototype@METADATA[["SAMPLE"]], 20)

  ##replace all remaining NA values by 0
  ##all remaining values are numbers
  prototype@METADATA <- replace(prototype@METADATA, is.na(prototype@METADATA), 0L)

  # Return --------------------------------------------------------------------------------------
  return(prototype)
}
