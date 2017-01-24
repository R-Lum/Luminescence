#' Export RLum-objects to CSV
#'
#' This function exports \code{\linkS4class{RLum}}-objects to CSV-files using the R function
#' \code{\link[utils]{write.table}}. All \code{\linkS4class{RLum}}-objects are supported, but the
#' export is lossy, i.e. the pure numerical values are exported only. Information that cannot
#' be coerced to a \code{\link{data.frame}} or a \code{\link{matrix}} are discarded as well as
#' metadata.
#'
#' However, in combination with the implemented import functions, nearly every supported
#' import data format can be exported to CSV-files, this gives a great deal of freedom in terms of
#' compatibility with other tools.\cr
#'
#' \bold{Input is a list of objects}\cr
#'
#' If the input is a \code{\link{list}} of objects all explicit function arguments can be provided
#' as \code{\link{list}}.
#'
#' @param object \code{\linkS4class{RLum}} or a \code{\link{list}} of \code{RLum} objects (\bold{required}): objects to be written
#'
#' @param path \code{\link{character}} (optional): character string naming folder for the output to be written. If nothing
#' is provided \code{path} will be set to the working directory. Note: this argument is ignored if the
#' the argument \code{export} is set to \code{FALSE}.
#'
#' @param prefix \code{\link{character}} (with default): optional prefix to name the files. This prefix
#' is valid for all written files
#'
#' @param export \code{\link{logical}} (with default): enable or disable the file export. If set to \code{FALSE}
#' nothing is written to the file connection, but a list comprising objects of type \code{link{data.frame}} and \code{\link{matrix}}
#' is returned instead
#'
#' @param \dots further arguments that will be passed to the function \code{\link[utils]{write.table}}. All arguments
#' except the argument \code{file} are supported
#'
#'
#' @return The function returns either a CSV-file (or many of them) or for the option \code{export == FALSE}
#' a list comprising objects of type \code{link{data.frame}} and \code{\link{matrix}}
#'
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Data}}, \code{\linkS4class{RLum.Results}},
#' \code{\link[utils]{write.table}}
#'
#' @keywords IO
#'
#' @examples
#'
#' ##transform values to a list
#' data(ExampleData.BINfileData, envir = environment())
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)[[1]]
#' write_RLum2CSV(object, export = FALSE)
#'
#' \dontrun{
#'
#' ##export data to CSV-files in the working directory;
#' ##BE CAREFUL, this example creates many files on your file system
#' data(ExampleData.BINfileData, envir = environment())
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)[[1]]
#' write_RLum2CSV(object, export = FALSE)
#'
#' }
#'
#' @export
write_RLum2CSV <- function(
  object,
  path = NULL,
  prefix = "",
  export = TRUE,
  ...

){

  # General tests -------------------------------------------------------------------------------
  if(missing(object)){
    stop("[write_RLum2CSV()] input object is missing!", call. = FALSE)

  }


  # Self-call -----------------------------------------------------------------------------------
  ##this option allows to work on a list of RLum-objects
  if(is.list(object)){

    ##extent the list of arguments if set
      ##path
      path <- rep(list(path), length = length(object))

      ##prefix ... create automatic prefix if nothing is provided
      if(prefix == ""){
        prefix <- as.list(paste0("[[",1:length(object),"]]_"))

      }else{
        prefix <- rep(list(prefix), length = length(object))

      }

      ##export
      export <- rep(list(export), length = length(object))

    ##execute the self-call function
      temp <- lapply(1:length(object), function(x){
        write_RLum2CSV(
          object = object[[x]],
          path = path[[x]],
          prefix = prefix[[x]],
          export = export[[x]],
          ...
        )

      })

      ##this prevents that we get a list of NULL
      if(is.null(unlist(temp))){
        return(NULL)

      }else{
        return(temp)

      }

  }

  # Integrity tests -----------------------------------------------------------------------------

  ##check path

    ##if NULL condition
    if(export == TRUE && is.null(path)){
      path <- getwd()
      message(paste0("[write_RLum2CSV()] Path automatically set to: ", path))

    }

    ##non NULL conditon
    if(export == TRUE && !dir.exists(path)){
      stop("[write_RLum2CSV()] Diretory provided via the argument 'path' does not exist!", call. = FALSE)

    }

  ## What do we need at the end of the day is a named list of data.frames or matrices we can export
  ## using the function write.table; the name of the list elements will become the file names
  if(inherits(object, "RLum")){
    if(is(object, "RLum.Analysis") ||
       is(object, "RLum.Data.Curve") ||
       is(object, "RLum.Data.Spectrum") || is(object, "RLum.Data.Image")){

      ##extract all elements ... depending on the input
      if(is(object, "RLum.Analysis")){
        ##tricky, we cannot use get_RLum() as the function lapply calls as.list() for an object!
        object_list <- lapply(object, function(x){get_RLum(x)})

        ##change names of the list and produce the right format straight away
        names(object_list) <- paste0(1:length(object_list),"_",names(object))

      } else {

        ##get object and make list
        object_list <- list(get_RLum(object))

        ##set new name
        names(object_list) <- paste0("1_",object@recordType)

      }

    }else if(is(object, "RLum.Results")){

      ##we just try the typical R way and hope the best
      object_list <- unlist(object@data, recursive = FALSE)

      ##sort out objects we do not like and we cannot procede ...
      object_list <- object_list[vapply(object_list, function(x) {
        is.data.frame(x) |
          is.matrix(x) |
          is.numeric(x)
      }, vector(mode = "logical", length = 1))]


      ##adjust the names
      names(object_list) <- paste0(1:length(object_list),"_",names(object_list))


    }else{
      try(stop("[write_RLum2CSV()] One particular RLum-object is not yet supported! NULL returned!", call. = FALSE))
      return(NULL)

    }

  }else{
   stop("[write_RLum2CSV()] Object needs to be a member of the object class RLum!", call. = FALSE)

  }

  # Export --------------------------------------------------------------------------------------
  if(export){

    ##set export settings for write.table
    export_settings.default <- list(
      append = FALSE,
      quote = TRUE,
      sep = ";",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = FALSE,
      col.names = FALSE,
      qmethod = c("escape", "double"),
      fileEncoding = ""

    )

    ##modify on demand
    export_settings <- modifyList(x = export_settings.default, val = list(...))

    ##write files to file system
    for(i in 1:length(object_list)){
      utils::write.table(
        x = object_list[[i]],
        file = paste0(path,"/",prefix, names(object_list)[i],".csv"),
        append = export_settings$append,
        quote =  export_settings$quote,
        sep =  export_settings$sep,
        eol =  export_settings$eol,
        na =  export_settings$na,
        dec =  export_settings$dec,
        row.names =  export_settings$row.names,
        col.names =  export_settings$col.names,
        qmethod =  export_settings$qmethod,
        fileEncoding =  export_settings$fileEncoding)

    }


  }else{
    return(object_list)

  }

}
