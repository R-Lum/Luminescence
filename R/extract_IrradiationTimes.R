extract_IrradiationTimes <- structure(function(#Extract irradiation times from an XSYG file
  ### Extracts irradiation times, dose and times since last irradiation, from a Freiberg Instruments
  ### XSYG-file. These information can be further used to update an existing BINX-file

  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr

  ##section<<
  ## version 0.1
  # ===========================================================================

  file.XSYG,
  ### \code{\link{character}} (\bold{required}): path and file name of the XSYG file.

  file.BINX,
  ### \code{\link{character}} (optional): path and file name of an existing BINX-file. If a file name
  ### is provided the file will be updated with the information from the XSYG file in the same
  ### folder as the original BINX-file.\cr
  ### Note: The XSYG and the BINX-file have to be originate from the same measurement!

  recordType = c("irradiation (NA)", "IRSL (UVVIS)", "OSL (UVVIS)", "TL (UVVIS)"),
  ### \code{\link{character}} (with default): select relevant curves types from the XSYG file.
  ### As the XSYG-file format comprises much more information than usually needed for routine
  ### data analysis and allowed in the BINX-file format, only the relevant curves are selected
  ### by using the function \code{\link{get_RLum.Analysis}}. The argument
  ### \code{recordType} works as described for this function. \cr
  ###
  ### Note: A wrong selection will causes a function error. Please change this argument only
  ### if you have reasons to do so.

  txtProgressBar = TRUE
  ### \code{\link{logical}} (with default): enables \code{TRUE} or disables \code{FALSE}
  ### the progression bars during import and export

){

  # Integrity tests -----------------------------------------------------------------------------

  ##XSYG
  ##check if file exists
  if(file.exists(file.XSYG) == FALSE){

    stop("[extract_IrradiationTimes()] Wrong XSYG file name or file does not exsits!")

  }

  ##check if file is XML file
  if(tail(unlist(strsplit(file.XSYG, split = "\\.")), 1) != "xsyg" &
     tail(unlist(strsplit(file.XSYG, split = "\\.")), 1) != "XSYG" ){

    stop("[extract_IrradiationTimes()] File is not of type 'XSYG'!")

  }

  ##BINX
  if(!missing(file.BINX)){

  ##check if file exists
  if(file.exists(file.BINX) == FALSE){

    stop("[extract_IrradiationTimes()] Wrong BINX file name or file does not exsits!")

  }

  ##check if file is XML file
  if(tail(unlist(strsplit(file.BINX, split = "\\.")), 1) != "binx" &
     tail(unlist(strsplit(file.BINX, split = "\\.")), 1) != "BINX" ){

    stop("[extract_IrradiationTimes()] File is not of type 'BINX'!")

   }

  }


  # Settings and import XSYG --------------------------------------------------------------------

  temp.XSYG <- readXSYG2R(file.XSYG, txtProgressBar = txtProgressBar)

  if(!missing(file.BINX)){
   temp.BINX <- readBIN2R(file.BINX, txtProgressBar = txtProgressBar)
   temp.BINX.dirname <- (dirname(file.XSYG))
  }


  # Some data preparation -----------------------------------------------------------------------
  ##set list
  temp.sequence.list <- list()

  ##select all analysis objects and combinde them
  for(i in 1:length(temp.XSYG)){

    ##select sequence and reduce the data set to really wanted values
    temp.sequence.list[[i]] <- get_RLum.Analysis(temp.XSYG[[i]]$Sequence.Object,
                                     recordType = recordType,
                                     keep.object = TRUE)


  ##get corresponding position number, this will be needed later on
  temp.sequence.position <- as.numeric(as.character(temp.XSYG[[i]]$Sequence.Header["position",]))

  }


  ##merge objects
  if(length(temp.sequence.list)>1){

    temp.sequence <- merge_RLum.Analysis(temp.sequence.list)

  }else{

    temp.sequence <- temp.sequence.list[[1]]

  }


  # Grep relevant information -------------------------------------------------------------------

  ##Sequence STEP
  STEP <- sapply(1:length_RLum.Analysis(temp.sequence), function(x){

    get_RLum.Analysis(temp.sequence, record.id = x)@recordType

  })

   #START time of each step
  temp.START <- unname(sapply(1:length_RLum.Analysis(temp.sequence), function(x){

    get_RLum.Data.Curve(get_RLum.Analysis(temp.sequence, record.id = x), info.object = c("startDate"))

  }))

  ##DURATION of each STEP
  DURATION.STEP <- sapply(1:length_RLum.Analysis(temp.sequence), function(x){

      max(get_RLum.Data.Curve(get_RLum.Analysis(temp.sequence, record.id = x))[,1])

  })

 ##a little bit reformatting.
 START <- strptime(temp.START, format = "%Y%m%d%H%M%S", tz = "GMT")

 ##Calculate END time of each STEP
 END <- START + DURATION.STEP

 ##add position number
 POSITION <- rep(temp.sequence.position, each = length_RLum.Analysis(temp.sequence))

 ##Combine the results
 temp.results <- data.frame(POSITION,STEP,START,DURATION.STEP,END)


 # Calculate irradiation duration ------------------------------------------------------------

 ##set objects
 time.irr.duration <- NA

 IRR_TIME <- unlist(sapply(1:nrow(temp.results), function(x){

   if(temp.results[x,"STEP"] == "irradiation (NA)"){

     time.irr.duration <<- temp.results[x,"DURATION.STEP"]
     return(0)

   }else{

     if(is.na(time.irr.duration)){

       return(0)

     }else{

       return(time.irr.duration)

     }

   }

 }))


  # Calculate time since irradiation ------------------------------------------------------------

  ##set objects
  time.irr.end <- NA

  TIMESINCEIRR <- unlist(sapply(1:nrow(temp.results), function(x){

   if(temp.results[x,"STEP"] == "irradiation (NA)"){

      time.irr.end<<-temp.results[x,"END"]
      return(-1)

   }else{

     if(is.na(time.irr.end)){

       return(-1)

      }else{

        return(difftime(temp.results[x,"START"],time.irr.end, units = "secs"))

      }

   }

  }))

  # Combine final results -----------------------------------------------------------------------

  ##results table, export as CSV
  results <- cbind(temp.results,IRR_TIME, TIMESINCEIRR)

  # Write BINX-file if wanted -------------------------------------------------------------------

    ##(1) remove all irradiation steps as there is no record in the BINX file and update information
    results.BINX <- results[-which(results[,"STEP"] == "irradiation (NA)"),]

    ##(1a)  update information
    temp.BINX@METADATA[,c("IRR_TIME", "TIMESINCEIRR")] <- results.BINX[,c("IRR_TIME","TIMESINCEIRR")]

    ##(2) compare entries in the BINX-file with the entries in the table to make sure
    ## that both have the same length
    if(!missing(file.BINX)){
      if(nrow(results.BINX) == nrow(temp.BINX@METADATA)){

        ##update BINX-file
        writeR2BIN(temp.BINX, version = "06",
                   file = paste0(file.BINX,"_extract_IrradiationTimes.BINX"),
                   txtProgressBar = txtProgressBar)


      }
    }else{

      warning("XSYG and BINX-file do not contain similar entries. BINX-file update skipped!")

    }


  # Output --------------------------------------------------------------------------------------
  return(set_RLum.Results(data = list(irr.times = results)))

  ##value<<
  ## An \code{\linkS4class{RLum.Results}} object is returned with the following structure:\cr
  ## .. $irr.times (data.frame)\cr
  ##
  ## If a BINX-file path and name is set, the output will be additionally transferred
  ## to a new BINX-file with the function name as suffix. For the output
  ## the path of the input BINX-file itself is used.

  ##details<<
  ## The function was written to compensate missing information in the BINX-file output of
  ## Freiberg Instruments lexsyg readers. As all information are available within the XSYG-file anyway,
  ## these information can be extracted and used for further analysis or/and to stored
  ## in a new BINX-file, which can be further used by other software, e.g. Analyst (Geoff Duller). \cr
  ##
  ## Typical application example: g-value estimation from fading measurements using the Analyst
  ## or any other self written script.\cr
  ##
  ## Beside the some simple data transformation steps the function applies the functions \code{\link{readXSYG2R}},
  ## \code{\link{readBIN2R}}, \code{\link{writeR2BIN}} for data import and export.

  ##references<<
  ##Duller, G., 2007. Analyst.

  ##note<<
  ## The produced output object contains still the irradiation steps to keep the output transparent.
  ## However, for the BINX-file export this steps are removed as the BINX-file format description
  ## does not allow irradiations as separat sequences steps.\cr
  ##
  ## Know issue: The 'fading correction' menu in the Analyst will not work appear with the produced
  ## BIN/BINX-file due to hidden bits, which are not reproduced by the function \code{writeR2BIN()}.

  ##seealso<<
  ## \code{\linkS4class{RLum.Results}}, \code{\linkS4class{Risoe.BINfileData}},
  ## \code{\link{readXSYG2R}}, \code{\link{readBIN2R}},
  ## \code{\link{writeR2BIN}}

  ##keyword<<
  ## IO
  ## manip



}, ex=function(){

  ## (1) - example for your own data
  ##
  ## set files and run function
  #
  #   file.XSYG <- file.choose()
  #   file.BINX <- file.choose()
  #
  #     output <- extract_IrradiationTimes(file.XSYG = file.XSYG, file.BINX = file.BINX)
  #     get_RLum.Results(output)
  #
  ## export results additionally to a CSV.file in the same directory as the XSYG-file
  #       write.table(x = get_RLum.Results(output),
  #                   file = paste0(file.BINX,"_extract_IrradiationTimes.csv"),
  #                   sep = ";",
  #                   row.names = FALSE)

})