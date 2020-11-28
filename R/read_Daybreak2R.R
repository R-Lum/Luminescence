#' Import measurement data produced by a Daybreak TL/OSL reader into R
#'
#' Import a TXT-file (ASCII file) or a DAT-file (binary file) produced by a
#' Daybreak reader into R. The import of the DAT-files is limited to the file
#' format described for the software TLAPLLIC v.3.2 used for a Daybreak, model 1100.
#'
#' @param file [character] or [list] (**required**):
#' path and file name of the file to be imported. Alternatively a list of file
#' names can be provided or just the path a folder containing measurement data.
#' Please note that the specific, common, file extension (txt) is likely
#' leading to function failures during import when just a path is provided.
#'
#' @param raw [logical] (*with default*):
#' if the input is a DAT-file (binary) a [data.table::data.table] instead of
#' the [RLum.Analysis-class] object can be returned for debugging purposes.
#'
#' @param verbose [logical] (*with default*):
#' enables or disables terminal feedback
#'
#' @param txtProgressBar [logical] (*with default*):
#' enables or disables [txtProgressBar].
#'
#' @return
#' A list of [RLum.Analysis-class] objects (each per position) is provided.
#'
#' @note
#' **`[BETA VERSION]`**
#' This function still needs to be tested properly. In particular
#' the function has underwent only very rough rests using a few files.
#'
#' @section Function version: 0.3.2
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)\cr
#' Antoine Zink, C2RMF, Palais du Louvre, Paris (France)
#'
#' The ASCII-file import is based on a suggestion by Willian Amidon and Andrew Louis Gorin
#'
#' @seealso [RLum.Analysis-class], [RLum.Data.Curve-class], [data.table::data.table]
#'
#' @keywords IO
#'
#' @examples
#'
#' \dontrun{
#' file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
#' temp <- read_Daybreak2R(file)
#' }
#'
#' @md
#' @export
read_Daybreak2R <- function(
  file,
  raw = FALSE,
  verbose = TRUE,
  txtProgressBar = TRUE
){

  ##TODO
  ## - run tests
  ## - check where the warning messages are comming from
  ## - implement further integegrity tests  (ASCII import)

  # Self Call -----------------------------------------------------------------------------------
  # Option (a): Input is a list, every element in the list will be treated as file connection
  # with that many file can be read in at the same time
  # Option (b): The input is just a path, the function tries to grep ALL Daybreaks-txt files in the
  # directory and import them, if this is detected, we proceed as list

  if(is(file, "character")) {

    ##If this is not really a path we skip this here
    if (dir.exists(file) & length(dir(file)) > 0) {
      if(verbose){
        cat("[read_Daybreak2R()] Directory detected, trying to extract '*.txt' files ...\n")
      }

      file <-
        as.list(paste0(file,dir(
          file, recursive = FALSE, pattern = ".txt"
        )))

    }

  }

  ##if the input is already a list
  if (is(file, "list")) {
    temp.return <- lapply(1:length(file), function(x) {
      read_Daybreak2R(
        file = file[[x]],
        txtProgressBar = txtProgressBar
      )
    })

    ##return
      return(temp.return)

  }


  # Integrity checks ----------------------------------------------------------------------------

  ##check if file exists
  if(!file.exists(file)){
    stop("[read_Daybreak2R()] file name does not seem to exist.", call. = FALSE)

  }


  ##check for file extension ... distinguish between TXT and DAT
  if(substr(file, start = nchar(file) - 3, stop = nchar(file)) == ".DAT"){

     # Read DAT-file ------------------------------------------------------------------------------
      on.exit(close(con))

      ##screen file to get information on the number of stored records
      con<-file(file,"rb")
      file.data <- file.info(file)
      max.pt<-readBin(con,what="int",6,size=2,endian="little")[6]
      file.size<-file.data$size
      n.length<-file.size/(190+8*(max.pt+1)) ##190 is is size of the header for each data set
      close(con)

      ##import data
      con <- file(file, "rb")

      ##pre-define data.table
      results.DATA <-
        data.table::data.table(
          ID = integer(length = n.length),
          MAXPT = integer(length = n.length),
          SPACING = integer(length = n.length),
          NDISK = integer(length = n.length),
          NRUN = integer(length = n.length),
          D1 = integer(length = n.length),
          NPT = integer(length = n.length),
          NATL = logical(length = n.length),
          TLRUN = logical(length = n.length),
          BEFORE_IRRAD = logical(length = n.length),
          SHIFT = double(length = n.length),
          RAMPRATE = double(length = n.length),
          GRATE = double(length = n.length),
          BRATE = double(length = n.length),
          ARATE = double(length = n.length),
          GAMMADOSE = double(length = n.length),
          BETADOSE = double(length = n.length),
          ALPHADOSE = double(length = n.length),
          BLEACHINGTIME = double(length = n.length),
          GRUNIT = character(length = n.length),
          BRUNIT = character(length = n.length),
          ARUNIT = character(length = n.length),
          BFILTER = character(length = n.length),
          GSOURCE = character(length = n.length),
          BSOURCE = character(length = n.length),
          ASOURCE = character(length = n.length),
          IRRAD_DATE = character(length = n.length),
          RUNREMARK = character(length = n.length),
          DATA = list()
        )

      ##TERMINAL FEEDBACK
      if(verbose){
        cat("\n[read_Daybreak2R()]")
        cat(paste("\n >> Importing:", file[1],"\n"))
      }

      ##PROGRESS BAR
      if(txtProgressBar & verbose){
        pb <- txtProgressBar(min=0,max=n.length, char = "=", style=3)
      }

      ##LOOP over file
      i <- 1
      while (i<n.length){

        #integer
        ligne1<-readBin(con,what="int",6,size=2,endian="little")
        i_NPT<-ligne1[1]
        i_SPACING<-ligne1[2]
        i_NRUN<-ligne1[3]
        i_NDISK<-ligne1[4]
        i_D1<-ligne1[5]
        i_MAXPT<-ligne1[6]

        #boolean
        ligne2<-readBin(con,what="logical",3,size=2,endian="little")
        i_BEFORE_IRRAD<-ligne2[1]
        i_TLRUN<-ligne2[2]
        i_NATL<-ligne2[3]

        #double (real)
        ligne3<-readBin(con,what="double",9,size=8,endian="little")
        i_BLEACHINGTIME<-ligne3[1]#0
        i_RAMPRATE<-ligne3[2] #5
        i_GRATE<-ligne3[3]#0.00000
        i_BRATE<-ligne3[4]#0.074800
        i_ARATE<-ligne3[5]#0.114740
        i_GAMMADOSE<-ligne3[6]#0
        i_BETADOSE<-ligne3[7]#0
        i_ALPHADOSE<-ligne3[8]#0
        i_SHIFT<-ligne3[9] #0

        #string[7]
        temp <- readBin(con, "raw", 24, 1)
        i_GRUNIT <- rawToChar(temp[3:8])
        i_BRUNIT <- rawToChar(temp[10:16])
        i_ARUNIT <- rawToChar(temp[18:24])

        #string[6]
        temp <- readBin(con, "raw", 32, 1)
        i_BFILTER <- rawToChar(temp[2:8])
        i_GSOURCE <- rawToChar(temp[10:16])
        i_BSOURCE <- rawToChar(temp[18:24])
        i_ASOURCE <- rawToChar(temp[26:32])

        #date record
        raw_IRRAD_DATE<-readBin(con,what="raw",4,size=1,endian="little")#27-Nov-2006
        bitDATE<-as.integer(rawToBits(raw_IRRAD_DATE))
        DATE.AAAA<-sum(bitDATE[seq(12,16)]*c(1,2,4,8,16))+1980
        DATE.MM<-sum(bitDATE[seq(1,4)]*c(1,2,4,8))
        DATE.JJ<-sum(bitDATE[seq(5,9)]*c(1,2,4,8,16))
        i_IRRAD_DATE<-paste0(DATE.AAAA,"-",MM=DATE.MM,"-",JJ=DATE.JJ)

        #string[40]
        i_RUNREMARK <- readBin(con, "raw", n = 40)
        i_RUNREMARK <- rawToChar(i_RUNREMARK[2:40])

        i_DATA<-readBin(con,what="double",i_MAXPT+1,size=8,endian="little")

        results.DATA[i,':='(ID=i,MAXPT=i_MAXPT,SPACING=i_SPACING,NDISK=i_NDISK,NRUN=i_NRUN,D1=i_D1,NPT=i_NPT,
                            NATL=i_NATL,TLRUN=i_TLRUN,BEFORE_IRRAD=i_BEFORE_IRRAD,
                            SHIFT=i_SHIFT,RAMPRATE=i_RAMPRATE,
                            GRATE=i_GRATE,BRATE=i_BRATE,ARATE=i_ARATE,
                            GAMMADOSE=i_GAMMADOSE,BETADOSE=i_BETADOSE,ALPHADOSE=i_ALPHADOSE,
                            BLEACHINGTIME=i_BLEACHINGTIME,
                            GRUNIT=i_GRUNIT,BRUNIT=i_BRUNIT, ARUNIT=i_ARUNIT,
                            BFILTER=i_BFILTER,
                            GSOURCE=i_GSOURCE,BSOURCE=i_BSOURCE,ASOURCE=i_ASOURCE,
                            IRRAD_DATE=i_IRRAD_DATE,
                            RUNREMARK=i_RUNREMARK,
                            DATA=list(i_DATA))]

        ##update
        i <- i + 1

        ##update progress bar
        if (txtProgressBar & verbose) {
          setTxtProgressBar(pb, i)
        }

      }

      ##close ProgressBar
      if(txtProgressBar & verbose) close(pb)


      ## Output ... return an RLum.Analysis object or a data.table ... depending on what is wanted
      if(raw){
        return(results.DATA)

      ##Output RLum.Analysis
      }else{

       ##remove NULL entries, otherwise we have to deal with them later later
       results.DATA <- results.DATA[!sapply(X = results.DATA[["DATA"]], is.null),]

       ##we need a double loop, as one aliquot defines one object ...
       output <- lapply(unique(results.DATA[["NDISK"]]), function(i){

          ##subset
          DT <- results.DATA[results.DATA[["NDISK"]] == i,]

          ##create list of records
          records <- lapply(1:nrow(DT), function(j){
            set_RLum(
              class = "RLum.Data.Curve",
              originator = "read_Daybreak2R",
              recordType = NA_character_,
              data = matrix(
                data = c(
                  seq(from = 0, by = DT[["SPACING"]][j], length.out = DT[["MAXPT"]][j] + 1),
                  DT[["DATA"]][j][[1]]),
                ncol = 2),
              info = as.list(DT[j,1:(ncol(DT) - 1)])
            )
          })

          ##combine in RLum.Analysis object
          temp <- set_RLum(
            class = "RLum.Analysis",
            originator = "read_Daybreak2R",
            records =  records
            )

          ##set pid and return
          return(.set_pid(temp))

          })

      ##return object
      return(output)

      }

  }else{

    # Read ASCII file -----------------------------------------------------------------------------

    if(verbose){
      cat("\n[read_Daybreak] file extension not of type '.DAT' try to import ASCII-file ... \n")

    }

    ##read file
    file2read <- suppressWarnings(readLines(file))

    ##check whether this is a binary file
    if(!all(charToRaw(file2read[1]) <= as.raw(127))){
      stop("[read_Daybreak2R()] The provided file is no ASCII-file and cannot be imported!", call. = FALSE)

    }

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


    ##TERMINAL FEEDBACK
    if(verbose){
      cat("\n[read_Daybreak2R()]")
      cat(paste("\n >> Importing:", file[1],"\n"))
    }

    ##PROGRESS BAR
    if(txtProgressBar & verbose){
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
        temp.meta_data <- unlist(strsplit(data.list[[x]][2:header.length], split = "=", fixed = TRUE))

      }else{
        temp.meta_data <- unlist(strsplit(data.list[[x]][2:length(data.list[[x]])], split = "=", fixed = TRUE))

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
          suppressWarnings(as.numeric(gsub("^\\s+|\\s+$", "", temp.data[seq(2, length(temp.data), by = 4)])))
        point.y <-
          suppressWarnings(as.numeric(gsub("^\\s+|\\s+$", "", temp.data[seq(3,length(temp.data), by = 4)])))


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
      if (txtProgressBar & verbose) {
        setTxtProgressBar(pb, x)
      }

      ##return RLum object
      return(
        set_RLum(
          class = "RLum.Data.Curve",
          originator = "read_Daybreak2R",
          recordType = sub(" ", replacement = "_", x = info$DataType),
          curveType = "measured",
          data = data,
          info = info
        )
      )

    })

    ##close ProgressBar
    if(txtProgressBar & verbose){close(pb)}

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

      ##put in RLum.Analysis object
      object <- set_RLum(
        class = "RLum.Analysis",
        originator = "read_Daybreak2R",
        protocol = "Custom",
        records = temp.list
      )

      ##set parent id of records
      object <- .set_pid(object)

      return(object)


    })

    ##TERMINAL FEEDBACK
    if(verbose){
      cat(paste0("\n ",length(unlist(get_RLum(RLum.Analysis.list))), " records have been read sucessfully!\n"))
    }

    return(RLum.Analysis.list)
    }
}
