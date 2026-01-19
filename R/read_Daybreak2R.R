#' @title Import measurement data produced by a Daybreak TL/OSL reader into R
#'
#' @description
#' Import a TXT-file (ASCII file) or a DAT-file (binary file) produced by a
#' Daybreak reader into R. The import of the DAT-files is limited to the file
#' format described for the software TLAPLLIC v.3.2 used for Daybreak models
#' 1100 and 1150, and for FLConsole (any release) for Daybreak models 11xx and
#' 2200.
#'
#' @param file [character] or [list] (**required**):
#' path and file name of the file to be imported. Alternatively a list of file
#' names can be provided or just the path a folder containing measurement data.
#' Please note that the specific, common, file extension (`.txt`) is likely
#' leading to function failures during import when just a path is provided.
#'
#' @param raw [logical] (*with default*):
#' if the input is a DAT-file (binary) a [data.table::data.table] instead of
#' the [Luminescence::RLum.Analysis-class] object can be returned for debugging purposes.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable [txtProgressBar].
#'
#' @param ... not in use, for compatibility reasons only
#'
#' @return
#' A list of [Luminescence::RLum.Analysis-class] objects (each per position) is provided.
#'
#' @note
#' **`[BETA VERSION]`**
#' This function still needs to be tested properly. In particular
#' the function has underwent only very rough tests using a few files.
#'
#' @section Function version: 0.4.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Andrzej Bluszcz, Silesian University of Technology, Gliwice (Poland)\cr
#' Antoine Zink, C2RMF, Palais du Louvre, Paris (France)
#'
#' The ASCII-file import is based on a suggestion by William Amidon and
#' Andrew Louis Gorin.
#'
#' @seealso [Luminescence::RLum.Analysis-class], [Luminescence::RLum.Data.Curve-class], [data.table::data.table]
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
#' @export
read_Daybreak2R <- function(
  file,
  raw = FALSE,
  verbose = TRUE,
  txtProgressBar = TRUE,
  ...
) {
  .set_function_name("read_Daybreak2R")
  on.exit(.unset_function_name(), add = TRUE)

  ##TODO
  ## - run tests
  ## - check where the warning messages are coming from
  ## - implement further integrity tests  (ASCII import)

  # Self Call -----------------------------------------------------------------------------------
  # Option (a): Input is a list, every element in the list will be treated as file connection
  # with that many file can be read in at the same time
  # Option (b): The input is just a path, the function tries to grep ALL Daybreaks-txt files in the
  # directory and import them, if this is detected, we proceed as list

  .validate_class(file, c("character", "list"))
  .validate_logical_scalar(raw)
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(txtProgressBar)
  if (!verbose)
    txtProgressBar <- FALSE

  if (is.character(file)) {
    .validate_length(file, 1)

    ##If this is not really a path we skip this here
    if (dir.exists(file) && length(dir(file)) > 0) {
      if(verbose){
        cat("[read_Daybreak2R()] Directory detected, trying to extract '*.txt' files ...\n")
      }

      file <- as.list(dir(file, pattern = ".txt", full.names = TRUE,
                          recursive = FALSE))
    }
  }

  ##if the input is already a list
  if (inherits(file, "list")) {
    temp.return <- lapply(seq_along(file), function(x) {
      read_Daybreak2R(
        file = file[[x]],
        txtProgressBar = txtProgressBar,
        verbose = verbose
      )
    })

    ##return
      return(temp.return)
  }


  ## Integrity checks -------------------------------------------------------
  ##check if file exists
  if(!file.exists(file)){
    .throw_error("File '", file, "' does not exist")
  }

  ##check for file extension ... distinguish between TXT and DAT
  if (endsWith(toupper(file), ".DAT")) {
    ## Read DAT-file --------------------------------------------------------
    on.exit(close(con), add = TRUE)
      ##screen file to get information on the number of stored records
      con <-file(file, "rb")
      file.data <- file.info(file)
      max.pt <- readBin(con,what="int",6,size=2,endian="little")[6]
      file.size <- file.data$size
      n.length <- file.size/(190+8*(max.pt+1)) ##190 is is size of the header for each data set
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
          DEVN = integer(length = n.length), # DEVN <- D1 %/% 100 codes a stimulation device
                       # TL dev is 0; Bleach dev is 10+; OSL/MOSL dev is 20+; 30+ is isoTL; Irrad dev is 100+; missing pnt is 200
          NPT = integer(length = n.length),
          NATL = logical(length = n.length),
          TLRUN = logical(length = n.length),
          BEFORE_IRRAD = logical(length = n.length),
          NORMALIZATION = logical(length = n.length),
          SINGLE_ALIQUOT = logical(length = n.length),
          SEKOND = logical(length = n.length),
          SHIFT = double(length = n.length),
                       # tb <- as.integer(writeBin(SHIFT, con = raw()))
                       # NTB[0:3] <- tb[0:3] number of channels with each time base 10, 100, 1000, 10000 ms
                       # TEMPERATURE <- tb[4]*256 + tb[7]
                       # OSLBG <- (tb[5]*256 + tb[6])/10.0
          NTB = integer(length = n.length),
          TEMPERATURE = integer(length = n.length),
          OSLBG = double(length = n.length),
          RAMPRATE = double(length = n.length),
          LPOWER = double(length = n.length), # LPOWER <- RAMPRATE  % of max light power
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
        cat("\n[read_Daybreak()] Importing ...")
        cat("\n path: ", dirname(file[1]))
        cat("\n file: ", .shorten_filename(basename(file[1])))
        cat("\n")
      }

      ##PROGRESS BAR
      if (txtProgressBar)
        pb <- txtProgressBar(min=0,max=n.length, char = "=", style=3)

      ##LOOP over file
      i <- 1
      while (i <= n.length) {
        #integer
        ligne1<-readBin(con,what="int",6,size=2,endian="little")
        i_NPT<-ligne1[1]
        i_SPACING<-ligne1[2]
        i_NRUN<-ligne1[3]
        i_NDISK<-ligne1[4]
        i_D1<-ligne1[5]
        i_MAXPT<-ligne1[6]
        i_DEVN <- i_D1 %/% 100

        #boolean
        ligne2 <- readBin(con, what = "logical", 6, size = 1, endian = "little")
        i_BEFORE_IRRAD<-ligne2[1]
        i_NORMALIZATION <- ligne2[2]
        i_TLRUN <- ligne2[3]
        i_SINGLE_ALIQUOT <- ligne2[4]
        i_NATL <- ligne2[5]
        i_SEKOND <- ligne2[6]

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
        if (i_DEVN == 0) {
          ## nocov start
          i_LPOWER <- NA
          i_TEMPERATURE <- NA
          i_OSLBG <- NA
          i_NTB <- NA
          ## nocov end
        } else {
          i_LPOWER <- i_RAMPRATE
          tb <- writeBin(i_SHIFT, con = raw())
          i_NTB <- readBin(tb, what = "integer", 1, size = 4)
          # i_RAMPRATE <- NA
          # i_SHIFT <- NA
          i_TEMPERATURE <- as.integer(tb[5]) * 256 + as.integer(tb[8])
          i_OSLBG <- (as.integer(tb[6]) * 256 + as.integer(tb[7])) / 10.0
        }

        #string[7]
        i_GRUNIT <- i_BRUNIT <- i_ARUNIT <- ""
        temp <- readBin(con, "raw", 24, 1)
        l <- as.numeric(temp[1])
        if (l > 0) i_GRUNIT <- rawToChar(temp[2:(1+l)])
        l <- as.numeric(temp[9])
        if (l > 0) i_BRUNIT <- rawToChar(temp[10:(9+l)])
        l <- as.numeric(temp[17])
        if (l > 0) i_ARUNIT <- rawToChar(temp[18:(17+l)])

        #string[7]
        i_BFILTER <- i_GSOURCE <- i_BSOURCE <- i_ASOURCE <- ""
        temp <- readBin(con, "raw", 32, 1)
        l <- as.numeric(temp[1])
        if (l > 0) i_BFILTER <- rawToChar(temp[2:(1+l)])
        l <- as.numeric(temp[9])
        if (l > 0) i_GSOURCE <- rawToChar(temp[10:(9+l)])
        l <- as.numeric(temp[17])
        if (l > 0) i_BSOURCE <- rawToChar(temp[18:(17+l)])
        l <- as.numeric(temp[25])
        if (l > 0) i_ASOURCE <- rawToChar(temp[26:(25+l)])

        #date record
        raw_IRRAD_DATE <- readBin(con, what = "raw", 2, size = 1, endian = "little") # 27-Nov-2006
        bitDATE<-as.integer(rawToBits(raw_IRRAD_DATE))
        DATE.AAAA <- sum(bitDATE[10:16] * c(1, 2, 4, 8, 16, 32, 64)) + 1900
        if (DATE.AAAA < 1950) DATE.AAAA <- DATE.AAAA + 128
        DATE.MM<-sum(bitDATE[seq(1,4)]*c(1,2,4,8))
        DATE.JJ<-sum(bitDATE[seq(5,9)]*c(1,2,4,8,16))
        i_IRRAD_DATE<-paste0(DATE.AAAA,"-",MM=DATE.MM,"-",JJ=DATE.JJ)

        #string[41]
        i_RUNREMARK <- readBin(con, "raw", n = 42)
        l <- as.numeric(i_RUNREMARK[1])
        i_RUNREMARK <- if (l > 0) rawToChar(i_RUNREMARK[2:(1 + l)]) else ""

        i_DATA <- readBin(con,what="double",i_MAXPT+1,size=8,endian="little")

        results.DATA[i,':='(ID=i,MAXPT=i_MAXPT,SPACING=i_SPACING,NDISK=i_NDISK,NRUN=i_NRUN,D1=i_D1,
                            DEVN = i_DEVN,
                            NPT = i_NPT,
                            NATL=i_NATL,TLRUN=i_TLRUN,BEFORE_IRRAD=i_BEFORE_IRRAD,
                            NORMALIZATION = i_NORMALIZATION,
                            SINGLE_ALIQUOT = i_SINGLE_ALIQUOT,
                            SEKOND = i_SEKOND,
                            SHIFT = i_SHIFT,
                            NTB = i_NTB,
                            TEMPERATURE = i_TEMPERATURE,
                            OSLBG = i_OSLBG,
                            RAMPRATE = i_RAMPRATE,
                            LPOWER = i_LPOWER,
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
        if (txtProgressBar) {
          setTxtProgressBar(pb, i)
        }
      }

    if (txtProgressBar)
      close(pb)

    ## Output a data.table
    if (raw)
      return(results.DATA)

    ## Output an RLum.Analysis object
    ## remove NULL entries, otherwise we have to deal with them later
    results.DATA <- results.DATA[!sapply(results.DATA[["DATA"]], is.null), ]

    ## we need a double loop, as one aliquot defines one object ...
    output <- lapply(unique(results.DATA[["NDISK"]]), function(i) {

          ##subset
          DT <- results.DATA[results.DATA[["NDISK"]] == i,]

          ##create list of records
          records <- lapply(1:nrow(DT), function(j){
            if (DT[["DEVN"]][j] == 0) {
              xx <- seq(from = DT[["SPACING"]][j], by = DT[["SPACING"]][j], length.out = DT[["NPT"]][j]) # nocov
            } else {
              tb <- c(0.01, 0.1, 1.0, 10.0)
              last_t <- 0
              BT <- as.integer(writeBin(DT[["NTB"]][j], con = raw()))
              xx <- numeric()
              for (k in 1:4) {
                if (BT[k] > 0) {
                  xx <- c(xx, seq(from = last_t + tb[k], by = tb[k], length.out = BT[k]))
                  last_t <- xx[length(xx)]
                }
              }
            }
            dev <- as.integer(DT[["DEVN"]][j])
            set_RLum(
              class = "RLum.Data.Curve",
              originator = "read_Daybreak2R",
              recordType = ifelse(dev == 0, "TL (PMT)",
                           ifelse((dev >= 10) && (dev < 20), "bleaching (PMT)",
                           ifelse((dev >= 20) && (dev < 30), "OSL (PMT)",
                           ifelse((dev >= 30) && (dev < 40), "isoTL (PMT)",
                           ifelse((dev == 200), "missing_point (NA)", NA_character_))))),
              curveType = "measured",
              data = matrix(
                data = c(
                    xx, DT[["DATA"]][j][[1]][2:(DT[["NPT"]][j] + 1)]),
                ncol = 2),
              info = as.list(DT[j,1:(ncol(DT) - 1)])
            )
          })

          ##combine in RLum.Analysis object
          temp <- set_RLum(
            class = "RLum.Analysis",
            originator = "read_Daybreak2R",
            protocol = "Custom",
            records =  records
            )

      .set_pid(temp)
    })

  }else{

    ## Read ASCII file ------------------------------------------------------
    if(verbose){
      cat("\n[read_Daybreak()] File extension not of type '.DAT', trying to import ASCII-file ... \n")
    }

    ##read file
    file2read <- suppressWarnings(readLines(file))

    ## remove all the empty lines
    file2read <- file2read[nzchar(file2read)]

    ## check whether the file contains non-ASCII characters: the [^ -~]
    ## regexp matches all ASCII characters from space to tilde
    if (any(grepl("[^ -~]", file2read[1])))
      .throw_error("The provided file is not ASCII and cannot be imported")

    ##(1)
    ##get all rows with the term "[NewRecord]" - that's what we are interested in and it defines
    ##the number of elements we need
    records.row_number <- grep(pattern = "\\[NewRecord\\]", x = file2read)
    if (length(records.row_number) == 0) {
      .throw_error("File '", file, "' doesn't appear to be in Daybreak format")
    }

    ##(1)
    ##make a list ... this is not essentially needed but it makes things easier
    data.list <- lapply(1:length(records.row_number), function(x) {
      last <- records.row_number[x + 1]
      last <- if (!is.na(last)) last - 1 else length(file2read)
      file2read[records.row_number[x]:last]
    })

      ##clear memory
      rm(file2read)

    ##TERMINAL FEEDBACK
    if(verbose){
      cat("\n[read_Daybreak()] Importing ...")
      cat("\n path: ", dirname(file[1]))
      cat("\n file: ", .shorten_filename(basename(file[1])))
      cat("\n")
    }

    ##PROGRESS BAR
    if(txtProgressBar & verbose)
      pb <- txtProgressBar(min = 0,max=length(data.list), char = "=", style = 3)

    ##(2)
    ##Loop over the list to create RLum.Data.Curve objects
    RLum.Data.Curve.list <- lapply(seq_along(data.list), function(x) {
      record <- data.list[[x]]
      record.length <- length(record)

      ##get header length until the argument 'Points'
      header.length <- grep("Points", x = record, fixed = TRUE)

      last.idx <- if (length(header.length) > 0) header.length else record.length
      temp.meta_data <- unlist(strsplit(record[2:last.idx], split = "=", fixed = TRUE))

      ##get list names for the info element list
      info.names <- temp.meta_data[seq(1,length(temp.meta_data), by = 2)]

      ##info elements
      info <- as.list(temp.meta_data[seq(2,length(temp.meta_data), by = 2)])
      names(info) <- info.names

      ##add position, which is 'Disk'
      info <- c(info, position = as.integer(info$Disk))

      if(length(header.length)>0){
        ## get measurement data ... this construction makes no assumption on
        ## the number of columns
        temp.data <- data.table::tstrsplit(
          record[header.length + 1:as.numeric(info[["Points"]])], ";", fixed = TRUE,
          type.convert = TRUE)

        ## construct data matrix
        data <- matrix(unlist(temp.data[2:3]), ncol = 2)

      } else if ("IrradTime" %in% names(info)) {
          point.x <- c(1,as.numeric(info$IrradTime))
          point.y <- rep(1, 2)
          data <- matrix(c(point.x,point.y), ncol = 2)
      } else if (info[["DataType"]] == "Command") {
        ## Note from Andrzej Bluszcz:
        # The regular [NewRecord] DataType=Command was used 2009 through 2023 and
        # later it was replaced by a single line Commands=abc belonging formally to the previous record.
        # It was probably not an official Daybreak release, but my modification used locally in our lab.
        # Nevertheless, it is wiser to use record[header.length + 1:as.numeric(info[["Points"]])]
        # to extract data points
        data <- matrix(0, ncol = 2)
      }

      ##update progress bar
      if (txtProgressBar)
        setTxtProgressBar(pb, x)

      ##return RLum object
      set_RLum(
          class = "RLum.Data.Curve",
          originator = "read_Daybreak2R",
          recordType = sub(" ", replacement = "_", x = info$DataType),
          curveType = "measured",
          data = data,
          info = info
      )
    })

    ##close ProgressBar
    if (txtProgressBar)
      close(pb)

    ##(3)
    ##Now we have to find out how many aliquots we do have
    positions.id <- vapply(RLum.Data.Curve.list, get_RLum, info.object = "position", integer(1))

    ##(4)
    ##now combine everything in an RLum.Analysis object in accordance to the position number
    output <- lapply(unique(positions.id), function(x) {
      ##get list ids for position number
      temp.list <- RLum.Data.Curve.list[which(positions.id == x)]

      ##put in RLum.Analysis object
      object <- set_RLum(
        class = "RLum.Analysis",
        originator = "read_Daybreak2R",
        protocol = "Custom",
        records = temp.list
      )

      ##set parent id of records
      .set_pid(object)
    })
  }

  ## terminal feedback
  if (verbose) {
    cat("\n ", length(unlist(get_RLum(output))),
        "records have been read successfully\n")
  }

  output
}
