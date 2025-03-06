#' @title Import XSYG files to R
#'
#' @description Imports XSYG-files produced by a Freiberg Instruments lexsyg reader into R.
#'
#' @details
#' **How does the import function work?**
#'
#' The function uses the `'XML'` package to parse the file structure. Each
#' sequence is subsequently translated into an [RLum.Analysis-class] object.
#'
#' **General structure XSYG format**
#'
#' ```
#' <?xml?>
#' <Sample>
#'   <Sequence>
#'     <Record>
#'       <Curve name="first curve" />
#'       <Curve name="curve with data">x0 , y0 ; x1 , y1 ; x2 , y2 ; x3 , y3</Curve>
#'     </Record>
#'   </Sequence>
#' </Sample>
#' ```
#'
#' So far, each
#' XSYG file can only contain one `<Sample></Sample>`, but multiple
#' sequences.
#'
#' Each record may comprise several curves.
#'
#' **TL curve recalculation**
#'
#' On the FI lexsyg device TL curves are recorded as time against count values.
#' Temperature values are monitored on the heating plate and stored in a
#' separate curve (time vs. temperature). If the option
#' `recalculate.TL.curves = TRUE` is chosen, the time values for each TL
#' curve are replaced by temperature values.
#'
#' Practically, this means combining two matrices (Time vs. Counts and Time vs.
#' Temperature) with different row numbers by their time values. Three cases
#' are considered:
#'
#' 1. HE: Heating element
#' 2. PMT: Photomultiplier tube
#' 3. Interpolation is done using the function [approx]
#'
#' CASE (1): `nrow(matrix(PMT))` > `nrow(matrix(HE))`
#'
#' Missing temperature values from the heating element are calculated using
#' time values from the PMT measurement.
#'
#' CASE (2): `nrow(matrix(PMT))` < `nrow(matrix(HE))`
#'
#' Missing count values from the PMT are calculated using time values from the
#' heating element measurement.
#'
#' CASE (3): `nrow(matrix(PMT))` == `nrow(matrix(HE))`
#'
#' A new matrix is produced using temperature values from the heating element
#' and count values from the PMT.
#'
#' **Note:**
#' Please note that due to the recalculation of the temperature
#' values based on values delivered by the heating element, it may happen that
#' multiple count values exists for each temperature value and temperature
#' values may also decrease during heating, not only increase.
#'
#' **Advanced file import**
#'
#' To allow for a more efficient usage of the function, instead of single path
#' to a file just a directory can be passed as input. In this particular case
#' the function tries to extract all XSYG-files found in the directory and import
#' them all. Using this option internally the function constructs as list of
#' the XSYG-files found in the directory. Please note no recursive detection
#' is supported as this may lead to endless loops.
#'
#' @param file [character] or [list] (**required**):
#' path and file name of the XSYG file. If input is a `list` it should comprise
#' only `character`s representing each valid path and XSYG-file names.
#' Alternatively, the input character can be just a directory (path), in which
#' case the function tries to detect and import all XSYG-files found in the
#' directory.
#'
#' @param recalculate.TL.curves [logical] (*with default*):
#' if set to `TRUE`, TL curves are returned as temperature against count values
#' (see details for more information) Note: The option overwrites the time vs.
#' count TL curve. Select `FALSE` to import the raw data delivered by the
#' lexsyg. Works for TL curves and spectra.
#'
#' @param n_records [numeric] (*with default*): set the number of records to be imported; by default
#' the function attempts to import all records
#'
#' @param fastForward [logical] (*with default*):
#' if `TRUE` for a more efficient data processing only a list of [RLum.Analysis-class]
#' objects is returned.
#'
#' @param import [logical] (*with default*):
#' if set to `FALSE`, only the XSYG file structure is shown.
#'
#' @param pattern [regex] (*with default*):
#' optional regular expression if `file` is a link to a folder, to select just
#' specific XSYG-files
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal. If verbose is `FALSE` the `txtProgressBar` is also switched off
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar during import. Ignored if `verbose = FALSE`.
#'
#' @return
#' **Using the option `import = FALSE`**
#'
#' A list consisting of two elements is shown:
#' - [data.frame] with information on file.
#' - [data.frame] with information on the sequences stored in the XSYG file.
#'
#' **Using the option `import = TRUE` (default)**
#'
#' A list is provided, the list elements
#' contain: \item{Sequence.Header}{[data.frame] with information on the
#' sequence.} \item{Sequence.Object}{[RLum.Analysis-class]
#' containing the curves.}
#'
#' @note
#' This function is a beta version as the XSYG file format is not yet
#' fully specified. Thus, further file operations (merge, export, write) should
#' be done using the functions provided with the package `'XML'`.
#'
#' **So far, no image data import is provided!** \cr
#' Corresponding values in the XSXG file are skipped.
#'
#'
#' @section Function version: 0.6.14
#'
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'
#' @seealso `'XML'`, [RLum.Analysis-class], [RLum.Data.Curve-class], [approx]
#'
#'
#' @references
#' Grehl, S., Kreutzer, S., Hoehne, M., 2013. Documentation of the
#' XSYG file format. Unpublished Technical Note. Freiberg, Germany
#'
#' **Further reading**
#'
#' XML: [https://en.wikipedia.org/wiki/XML]()
#'
#' @keywords IO
#'
#' @examples
#'
#' ##(1) import XSYG file to R (uncomment for usage)
#'
#' #FILE <- file.choose()
#' #temp <- read_XSYG2R(FILE)
#'
#' ##(2) additional examples for pure XML import using the package XML
#' ##    (uncomment for usage)
#'
#'   ##import entire XML file
#'   #FILE <- file.choose()
#'   #temp <- XML::xmlRoot(XML::xmlTreeParse(FILE))
#'
#'   ##search for specific subnodes with curves containing 'OSL'
#'   #getNodeSet(temp, "//Sample/Sequence/Record[@@recordType = 'OSL']/Curve")
#'
#' ##(2) How to extract single curves ... after import
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##grep one OSL curves and plot the first curve
#' OSLcurve <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType="OSL")[[1]]
#'
#' ##(3) How to see the structure of an object?
#' structure_RLum(OSL.SARMeasurement$Sequence.Object)
#'
#' @md
#' @export
read_XSYG2R <- function(
  file,
  recalculate.TL.curves = TRUE,
  n_records = NULL,
  fastForward = FALSE,
  import = TRUE,
  pattern = ".xsyg",
  verbose = TRUE,
  txtProgressBar = TRUE
) {
  .set_function_name("read_XSYG2R")
  on.exit(.unset_function_name(), add = TRUE)

  ##TODO: this function should be reshaped:
  ##  - metadata from the sequence should go into the info slot of the RLum.Analysis object
  ##   >> however, the question is whether this works with subsequent functions
  ##  - currently not all metadata are supported, it should be extended
  ##  - the should be a mode importing ALL metadata
  ##  - xlum should be general, xsyg should take care about subsequent details

  .validate_class(file, c("character", "list"))
  .validate_class(n_records, c("numeric", "NULL"))

  # Self Call -----------------------------------------------------------------------------------
  # Option (a): Input is a list, every element in the list will be treated as file connection
  # with that many file can be read in at the same time
  # Option (b): The input is just a path, the function tries to grep ALL xsyg/XSYG files in the
  # directory and import them, if this is detected, we proceed as list
  if (is.character(file)) {
    .validate_length(file, 1)

    ##If this is not really a path we skip this here
    if (dir.exists(file) & length(dir(file)) > 0) {
      if (verbose)
        message("\n[read_XSYG2R()] Directory detected, trying to extract ",
                "'*.xsyg' files ...\n")
      file <- as.list(dir(file, recursive = TRUE, pattern = pattern, full.names = TRUE))
      if (length(file) == 0) {
        if (verbose)
          .throw_message("No files matching the given pattern ",
                         "found in directory, NULL returned")
        return(NULL)
      }
    }
  }

  if (is(file, "list")) {
    temp.return <- lapply(seq_along(file), function(x) {
      read_XSYG2R(
        file = file[[x]],
        recalculate.TL.curves = recalculate.TL.curves,
        fastForward = fastForward,
        import = import,
        verbose = verbose,
        txtProgressBar = txtProgressBar
      )
    })

    ##return
    if (fastForward) {
      if(import){
        return(unlist(temp.return, recursive = FALSE))

      } else{
        return(as.data.frame(data.table::rbindlist(temp.return)))
      }

    } else{
      return(temp.return)
    }
  }

  ## Integrity checks -------------------------------------------------------

  ## check for URL and attempt download
  url_file <- .download_file(file, tempfile("read_XSYG2R_FILE"),
                             verbose = verbose)

  if(!is.null(url_file))
    file <- url_file

  ## normalise path, just in case
  file <- suppressWarnings(normalizePath(file))

  ## check whether file exist
  if(!file.exists(file)) {
    if(verbose)
      .throw_message("File does not exist, nothing imported, NULL returned")
    return(NULL)

  }

  # (0) config --------------------------------------------------------------
  #version.supported <- c("1.0")

  #additional functions
  #get spectrum values
  # TODO: This function could be written also in C++, however, not necessary due to a low demand
  get_XSYG.spectrum.values <- function(curve.node){
    ##1st grep wavelength table
    wavelength <- XML::xmlAttrs(curve.node)["wavelengthTable"]

    ##string split
    wavelength <- as.numeric(unlist(strsplit(wavelength, split = ";", fixed = TRUE)))

    ##2nd grep time values
    curve.node <- unlist(strsplit(XML::xmlValue(curve.node), split = ";", fixed = TRUE))
    curve.node <- strsplit(curve.node, split = ",", fixed = TRUE)
    curve.node.time <- as.numeric(vapply(curve.node, function(x) x[1], character(1)))

    curve.node.time <- as.numeric(vapply(curve.node, function(x) x[1], character(1)))

    ##3rd grep count values
    curve.node.count <- vapply(curve.node, function(x) {
      if(length(x) == 2)
        x[2]
      else
        x[3]
    }, character(1))

    ##remove from pattern...
    curve.node.count <- do.call(
      what = "gsub", list(pattern="[[]|[]]", replacement="",
                                             x=curve.node.count))


    ##4th combine to spectrum matrix
    spectrum.matrix <- matrix(NA, nrow = length(wavelength), ncol = length(curve.node.time))
    for(i in 1:length(curve.node.time)) {
      tmp <- as.numeric(unlist(strsplit(curve.node.count[i], "|", fixed = TRUE)))
      if(length(tmp) == length(wavelength))
        spectrum.matrix[,i] <- tmp
    }

    ## remove NA values from matrix
    id_NA <- colSums(is.na(spectrum.matrix)) != nrow(spectrum.matrix)
    spectrum.matrix <- spectrum.matrix[, id_NA]

    ##change row names (rows are wavelength)
    rownames(spectrum.matrix) <- round(wavelength, digits = 3)

    ##change column names (columns are time/temp values)
    colnames(spectrum.matrix) <- round(curve.node.time[id_NA], digits=3)

    return(spectrum.matrix)
  }

  # (1) Integrity tests -----------------------------------------------------
  ##set HUGE for larger nodes
  HUGE <- 524288

  ##parse XML tree using the package XML
  temp <- try(
    XML::xmlRoot(XML::xmlTreeParse(file, useInternalNodes = TRUE, options = HUGE, error = NULL)),
    silent = TRUE)

  ##show error
  if(inherits(temp, "try-error")){
    if(verbose)
      .throw_message("XML file not readable, nothing imported, NULL returned")
    return(NULL)
  }

  # (2) Further file processing ---------------------------------------------
  ##==========================================================================##
  ##SHOW STRUCTURE
  if(!import){
    ##sample information
    temp.sample <- as.data.frame(XML::xmlAttrs(temp), stringsAsFactors = FALSE)

    ##grep sequences files

    ##set data.frame
    names.sequence.header <- names(XML::xmlAttrs(temp[[1]]))
    temp.sequence.header <- data.frame(t(1:length(names.sequence.header)),
                                       stringsAsFactors = FALSE)
    colnames(temp.sequence.header) <- names.sequence.header

    ##fill information in data.frame
    for(i in 1:XML::xmlSize(temp)){
      temp.sequence.header[i,] <- t(XML::xmlAttrs(temp[[i]]))
    }

      ##additional option for fastForward == TRUE
      if(fastForward){

        ##change column header
        temp.sample <- t(temp.sample)
        colnames(temp.sample) <- paste0("sample::", colnames(temp.sample))
        output <- cbind(temp.sequence.header, temp.sample)

      }else{
        output <-  list(Sample = temp.sample, Sequences = temp.sequence.header)
      }

    return(output)

  } else {
    ##==========================================================================##
    ##IMPORT XSYG FILE
    if (verbose) {
      cat("\n[read_XSYG2R()] Importing ...")
      cat("\n path: ", dirname(file))
      cat("\n file: ", .shorten_filename(basename(file)))
      cat("\n")
    }

    ##PROGRESS BAR
    if(verbose && txtProgressBar){
      pb <- txtProgressBar(min=0,max=XML::xmlSize(temp), char = "=", style=3)
    }

    ## set n_records
    if(is.null(n_records))
      n_records <- XML::xmlSize(temp)

    ##loop over the entire sequence by sequence
    output <- lapply(1:min(XML::xmlSize(temp),n_records[1]), function(x){
      ##read sequence header
      temp.sequence.header <- as.data.frame(XML::xmlAttrs(temp[[x]]), stringsAsFactors = FALSE)

      ##account for non set value
      if(length(temp.sequence.header)!= 0)
        colnames(temp.sequence.header) <- ""

      ###-----------------------------------------------------------------------
      ##LOOP
      ##read records >> records are combined to one RLum.Analysis object
      temp.sequence.object <- unlist(lapply(1:XML::xmlSize(temp[[x]]), function(i){
        ##get recordType
        temp.sequence.object.recordType <- try(XML::xmlAttrs(temp[[x]][[i]])["recordType"],
                                               silent = TRUE)


        ##the XSYG file might be broken due to a machine error during the measurement, this
        ##control flow helps; if a try-error is observed NULL is returned
        if(!inherits(temp.sequence.object.recordType, "try-error")){

         ##create a fallback, the function should not fail
         if(is.null(temp.sequence.object.recordType) || is.na(temp.sequence.object.recordType)){
           temp.sequence.object.recordType <- "not_set"
         }

        ##correct record type in depending on the stimulator
        if(temp.sequence.object.recordType == "OSL"){
          if(XML::xmlAttrs(temp[[x]][[i]][[
            XML::xmlSize(temp[[x]][[i]])]])["stimulator"] == "ir_LED_850" |
            XML::xmlAttrs(temp[[x]][[i]][[
              XML::xmlSize(temp[[x]][[i]])]])["stimulator"] == "ir_LD_850"){

            temp.sequence.object.recordType  <- "IRSL"
          }
        }

        ##loop 3rd level
        lapply(1:XML::xmlSize(temp[[x]][[i]]), function(j){
          ##get values
          temp.sequence.object.curveValue <- temp[[x]][[i]][[j]]
          attrs <- XML::xmlAttrs(temp.sequence.object.curveValue)

          ##get curveType
          temp.sequence.object.curveType <- as.character(attrs["curveType"])

          ##get detector
          temp.sequence.object.detector <- as.character(attrs["detector"])

          ##get stimulator
          temp.sequence.object.stimulator <- as.character(attrs["stimulator"])

          ##get additional information
          temp.sequence.object.info <- c(as.list(attrs),
                                         position = as.integer(as.character(temp.sequence.header["position",])),
                                         name = as.character(temp.sequence.header["name",]))

          ## TL curve recalculation ============================================
          if(recalculate.TL.curves){
            ##TL curve heating values is stored in the 3rd curve of every set
            if(temp.sequence.object.recordType == "TL" && j == 1){
              #grep values from PMT measurement or spectrometer
              if(!"Spectrometer" %in% temp.sequence.object.detector){
                temp.sequence.object.curveValue.PMT <- src_get_XSYG_curve_values(XML::xmlValue(
                  temp[[x]][[i]][[j]]))

                ##round values (1 digit is the technical resolution of the heating element)
                temp.sequence.object.curveValue.PMT[,1] <- round(
                  temp.sequence.object.curveValue.PMT[,1], digits = 1)

                #grep values from heating element
                temp.sequence.object.curveValue.heating.element <- src_get_XSYG_curve_values(XML::xmlValue(
                  temp[[x]][[i]][[3]]))

              }else{
                temp.sequence.object.curveValue.spectrum <- get_XSYG.spectrum.values(
                  temp.sequence.object.curveValue)

                ##get time values which are stored in the row labels
                temp.sequence.object.curveValue.spectrum.time <- as.numeric(
                  colnames(temp.sequence.object.curveValue.spectrum))

                ##round values (1 digit is technical resolution of the heating element)
                temp.sequence.object.curveValue.spectrum.time <- round(
                  temp.sequence.object.curveValue.spectrum.time, digits = 1)

              }

              #grep values from heating element
              temp.sequence.object.curveValue.heating.element <- src_get_XSYG_curve_values(XML::xmlValue(
                temp[[x]][[i]][[3]]))

              temp.element <- temp.sequence.object.curveValue.heating.element[, 1]
              if(!"Spectrometer" %in% temp.sequence.object.detector){
                #reduce matrix values to values of the detection
                temp.sequence.object.curveValue.heating.element <-
                  temp.sequence.object.curveValue.heating.element[
                      temp.element >= min(temp.sequence.object.curveValue.PMT[, 1]) &
                      temp.element <= max(temp.sequence.object.curveValue.PMT[, 1]), ,
                      drop = FALSE]
              }else{
                #reduce matrix values to values of the detection
                temp.sequence.object.curveValue.heating.element <-
                  temp.sequence.object.curveValue.heating.element[
                      temp.element >= min(temp.sequence.object.curveValue.spectrum.time) &
                      temp.element <= max(temp.sequence.object.curveValue.spectrum.time), ,
                      drop = FALSE]
              }
              ## calculate corresponding heating rate, this makes only sense
              ## for linear heating, therefore is has to be the maximum value

              ##remove 0 values (not measured) and limit to peak
              heating.rate.values <- temp.sequence.object.curveValue.heating.element[
                temp.sequence.object.curveValue.heating.element[,2] > 0 &
                  temp.sequence.object.curveValue.heating.element[,2] <=
                  max(temp.sequence.object.curveValue.heating.element[,2]),,drop = FALSE]

              heating.rate <- (heating.rate.values[length(heating.rate.values[,2]), 2] -
                                 heating.rate.values[1,2])/
                (heating.rate.values[length(heating.rate.values[,1]), 1] -
                   heating.rate.values[1,1])

              ##round values
              heating.rate <- round(heating.rate, digits=1)

              ##add to info element
              temp.sequence.object.info <- c(temp.sequence.object.info,
                                             RATE = heating.rate)

              ##PERFORM RECALCULATION
              ##check which object contains more data
              if(!"Spectrometer" %in% temp.sequence.object.detector){
                ##CASE (1)
                if(nrow(temp.sequence.object.curveValue.PMT) >
                   nrow(temp.sequence.object.curveValue.heating.element)){
                  temp.sequence.object.curveValue.heating.element.i <- approx(
                    x = temp.sequence.object.curveValue.heating.element[,1],
                    y = temp.sequence.object.curveValue.heating.element[,2],
                    xout = temp.sequence.object.curveValue.PMT[,1],
                    rule = 2)

                  temperature.values <-
                    temp.sequence.object.curveValue.heating.element.i$y

                  count.values <-
                    temp.sequence.object.curveValue.PMT[,2]

                  ##CASE (2)
                }else if((nrow(temp.sequence.object.curveValue.PMT) <
                          nrow(temp.sequence.object.curveValue.heating.element))){
                  temp.sequence.object.curveValue.PMT.i <- approx(
                    x = temp.sequence.object.curveValue.PMT[,1],
                    y = temp.sequence.object.curveValue.PMT[,2],
                    xout = temp.sequence.object.curveValue.heating.element[,1],
                    rule = 2)

                  temperature.values <-
                    temp.sequence.object.curveValue.heating.element[,2]

                  count.values <- temp.sequence.object.curveValue.PMT.i$y

                  ##CASE (3)
                }else{
                  temperature.values <-
                    temp.sequence.object.curveValue.heating.element[,2]

                  count.values <- temp.sequence.object.curveValue.PMT[,2]
                }

                ##combine as matrix
                temp.sequence.object.curveValue <- as.matrix(cbind(
                  temperature.values,
                  count.values))

                ##set curve identifier
                temp.sequence.object.info$curveDescripter <- "Temperature [\u00B0C]; Counts [a.u.]"

              }else{
                ##CASE (1) here different approach. in contrast to the PMT measurements, as
                ##         usually the resolution should be much, much lower for such measurements
                ##         Otherwise we would introduce some pseudo signals, as we have to
                ##         take care of noise later one

                if(length(temp.sequence.object.curveValue.spectrum.time) !=
                   nrow(temp.sequence.object.curveValue.heating.element)){
                  temp.sequence.object.curveValue.heating.element.i <- approx(
                    x = temp.sequence.object.curveValue.heating.element[,1],
                    y = temp.sequence.object.curveValue.heating.element[,2],
                    xout = temp.sequence.object.curveValue.spectrum.time,
                    rule = 2,
                    ties = mean,
                    na.rm = FALSE)

                  temperature.values <-
                    temp.sequence.object.curveValue.heating.element.i$y

                  ##check for duplicated values and if so, increase this
                  idx.dup <- which(duplicated(temperature.values))
                  if (length(idx.dup) > 0) {
                    temperature.values[idx.dup] <- temperature.values[idx.dup] + 1
                    .throw_warning("Temperature values are found to be ",
                                   "duplicated and increased by 1 K.")
                  }
                  ##CASE (2)  (equal)
                }else{
                  ##CASE (2)  (equal)
                  temperature.values <-
                    temp.sequence.object.curveValue.heating.element[,2]
                }

                ##reset values of the matrix
                colnames(temp.sequence.object.curveValue.spectrum) <- temperature.values
                temp.sequence.object.curveValue <- temp.sequence.object.curveValue.spectrum

                ##change curve descriptor
                temp.sequence.object.info$curveDescripter <- "Temperature [\u00B0C]; Wavelength [nm]; Counts [1/ch]"
              }

            }##endif
          }##endif recalculate.TL.curves == TRUE


    # Cleanup info objects ------------------------------------------------------------------------
    if("curveType" %in% names(temp.sequence.object.info))
      temp.sequence.object.info[["curveType"]] <- NULL

    # Set RLum.Data-objects -----------------------------------------------------------------------
          if("Spectrometer" %in% temp.sequence.object.detector == FALSE){

            if(is(temp.sequence.object.curveValue, "matrix") == FALSE){

              temp.sequence.object.curveValue <-
                src_get_XSYG_curve_values(XML::xmlValue(temp.sequence.object.curveValue))
            }

            set_RLum(
              class = "RLum.Data.Curve",
              originator = "read_XSYG2R",
              recordType = paste(temp.sequence.object.recordType,
                                 " (", temp.sequence.object.detector,")",
                                 sep = ""),
              curveType = temp.sequence.object.curveType,
              data = temp.sequence.object.curveValue,
              info = temp.sequence.object.info)

          }else if("Spectrometer" %in% temp.sequence.object.detector == TRUE) {


            if(is(temp.sequence.object.curveValue, "matrix") == FALSE){

              temp.sequence.object.curveValue <-
                get_XSYG.spectrum.values(temp.sequence.object.curveValue)
            }

            set_RLum(
              class = "RLum.Data.Spectrum",
              originator = "read_XSYG2R",
              recordType = paste(temp.sequence.object.recordType,
                                 " (",temp.sequence.object.detector,")",
                                 sep = ""),
              curveType = temp.sequence.object.curveType,
              data = temp.sequence.object.curveValue,
              info = temp.sequence.object.info)
          }
        })

        }else{

         return(NULL)

        }##if-try condition

      }),
       use.names = FALSE)

      ##if the XSYG file is broken we get NULL as list element
      if (!is.null(temp.sequence.object)) {
        ##set RLum.Analysis object
        temp.sequence.object <-  set_RLum(
          originator = "read_XSYG2R",
          class = "RLum.Analysis",
          records = temp.sequence.object,
          protocol = as.character(temp.sequence.header["protocol",1]),
          info = list(file = file)
        )

        ##set parent uid of RLum.Anlaysis as parent ID of the records
        temp.sequence.object <- .set_pid(temp.sequence.object)

        ##update progress bar
        if (verbose && txtProgressBar) {
          setTxtProgressBar(pb, x)
        }

        ##merge output and return values
        if(fastForward){
          return(temp.sequence.object)

        }else{
          return(list(Sequence.Header = temp.sequence.header, Sequence.Object = temp.sequence.object))
        }

      }else{
        return(temp.sequence.object)
      }

    })##end loop for sequence list

    ##close ProgressBar
    if(verbose && txtProgressBar ){close(pb)}

    ##show output information
    if(length(output[sapply(output, is.null)]) == 0){
      if(verbose)
        paste("\t >>",XML::xmlSize(temp), " sequence(s) loaded successfully.\n")

    }else{

      if(verbose){
        paste("\t >>",XML::xmlSize(temp), " sequence(s) in file.", XML::xmlSize(temp)-length(output[sapply(output, is.null)]), "sequence(s) loaded successfully. \n")
      }

      .throw_warning(length(output[sapply(output, is.null)]),
                     " incomplete sequence(s) removed.")
    }

    ##output
    invisible(output)

  }#end if

  ##get rid of the NULL elements (as stated before ... invalid files)
  return(.rm_NULL_elements(output))
}
