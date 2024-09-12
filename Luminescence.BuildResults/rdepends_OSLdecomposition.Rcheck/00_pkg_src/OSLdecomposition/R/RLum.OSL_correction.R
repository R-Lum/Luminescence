#' Check and correct CW-OSL curves in RLum.Analysis data sets
#'
#' CW-OSL measurements are often affected by background signals or might be measured under
#' inconsistent detection settings. This function provides tools
#' to test and solve some common problems.
#'
#' This function processes data sets created within the [Luminescence-package] (Kreutzer et al. 2012).
#' Those data sets must be formatted as [RLum.Analysis-class] objects. Output objects will also be
#' [RLum.Analysis-class] objects and are meant for further analysis with [RLum.OSL_global_fitting].
#'
#' The data preparation tools are executed in the following order:
#' \enumerate{
#'   \item{`check_consistency`}
#'   \item{`remove_light_off`}
#'   \item{`limit_duration`}
#'   \item{`PMT_pulse_pair_resolution`}
#'   \item{`background_sequence`}
#'   \item{`subtract_offset`}
#' }
#'
#' **Currently, not all functions are available.**
#'
#' **Details to** `remove_light_off`:
#' The algorithm does the following: (1) Create global reference curve with [sum_OSLcurves]
#' (2) Search for the maximum in the first half of the reference curve and remove all data points
#' before the maximum . Do this for all curves of the selected 'record_type'.
#' (3) Search for an infliction point with
#' negative curvature (minimum of second differential) in the second half of the reference curve.
#' If the next data point has at least 50% less signal, remove all data points after the infliction
#' point. Do this for all curves of the selected 'record_type'.
#'
#' **Details to** `PMT_pulse_pair_resolution`:
#'
#' The algorithm corrects non-linearity of signal values due to insufficient pulse-pair resolution
#' of the photo-multiplier tube (PMT). Equation (6-2) of the *Hamamatsu Photomultiplier Handbook* is used:
#'
#' \deqn{I_corrected = I_measured / (1 - I_measured * resolution)}
#'
#' The algorithm does not account for PMT saturation and PMT aging effects.
#' As default pulse-pair resolution 18 *ns* is pre-defined, the *Hamamatsu* H7360 series pulse-pair resolution according to the data sheet.
#' The H7360-02 is the default PMT in *Freiberg Instruments lexsyg* OSL/TL readers.
#' *DTU Physics Risoe* TL/OSL reader deploy *ET Enterprise* 9235B series PMTs as default.
#' For these PMTs, the pulse-pair resolutions is not given in the data sheets and relies on the operation voltage.
#' However, due to the pulse properties given in the data sheets, it is unlikely
#' that those PMTs have a better pulse-pair resolution than 18 *ns*.
#'
#' **Impact of a pulse-pair resolution correction of 18 ns**
#'
#' | Measured signal | Corrected signal | Signal underestimation
#' |---|---|---|
#' | 1000 cts/s | 1000 cts/s | 0.00 %
#' | 10000 cts/s | 10002 cts/s | 0.02 %
#' | 50000 cts/s | 50045 cts/s | 0.09 %
#' | 100000 cts/s | 100180 cts/s | 0.18 %
#' | 500000 cts/s | 504541 cts/s | 0.91 %
#' | 1000000 cts/s | 1018330 cts/s | 1.83 %
#'
#'
#' @param object [RLum.Analysis-class] or [list] of [RLum.Analysis-class] (**required**):
#' Data set of one or multiple CW-OSL measured aliquots.
#'
#' @param record_type [character] (*with default*):
#' Type of records selected from the input `object`, see
#' `object[[]]@records[[]]@recordType`. Common are: `"OSL"`,`"SGOSL"` or `"IRSL"`.
#'
#' @param background_sequence [numeric] vector (*optional*):
#' Indices of list items with CW-OSL measurements of empty aliquots.
#' The records in these list items are used to calculate one average CW-OSL background curve
#' with [sum_OSLcurves]. This background curve is subtracted from each
#' CW-OSL record of the data set. The attributes `@recordType` of the background
#' measurements will be renamed to `"{record_type}background"`.
#'
#' @param subtract_offset [numeric] (*optional*):
#' Signal offset value in counts per second (*cts/s*). Value is handled as background
#' level and will be subtracted from each CW-OSL record.
#'
#' @param check_consistency [logical] (*with default*):
#' The CW-OSL component identification and separation procedure requires uniform detection parameters
#' throughout the whole data set. If `TRUE`, all records are compared for their
#' channel width and their number of channels. Those records with the most frequent
#' set of channel parameters keep their `@recordType` attribute, while records
#' with other sets of measurement parameter will be enumerated `record_type`
#' `"{record_type}2"`, `"{record_type}3"` and so on.
#'
#' @param remove_light_off [logical] (*with default*):
#' Checks if the records contain zero-signal intervals at beginning and/or end of the
#' measurement and removes them. Useful to tailor single-grain measurements.
#'
#' @param limit_duration [numeric] (*with default*):
#' Reduce measurement duration to input value in seconds (*s*).
#' Long measurement duration can lead to over-fitting at the component identification
#' of Step 1 which may induce systematic errors, see Mittelstrass (2019). Thus, limiting
#' the OSL record length ensures sufficient accuracy regarding the Fast and Medium component analysis.
#' If however, slow decaying components are of interest, `limit_duration = NA` is recommended.
#'
#' @param PMT_pulse_pair_resolution [numeric] (*with default*):
#' Time span of the pulse-pair resolution of the PMT in nanoseconds (*ns*).
#' If a value is given, the signal values will be corrected for time-resolution related
#' non-linearity at height counting rates, see *Details*.
#' Set `PMT_pulse_pair_resolution = NA` if algorithm shall be omitted.
#'
#' @param verbose [logical] (*with default*):
#' Enables console text output.
#'
#'
#' @section Last updates:
#'
#' 2022-01-02, DM: Revised `PMT_pulse_pair_resolution` algorithm.
#'
#' @author
#' Dirk Mittelstrass, \email{dirk.mittelstrass@@luminescence.de}
#'
#' Please cite the package the following way:
#'
#' Mittelstraß, D., Schmidt, C., Beyer, J., Heitmann, J. and Straessner, A.:
#' R package OSLdecomposition: Automated identification and separation of quartz CW-OSL signal components, *in preparation*.
#'
#' @seealso [RLum.OSL_global_fitting], [RLum.OSL_decomposition], [sum_OSLcurves]
#'
#' @references
#'
#' Hamamatsu, 2007. Photomultiplier Tubes: Basics and Applications, Third Edition (Edition 3A).
#' Hamamatsu Photonics K. K., Hamamatsu City.
#'
#' Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M., 2012.
#' Introducing an R package for luminescence dating analysis. Ancient TL, 30 (1), 1-8.
#'
#' Mittelstraß, D., 2019. Decomposition of weak optically stimulated luminescence signals and
#' its application in retrospective dosimetry at quartz (Master thesis). TU Dresden, Dresden.
#'
#' @return
#'
#' The input `object`, a [list] of [RLum.Analysis-class] objects, is given back with eventual changes
#' in the elements `object[[]]@records[[]]@recordType` and `object[[]]@records[[]]@data`.
#'
#' The returned data set contains a new list element `object[["CORRECTION"]]` which provides
#' a [list] of the input parameters and additional data depending on the applied tools.
#'
#' @examples
#'
#' # 'FB_10Gy' is a dose recovery test with the Fontainebleau quartz
#' # measured with a lexsyg research with green LED stimulation
#' data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
#' data_set <- Luminescence::read_BIN2R(data_path, fastForward = TRUE)
#'
#' # To correct for the background signal, subtracted the average curve from the
#' # OSL curves of an empty aliquot (list item 11) from all other OSL records:
#' data_set_corrected <- RLum.OSL_correction(data_set, background = 11, remove_light_off = FALSE)
#'
#' \donttest{
#' # Plot background corrected global average CW-OSL curve
#' sum_OSLcurves(data_set_corrected, output.plot = TRUE, record_type = "OSL")
#'
#' # Plot background curve
#' sum_OSLcurves(data_set_corrected, output.plot = TRUE, record_type = "OSLbackground")
#' }
#'
#' @md
#' @export

RLum.OSL_correction <- function(
  object,
  record_type = "OSL",
  background_sequence = NULL,
  subtract_offset = 0,
  check_consistency = TRUE,
  remove_light_off = TRUE,
  limit_duration = 20,
  PMT_pulse_pair_resolution = 18,
  verbose = TRUE
){

  # Changelog:
  # * 2020-05-24, DM: First reasonable version
  # * 2020-11-05, DM: Added roxygen documentation
  # * 2021-02-15, DM: Enabled `remove_light_off` and renamed `cut_records` into `limit_duration`. Removed `report` parameter
  # * 2021-11-23, DM: Added pulse-pair-resolution correction
  # * 2022-01-02, DM: Revised `PMT_pulse_pair_resolution` algorithm.
  #
  # ToDo:
  # * Check for Zero as first value at the time axis
  # * enhance 'check_consistency' to accept vectors of @info-arguments, include LPOWER and LIGHTSOURCE per default and print arguments
  # * enhance 'background' to accept whole RLum objects
  # * deploy Luminescence::verify_SingleGrainData() for 'check_single_grain_signal'
  # * handle previous CORRECTION steps
  # * new 'reduce data' argument to delete all unnecessary data (like TL curves etc.)
  # * IMPORTANT: If a RLum.object is manipulated, change its @info accordingly
  # * The routine for 'remove_light_off' is quite slow and needs performance improvement


  ##########################
  #### Data preparation ####
  ##########################

  object_name <- deparse(substitute(object))

  # define new list object to safely ignore incompatible list elements
  data_set <- list()
  data_set_overhang <- list()

  # Build overview list
  cor_data <- list(parameters = list(record_type = record_type,
                                     check_consistency = check_consistency,
                                     remove_light_off = remove_light_off,
                                     background_sequence = background_sequence,
                                     subtract_offset = subtract_offset))

  # Test if object is a list. If not, create a list
  if (is.list(object)) {

    for (i in 1:length(object)) {

      if (inherits(object[[i]], "RLum.Analysis")) {

        data_set[[length(data_set) + 1]] <- object[[i]]
      } else {

        element_name <- names(object)[i]
        if (element_name == "CORRECTION") {
          warning("Data set was already manipulated by [RLum.OSL_correction()]. Old information in $CORRECTION were overwritten")

          } else {

            data_set_overhang[[element_name]] <- object[[i]]
            warning("List element ", i, " is not of type 'RLum.Analysis' and was not included in fitting procedure")}}}


  } else {

    data_set <- list(object)
    warning("Input was not of type list, but output is of type list")}

  if (length(data_set) == 0) stop("Input object contains no RLum.Analysis data")

  if (!(check_consistency) && !(is.null(background_sequence))) {
    stop("Background correction requires consistent data! Please set 'check_consistency=TRUE' and try again.")}


  ########################
  #### Start workflow ####
  ########################

  correction_step <- 0

  if (check_consistency) {
    correction_step <- correction_step + 1 #########################################################
    if(verbose) cat("CORRECTION STEP", correction_step, "----- Check records for consistency in the detection settings -----\n")

    # measure computing time
    time.start <- Sys.time()

    # Characteristics vector. Will be extentable later
    Cvector <- c("Channels", "Channel width")

    ### Build table ###
    Ctable <- data.frame(NULL)
    for (j in 1:length(data_set)) {
      for (i in 1:length(data_set[[j]]@records)) {
        if (data_set[[j]]@records[[i]]@recordType == record_type) {

          channels <- length(data_set[[j]]@records[[i]]@data[,1])
          channel_width <- data_set[[j]]@records[[i]]@data[,1][2] - data_set[[j]]@records[[i]]@data[,1][1]

          new_line <- data.frame(sequence = j,
                        record = i,
                        record_type = as.character(record_type),
                        channels = channels,
                        channel_width = channel_width)


          ####### Add code for @info parameters here

          # build parameter string
          new_line <- cbind(new_line,
                            data.frame(string = paste(new_line[4:length(new_line)], collapse = ", ")))

          # add measurement to overview table
          Ctable <- rbind(Ctable, new_line)}}

    } #### end loop

    # now get parameter statistics
    Cstats <- as.data.frame(table(Ctable$string))

    N <- nrow(Cstats)
    if (N > 1) {

      # sort rows for frequency
      Cstats <- Cstats[order(-Cstats$Freq),]

      # Add record_type column and numerate them
      Cstats$record_type <- record_type
      Cstats$record_type[2:N] <- paste0(record_type, 2:N)

      # Insert new levels in OSL record type colection
      levels(Ctable$record_type) <- Cstats$record_type


      sequences_with_replacements <- c(NULL)
      for (i in 2:N) {
        # Replace record_types in the condition table
        Ctable$record_type[Ctable$string == Cstats$Var1[i]] <- Cstats$record_type[i]

        # Remember the measurement sequences
        sequences_with_replacements <- c(sequences_with_replacements,
                                         Ctable$sequence[Ctable$string == Cstats$Var1[i]])
      }
      # delete doublettes
      sequences_with_replacements <- unique(sequences_with_replacements)

      # Rename the recordType properties in the RLum.curve objects
      for (i in 1:nrow(Ctable)) {

        data_set[[Ctable$sequence[i]]]@records[[Ctable$record[i]]]@recordType <- as.character(Ctable$record_type[i])}

      # Display statistics
      colnames(Cstats) <- c("settings", "frequency", "record_type")
      Cvector
      if(verbose) cat(paste0("Frequency table of different sets of detection settings (",
                             paste(Cvector, collapse = ", "),"):\n"))
      if(verbose) print(Cstats)
      if(verbose) cat("RLum.Data.Curve@RecordType changed to",
                      paste(paste0(record_type, 2:N), collapse = " or "),
                      "in sequence:", paste(sequences_with_replacements, collapse = ", "), "\n")
      if(verbose) cat("Further data manipulations are performed just on", record_type,"records\n")

    } else {

      if(verbose) cat("All", record_type, "records have the same detection settings\n")}

    cor_data <- c(cor_data, list(measurement_characteristics = Ctable,
                            character_statistics = Cstats))


    # print needed computing time
    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")
  }

  if (remove_light_off) {
    correction_step <- correction_step + 1 ########################################################
    if(verbose) cat("CORRECTION STEP", correction_step,"----- Remove not stimulated measurement parts -----\n")
    time.start <- Sys.time()

    # Algorithm: (1) Create a global reference curve of all [record_type] curves
    # (2) Divide that curve into two halves
    # (3) Search for the maximum value in the first half, cut all values before

    ref_curve <- sum_OSLcurves(data_set, record_type = record_type, output.plot = FALSE, verbose = FALSE)
    ref_length <- length(ref_curve$signal)

    # Where is the maximum in the first half?
    ref_curve_max <- max(ref_curve$signal[1:ceiling(ref_length / 2)])
    light_on <- which(ref_curve$signal == ref_curve_max)


    # To get the light-off time, we calculate the second differential
    # Its minimum ist that negative curvature data point which is caused by the light-off event
    ref_diff2 <-  c(diff(c(0, diff(ref_curve$signal))), 0)
    ref_diff2_min <- min(ref_diff2[floor(ref_length / 2):ref_length])

    light_off <- which(ref_diff2 == ref_diff2_min)

    # be sure, the light-off event is not just a noise artifact ...
    if ((ref_diff2_min >= 0) |
        (ref_curve$signal[light_off] < 2 * ref_curve$signal[light_off + 1])) {

      # ... or set end of stimulation equal to end of the measurement
      light_off <- ref_length}

    if ((light_on == 1) & (light_off == ref_length)) {

      if(verbose) cat("No measurement parts with stimulation light turned off detected\n")

    } else {

      stimulated <- c(light_on:light_off)
      records_changed <- 0

      # go through all records
      for (j in 1:length(data_set)) {
        for (i in c(1:length(data_set[[j]]@records))) {
          if (data_set[[j]]@records[[i]]@recordType == record_type) {

            # read record
            time <- data_set[[j]]@records[[i]]@data[,1]
            signal <- data_set[[j]]@records[[i]]@data[,2]

            # reduce record
            time <- time[1:length(stimulated)]
            signal <- signal[stimulated]

            # write record
            data_set[[j]]@records[[i]]@data <- matrix(c(time, signal), ncol = 2)

            records_changed <- records_changed + 1 }}}


      # write console output
      if(verbose) {

        channel.width <- ref_curve$time[2] - ref_curve$time[1]
        cat("Measurement parts with stimulation light turned off detected and removed:\n",
            channel.width * (light_on - 1), "s at the beginning and",
            channel.width * (length(ref_curve$time) - light_off), "s at the end.\n")

        cat("-> Length of", records_changed, record_type , "records reduced from",
            ref_curve$time[length(ref_curve$time)], "s to",
            ref_curve$time[length(stimulated)], "s\n")}
    }



    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")}


  if (is.numeric(limit_duration) && (limit_duration > 0)) {
    correction_step <- correction_step + 1 ########################## CUT ###############################
    if(verbose) cat("CORRECTION STEP", correction_step,"----- Limit measurement duration -----\n")
    time.start <- Sys.time()
    records_changed <- 0

    for (j in 1:length(data_set)) {
      for (i in c(1:length(data_set[[j]]@records))) {
        if (data_set[[j]]@records[[i]]@recordType == record_type) {

          time <- data_set[[j]]@records[[i]]@data[,1]
          signal <- data_set[[j]]@records[[i]]@data[,2]
          channel_width <- time[2] - time[1]
          before <- after <- time[length(time)]

          ##### cut curve, if too long #####
          if (before > limit_duration) {

            # reduce channel number and write record back into the RLum object
            time <- time[1:ceiling(limit_duration / channel_width)]
            signal <- signal[1:ceiling(limit_duration / channel_width)]
            data_set[[j]]@records[[i]]@data <- matrix(c(time, signal), ncol = 2)
            records_changed <- records_changed + 1
            after <- time[length(time)]}}}}

    if (records_changed==0) {
      if(verbose) cat("No record was shorter than", limit_duration, "s\n")
    } else{
      if(verbose) cat("Reduced length of", records_changed, record_type, "records from",
                      before, "s to", after, "s\n")}

    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")
  }

  if (is.numeric(PMT_pulse_pair_resolution) && (PMT_pulse_pair_resolution > 0)) {
    correction_step <- correction_step + 1 ######################### PMT PULSE-PAIR RESOLUTION ################################
    if(verbose) cat("CORRECTION STEP", correction_step,"----- Correct for PMT pulse-pair resolution -----\n")
    time.start <- Sys.time()
    # See Hamamatsu PMT handbook chapter 6.3 section 2c for details

    # from nsec to sec:
    resolution <- PMT_pulse_pair_resolution * 10^-9

    # To save computing time (especially in case of single-grain measurements)
    # channels are only manipulated, if the signal is above the significance level
    significance_threshold <- 0.5
    significance_level <- -0.5 * significance_threshold + 0.5 * sqrt(significance_threshold^2 + 2 / resolution)
    if(verbose) cat("Every channel with a signal higher than ", round(significance_level) ," counts/sec will be increased. \n")
    records_tested <- 0
    records_corrected <- 0
    first_ch_median <- c(NULL)
    first_ch_max <- 0

    for (j in 1:length(data_set)) {
      for (i in c(1:length(data_set[[j]]@records))) {
        if (data_set[[j]]@records[[i]]@recordType == record_type) {

          time <- data_set[[j]]@records[[i]]@data[,1]
          signal <- data_set[[j]]@records[[i]]@data[,2]
          channel_width <- time[2] - time[1]
          correction_was_necessary <- FALSE


          for (x in 1:length(signal)) {
            old_counts <- signal[x] / channel_width

            if(old_counts > significance_level)
            {
              new_counts <- old_counts / (1 - old_counts * resolution)

              # Add to the first channel statistic
              if (x == 1) {
                relative_increase <- 100 * (new_counts / old_counts - 1)
                relative_increase <- round(relative_increase, digits = 2)
                first_ch_median <- c(first_ch_median, relative_increase)
                if (relative_increase > first_ch_max) first_ch_max <- relative_increase
              }

              # the functions used in this package could handle float number.
              # Therefore, rounding would no be necessary. But to ensure full compatibility with
              # other possibly used packages, we transform the results back to integers
              signal[x] <- round(new_counts * channel_width)
              correction_was_necessary <- TRUE
            }
          }
          records_tested <- records_tested + 1
          if (correction_was_necessary) {
            data_set[[j]]@records[[i]]@data <- matrix(c(time, signal), ncol = 2)
            records_corrected <- records_corrected + 1
          }
        }}}

    if (records_corrected > 0) {
      if(verbose) {
        cat(records_corrected, " of ", records_tested, " ", record_type,
            " records contained signal values which needed to be corrected.\n")
        cat("Signal statistics or corrected first channels:\n")
        cat("Median increase = ", stats::median(first_ch_median), " % / Maximum increase = ", first_ch_max, " %\n")
      }
    }
    else
    {
      if(verbose) cat("No records exceeding the signal limits found.\n")
    }

    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")
  }

  if (!(is.null(background_sequence) || is.na(background_sequence))
      && is.vector(background_sequence)
      && (all(background_sequence> 0) ) && (all(background_sequence<= length(data_set)))) {
    correction_step <- correction_step + 1 ######################### BACKGROUND ################################
    if(verbose) cat("CORRECTION STEP", correction_step,"----- Subtract background measurement -----\n")
    time.start <- Sys.time()


    N <- 0
    # create background curve
    background_curve <- sum_OSLcurves(data_set, record_type,
                             aliquot_selection = background_sequence,
                             output.plot = FALSE,
                             verbose = TRUE)

    # rename background OSL curves
    for (j in background_sequence) {
      for (i in c(1:length(data_set[[j]]@records))) {
        if (data_set[[j]]@records[[i]]@recordType == record_type) {

          N <- N + 1
          data_set[[j]]@records[[i]]@recordType <- paste0(record_type, "background")}}}

    #if(verbose) cat("Global background curve created from", N, record_type,"records of sequence \n")
    if(verbose) cat("RLum.Data.Curve@RecordType is changed to",
                    paste0(record_type, "background"),"for those records\n")

    # subtract background curve
    for (j in 1:length(data_set)) {
      for (i in c(1:length(data_set[[j]]@records))) {
        if (data_set[[j]]@records[[i]]@recordType == record_type) {

          time <- data_set[[j]]@records[[i]]@data[,1]
          signal <- data_set[[j]]@records[[i]]@data[,2]

          signal <- signal - background_curve$signal

          N <- N + 1
          data_set[[j]]@records[[i]]@data <- matrix(c(time, signal), ncol = 2)}}}

    cor_data <- c(cor_data, list(background_curve = background_curve))

    if(verbose) cat("Background saved at @CORRECTION@background_curve\n")
    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")

  } else {
    if (!(is.null(background_sequence))) {

      if(verbose) cat("Invalid 'background' argument. Step skipped\n")
      warning("Invalid 'background' argument")}}


  if (FALSE) {
    correction_step <- correction_step + 1 ######################### NOISE LEVEL CHECK ################################
    if(verbose) cat("CORRECTION STEP", correction_step,"----- Check measurements for sufficient signal levels -----\n")
    time.start <- Sys.time()

    # Roxygen2 Text:
    # @param check_signal_level [numeric] (*optional*):
    # Checks if the CW-OSL curves of each `object` list item have sufficient signal-to-noise
    # ratios to enable component resolved data analysis. List items where this is
    # not the case will be removed from the data set. This is useful to remove no-signal grains from
    # single grain measurements.

    # Algorithm idea:
    # If >= 50 % of all records in an list item are just noise, rename them to "OSLnoise"
    # When is something noise? When the Sign-Test says it is with p > .05 probability
    #
    # Alternatively: Use the function from the Luminescence package: verify_SingleGrainData()

    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")
  }


  if (subtract_offset != 0) {
    correction_step <- correction_step + 1 ######################### OFFSET ################################
    if(verbose) cat("CORRECTION STEP", correction_step,"----- Subtract offset value -----\n")
    time.start <- Sys.time()

    for (j in 1:length(data_set)) {
      for (i in c(1:length(data_set[[j]]@records))) {
        if (data_set[[j]]@records[[i]]@recordType == record_type) {

          time <- data_set[[j]]@records[[i]]@data[,1]
          signal <- data_set[[j]]@records[[i]]@data[,2]
          channel_width <- time[2] - time[1]

          signal <- signal - subtract_offset * channel_width
          data_set[[j]]@records[[i]]@data <- matrix(c(time, signal), ncol = 2)}}}

    if(verbose) cat("Offset of", subtract_offset, "counts per second subtracted from every", record_type, "record\n")
    if(verbose) cat("(time needed:", round(as.numeric(difftime(Sys.time(), time.start, units = "s")), digits = 2),"s)\n\n")

  }

  ################################ REPORT  ################################
  if (FALSE) {

    # Include before/after images into the report. Use plot_OSLcurve(detail = "raw") for that

    .render_report(
      nature = "correction",
      cor_data = cor_data,
      data_set = data_set,
      object_name = object_name,
      image_format = NULL,
      report_dir = NULL,
      verbose = verbose)}

  # Return decomposed data
  object <- c(data_set, data_set_overhang, CORRECTION = list(cor_data))
  invisible(object)
}
