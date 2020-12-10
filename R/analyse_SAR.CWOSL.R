#' @title  Analyse SAR CW-OSL measurements
#'
#' @description The function performs a SAR CW-OSL analysis on an
#' [RLum.Analysis-class] object including growth curve fitting.
#'
#' @details
#' The function performs an analysis for a standard SAR protocol measurements
#' introduced by Murray and Wintle (2000) with CW-OSL curves. For the
#' calculation of the `Lx/Tx` value the function [calc_OSLLxTxRatio] is
#' used. For **changing the way the Lx/Tx error is calculated** use the argument
#' `background.count.distribution` and `sigmab`, which will be passed to the function
#' [calc_OSLLxTxRatio].
#'
#' **Argument `object` is of type `list`**
#'
#' If the argument `object` is of type [list] containing **only**
#' [RLum.Analysis-class] objects, the function re-calls itself as often as elements
#' are in the list. This is useful if an entire measurement wanted to be analysed without
#' writing separate for-loops. To gain in full control of the parameters (e.g., `dose.points`) for
#' every aliquot (corresponding to one [RLum.Analysis-class] object in the list), in
#' this case the arguments can be provided as [list]. This `list` should
#' be of similar length as the `list` provided with the argument `object`,
#' otherwise the function will create an own list of the requested length.
#' Function output will be just one single [RLum.Results-class] object.
#'
#' Please be careful when using this option. It may allow a fast an efficient data analysis, but
#' the function may also break with an unclear error message, due to wrong input data.
#'
#' **Working with IRSL data**
#'
#' The function was originally designed to work just for 'OSL' curves,
#' following the principles of the SAR protocol. An IRSL measurement protocol
#' may follow this procedure, e.g., post-IR IRSL protocol (Thomsen et al.,
#' 2008). Therefore this functions has been enhanced to work with IRSL data,
#' however, the function is only capable of analysing curves that follow the
#' SAR protocol structure, i.e., to analyse a post-IR IRSL protocol, curve data
#' have to be pre-selected by the user to fit the standards of the SAR
#' protocol, i.e., Lx,Tx,Lx,Tx and so on.
#'
#' Example: Imagine the measurement contains `pIRIR50` and `pIRIR225` IRSL curves.
#' Only one curve type can be analysed at the same time: The `pIRIR50` curves or
#' the `pIRIR225` curves.
#'
#' **Supported rejection criteria**
#'
#' `[recycling.ratio]`: calculated for every repeated regeneration dose point.
#'
#' `[recuperation.rate]`: recuperation rate calculated by comparing the
#' Lx/Tx values of the zero regeneration point with the Ln/Tn value (the Lx/Tx
#' ratio of the natural signal). For methodological background see Aitken and
#' Smith (1988).
#'
#' `[testdose.error]`: set the allowed error for the testdose, which per
#' default should not exceed 10\%. The test dose error is calculated as `Tx_net.error/Tx_net`.
#'
#' `[palaeodose.error]`: set the allowed error for the De value, which per
#' default should not exceed 10\%.
#'
#' @param object [RLum.Analysis-class] (**required**):
#' input object containing data for analysis, alternatively a [list] of
#' [RLum.Analysis-class] objects can be provided.
#'
#' @param signal.integral.min [integer] (**required**):
#' lower bound of the signal integral. Can be a [list] of [integer]s, if `object` is
#' of type [list]. If the input is vector (e.g., `c(1,2)`) the 2nd value will be interpreted
#' as the minimum signal integral for the `Tx` curve. Can be set to `NA`, in this
#' case no integrals are taken into account.
#'
#' @param signal.integral.max [integer] (**required**):
#' upper bound of the signal integral. Can be a [list] of [integer]s, if `object` is
#' of type [list]. If the input is vector (e.g., `c(1,2)`) the 2nd value will be interpreted
#' as the maximum signal integral for the `Tx` curve. Can be set to `NA`, in this
#' case no integrals are taken into account.
#'
#'
#' @param background.integral.min [integer] (**required**):
#' lower bound of the background integral. Can be a [list] of [integer]s, if `object` is
#' of type [list]. If the input is vector (e.g., `c(1,2)`) the 2nd value will be interpreted
#' as the minimum background integral for the `Tx` curve. Can be set to `NA`, in this
#' case no integrals are taken into account.
#'
#'
#' @param background.integral.max [integer] (**required**):
#' upper bound of the background integral. Can be a [list] of [integer]s, if `object` is
#' of type [list]. If the input is vector (e.g., `c(1,2)`) the 2nd value will be interpreted
#' as the maximum background integral for the `Tx` curve. Can be set to `NA`, in this
#' case no integrals are taken into account.
#'
#' @param OSL.component [character] or [integer] (*optional*): s single index
#' or a [character] defining the signal component to be evaluated.
#' It requires that the object was processed by `[OSLdecomposition::RLum.OSL_decomposition]`.
#' This argument can either be the name of the OSL component assigned by
#' `[OSLdecomposition::RLum.OSL_global_fitting]` or the index in the descending
#' order of decay rates. Then `"1"` selects the fastest decaying component, `"2"`
#' the second fastest and so on. Can be a [list] of [integer]s or strings (or mixed)
#' If object is a [list] and this parameter is provided as [list] it alternates over
#' the elements (aliquots) of the object list, e.g., `list(1,2)` processes the first
#' aliquot with component `1` and the second aliquot with component `2`.
#' `NULL` does not process any component.
#'
#' @param rejection.criteria [list] (*with default*):
#' provide a *named* list and set rejection criteria in **percentage**
#' for further calculation. Can be a [list] in a [list], if `object` is of type [list].
#' Note: If an *unnamed* [list] is provided the new settings are ignored!
#'
#' Allowed arguments are `recycling.ratio`, `recuperation.rate`,
#' `palaeodose.error`, `testdose.error` and `exceed.max.regpoint = TRUE/FALSE`.
#' Example: `rejection.criteria = list(recycling.ratio = 10)`.
#' Per default all numerical values are set to 10, `exceed.max.regpoint = TRUE`.
#' Every criterion can be set to `NA`. In this value are calculated, but not considered, i.e.
#' the RC.Status becomes always `'OK'`
#'
#' @param dose.points [numeric] (*optional*):
#' a numeric vector containing the dose points values Using this argument
#' overwrites dose point values in the signal curves. Can be a [list] of
#' [numeric] vectors, if `object` is of type [list]
#'
#' @param mtext.outer [character] (*optional*):
#' option to provide an outer margin `mtext`. Can be a [list] of [character]s,
#' if `object` is of type [list]
#'
#' @param plot [logical] (*with default*): enables or disables plot output.
#'
#' @param plot_onePage [logical] (*with default*): enables or disables on page plot output
#'
#' @param plot.single [logical] (*with default*) or [numeric] (*optional*):
#' single plot output (`TRUE/FALSE`) to allow for plotting the results in single plot windows.
#' If a [numeric] vector is provided the plots can be selected individually, i.e.
#' `plot.single = c(1,2,3,4)` will plot the TL and Lx, Tx curves but not the legend (5) or the
#' growth curve (6), (7) and (8) belong to rejection criteria plots. Requires
#' `plot = TRUE`.
#'
#' @param onlyLxTxTable [logical] (with default): If `TRUE` the dose response
#' curve fitting and plotting is skipped.
#' This allows to get hands on the `Lx/Tx` table for large datasets
#' without the need for a curve fitting.
#'
#' @param ... further arguments that will be passed to the function
#' [plot_GrowthCurve] or [calc_OSLLxTxRatio]
#' (supported: `background.count.distribution`, `sigmab`, `sig0`).
#' **Please note** that if you consider to use the early light subtraction
#' method you should provide your own `sigmab` value!
#
#' @return
#' A plot (*optional*) and an [RLum.Results-class] object is
#' returned containing the following elements:
#'
#' \item{data}{[data.frame] containing De-values, De-error and further parameters}
#' \item{LnLxTnTx.values}{[data.frame] of all calculated Lx/Tx values including signal,
#' background counts and the dose points}
#' \item{rejection.criteria}{[data.frame] with values that might by used as rejection criteria.
#' `NA` is produced if no R0 dose point exists.}
#' \item{Formula}{[formula] formula that have been used for the growth curve fitting }
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @note
#' This function must not be mixed up with the function
#' [Analyse_SAR.OSLdata], which works with
#' [Risoe.BINfileData-class] objects.
#'
#' **The function currently does support only 'OSL', 'IRSL' and 'POSL' data!**
#'
#' @section Function version: 0.9.2
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University
#' (United Kingdom)
#'
#' @seealso [calc_OSLLxTxRatio], [plot_GrowthCurve], [RLum.Analysis-class],
#' [RLum.Results-class], [get_RLum]
#'
#' @references
#' Aitken, M.J. and Smith, B.W., 1988. Optical dating: recuperation
#' after bleaching. Quaternary Science Reviews 7, 387-393.
#'
#' Duller, G., 2003. Distinguishing quartz and feldspar in single grain
#' luminescence measurements. Radiation Measurements, 37 (2), 161-165.
#'
#' Murray, A.S. and Wintle, A.G., 2000. Luminescence dating of quartz using an
#' improved single-aliquot regenerative-dose protocol. Radiation Measurements
#' 32, 57-73.
#'
#' Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. Laboratory
#' fading rates of various luminescence signals from feldspar-rich sediment
#' extracts. Radiation Measurements 43, 1474-1486.
#' doi:10.1016/j.radmeas.2008.06.002
#'
#' @keywords datagen plot
#'
#' @examples
#'
#' ##load data
#' ##ExampleData.BINfileData contains two BINfileData objects
#' ##CWOSL.SAR.Data and TL.SAR.Data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##transform the values from the first position in a RLum.Analysis object
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
#' ##perform SAR analysis and set rejection criteria
#' results <- analyse_SAR.CWOSL(
#' object = object,
#' signal.integral.min = 1,
#' signal.integral.max = 2,
#' background.integral.min = 900,
#' background.integral.max = 1000,
#' log = "x",
#' fit.method = "EXP",
#' rejection.criteria = list(
#'   recycling.ratio = 10,
#'   recuperation.rate = 10,
#'   testdose.error = 10,
#'   palaeodose.error = 10,
#'   exceed.max.regpoint = TRUE)
#')
#'
#' ##show De results
#' get_RLum(results)
#'
#' ##show LnTnLxTx table
#' get_RLum(results, data.object = "LnLxTnTx.table")
#'
#' @md
#' @export
analyse_SAR.CWOSL<- function(
  object,
  signal.integral.min = NA,
  signal.integral.max = NA,
  background.integral.min = NA,
  background.integral.max = NA,
  OSL.component = NULL,
  rejection.criteria = list(),
  dose.points = NULL,
  mtext.outer = "",
  plot = TRUE,
  plot_onePage = FALSE,
  plot.single = FALSE,
  onlyLxTxTable = FALSE,
  ...
) {

# SELF CALL -----------------------------------------------------------------------------------
if(is.list(object)){
  ##clean object input and expand parameters
  object <- .rm_nonRLum(object)
  parm <- .expand_parameters(length(object))

  ##handle main separately
  if("main"%in% names(list(...))){
    if(class(list(...)$main) == "list"){
      main <- rep(list(...)$main,length = length(object))

    }else{
      main <- rep(as.list(list(...)$main),length = length(object))
    }

  }else{
    main <- as.list(paste0("ALQ #",1:length(object)))

  }

  results <- merge_RLum(lapply(1:length(object), function(x){
    analyse_SAR.CWOSL(
      object = object[[x]],
      signal.integral.min = parm$signal.integral.min[[x]],
      signal.integral.max = parm$signal.integral.max[[x]],
      background.integral.min = parm$background.integral.min[[x]],
      background.integral.max = parm$background.integral.max[[x]],
      OSL.component = parm$OSL.component[[x]],
      dose.points = parm$dose.points[[x]],
      mtext.outer = parm$mtext.outer[[x]],
      plot = parm$plot[[x]],
      rejection.criteria = parm$rejection.criteria[[x]],
      plot.single = parm$plot.single[[x]],
      plot_onePage = parm$plot_onePage[[x]],
      onlyLxTxTable = parm$onlyLxTxTable[[x]],
      main = main[[x]],
      ...)
  }))

  ##return
  ##DO NOT use invisible here, this will prevent the function from stopping
  if(length(results) == 0) return(NULL)

  return(results)
}

# CONFIG  -----------------------------------------------------------------
##set error list, this allows to set error messages without breaking the function
error.list <- list()

# General Integrity Checks ---------------------------------------------------
  ##MISSING INPUT
  if(class(object)[1] != "RLum.Analysis")
    stop("[analyse_SAR.CWOSL()] Input object is not of type 'RLum.Analysis'!",
         call. = FALSE)

  ##skip all those tests if signal integral is NA
  if(any(is.na(c(signal.integral.min, signal.integral.max, background.integral.min, background.integral.max)))){
    signal.integral <- background.integral <- NA
    signal.integral.Tx <- background.integral.Tx <- NULL
    warning("[analyse_SAR.CWOSL()] No signal or background integral applied, because they were set to NA!", call. = FALSE)

  } else {
  ##build signal and background integrals
  signal.integral <- c(signal.integral.min[1]:signal.integral.max[1])
  background.integral <- c(background.integral.min[1]:background.integral.max[1])

        ##account for the case that Lx and Tx integral differ
        if (length(signal.integral.min) == 2 &
            length(signal.integral.max) == 2) {
          signal.integral.Tx <-
            c(signal.integral.min[2]:signal.integral.max[2])

        }else{
          signal.integral.Tx <- NULL

        }

        if (length(background.integral.min) == 2 &
            length(background.integral.max) == 2) {
          background.integral.Tx <-
            c(background.integral.min[2]:background.integral.max[2])

        }else{
          background.integral.Tx <- NULL

        }

        ##Account for the case that the use did not provide everything ...
        if(is.null(signal.integral.Tx) & !is.null(background.integral.Tx)){
          signal.integral.Tx <- signal.integral

          warning("[analyse_SAR.CWOSL()] background integral for Tx curves set, but not for the signal integral; signal integral for Tx automatically set.")
        }

      if(!is.null(signal.integral.Tx) & is.null(background.integral.Tx)){
        background.integral.Tx <- background.integral
        warning("[analyse_SAR.CWOSL()] signal integral for Tx curves set, but not for the background integral; background integral for Tx automatically set.")
      }

    ##INTEGRAL LIMITS
    if(!is(signal.integral, "integer") | !is(background.integral, "integer")){
      stop("[analyse_SAR.CWOSL()] 'signal.integral' or 'background.integral' is not of type integer!",  call. = FALSE)
    }
  }

  ## try to extract the correct curves for the sequence based on allowed curve types and
  ## the curve type used most frequently
  ## now remove all non-allowed curves
  CWcurve.type <- regmatches(
    x = names(object),
    m = regexpr("(OSL[a-zA-Z]*|IRSL[a-zA-Z]*|POSL[a-zA-Z]*)", names(object), perl = TRUE))

  if(length(CWcurve.type) == 0) {
    try(stop("[analyse_SAR.CWOSL()] No record of type 'OSL', 'IRSL', 'POSL' detected! NULL returned.",
             call. = FALSE))
     return(NULL)

  }

  ## now get the type which is used most
  CWcurve.type <- names(which.max(table(CWcurve.type)))

# Rejection criteria ------------------------------------------------------
  if(is.null(rejection.criteria) || class(rejection.criteria)[1] != "list")
    rejection.criteria <- list()

  ##set list
    rejection.criteria <- modifyList(x = list(
      recycling.ratio = 10,
      recuperation.rate = 10,
      palaeodose.error = 10,
      testdose.error = 10,
      exceed.max.regpoint = TRUE
    ),
    val = rejection.criteria,
  keep.null = TRUE)

# Deal with extra arguments ----------------------------------------------------
  ##deal with addition arguments
  extraArgs <- list(...)

  main <- if("main" %in% names(extraArgs)) extraArgs$main else  ""
  log <- if("log" %in% names(extraArgs)) extraArgs$log else ""
  cex <- if("cex" %in% names(extraArgs)) extraArgs$cex else 1

  background.count.distribution <-
    if ("background.count.distribution" %in% names(extraArgs)) {
      extraArgs$background.count.distribution
    } else  {
      "non-poisson"
    }

  sigmab <- if("sigmab" %in% names(extraArgs)) extraArgs$sigmab else NULL
  sig0 <- if("sig0" %in% names(extraArgs)) extraArgs$sig0 else 0

# Protocol Integrity Checks --------------------------------------------------
  ##check overall structure of the object
  ##every SAR protocol has to have equal number of curves

  ##grep curve types from analysis value and remove unwanted information
  temp.ltype <- sapply(1:length(object@records), function(x) {
     ##export as global variable
     object@records[[x]]@recordType <<- gsub(" .*", "", object@records[[x]]@recordType)
     object@records[[x]]@recordType

  })

  ##problem: FI lexsyg devices provide irradiation information in a separate curve
  if("irradiation"%in%temp.ltype){

    ##grep irradiation times
    temp.irradiation <- structure_RLum(object)
    temp.irradiation <- temp.irradiation[
      temp.irradiation$recordType == "irradiation", "x.max"]

    ##remove every 2nd entry (test dose) and add "0" dose for natural signal
    temp.Dose <- c(0,temp.irradiation)

    ##remove irradiation entries from file
    object <- set_RLum(class = "RLum.Analysis",
               records = get_RLum(object, recordType = c(CWcurve.type, "TL")),
               protocol = "SAR")

  }

  ##check if the wanted curves are a multiple of two
  ##gsub removes unwanted information from the curves
  if(table(temp.ltype)[CWcurve.type]%%2!=0){
    error.list[[1]] <- "[analyse_SAR.CWOSL()] Input OSL/IRSL curves are not a multiple of two."
  }

  ##check if the curve lengths differ
  temp.matrix.length <- unlist(sapply(1:length(object@records), function(x) {
                          if(object@records[[x]]@recordType==CWcurve.type){
                              length(object@records[[x]]@data[,1])
                          }
  }))

  if(length(unique(temp.matrix.length))!=1){
    error.list[[2]] <- "[analyse_SAR.CWOSL()] Input curves lengths differ."

  }

  ##just proceed if error list is empty
  if (length(error.list) == 0) {

    ##check background integral
    if (!all(is.na(signal.integral)) &&
        max(signal.integral) == min(signal.integral)) {
      signal.integral <-
        c(min(signal.integral) : (max(signal.integral) + 1))

      warning("[analyse_SAR.CWOSL()] integral signal limits cannot be equal, reset automatically!")

    }

    ##background integral should not be longer than curve channel length
    if (!all(is.na(background.integral)) &&
             max(background.integral) == min(background.integral)) {
      background.integral <-
        c((min(background.integral) - 1) : max(background.integral))

    }

    if (!all(is.na(background.integral)) &&
        max(background.integral) > temp.matrix.length[1]) {
      background.integral <-
          c((temp.matrix.length[1] - length(background.integral)):temp.matrix.length[1])

      ##prevent that the background integral becomes negative
      if(min(background.integral) < max(signal.integral)){
        background.integral <- c((max(signal.integral) + 1):max(background.integral))

      }

      warning("[analyse_SAR.CWOSL()] Background integral out of bounds. Set to: c(",
        min(background.integral),":", max(background.integral),")", call. = FALSE
      )

    }

    ##Do the same for the Tx-if set
    if (!is.null(background.integral.Tx)) {
      if (max(background.integral.Tx) == min(background.integral.Tx)) {
        background.integral.Tx <-
          c((min(background.integral.Tx) - 1) : max(background.integral.Tx))

      }

      if (max(background.integral.Tx) > temp.matrix.length[2]) {
        background.integral.Tx <-
          c((temp.matrix.length[2] - length(background.integral.Tx)):temp.matrix.length[2])


        ##prevent that the background integral becomes negative
        if (min(background.integral.Tx) < max(signal.integral.Tx)) {
          background.integral.Tx <-
            c((max(signal.integral.Tx) + 1):max(background.integral.Tx))


        }

        warning(
          "Background integral for Tx out of bounds. Set to: c(",
          min(background.integral.Tx),
          ":",
          max(background.integral.Tx),
          ")"
        )

      }
    }


  # Grep Curves -------------------------------------------------------------
   ##grep relevant curves from RLum.Analyis object
    OSL.Curves.ID <-
      get_RLum(object, recordType = CWcurve.type, get.index = TRUE)

    ##separate curves by Lx and Tx (it makes it much easier)
    OSL.Curves.ID.Lx <-
      OSL.Curves.ID[seq(1,length(OSL.Curves.ID),by = 2)]
    OSL.Curves.ID.Tx <-
      OSL.Curves.ID[seq(2,length(OSL.Curves.ID),by = 2)]

    ##get index of TL curves
    TL.Curves.ID <-
      suppressWarnings(get_RLum(object, recordType = "TL$", get.index = TRUE))

    ##separate TL curves which is always coming before the OSL curve
    ##Note: we do not check anymore whether the sequence makes sense.
    TL.Curves.ID.Lx <- TL.Curves.ID[TL.Curves.ID%in%(OSL.Curves.ID.Lx - 1)]
    TL.Curves.ID.Tx <- TL.Curves.ID[TL.Curves.ID%in%(OSL.Curves.ID.Tx - 1)]

# Calculate LnLxTnTx values  --------------------------------------------------
  ##calculate LxTx values using external function
  LnLxTnTx <- try(lapply(seq(1,length(OSL.Curves.ID),by = 2), function(x){
      if(!is.null(OSL.component) && length(OSL.component) > 0){
       temp.LnLxTnTx <- get_RLum(
          calc_OSLLxTxDecomposed(
            Lx.data = object@records[[OSL.Curves.ID[x]]]@info$COMPONENTS,
            Tx.data = object@records[[OSL.Curves.ID[x + 1]]]@info$COMPONENTS,
            OSL.component = OSL.component,
            digits = 4,
            sig0 = sig0))

      } else {
       temp.LnLxTnTx <- get_RLum(
          calc_OSLLxTxRatio(
            Lx.data = object@records[[OSL.Curves.ID[x]]]@data,
            Tx.data = object@records[[OSL.Curves.ID[x + 1]]]@data,
            signal.integral = signal.integral,
            signal.integral.Tx = signal.integral.Tx,
            background.integral = background.integral,
            background.integral.Tx = background.integral.Tx,
            background.count.distribution = background.count.distribution,
            sigmab = sigmab,
            sig0 = sig0))
      }

        ##grep dose
        if (exists("temp.irradiation") == FALSE) {
          temp.Dose <- object@records[[OSL.Curves.ID[x]]]@info$IRR_TIME

          ##for the case that no information on the dose can be found
          if (is.null(temp.Dose)) {
            temp.Dose <- NA
          }

          temp.LnLxTnTx <-
            cbind(Dose = temp.Dose, temp.LnLxTnTx)

        }else{
          temp.LnLxTnTx <- cbind(Dose = temp.Dose[x], temp.LnLxTnTx)

        }
      }), silent = TRUE)

    ##this is basically for the OSL.component case to avoid that everything
    ##fails if something goes wrong therein
    if(inherits(LnLxTnTx, "try-error")){
      try(stop(
        "[analyse_SAR.CWOSL()] Something went wrong while generating the LxTx-table. Return NULL.",
         call. = FALSE))
      return(NULL)
    }

    ##combine
    LnLxTnTx <- data.table::rbindlist(LnLxTnTx)

    # Set regeneration points -------------------------------------------------
    ##overwrite dose point manually
    if (!is.null(dose.points) & length(dose.points) > 0) {
      if (length(dose.points) != length(LnLxTnTx$Dose)) {
        stop("[analyse_SAR.CWOSL()] length 'dose.points' differs from number of curves.",
           call. = FALSE)
      }

      LnLxTnTx$Dose <- dose.points
    }

    ##check whether we have dose points at all
    if (is.null(dose.points) & anyNA(LnLxTnTx$Dose)) {
      stop("[analyse_SAR.CWOSL()] 'dose.points' contains NA values or have not been set!",
           call. = FALSE)

    }

    ##check whether the first OSL/IRSL curve (i.e., the Natural) has 0 dose. If not
    ##not, it is probably a Dose Recovery Test with the given dose that is treated as the
    ##unknown dose. We overwrite this value and warn the user.
    if (LnLxTnTx$Dose[1] != 0) {
      warning("[analyse_SAR.CWOSL()] The natural signal has a dose of ", LnLxTnTx$Dose[1],
              " s, which is indicative of a dose recovery test. The natural dose was set to 0.", call. = FALSE)
      LnLxTnTx$Dose[1] <- 0
    }

    #generate unique dose id - this are also the # for the generated points
    temp.DoseID <- c(0:(length(LnLxTnTx$Dose) - 1))
    temp.DoseName <- paste("R",temp.DoseID,sep = "")
    temp.DoseName <-
      cbind(Name = temp.DoseName,Dose = LnLxTnTx$Dose)


    ##set natural
    temp.DoseName[temp.DoseName[,"Name"] == "R0","Name"] <-
      "Natural"

    ##set R0
    temp.DoseName[temp.DoseName[,"Name"] != "Natural" &
                    temp.DoseName[,"Dose"] == 0,"Name"] <- "R0"

    ##correct numeration numeration of other dose points

    ##how many dose points do we have with 0?
    non.temp.zero.dose.number <- nrow(temp.DoseName[temp.DoseName[, "Dose"] != 0,])

    temp.DoseName[temp.DoseName[,"Name"] != "Natural" &
                    temp.DoseName[,"Name"] != "R0","Name"] <- paste("R",c(1:non.temp.zero.dose.number),sep =
                                                                      "")

    ##find duplicated doses (including 0 dose - which means the Natural)
    temp.DoseDuplicated <- duplicated(temp.DoseName[,"Dose"])

    ##combine temp.DoseName
    temp.DoseName <-
      cbind(temp.DoseName,Repeated = temp.DoseDuplicated)

    ##correct value for R0 (it is not really repeated)
    temp.DoseName[temp.DoseName[,"Dose"] == 0,"Repeated"] <- FALSE

    ##combine in the data frame
    temp.LnLxTnTx <- data.frame(Name = temp.DoseName[,"Name"],
                                Repeated = as.logical(temp.DoseName[,"Repeated"]))

    LnLxTnTx <- cbind(temp.LnLxTnTx,LnLxTnTx)
    LnLxTnTx[,"Name"] <- as.character(LnLxTnTx[,"Name"])

    # Calculate Recycling Ratio -----------------------------------------------
    ##Calculate Recycling Ratio
    if (length(LnLxTnTx[LnLxTnTx[,"Repeated"] == TRUE,"Repeated"]) > 0) {
      ##identify repeated doses
      temp.Repeated <-
        LnLxTnTx[LnLxTnTx[,"Repeated"] == TRUE,c("Name","Dose","LxTx")]

      ##find concerning previous dose for the repeated dose
      temp.Previous <-
        t(sapply(1:length(temp.Repeated[,1]),function(x) {
          LnLxTnTx[LnLxTnTx[,"Dose"] == temp.Repeated[x,"Dose"] &
                     LnLxTnTx[,"Repeated"] == FALSE,c("Name","Dose","LxTx")]
        }))


      ##convert to data.frame
      temp.Previous <- as.data.frame(temp.Previous)

      ##set column names
      temp.ColNames <-
        unlist(lapply(1:length(temp.Repeated[,1]),function(x) {
          temp <- paste("Recycling ratio (", temp.Repeated[x,"Name"],"/",
                temp.Previous[temp.Previous[,"Dose"] == temp.Repeated[x,"Dose"],"Name"],
                ")",
                sep = "")
          return(temp[1])
        }))


      ##Calculate Recycling Ratio
      RecyclingRatio <-
        round(as.numeric(temp.Repeated[,"LxTx"]) / as.numeric(temp.Previous[,"LxTx"]),
              digits = 4)

      ##Just transform the matrix and add column names
      RecyclingRatio <- t(RecyclingRatio)
      colnames(RecyclingRatio) <- temp.ColNames

    }else{
      RecyclingRatio <- NA
    }


    # Calculate Recuperation Rate ---------------------------------------------
    ##Recuperation Rate (capable to handle multiple type of recuperation values)
    if (length(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","Name"]) > 0) {
      Recuperation <-
        sapply(1:length(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","Name"]),
               function(x) {
                 round(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","LxTx"][x] /
                         LnLxTnTx[LnLxTnTx[,"Name"] == "Natural","LxTx"],
                       digits = 4)
               })
      ##Just transform the matrix and add column names
      Recuperation  <-  t(Recuperation)
      colnames(Recuperation)  <-
        unlist(strsplit(paste(
          "Recuperation rate",
          1:length(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","Name"]), collapse = ";"
        ), ";"))

    }else{
      Recuperation <- NA
    }


    # Evaluate and Combine Rejection Criteria ---------------------------------
    temp.criteria <- c(
      if(!is.null(colnames(RecyclingRatio))){
       colnames(RecyclingRatio)}else{NA},
      if(!is.null(colnames(Recuperation))){
        colnames(Recuperation)}else{NA})

    temp.value <- c(RecyclingRatio,Recuperation)

    temp.threshold <-
      c(rep(
        rejection.criteria$recycling.ratio / 100, length(RecyclingRatio)
      ),
      rep(
        rejection.criteria$recuperation.rate / 100,
        length(Recuperation)
      ))

    ##RecyclingRatio
    temp.status.RecyclingRatio <- rep("OK", length(RecyclingRatio))
    if (!any(is.na(RecyclingRatio)) & !is.na(rejection.criteria$recycling.ratio))
      temp.status.RecyclingRatio[abs(1 - RecyclingRatio) > (rejection.criteria$recycling.ratio / 100)] <- "FAILED"

    ##Recuperation
    if (!is.na(Recuperation)[1] &
        !is.na(rejection.criteria$recuperation.rate)) {
      temp.status.Recuperation  <-
        sapply(1:length(Recuperation), function(x) {
          if (Recuperation[x] > rejection.criteria$recuperation.rate / 100) {
            "FAILED"

          } else{
            "OK"

          }

        })

    } else{
      temp.status.Recuperation <- "OK"

    }


    # Provide Rejection Criteria for Testdose error --------------------------
    testdose.error.calculated <- (LnLxTnTx$Net_TnTx.Error/LnLxTnTx$Net_TnTx)[1]

    testdose.error.threshold <-
      rejection.criteria$testdose.error / 100

    if (is.na(testdose.error.calculated)) {
      testdose.error.status <- "FAILED"

    }else{
      if(!is.na(testdose.error.threshold)){
        testdose.error.status <- ifelse(
          testdose.error.calculated <= testdose.error.threshold,
          "OK", "FAILED"
        )

      }else{
        testdose.error.status <- "OK"

      }

    }

    testdose.error.data.frame <- data.frame(
      Criteria = "Testdose error",
      Value = testdose.error.calculated,
      Threshold = testdose.error.threshold,
      Status =  testdose.error.status,
      stringsAsFactors = FALSE
    )


    RejectionCriteria <- data.frame(
      Criteria = temp.criteria,
      Value = temp.value,
      Threshold = temp.threshold,
      Status = c(temp.status.RecyclingRatio,temp.status.Recuperation),
      stringsAsFactors = FALSE
    )

    RejectionCriteria <- rbind(RejectionCriteria, testdose.error.data.frame)

    ##========================================================================##
    ##PLOTTING
    ##========================================================================##
    if (plot) {
      ##make sure the par settings are good after the functions stops
      ##Why this is so complicated? Good question, if par() is called in the
      ##single mode, it starts a new plot and then subsequent functions like
      ##analyse_pIRIRSequence() produce an odd plot output.
      par.default <- par()[c("oma","mar","cex", "mfrow", "mfcol")]
      on_exit <- function(x = par.default){
        par(
          oma = x$oma,
          mar = x$mar,
          cex = x$cex,
          mfrow = x$mfrow,
          mfcol = x$mfcol
        )
      }

      ##colours and double for plotting
      col <- get("col", pos = .LuminescenceEnv)

      # plot everyting on one page ... doing it here is much cleaner than
      # Plotting - one Page config -------------------------------------------------------
      if(plot_onePage){
      on.exit(on_exit())

      plot.single <- TRUE
      layout(matrix(
        c(1, 1, 3, 3, 6, 6, 7,
          1, 1, 3, 3, 6, 6, 8,
          2, 2, 4, 4, 9, 9, 10,
          2, 2, 4, 4, 9, 9, 10,
          5, 5, 5, 5, 5, 5, 5), 5, 7, byrow = TRUE
      ))
      par(oma = c(0, 0, 0, 0),
          mar = c(4, 4, 3, 1),
          cex = cex * 0.6)

      }


      # Plotting - old way config -------------------------------------------------------
      if (plot.single[1] == FALSE) {
        on.exit(on_exit())
        layout(matrix(
          c(1, 1, 3, 3,
            1, 1, 3, 3,
            2, 2, 4, 4,
            2, 2, 4, 4,
            5, 5, 5, 5), 5, 4, byrow = TRUE
        ))

        par(
          oma = c(0,0,0,0), mar = c(4,4,3,3), cex = cex * 0.6
        )

        ## 1 -> TL previous LnLx
        ## 2 -> LnLx
        ## 3 -> TL previous TnTx
        ## 4 -> TnTx
        ## 5 -> Legend

        ## set selected curves to allow plotting of all curves
        plot.single.sel <- c(1,2,3,4,5,6,7,8)

      }else{
        ##check for values in the single output of the function and convert
        if (!is(plot.single, "logical")) {
          if (!is(plot.single, "numeric")) {
            stop("[analyse_SAR.CWOSL()] Invalid data type for 'plot.single'.")
          }

          plot.single.sel  <- plot.single

        }else{
          plot.single.sel <- c(1,2,3,4,5,6,7,8)

        }

      }

      ##warning if number of curves exceed colour values
      if (length(col) < length(OSL.Curves.ID) / 2) {
        temp.message  <-
          paste(
            "\n[analyse_SAR.CWOSL()] To many curves! Only the first",
            length(col),"curves are plotted!"
          )
        warning(temp.message)
      }

      ##legend text
      legend.text <-
        paste(LnLxTnTx$Name,"\n(",LnLxTnTx$Dose,")", sep = "")


      ##get channel resolution (should be equal for all curves)
      resolution.OSLCurves <- round(object@records[[OSL.Curves.ID[1]]]@data[2,1] -
                                      object@records[[OSL.Curves.ID[1]]]@data[1,1],
                                    digits = 2)


      # Plotting TL Curves previous LnLx ----------------------------------------

      ##overall plot option selection for plot.single.sel
      if (1 %in% plot.single.sel) {
        ##check if TL curves are available
        if (length(TL.Curves.ID.Lx) > 0) {
          ##It is just an approximation taken from the data
          resolution.TLCurves <-  round(mean(diff(
            round(object@records[[TL.Curves.ID.Lx[[1]]]]@data[,1], digits = 1)
          )), digits = 1)

          ylim.range <- vapply(TL.Curves.ID.Lx, function(x) {
              range(object@records[[x]]@data[,2])

            }, numeric(2))

          plot(
            NA,NA,
            xlab = "T [\u00B0C]",
            ylab = paste("TL [cts/",resolution.TLCurves," \u00B0C]",sep =
                           ""),
            xlim = c(object@records[[TL.Curves.ID.Lx[[1]]]]@data[1,1],
                     max(object@records[[TL.Curves.ID.Lx[[1]]]]@data[,1])),
            ylim = c(1,max(ylim.range)),
            main = main,
            log = if (log == "y" | log == "xy") {
              "y"
            }else{
              ""
            }
          )

          #provide curve information as mtext, to keep the space for the header
          mtext(side = 3,
                expression(paste(
                  "TL previous ", L[n],",",L[x]," curves",sep = ""
                )),
                cex = cex * 0.7)

          ##plot TL curves
          sapply(1:length(TL.Curves.ID.Lx) ,function(x) {
            lines(object@records[[TL.Curves.ID.Lx[[x]]]]@data,col = col[x])

          })



        }else{
          plot(
            NA,NA,xlim = c(0,1), ylim = c(0,1), main = "",
            axes = FALSE,
            ylab = "",
            xlab = ""
          )
          text(0.5,0.5, "No TL curve detected")

        }
      }#plot.single.sel

      # Plotting LnLx Curves ----------------------------------------------------
      ##overall plot option selection for plot.single.sel
      if (2 %in% plot.single.sel) {
        ylim.range <- vapply(OSL.Curves.ID.Lx, function(x) {
          range(object@records[[x]]@data[,2])
        }, numeric(2))

        if((log == "x" | log == "xy") & object@records[[OSL.Curves.ID.Lx[[1]]]]@data[1,1] == 0){
          xlim <- c(object@records[[OSL.Curves.ID.Lx[1]]]@data[2,1],
                    max(object@records[[OSL.Curves.ID.Lx[1]]]@data[,1]) +
                      object@records[[OSL.Curves.ID.Lx[1]]]@data[2,1])

        }else{
        xlim  <- c(object@records[[OSL.Curves.ID.Lx[1]]]@data[1,1],
                   max(object@records[[OSL.Curves.ID.Lx[1]]]@data[,1]))

        }
        #open plot area LnLx
        plot(
          NA,NA,
          xlab = "Time [s]",
          ylab = paste(CWcurve.type," [cts/",resolution.OSLCurves," s]",sep =
                         ""),
          xlim = xlim,
          ylim = range(ylim.range),
          main = main,
          log = log
        )

        #provide curve information as mtext, to keep the space for the header
        mtext(side = 3, expression(paste(L[n],",",L[x]," curves",sep = "")),
              cex = cex * 0.7)

        ##plot curves
        sapply(1:length(OSL.Curves.ID.Lx), function(x) {
          if((log == "x" | log == "xy") & object@records[[OSL.Curves.ID.Lx[[x]]]]@data[1,1] == 0){
            object@records[[OSL.Curves.ID.Lx[[x]]]]@data[1,] <-
              object@records[[OSL.Curves.ID.Lx[[x]]]]@data[1,] +
              diff(c(object@records[[OSL.Curves.ID.Lx[[x]]]]@data[1,1],
                     object@records[[OSL.Curves.ID.Lx[[x]]]]@data[2,1]))
            warnings("[analyse_SAR.CWOSL()] curves shifted by one chanel for log-plot.")
          }
          lines(object@records[[OSL.Curves.ID.Lx[[x]]]]@data,col = col[x])

        })

        ##mark integration limit Lx curves
        abline(v = c(
            object@records[[OSL.Curves.ID.Lx[1]]]@data[min(signal.integral),1],
            object@records[[OSL.Curves.ID.Lx[1]]]@data[max(signal.integral),1],
            object@records[[OSL.Curves.ID.Lx[1]]]@data[min(background.integral),1],
            object@records[[OSL.Curves.ID.Lx[1]]]@data[max(background.integral),1]),
          lty = 2,
          col = "gray")

        ##mtext, implemented here, as a plot window has to be called first
        mtext(
          mtext.outer,
          side = 4,
          outer = TRUE,
          line = -1.7,
          cex = cex,
          col = "blue"
        )

      }# plot.single.sel

      # Plotting TL Curves previous TnTx ----------------------------------------

      ##overall plot option selection for plot.single.sel
      if (3 %in% plot.single.sel) {
        ##check if TL curves are available
        if (length(TL.Curves.ID.Tx) > 0) {
          ##It is just an approximation taken from the data
          resolution.TLCurves <-  round(mean(diff(
            round(object@records[[TL.Curves.ID.Tx[[1]]]]@data[,1], digits = 1)
          )), digits = 1)

          ylim.range <- vapply(TL.Curves.ID.Tx, function(x) {
            range(object@records[[x]]@data[,2])
          }, numeric(2))

          plot(
            NA,NA,
            xlab = "T [\u00B0C]",
            ylab = paste("TL [cts/",resolution.TLCurves," \u00B0C]",sep = ""),
            xlim = c(object@records[[TL.Curves.ID.Tx[[1]]]]@data[1,1],
                     max(object@records[[TL.Curves.ID.Tx[[1]]]]@data[,1])),
            ylim = c(1,max(ylim.range)),
            main = main,
            log = if (log == "y" | log == "xy") {
              "y"
            }else{
              ""
            }
          )

          #provide curve information as mtext, to keep the space for the header
          mtext(side = 3,
                expression(paste(
                  "TL previous ", T[n],",",T[x]," curves",sep = ""
                )),
                cex = cex * 0.7)

          ##plot TL curves
          sapply(1:length(TL.Curves.ID.Tx) ,function(x) {
            lines(object@records[[TL.Curves.ID.Tx[[x]]]]@data,col = col[x])

          })



        }else{
          plot(
            NA,NA,xlim = c(0,1), ylim = c(0,1), main = "",
            axes = FALSE,
            ylab = "",
            xlab = ""
          )
          text(0.5,0.5, "No TL curve detected")

        }

      }#plot.single.sel

      # Plotting TnTx Curves ----------------------------------------------------
      ##overall plot option selection for plot.single.sel
      if (4 %in% plot.single.sel) {
        ylim.range <- vapply(OSL.Curves.ID.Tx, function(x) {
          range(object@records[[x]]@data[,2])
        }, numeric(2))

        if((log == "x" | log == "xy") & object@records[[OSL.Curves.ID.Tx[[1]]]]@data[1,1] == 0){
          xlim <- c(object@records[[OSL.Curves.ID.Tx[1]]]@data[2,1],
                    max(object@records[[OSL.Curves.ID.Tx[1]]]@data[,1]) +
                      object@records[[OSL.Curves.ID.Tx[1]]]@data[2,1])


        }else{
          xlim <- c(object@records[[OSL.Curves.ID.Tx[1]]]@data[1,1],
                    max(object@records[[OSL.Curves.ID.Tx[1]]]@data[,1]))
        }

        #open plot area LnLx
        plot(
          NA,NA,
          xlab = "Time [s]",
          ylab = paste0(CWcurve.type ," [cts/",resolution.OSLCurves," s]"),
          xlim = xlim,
          ylim = range(ylim.range),
          main = main,
          log = log
        )

        #provide curve information as mtext, to keep the space for the header
        mtext(side = 3,
              expression(paste(T[n],",",T[x]," curves",sep = "")),
              cex = cex * 0.7)

        ##plot curves and get legend values
        sapply(1:length(OSL.Curves.ID.Tx) ,function(x) {

          ##account for log-scale and 0 values
          if((log == "x" | log == "xy") & object@records[[OSL.Curves.ID.Tx[[x]]]]@data[1,1] == 0){
            object@records[[OSL.Curves.ID.Tx[[x]]]]@data[1,] <-
              object@records[[OSL.Curves.ID.Tx[[x]]]]@data[1,] +
                 diff(c(object@records[[OSL.Curves.ID.Tx[[x]]]]@data[1,1],
                      object@records[[OSL.Curves.ID.Tx[[x]]]]@data[2,1]))

            warnings("[analyse_SAR.CWOSL()] curves shifted by one chanel for log-plot.")

          }

          lines(object@records[[OSL.Curves.ID.Tx[[x]]]]@data,col = col[x])

        })

        ##mark integration limit Tx curves
        abline(v = c(
          object@records[[OSL.Curves.ID.Tx[1]]]@data[min(signal.integral),1],
          object@records[[OSL.Curves.ID.Tx[1]]]@data[max(signal.integral),1],
          object@records[[OSL.Curves.ID.Tx[1]]]@data[min(background.integral),1],
          object@records[[OSL.Curves.ID.Tx[1]]]@data[max(background.integral),1]),
          lty = 2,
          col = "gray")


      }# plot.single.sel

      # Plotting Legend ----------------------------------------
      ##overall plot option selection for plot.single.sel
      if (5 %in% plot.single.sel) {
        par.margin  <- par()$mar
        par.mai  <- par()$mai
        par(mar = c(1,1,1,1), mai = c(0,0,0,0))

        plot(
          c(1:(length(
            OSL.Curves.ID
          ) / 2)),
          rep(7,length(OSL.Curves.ID) / 2),
          type = "p",
          axes = FALSE,
          xlab = "",
          ylab = "",
          pch = 20,
          col = unique(col[1:length(OSL.Curves.ID)]),
          cex = 4 * cex,
          ylim = c(0,10)
        )

        ##add text
        text(c(1:(length(
          OSL.Curves.ID
        ) / 2)),
        rep(7,length(OSL.Curves.ID) / 2),
        legend.text,
        offset = 1,
        pos = 1)


        ##add line
        abline(h = 10,lwd = 0.5)

        #reset margin
        par(mar = par.margin, mai = par.mai)

      }#plot.single.sel


    }##end plot


    # Plotting  GC  ----------------------------------------

    ##create data.frame
    temp.sample <- data.frame(
      Dose = LnLxTnTx$Dose,
      LxTx = LnLxTnTx$LxTx,
      LxTx.Error = LnLxTnTx$LxTx.Error,
      TnTx = LnLxTnTx$Net_TnTx
    )


    ##overall plot option selection for plot.single.sel
    if (plot == TRUE && 6 %in% plot.single.sel) {
      plot  <-  TRUE

    }else {
      plot  <- FALSE

    }

    ##Fit and plot growth curve
    if(!onlyLxTxTable){
      temp.GC <- do.call(plot_GrowthCurve, args = modifyList(
          list(
            sample = temp.sample,
            output.plot = plot,
            output.plotExtended.single = plot_onePage,
            cex.global = if(plot_onePage) .6 else 1
            ),
          list(...)
        ))

        ##if null
        if(is.null(temp.GC)){
          temp.GC <- data.frame(
            De = NA,
            De.Error = NA,
            D01 = NA,
            D01.ERROR = NA,
            D02 = NA,
            D02.ERROR = NA,
            De.MC = NA,
            Fit = NA,
            RC.Status = NA,
            stringsAsFactors = FALSE
          )
          temp.GC.fit.Formula <- NA

          ##create empty plots if needed, otherwise subsequent functions may crash
          if(plot){
            if("output.plotExtended" %in% list(...) && list(...)$output.plotExtended == FALSE){
              shape::emptyplot()

            }else{
              shape::emptyplot()
              shape::emptyplot()
              shape::emptyplot()

            }

          }

        }else{

          ##grep information on the fit object
          temp.GC.fit.Formula  <- get_RLum(temp.GC, "Formula")

          ##grep results
          temp.GC <- get_RLum(temp.GC)

          # Provide Rejection Criteria for Palaeodose error --------------------------
          if(is.na(temp.GC[,1])){
            palaeodose.error.calculated <- NA

          }else{
            palaeodose.error.calculated <- round(temp.GC[,2] / temp.GC[,1], digits = 5)

          }

          palaeodose.error.threshold <-
            rejection.criteria$palaeodose.error / 100

          if (is.na(palaeodose.error.calculated)) {
            palaeodose.error.status <- "FAILED"

          }else{
            if(!is.na(palaeodose.error.threshold)){
              palaeodose.error.status <- ifelse(
                palaeodose.error.calculated <= palaeodose.error.threshold,
                "OK", "FAILED"
              )


            }else{
              palaeodose.error.status <- "OK"


            }

          }

          palaeodose.error.data.frame <- data.frame(
            Criteria = "Palaeodose error",
            Value = palaeodose.error.calculated,
            Threshold = palaeodose.error.threshold,
            Status =  palaeodose.error.status,
            stringsAsFactors = FALSE
          )


          ##add exceed.max.regpoint
          if (!is.na(temp.GC[,1]) & !is.na(rejection.criteria$exceed.max.regpoint) && rejection.criteria$exceed.max.regpoint) {
            status.exceed.max.regpoint <-
              ifelse(max(LnLxTnTx$Dose) < temp.GC[,1], "FAILED", "OK")

          }else{
            status.exceed.max.regpoint <- "OK"

          }

          exceed.max.regpoint.data.frame <- data.frame(
            Criteria = "De > max. dose point",
            Value = as.numeric(temp.GC[,1]),
            Threshold = if(is.na(rejection.criteria$exceed.max.regpoint)){
                NA
              }else if(!rejection.criteria$exceed.max.regpoint){
                Inf
              }else{
                as.numeric(max(LnLxTnTx$Dose))
              },
            Status =  status.exceed.max.regpoint
          )


          ##add to RejectionCriteria data.frame
          RejectionCriteria <- rbind(RejectionCriteria,
                                     palaeodose.error.data.frame,
                                     exceed.max.regpoint.data.frame)


        ##add recjection status
        if (length(grep("FAILED",RejectionCriteria$Status)) > 0) {
          temp.GC <- data.frame(temp.GC, RC.Status = "FAILED", stringsAsFactors = FALSE)

        }else{
          temp.GC <- data.frame(temp.GC, RC.Status = "OK", stringsAsFactors = FALSE)

        }
       }#endif for is.null

     ##end onlyLxTxTable
     }else{
       temp.GC <- data.frame(
         De = NA, De.Error = NA,
         D01 = NA, D01.ERROR = NA, D02 = NA, D02.ERROR = NA,
         De.MC = NA, Fit = NA)
       temp.GC.fit.Formula <- NULL
     }

      ##add information on the integration limits
      temp.GC.extended <-
        data.frame(
          signal.range = paste(min(signal.integral),":",
                               max(signal.integral)),
          background.range = paste(min(background.integral),":",
                                   max(background.integral)),
          signal.range.Tx = paste(min(ifelse(is.null(signal.integral.Tx),NA,signal.integral.Tx)),":",
                                  max(ifelse(is.null(signal.integral.Tx),NA,signal.integral.Tx))),
          background.range.Tx = paste(min(ifelse(is.null(background.integral.Tx), NA,background.integral.Tx)) ,":",
                                      max(ifelse(is.null(background.integral.Tx), NA,background.integral.Tx))),
          stringsAsFactors = FALSE
        )


# Set return Values -----------------------------------------------------------
    ##generate unique identifier
    UID <- create_UID()

    temp.results.final <- set_RLum(
      class = "RLum.Results",
      data = list(
        data = as.data.frame(c(temp.GC, temp.GC.extended, UID = UID), stringsAsFactors = FALSE),
        LnLxTnTx.table = cbind(LnLxTnTx, UID = UID, stringsAsFactors = FALSE),
        rejection.criteria = cbind(RejectionCriteria, UID, stringsAsFactors = FALSE),
        Formula = temp.GC.fit.Formula
      ),
      info = list(call = sys.call())
    )

    # Plot graphical interpretation of rejection criteria -----------------------------------------

    if (plot && 7 %in% plot.single.sel) {
      ##set graphical parameter
      if (!plot.single[1]) par(mfrow = c(1,2))

      ##Rejection criteria
      temp.rejection.criteria <- get_RLum(temp.results.final,
                                          data.object = "rejection.criteria")

      temp.rc.reycling.ratio <- temp.rejection.criteria[
        grep("Recycling ratio",temp.rejection.criteria[,"Criteria"]),]

      temp.rc.recuperation.rate <- temp.rejection.criteria[
        grep("Recuperation rate",temp.rejection.criteria[,"Criteria"]),]

      temp.rc.palaedose.error <- temp.rejection.criteria[
        grep("Palaeodose error",temp.rejection.criteria[,"Criteria"]),]

      temp.rc.testdose.error <- temp.rejection.criteria[
        grep("Testdose error",temp.rejection.criteria[,"Criteria"]),]

      plot(
        NA,NA,
        xlim = c(-0.5,0.5),
        ylim = c(0,40),
        yaxt = "n", ylab = "",
        xaxt = "n", xlab = "",
        bty = "n",
        main = "Rejection criteria"
      )

      axis(
        side = 1, at = c(-0.2,-0.1,0,0.1,0.2), labels = c("- 0.2", "- 0.1","0/1","+ 0.1", "+ 0.2")
      )

      ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
      ##polygon for recycling ratio
      text(x = -0.35, y = 35, "Recycling R.", pos = 3, srt = 90, cex = 0.8*cex, offset = 0)
      polygon(x = c(
          -as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1],-as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1],
          as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1],
          as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1]
        ),
        y = c(31,39,39,31),
        col = "gray",
        border = NA)

      polygon(
        x = c(-0.3, -0.3, 0.3, 0.3) ,
        y = c(31, 39, 39, 31),
        border = ifelse(any(
          grepl(pattern = "FAILED", temp.rc.reycling.ratio$Status)
        ), "red", "black"))


      ##consider possibility of multiple pIRIR signals and multiple recycling ratios
      if (nrow(temp.rc.recuperation.rate) > 0) {
        col.id  <- 1
        for (i in seq(1,nrow(temp.rc.recuperation.rate),
                      length(unique(temp.rc.recuperation.rate[,"Criteria"])))) {
          for (j in 0:length(unique(temp.rc.recuperation.rate[,"Criteria"]))) {
            points(
              temp.rc.reycling.ratio[i + j, "Value"] - 1,
              y = 35,
              pch = col.id,
              col = col.id,
              cex = 1.3 * cex
            )

          }
          col.id <- col.id + 1
        }
        rm(col.id)

        ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
        ##polygon for recuperation rate
        text(
          x = -0.35, y = 25, "Recuperation", pos = 3, srt = 90, cex = 0.8*cex, offset = 0,
        )
        polygon(
          x = c(
            0,
            0,
            as.numeric(as.character(
              temp.rc.recuperation.rate$Threshold
            ))[1],
            as.numeric(as.character(
              temp.rc.recuperation.rate$Threshold
            ))[1]
          ),
          y = c(21,29,29,21),
          col = "gray",
          border = NA
        )

        polygon(
          x = c(-0.3, -0.3, 0.3, 0.3) ,
          y = c(21, 29, 29, 21),
          border = ifelse(any(
            grepl(pattern = "FAILED", temp.rc.recuperation.rate$Status)
          ), "red", "black")
        )
        polygon(
          x = c(-0.3,-0.3,0,0) , y = c(21,29,29,21), border = NA, density = 10, angle = 45
        )

        for (i in 1:nrow(temp.rc.recuperation.rate)) {
          points(
            temp.rc.recuperation.rate[i, "Value"],
            y = 25,
            pch = i,
            col = i,
            cex = 1.3 * cex
          )

        }
      }

      ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
      ##polygon for testdose error
      text(
        x = -0.35, y = 15, "Testdose Err.", pos = 3, srt = 90, cex = 0.8*cex, offset = 0,
      )

      polygon(
        x = c(
          0,
          0,
          as.numeric(as.character(temp.rc.testdose.error$Threshold))[1],
          as.numeric(as.character(temp.rc.testdose.error$Threshold))[1]
        ),
        y = c(11,19,19,11),
        col = "gray",
        border = NA
      )
      polygon(
        x = c(-0.3, -0.3, 0.3, 0.3) ,
        y = c(11, 19, 19, 11),
        border = ifelse(any(
          grepl(pattern = "FAILED", temp.rc.testdose.error$Status)
        ), "red", "black")
      )
      polygon(
        x = c(-0.3,-0.3,0,0) , y = c(11,19,19,11), border = NA, density = 10, angle = 45
      )


      for (i in 1:nrow(temp.rc.testdose.error)) {
        points(
          temp.rc.testdose.error[i, "Value"],
          y = 15,
          pch = i,
          col = i,
          cex = 1.3 * cex
        )
      }

      ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
      ##polygon for palaeodose error
      text(
        x = -0.35, y = 5, "Palaeodose Err.", pos = 3, srt = 90, cex = 0.8*cex, offset = 0,
      )
      polygon(
        x = c(
          0,
          0,
          as.numeric(as.character(temp.rc.palaedose.error$Threshold))[1],
          as.numeric(as.character(temp.rc.palaedose.error$Threshold))[1]
        ),
        y = c(1,9,9,1),
        col = "gray",
        border = NA
      )
      polygon(
        x = c(-0.3, -0.3, 0.3, 0.3) ,
        y = c(1, 9, 9, 1),
        border = ifelse(any(
          grepl(pattern = "FAILED", temp.rc.palaedose.error$Status)
        ), "red", "black")
      )
      polygon(
        x = c(-0.3,-0.3,0,0) , y = c(1,9,9,1), border = NA, density = 10, angle = 45
      )

      if(nrow(temp.rc.palaedose.error) != 0){
        for (i in 1:nrow(temp.rc.palaedose.error)) {
          if(!is.na(temp.rc.palaedose.error[i, "Value"])){
            points(
              temp.rc.palaedose.error[i, "Value"],
              y = 5,
              pch = i,
              col = i,
              cex = 1.3 * cex
            )
          }
        }
      }
    }


    if (plot == TRUE && 8 %in% plot.single.sel) {
      ##graphical representation of IR-curve
      temp.IRSL <- suppressWarnings(get_RLum(object, recordType = "IRSL"))
      if(length(temp.IRSL) != 0){
        if(class(temp.IRSL) == "RLum.Data.Curve"){
          plot_RLum.Data.Curve(temp.IRSL, par.local = FALSE)

        }else if(class(temp.IRSL) == "list"){
          plot_RLum.Data.Curve(temp.IRSL[[length(temp.IRSL)]], par.local = FALSE)
          warning(
            "[analyse_SAR.CWOSL()] Multiple IRSL curves detected (IRSL test), show only the last one.",
            immediate. = TRUE,
            call. = FALSE
          )
        }else{
          shape::emptyplot()

        }

      }else{
        plot(1, type="n", axes=F, xlab="", ylab="")
        text(x = c(1,1), y = c(1, 1), labels = "No IRSL curve detected!")

      }

    }


    # Return --------------------------------------------------------------------------------------
    invisible(temp.results.final)

  }else{
    warning(paste0(
      "\n",
      paste(unlist(error.list), collapse = "\n"),"\n... >> nothing was done here!"
    ), call. = FALSE)
    invisible(NULL)

  }

}
