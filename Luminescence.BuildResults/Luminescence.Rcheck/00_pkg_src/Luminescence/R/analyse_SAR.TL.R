#' @title Analyse SAR TL measurements
#'
#' @description The function performs a SAR TL analysis on a
#' [RLum.Analysis-class] object including growth curve fitting.
#'
#' @details This function performs a SAR TL analysis on a set of curves. The SAR
#' procedure in general is given by Murray and Wintle (2000). For the
#' calculation of the `Lx/Tx` value the function [calc_TLLxTxRatio] is
#' used.
#'
#' **Provided rejection criteria**
#'
#' `[recyling.ratio]`: calculated for every repeated regeneration dose point.
#'
#' `[recuperation.rate]`: recuperation rate calculated by
#' comparing the `Lx/Tx` values of the zero regeneration point with the `Ln/Tn`
#' value (the `Lx/Tx` ratio of the natural signal).  For methodological
#' background see Aitken and Smith (1988)
#'
#' @param object [RLum.Analysis-class] or a [list] of such objects (**required**) :
#' input object containing data for analysis
#'
#' @param object.background currently not used
#'
#' @param signal.integral.min [integer] (**required**):
#' requires the channel number for the lower signal integral bound
#' (e.g. `signal.integral.min = 100`)
#'
#' @param signal.integral.max [integer] (**required**):
#' requires the channel number for the upper signal integral bound
#' (e.g. `signal.integral.max = 200`)
#'
#' @param integral_input [character] (*with default*):
#' defines the input for the the arguments `signal.integral.min` and
#' `signal.integral.max`. These limits can be either provided `'channel'`
#' number (the default) or `'temperature'`. If `'temperature'` is chosen the
#' best matching channel is selected.
#'
#' @param sequence.structure [vector] [character] (*with default*):
#' specifies the general sequence structure. Three steps are allowed
#' (`"PREHEAT"`, `"SIGNAL"`, `"BACKGROUND"`), in addition a
#' parameter `"EXCLUDE"`. This allows excluding TL curves which are not
#' relevant for the protocol analysis.  (**Note:** None TL are removed by default)
#'
#' @param rejection.criteria [list] (*with default*):
#' list containing rejection criteria in percentage for the calculation.
#'
#' @param dose.points [numeric] (*optional*):
#' option set dose points manually
#'
#' @param log [character] (*with default*):
#' a character string which contains `"x"` if the x-axis is to be logarithmic,
#' `"y"` if the y axis is to be logarithmic and `"xy"` or `"yx"` if both axes
#' are to be logarithmic. See
#' [plot.default]).
#'
#' @param ... further arguments that will be passed to the function [plot_GrowthCurve]
#'
#' @return
#' A plot (*optional*) and an [RLum.Results-class] object is
#' returned containing the following elements:
#'
#' \item{De.values}{[data.frame] containing De-values and further parameters}
#' \item{LnLxTnTx.values}{[data.frame] of all calculated `Lx/Tx` values including signal, background counts and the dose points.}
#' \item{rejection.criteria}{[data.frame] with values that might by used as rejection criteria. NA is produced if no R0 dose point exists.}
#'
#' **note:** the output should be accessed using the function [get_RLum]
#'
#' @note
#' **THIS IS A BETA VERSION**
#'
#' None TL curves will be removed
#' from the input object without further warning.
#'
#' @section Function version: 0.3.0
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [calc_TLLxTxRatio], [plot_GrowthCurve], [RLum.Analysis-class],
#' [RLum.Results-class], [get_RLum]
#'
#' @references
#' Aitken, M.J. and Smith, B.W., 1988. Optical dating: recuperation
#' after bleaching.  Quaternary Science Reviews 7, 387-393.
#'
#' Murray, A.S. and Wintle, A.G., 2000. Luminescence dating of quartz using an
#' improved single-aliquot regenerative-dose protocol. Radiation Measurements
#' 32, 57-73.
#'
#' @keywords datagen plot
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##transform the values from the first position in a RLum.Analysis object
#' object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos=3)
#'
#' ##perform analysis
#' analyse_SAR.TL(
#'  object = object,
#'  signal.integral.min = 210,
#'  signal.integral.max = 220,
#'  fit.method = "EXP OR LIN",
#'  sequence.structure = c("SIGNAL", "BACKGROUND"))
#'
#' @md
#' @export
analyse_SAR.TL <- function(
  object,
  object.background,
  signal.integral.min,
  signal.integral.max,
  integral_input = "channel",
  sequence.structure = c("PREHEAT", "SIGNAL", "BACKGROUND"),
  rejection.criteria = list(recycling.ratio = 10, recuperation.rate = 10),
  dose.points,
  log = "",
  ...
){


  # Self-call -----------------------------------------------------------------------------------
  if(class(object) == "list"){
   if(!all(sapply(object, class) == "RLum.Analysis"))
     stop("[analyse_SAR.TL()] All elements in the input list need to be of class 'RLum.Analysis'!",
          call. = FALSE)

    ##run sequence
    results <- lapply(object, function(o){
      analyse_SAR.TL(
        object = o,
        object.background = object.background,
        signal.integral.min = signal.integral.min,
        signal.integral.max = signal.integral.max,
        integral_input = integral_input,
        sequence.structure =  sequence.structure,
        rejection.criteria = rejection.criteria,
        dose.points = dose.points,
        log = log,
        ...
      )
    })

    ##combine results
    results <- merge_RLum(results)

    ##return
    return(results)

  }

  # CONFIG  -----------------------------------------------------------------

  ##set allowed curve types
  type.curves <- c("TL")

  ##=============================================================================#
  # General Integrity Checks ---------------------------------------------------

  ##GENERAL

  ##MISSING INPUT
  if(missing("object")==TRUE){
    stop("[analyse_SAR.TL()] No value set for 'object'!", call. = FALSE)
  }

  if(missing("signal.integral.min") == TRUE){
    stop("[analyse_SAR.TL()] No value set for 'signal.integral.min'!", call. = FALSE)
  }

  if(missing("signal.integral.max") == TRUE){
    stop("[analyse_SAR.TL()] No value set for 'signal.integral.max'!", call. = FALSE)
  }

  ##INPUT OBJECTS
  if(is(object, "RLum.Analysis") == FALSE){
    stop("[analyse_SAR.TL()] Input object is not of type 'RLum.Analyis'!", call. = FALSE)
  }


  # Protocol Integrity Checks --------------------------------------------------

  ##Remove non TL-curves from object by selecting TL curves
  object@records <- get_RLum(object, recordType = type.curves)

  ##ANALYSE SEQUENCE OBJECT STRUCTURE

  ##set vector for sequence structure
  temp.protocol.step <- rep(sequence.structure,length(object@records))[1:length(object@records)]

  ##grep object strucute
  temp.sequence.structure <- structure_RLum(object)

  ##set values for step
  temp.sequence.structure[,"protocol.step"] <- temp.protocol.step

  ##remove TL curves which are excluded
  temp.sequence.structure <- temp.sequence.structure[which(
    temp.sequence.structure[,"protocol.step"]!="EXCLUDE"),]

  ##check integrity; signal and bg range should be equal
  if(length(
    unique(
      temp.sequence.structure[temp.sequence.structure[,"protocol.step"]=="SIGNAL","n.channels"]))>1){

    stop(paste(
      "[analyse_SAR.TL()] Signal range differs. Check sequence structure.\n",
      temp.sequence.structure
    ))
  }

  ##check if the wanted curves are a multiple of the structure
  if(length(temp.sequence.structure[,"id"])%%length(sequence.structure)!=0){
    stop("[analyse_SAR.TL()] Input TL curves are not a multiple of the sequence structure.")

  }


  # # Calculate LnLxTnTx values  --------------------------------------------------

  ##grep IDs for signal and background curves
  TL.preheat.ID <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "PREHEAT","id"]

  TL.signal.ID <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "SIGNAL","id"]

  TL.background.ID <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "BACKGROUND","id"]

  ##comfort ... translate integral limits from temperature to channel
  if(integral_input == "temperature"){
    signal.integral.min <-
      which.min(abs(
        signal.integral.min - get_RLum(object, record.id = TL.signal.ID[1])[, 1]
      ))
    signal.integral.max <-
      which.min(abs(
        signal.integral.max - get_RLum(object, record.id = TL.signal.ID[1])[, 1]
      ))
  }

  ##calculate LxTx values using external function
  for(i in seq(1,length(TL.signal.ID),by=2)){
    temp.LnLxTnTx <- get_RLum(
      calc_TLLxTxRatio(
        Lx.data.signal = get_RLum(object, record.id = TL.signal.ID[i]),
        Lx.data.background = if (length(TL.background.ID) == 0) {
          NULL
        } else{
          get_RLum(object, record.id = TL.background.ID[i])
        },
        Tx.data.signal = get_RLum(object, record.id = TL.signal.ID[i + 1]),
        Tx.data.background =  if (length(TL.background.ID) == 0){
          NULL

        }else{
          get_RLum(object, record.id = TL.background.ID[i + 1])

        },
        signal.integral.min,
        signal.integral.max
      )
    )

    ##grep dose
    temp.Dose <- object@records[[TL.signal.ID[i]]]@info$IRR_TIME

      ##take about NULL values
      if(is.null(temp.Dose)){
        temp.Dose <- NA

      }

    ##bind data.frame
    temp.LnLxTnTx <- cbind(Dose=temp.Dose, temp.LnLxTnTx)

    if(exists("LnLxTnTx")==FALSE){
      LnLxTnTx <- data.frame(temp.LnLxTnTx)

    }else{
      LnLxTnTx <- rbind(LnLxTnTx,temp.LnLxTnTx)

    }
  }

  ##set dose.points manually if argument was set
  if(!missing(dose.points)){
    temp.Dose <- dose.points
    LnLxTnTx$Dose <- dose.points

  }

  # Set regeneration points -------------------------------------------------
  #generate unique dose id - this are also the # for the generated points
  temp.DoseID <- c(0:(length(LnLxTnTx[["Dose"]]) - 1))
  temp.DoseName <- paste0("R", temp.DoseID)
  temp.DoseName <- cbind(Name = temp.DoseName, Dose = LnLxTnTx[["Dose"]])

  ##set natural
  temp.DoseName[temp.DoseName[, "Name"] == "R0", "Name"] <- "Natural"

  ##set R0
  temp.DoseName[temp.DoseName[,"Name"]!="Natural" & temp.DoseName[,"Dose"]==0,"Name"]<-"R0"

  ##find duplicated doses (including 0 dose - which means the Natural)
  temp.DoseDuplicated<-duplicated(temp.DoseName[,"Dose"])

  ##combine temp.DoseName
  temp.DoseName<-cbind(temp.DoseName,Repeated=temp.DoseDuplicated)

  ##correct value for R0 (it is not really repeated)
  temp.DoseName[temp.DoseName[,"Dose"]==0,"Repeated"]<-FALSE

  ##combine in the data frame
  temp.LnLxTnTx <- data.frame(Name = temp.DoseName[, "Name"],
                              Repeated = as.logical(temp.DoseName[, "Repeated"]))


  LnLxTnTx<-cbind(temp.LnLxTnTx,LnLxTnTx)
  LnLxTnTx[,"Name"]<-as.character(LnLxTnTx[,"Name"])

  # Calculate Recycling Ratio -----------------------------------------------

  ##Calculate Recycling Ratio

  if(length(LnLxTnTx[LnLxTnTx[,"Repeated"]==TRUE,"Repeated"])>0){

    ##identify repeated doses
    temp.Repeated<-LnLxTnTx[LnLxTnTx[,"Repeated"]==TRUE,c("Name","Dose","LxTx")]

    ##find concering previous dose for the repeated dose
    temp.Previous<-t(sapply(1:length(temp.Repeated[,1]),function(x){
      LnLxTnTx[LnLxTnTx[,"Dose"]==temp.Repeated[x,"Dose"] &
                 LnLxTnTx[,"Repeated"]==FALSE,c("Name","Dose","LxTx")]
    }))

    ##convert to data.frame
    temp.Previous<-as.data.frame(temp.Previous)

    ##set column names
    temp.ColNames<-sapply(1:length(temp.Repeated[,1]),function(x){
      paste(temp.Repeated[x,"Name"],"/",
            temp.Previous[temp.Previous[,"Dose"]==temp.Repeated[x,"Dose"],"Name"],
            sep="")
    })

    ##Calculate Recycling Ratio
    RecyclingRatio<-as.numeric(temp.Repeated[,"LxTx"])/as.numeric(temp.Previous[,"LxTx"])

    ##Just transform the matrix and add column names
    RecyclingRatio<-t(RecyclingRatio)
    colnames(RecyclingRatio)<-temp.ColNames

  }else{RecyclingRatio<-NA}


  # Calculate Recuperation Rate ---------------------------------------------


  ##Recuperation Rate
  if("R0" %in% LnLxTnTx[,"Name"]==TRUE){
    Recuperation<-round(LnLxTnTx[LnLxTnTx[,"Name"]=="R0","LxTx"]/
                          LnLxTnTx[LnLxTnTx[,"Name"]=="Natural","LxTx"],digits=4)
  }else{Recuperation<-NA}


  # Combine and Evaluate Rejection Criteria ---------------------------------

  RejectionCriteria <- data.frame(
    citeria = c(colnames(RecyclingRatio), "recuperation rate"),
    value = c(RecyclingRatio,Recuperation),
    threshold = c(
      rep(paste("+/-", rejection.criteria$recycling.ratio/100)
          ,length(RecyclingRatio)),
      paste("", rejection.criteria$recuperation.rate/100)
    ),
    status = c(

      if(is.na(RecyclingRatio)==FALSE){

        sapply(1:length(RecyclingRatio), function(x){
          if(abs(1-RecyclingRatio[x])>(rejection.criteria$recycling.ratio/100)){
            "FAILED"
          }else{"OK"}})}else{NA},

      if(is.na(Recuperation)==FALSE &
           Recuperation>rejection.criteria$recuperation.rate){"FAILED"}else{"OK"}

    ))

  ##============================================================================##
  ##PLOTTING
  ##============================================================================##

  # Plotting - Config -------------------------------------------------------
  ##grep plot parameter
  par.default <- par(no.readonly = TRUE)
  on.exit(par(par.default))

  ##grep colours
  col <- get("col", pos = .LuminescenceEnv)

  ##set layout matrix
  layout(matrix(c(
    1, 1, 2, 2,
    1, 1, 2, 2,
    3, 3, 4, 4,
    3, 3, 4, 4,
    5, 5, 5, 5
  ), 5, 4, byrow = TRUE))

  par(oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 3))

  ## 1 -> TL Lx
  ## 2 -> TL Tx
  ## 3 -> TL Lx Plateau
  ## 4 -> TL Tx Plateau
  ## 5 -> Legend

  ##recalculate signal.integral from channels to temperature
  signal.integral.temperature <- c(object@records[[TL.signal.ID[1]]]@data[signal.integral.min,1] :
                                     object@records[[TL.signal.ID[1]]]@data[signal.integral.max,1])


  ##warning if number of curves exceed colour values
  if(length(col)<length(TL.signal.ID/2)){
    cat("\n[analyse_SAR.TL.R] Warning: To many curves! Only the first",
        length(col),"curves are plotted!")
  }


  # # Plotting TL LnLx Curves ----------------------------------------------------
  ##matrix with LnLx curves
  LnLx_matrix <- vapply(seq(1, length(TL.signal.ID), by = 2), function(x){
    if(length(TL.background.ID) != 0){
      object@records[[TL.signal.ID[x]]]@data[,2] -
        object@records[[TL.background.ID[x]]]@data[,2]
    }else{
      object@records[[TL.signal.ID[x]]]@data[,2]
    }
  }, numeric(nrow(object@records[[TL.signal.ID[1]]]@data)))

  ##add time axis
  LnLx_matrix <- cbind(object@records[[TL.signal.ID[1]]]@data[,1], LnLx_matrix)

  ##matrix with TnTx curves
  TnTx_matrix <- vapply(seq(2, length(TL.signal.ID), by = 2), function(x){
    if(length(TL.background.ID) != 0){
      object@records[[TL.signal.ID[x]]]@data[,2] -
        object@records[[TL.background.ID[x]]]@data[,2]
    }else{
      object@records[[TL.signal.ID[x]]]@data[,2]
    }
  }, numeric(nrow(object@records[[TL.signal.ID[1]]]@data)))

  ##add time axis
  TnTx_matrix <- cbind(object@records[[TL.signal.ID[1]]]@data[,1], TnTx_matrix)

  ##catch log-scale problem
  if(log != ""){
    if(min(LnLx_matrix) <= 0 || min(TnTx_matrix) <= 0){
      warning("[analyse_SAR.TL()] log-scale needs positive values; log-scale disabled!", call. = FALSE)
    log <- ""
    }

  }

  #open plot area LnLx
  plot(NA,NA,
       xlab = "Temp. [\u00B0C]",
       ylab = paste0("TL [a.u.]"),
       xlim = range(LnLx_matrix[, 1]),
       ylim = range(LnLx_matrix[, -1]),
       main = expression(paste(L[n], ",", L[x], " curves", sep = "")),
       log = log
  )

  ##plot curves
  for (i in 2:ncol(LnLx_matrix)) {
    lines(x = LnLx_matrix[, 1], y = LnLx_matrix[, i],
          col = col[i - 1])
  }

  ##mark integration limits
  abline(v = range(signal.integral.temperature), lty = 2, col = "gray")

  #open plot area TnTx
  plot(NA,NA,
    xlab = "Temp. [\u00B0C]",
    ylab = paste0("TL [a.u.]"),
    xlim = range(TnTx_matrix[, 1]),
    ylim = range(TnTx_matrix[, -1]),
    main = expression(paste(T[n], ",", T[x], " curves", sep = "")),
    log = log
  )

  ##plot curves
  for (i in 2:ncol(TnTx_matrix)) {
    lines(x = TnTx_matrix[, 1], y = TnTx_matrix[, i],
          col = col[i - 1])
  }

  ##mark integration limits
  abline(v = range(signal.integral.temperature), lty = 2, col = "gray")

  ##clean
  rm(LnLx_matrix, TnTx_matrix)

  # Plotting Plateau Test LnLx -------------------------------------------------
  if(length(TL.background.ID) != 0){
    NTL.net.LnLx <-
      data.frame(object@records[[TL.signal.ID[1]]]@data[, 1],
                 object@records[[TL.signal.ID[1]]]@data[, 2] -
                   object@records[[TL.background.ID[1]]]@data[, 2])

    Reg1.net.LnLx <-
      data.frame(object@records[[TL.signal.ID[3]]]@data[, 1],
                 object@records[[TL.signal.ID[3]]]@data[, 2] -
                   object@records[[TL.background.ID[3]]]@data[, 2])


    TL.Plateau.LnLx <-
      data.frame(NTL.net.LnLx[, 1], Reg1.net.LnLx[, 2] / NTL.net.LnLx[, 2])

    ##Plot Plateau Test
    plot(
      NA,
      NA,
      xlab = "Temp. [\u00B0C]",
      ylab = "TL [a.u.]",
      xlim = c(
        min(signal.integral.temperature) * 0.9,
        max(signal.integral.temperature) * 1.1
      ),
      ylim = c(0, max(NTL.net.LnLx[, 2])),
      main = expression(paste("Plateau test ", L[n], ",", L[x], " curves", sep =
                                ""))
    )


    ##plot single curves
    lines(NTL.net.LnLx, col = col[1])
    lines(Reg1.net.LnLx, col = col[2])


    ##plot
    par(new = TRUE)
    plot(
      TL.Plateau.LnLx,
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(0,
               quantile(
                 TL.Plateau.LnLx[c(signal.integral.min:signal.integral.max), 2],
                 probs = c(0.90), na.rm = TRUE
               ) + 3),
      col = "darkgreen"
    )
    axis(4)


    # Plotting Plateau Test TnTx -------------------------------------------------
    ##get NTL signal
    NTL.net.TnTx <- matrix(
      data = c(object@records[[TL.signal.ID[2]]]@data[, 1],
                 object@records[[TL.signal.ID[2]]]@data[, 2] -
                   object@records[[TL.background.ID[2]]]@data[, 2]),
      ncol = 2)

    ##get signal from the first regeneration point
    Reg1.net.TnTx <- matrix(
      data = c(object@records[[TL.signal.ID[4]]]@data[, 1],
                 object@records[[TL.signal.ID[4]]]@data[, 2] -
                   object@records[[TL.background.ID[4]]]@data[, 2]),
      ncol = 2)


    ##combine values
    TL.Plateau.TnTx <- matrix(
      data = c(NTL.net.TnTx[, 1],
               Reg1.net.TnTx[, 2] / NTL.net.TnTx[, 2]),
      ncol = 2)

    ##Plot Plateau Test
    plot(
      NA,
      NA,
      xlab = "Temp. [\u00B0C]",
      ylab = "TL [a.u.]",
      xlim = c(
        min(signal.integral.temperature) * 0.9,
        max(signal.integral.temperature) * 1.1
      ),
      ylim = c(0, max(NTL.net.TnTx[, 2])),
      main = expression(paste("plateau Test ", T[n], ",", T[x], " curves", sep =
                                ""))
    )


    ##plot single curves
    lines(NTL.net.TnTx, col = col[1])
    lines(Reg1.net.TnTx, col = col[2])

    ##plot
    par(new = TRUE)
    plot(
      TL.Plateau.TnTx,
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(0,
               quantile(
                 TL.Plateau.TnTx[c(signal.integral.min:signal.integral.max), 2],
                 probs = c(0.90), na.rm = TRUE
               ) + 3),
      col = "darkgreen"
    )
    axis(4)

    # Plotting Legend ----------------------------------------
    plot(
      c(1:(length(TL.signal.ID) / 2)),
      rep(8, length(TL.signal.ID) / 2),
      type = "p",
      axes = FALSE,
      xlab = "",
      ylab = "",
      pch = 15,
      col = col[1:length(TL.signal.ID)],
      cex = 2,
      ylim = c(0, 10)
    )

    ##add text
    text(c(1:(length(TL.signal.ID) / 2)),
         rep(4, length(TL.signal.ID) / 2),
         paste(LnLxTnTx$Name, "\n(", LnLxTnTx$Dose, ")", sep = ""))

    ##add line
    abline(h = 10, lwd = 0.5)

    ##set failed text and mark De as failed
    if (length(grep("FAILED", RejectionCriteria$status)) > 0) {
      mtext("[FAILED]", col = "red")


    }
  }

  # Plotting  GC  ----------------------------------------
  #reset par
  par(par.default)

  ##create data.frame
  temp.sample <- data.frame(
    Dose = LnLxTnTx$Dose,
    LxTx = LnLxTnTx$LxTx,
    LxTx.Error = LnLxTnTx$LxTx.Error,
    TnTx = LnLxTnTx$TnTx
  )

  ##set NA values to 0
  temp.sample[is.na(temp.sample$LxTx.Error),"LxTx.Error"] <- 0

  ##run curve fitting
  temp.GC <- try(plot_GrowthCurve(
    sample = temp.sample,
    ...
  ))

  ##check for error
  if(inherits(temp.GC, "try-error")){
    return(NULL)

  }else{
    temp.GC <- get_RLum(temp.GC)[, c("De", "De.Error")]

  }


  ##add recjection status
  if(length(grep("FAILED",RejectionCriteria$status))>0){
    temp.GC <- data.frame(temp.GC, RC.Status="FAILED")

  }else{
    temp.GC <- data.frame(temp.GC, RC.Status="OK")

  }

  # Return Values -----------------------------------------------------------
  newRLumResults.analyse_SAR.TL <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = temp.GC,
      LnLxTnTx.table = LnLxTnTx,
      rejection.criteria = RejectionCriteria
    ),
    info = list(info = sys.call())
  )

  return(newRLumResults.analyse_SAR.TL)

}

