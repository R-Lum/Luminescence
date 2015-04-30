#' Analyse post-IR IRSL sequences
#'
#' The function performs an analysis of post-IR IRSL sequences including curve
#' fitting on \code{\linkS4class{RLum.Analysis}} objects.
#'
#' To allow post-IR IRSL protocol (Thomsen et al., 2008) measurement analysis
#' this function has been written as extended wrapper function for the function
#' \code{\link{analyse_SAR.CWOSL}}, facilitating an entire sequence analysis in
#' one run. With this, its functionality is strictly limited by the
#' functionality of the function \code{\link{analyse_SAR.CWOSL}}.
#'
#' @param object \code{\linkS4class{RLum.Analysis}}(\bold{required}): input
#' object containing data for analysis
#' @param signal.integral.min \code{\link{integer}} (\bold{required}): lower
#' bound of the signal integral. Provide this value as vector for different
#' integration limits for the different IRSL curves.
#' @param signal.integral.max \code{\link{integer}} (\bold{required}): upper
#' bound of the signal integral. Provide this value as vector for different
#' integration limits for the different IRSL curves.
#' @param background.integral.min \code{\link{integer}} (\bold{required}):
#' lower bound of the background integral. Provide this value as vector for
#' different integration limits for the different IRSL curves.
#' @param background.integral.max \code{\link{integer}} (\bold{required}):
#' upper bound of the background integral. Provide this value as vector for
#' different integration limits for the different IRSL curves.
#' @param dose.points \code{\link{numeric}} (optional): a numeric vector
#' containing the dose points values. Using this argument overwrites dose point
#' values in the signal curves.
#' @param sequence.structure \link{vector} \link{character} (with default):
#' specifies the general sequence structure. Allowed values are \code{"TL"} and
#' any \code{"IR"} combination (e.g., \code{"IR50"},\code{"pIRIR225"}).
#' Additionally a parameter \code{"EXCLUDE"} is allowed to exclude curves from
#' the analysis (Note: If a preheat without PMT measurement is used, i.e.
#' preheat as non TL, remove the TL step.)
#' @param plot \code{\link{logical}} (with default): enables or disables plot
#' output.
#' @param plot.single \code{\link{logical}} (with default): single plot output
#' (\code{TRUE/FALSE}) to allow for plotting the results in single plot
#' windows. Requires \code{plot = TRUE}.
#' @param \dots further arguments that will be passed to the function
#' \code{\link{analyse_SAR.CWOSL}} and \code{\link{plot_GrowthCurve}}
#' @return Plots (optional) and an \code{\linkS4class{RLum.Results}} object is
#' returned containing the following elements:
#' \item{De.values}{\link{data.frame} containing De-values, De-error and
#' further parameters}. \item{LnLxTnTx.values}{\link{data.frame} of all
#' calculated Lx/Tx values including signal, background counts and the dose
#' points.} \item{rejection.criteria}{\link{data.frame} with values that might
#' by used as rejection criteria. NA is produced if no R0 dose point
#' exists.}\cr
#'
#' The output should be accessed using the function
#' \code{\link{get_RLum.Results}}.
#' @note Best graphical output can be achieved by using the function \code{pdf}
#' with the following options:\cr \code{pdf(file = "...", height = 15, width =
#' 15)}
#' @section Function version: 0.1.4
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#' @seealso \code{\link{analyse_SAR.CWOSL}}, \code{\link{calc_OSLLxTxRatio}},
#' \code{\link{plot_GrowthCurve}}, \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Results}} \code{\link{get_RLum.Results}}
#' @references Murray, A.S., Wintle, A.G., 2000. Luminescence dating of quartz
#' using an improved single-aliquot regenerative-dose protocol. Radiation
#' Measurements 32, 57-73. doi:10.1016/S1350-4487(99)00253-X
#'
#' Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. Laboratory
#' fading rates of various luminescence signals from feldspar-rich sediment
#' extracts. Radiation Measurements 43, 1474-1486.
#' doi:10.1016/j.radmeas.2008.06.002
#' @keywords datagen plot
#' @examples
#'
#'
#' ### NOTE: For this example existing example data are used. These data are non pIRIR data.
#' ###
#' ##(1) Compile example data set based on existing example data (SAR quartz measurement)
#' ##(a) Load example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##(b) Transform the values from the first position in a RLum.Analysis object
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
#' ##(c) Grep curves and exclude the last two (one TL and one IRSL)
#' object <- get_RLum.Analysis(object, record.id = c(-29,-30))
#'
#' ##(d) Define new sequence structure and set new RLum.Analysis object
#' sequence.structure  <- c(1,2,2,3,4,4)
#' sequence.structure <- as.vector(sapply(seq(0,length(object)-1,by = 4),
#'                                        function(x){sequence.structure + x}))
#'
#' object <-  sapply(1:length(sequence.structure), function(x){
#'
#'   object[[sequence.structure[x]]]
#'
#' })
#'
#' object <- set_RLum.Analysis(records = object, protocol = "pIRIR")
#'
#' ##(2) Perform pIRIR analysis (for this example with quartz OSL data!)
#' ## Note: output as single plots to avoid problems with this example
#' results <- analyse_pIRIRSequence(object,
#'                              signal.integral.min = 1,
#'                              signal.integral.max = 2,
#'                              background.integral.min = 900,
#'                              background.integral.max = 1000,
#'                              fit.method = "EXP",
#'                              sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
#'                              main = "Pseudo pIRIR data set based on quartz OSL",
#'                              plot.single = TRUE)
#'
#'
#' ##(3) Perform pIRIR analysis (for this example with quartz OSL data!)
#' ## Alternative for PDF output, uncomment and complete for usage
#' ##
#' # pdf(file = "...", height = 15, width = 15)
#' #  results <- analyse_pIRIRSequence(object,
#' #         signal.integral.min = 1,
#' #         signal.integral.max = 2,
#' #         background.integral.min = 900,
#' #         background.integral.max = 1000,
#' #         fit.method = "EXP",
#' #         main = "Pseudo pIRIR data set based on quartz OSL")
#' #
#' #  dev.off()
#'
#'
#'
analyse_pIRIRSequence <- function(
  object,
  signal.integral.min,
  signal.integral.max,
  background.integral.min,
  background.integral.max,
  dose.points,
  sequence.structure = c("TL", "IR50", "pIRIR225"),
  plot = TRUE,
  plot.single = FALSE,
  ...
){

# CONFIG  -----------------------------------------------------------------


# General Integrity Checks ---------------------------------------------------

  ##GENERAL

    ##MISSING INPUT
    if(missing("object")==TRUE){
      stop("[analyse_pIRIRSequence()] No value set for 'object'!")
    }

    ##INPUT OBJECTS
    if(is(object, "RLum.Analysis")==FALSE){
      stop("[analyse_pIRIRSequence()] Input object is not of type 'RLum.Analyis'!")
    }

    ##CHECK ALLOWED VALUES IN SEQUENCE STRUCTURE
    temp.collect.invalid.terms <- paste(sequence.structure[
      (!grepl("TL",sequence.structure)) &
      (!grepl("IR",sequence.structure)) &
      (!grepl("EXCLUDE",sequence.structure))],
      collapse = ", ")

    if(temp.collect.invalid.terms != ""){
      stop("[analyse_pIRIRSequence()] ",
        temp.collect.invalid.terms, " not allowed in sequence.strucutre!")
    }


# Deal with extra arguments -------------------------------------------------------------------

  ##deal with addition arguments
  extraArgs <- list(...)

  mtext.outer <- if("mtext.outer" %in% names(extraArgs)) {extraArgs$mtext.outer} else
  {"MEASUREMENT INFO"}

  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {""}

  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else
  {""}

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else
  {.7}


# Protocol Integrity Checks --------------------------------------------------

  ##(1) Check structure and remove curves that fit not the recordType criteria

  ##get sequence structure
  temp.sequence.structure  <- get_structure.RLum.Analysis(object)

  ##remove data types that fit not to allow values
  temp.sequence.rm.id <- temp.sequence.structure[
    (!grepl("TL",temp.sequence.structure[, "recordType"])) &
    (!grepl("OSL", temp.sequence.structure[, "recordType"])) &
    (!grepl("IRSL", temp.sequence.structure[, "recordType"]))
    ,"id"]

  if(length(temp.sequence.rm.id)>0){

  ##removed record from data set
  object <- get_RLum.Analysis(object, record.id = -temp.sequence.rm.id,
                              keep.object = TRUE)

  ##compile warning message
  temp.sequence.rm.warning <- paste(
    temp.sequence.structure[temp.sequence.rm.id, "recordType"], collapse = ", ")

  temp.sequence.rm.warning <- paste(
    "Record types are unrecognised and have been removed:", temp.sequence.rm.warning)

  warning(temp.sequence.rm.warning)
  }

  ##(2) Apply user sequence structure

  ##get sequence structure
  temp.sequence.structure  <- get_structure.RLum.Analysis(object)

  ##set values to structure data.frame
  temp.sequence.structure[, "protocol.step"] <- rep(
    sequence.structure, nrow(temp.sequence.structure)/2/length(sequence.structure))

  ##remove values that have been excluded
  temp.sequence.rm.id <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "EXCLUDE" ,"id"]

  if(length(temp.sequence.rm.id)>0){

    ##remove from object
    object  <- get_RLum.Analysis(
      object, record.id = -temp.sequence.rm.id, keep.object = TRUE)

    ##remove from sequence structure
    sequence.structure  <- sequence.structure[sequence.structure != "EXCLUDE"]

    ##set new structure
    temp.sequence.structure  <- get_structure.RLum.Analysis(object)

    temp.sequence.structure[, "protocol.step"] <- rep(
      sequence.structure, nrow(temp.sequence.structure)/2/length(temp.sequence.structure))

    ##print warning message
    warning(length(temp.sequence.rm.id), " records have been removed due to EXCLUDE!")

  }

##============================================================================##
# Analyse data and plotting ----------------------------------------------------
##============================================================================##

  ##(1) find out how many runs are needed for the analysis by checking for "IR"
  ##    now should by every signal except the TL curves
  n.TL<- table(grepl("TL", sequence.structure))["TRUE"]
  if(is.na(n.TL)) {n.TL<- 0}
  n.loops <- as.numeric(length(grepl("TL", sequence.structure)) - n.TL)

  ##grep ids of TL curves (we need them later on)
  TL.curves.id <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "TL","id"]

  ##grep ids of all OSL curves (we need them later on)
  IRSL.curves.id <- temp.sequence.structure[
    grepl("IR", temp.sequence.structure[,"protocol.step"]),"id"]

  ##grep information on the names of the IR curves, we need them later on
  pIRIR.curve.names  <- unique(temp.sequence.structure[
    temp.sequence.structure[IRSL.curves.id,"id"],"protocol.step"])

  ##===========================================================================#
  ## set graphic layout using the layout option
  ## unfortunately a little bit more complicated then expected previously due
  ## the order of the produced plots by the previous functions

  if(plot.single == FALSE){
  ##first (Tx,Tn, Lx,Ln)
  temp.IRSL.layout.vector.first <- c(3,5,6,7,3,5,6,8)

  ##middle (any other Lx,Ln)
  if(n.loops > 2){
  temp.IRSL.layout.vector.middle <- as.vector(sapply(2:n.loops-1, function(x){

    offset <- (5*x)-1
    c((offset):(offset+3),
      (offset):(offset+2),offset+4)

  }))}

  ##last (Lx,Ln and legend)
  temp.IRSL.layout.vector.last <- c(
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 1,
           max(temp.IRSL.layout.vector.first) + 1),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 2,
           max(temp.IRSL.layout.vector.first) + 2),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 4,
           max(temp.IRSL.layout.vector.first) + 4),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 5,
           max(temp.IRSL.layout.vector.first) + 5),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 1,
           max(temp.IRSL.layout.vector.first) + 1),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 2,
           max(temp.IRSL.layout.vector.first) + 2),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 4,
           max(temp.IRSL.layout.vector.first) + 4),
    ifelse(n.loops > 2,max(temp.IRSL.layout.vector.middle) + 6,
           max(temp.IRSL.layout.vector.first) + 6))

  ##options for different sets of curves
  if(n.loops > 2){

    temp.IRSL.layout.vector <- c(temp.IRSL.layout.vector.first,
                                 temp.IRSL.layout.vector.middle,
                                 temp.IRSL.layout.vector.last)

  }else{

    temp.IRSL.layout.vector <- c(temp.IRSL.layout.vector.first,
                                 temp.IRSL.layout.vector.last)

  }

  ##get layout information
  def.par <- par(no.readonly = TRUE)

  ##set up layout matrix linked to the number of plot areas needed
  layout.matrix  <- c(
    rep(c(2,4,1,1),2), #header row with TL curves and info window
    temp.IRSL.layout.vector, #IRSL curves,
    rep((max(temp.IRSL.layout.vector)-3),8), #legend,
    rep((max(temp.IRSL.layout.vector)+1),1), #GC
    rep((max(temp.IRSL.layout.vector)+2),1), #TnTc
    rep((max(temp.IRSL.layout.vector)+3),2), #Rejection criteria
    rep((max(temp.IRSL.layout.vector)+1),1), #GC
    rep((max(temp.IRSL.layout.vector)+2),1), #TnTc
    rep((max(temp.IRSL.layout.vector)+3),2)) #Rejection criteria


  ##set layout
  nf <- layout(
    matrix(layout.matrix,(max(layout.matrix)/2+2), 4, byrow = TRUE),
     widths = c(rep(c(1,1,1,.75),6),c(1,1,1,1)),
     heights = c(rep(c(1),(2+2*n.loops)),c(0.20, 0.20)))


  ## show the regions that have been allocated to each plot for debug
  ##layout.show(nf)

  }

  ##(1) INFO PLOT
  plot(NA,NA,
       ylim = c(0,1), xlab = "",
       xlim = c(0,1), ylab = "",
       axes = FALSE,
       main = main)

  text(0.5,0.5, paste(sequence.structure, collapse = "\n"), cex = cex *2)



  ##(2) set loop
  for(i in 1:n.loops){

    ##compile record ids
    temp.id.sel <-
      sort(c(TL.curves.id, IRSL.curves.id[seq(i,length(IRSL.curves.id),by=n.loops)]))

    ##(a) select data set (TL curves has to be considered for the data set)
    temp.curves<- get_RLum.Analysis(object, record.id = temp.id.sel, keep.object = TRUE)

    ##(b) grep integral limits as they might be different for different curves
    if(length(signal.integral.min)>1){

      temp.signal.integral.min <- signal.integral.min[i]
      temp.signal.integral.max <- signal.integral.max[i]
      temp.background.integral.min <- background.integral.min[i]
      temp.backbround.integral.max <- background.integral.max[i]

    }else{

      temp.signal.integral.min <- signal.integral.min
      temp.signal.integral.max <- signal.integral.max
      temp.background.integral.min <- background.integral.min
      temp.background.integral.max <- background.integral.max

    }

    ##(c) call analysis sequence and plot

    ## call single plots
    if(i == 1){

      temp.plot.single  <- c(1,2,3,4,6)

    }else if(i == n.loops){

      temp.plot.single  <- c(2,4,5,6)

  }else{

     temp.plot.single  <- c(2,4,6)

  }

    ##start analysis
    temp.results <- analyse_SAR.CWOSL(
                    temp.curves,
                    signal.integral.min = temp.signal.integral.min,
                    signal.integral.max = temp.signal.integral.max,
                    background.integral.min = temp.background.integral.min,
                    background.integral.max = temp.background.integral.max,
                    plot = plot,
                    dose.points = dose.points,
                    plot.single = temp.plot.single,
                    output.plotExtended.single = TRUE,
                    cex.global = cex,
                    ...) ##TODO should be replaced be useful explizit arguments


    ##add signal nformation to the protocol step
    temp.results.pIRIR.De <- as.data.frame(
      c(get_RLum.Results(temp.results, "De.values"),
        data.frame(Signal = pIRIR.curve.names[i])))

    temp.results.pIRIR.LnLxTnTx <- as.data.frame(
     c(get_RLum.Results(temp.results, "LnLxTnTx.table"),
       data.frame(Signal = pIRIR.curve.names[i])))

    temp.results.pIRIR.rejection.criteria <- as.data.frame(
      c(get_RLum.Results(temp.results, "rejection.criteria"),
      data.frame(Signal = pIRIR.curve.names[i])))

    temp.results.pIRIR.formula <- list(get_RLum.Results(temp.results,
                                                             "Formula"))
    names(temp.results.pIRIR.formula)  <- pIRIR.curve.names[i]

    ##create now object
    temp.results  <- set_RLum.Results(
      data = list(
        De.values = temp.results.pIRIR.De,
        LnLxTnTx.table = temp.results.pIRIR.LnLxTnTx,
        rejection.criteria = temp.results.pIRIR.rejection.criteria,
        Formula =temp.results.pIRIR.formula))


    ##merge results
    if(exists("temp.results.final")){

      temp.results.final <- merge_RLum.Results(
        list(temp.results.final, temp.results))

    }else{

      temp.results.final <- temp.results

   }

  }


##============================================================================##
# Plotting additionals--------------------------------------------------------
##============================================================================##

if(plot == TRUE){

  ##plot growth curves
  plot(NA, NA,
       xlim = range(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$Dose),
       ylim = c(
         min(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx)+
         max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx.Error),
         max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx)+
         max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$LxTx.Error)),
       xlab = "Dose [s]",
       ylab = expression(L[x]/T[x]),
       main = "Summarised growth curves")


    ##set x for expression evaluation
    x <- seq(0,
             max(get_RLum.Results(temp.results.final, "LnLxTnTx.table")$Dose)*1.05,
             length = 100)

    for(j in 1:length(pIRIR.curve.names)){

     ##dose points
     temp.curve.points <- get_RLum.Results(
       temp.results.final,"LnLxTnTx.table")[,c("Dose", "LxTx", "LxTx.Error", "Signal")]

     temp.curve.points <- temp.curve.points[
       temp.curve.points[,"Signal"] == pIRIR.curve.names[j],
       c("Dose", "LxTx", "LxTx.Error")]

     points(temp.curve.points[-1,c("Dose", "LxTx")], col = j, pch = j)
     segments(x0 = temp.curve.points[-1,c("Dose")],
              y0 = temp.curve.points[-1,c("LxTx")] -
                temp.curve.points[-1,c("LxTx.Error")],
              x1 = temp.curve.points[-1,c("Dose")],
              y1 = temp.curve.points[-1,c("LxTx")] +
                temp.curve.points[-1,c("LxTx.Error")],
              col = j)

     ##De values
     lines(c(0, get_RLum.Results(temp.results.final, "De.values")[j,1]),
           c(temp.curve.points[1,c("LxTx")], temp.curve.points[1,c("LxTx")]),
           col = j,
           lty = 2)

     lines(c(rep(get_RLum.Results(temp.results.final, "De.values")[j,1], 2)),
           c(temp.curve.points[1,c("LxTx")], 0),
           col = j,
           lty = 2)

     ##curve
     temp.curve.formula  <- get_RLum.Results(
        temp.results.final, "Formula")[[pIRIR.curve.names[j]]]

     try(lines(x, eval(temp.curve.formula), col = j), silent = TRUE)

    }

    rm(x)

    ##plot legend
    legend("bottomright", legend = pIRIR.curve.names,
           lty = 1, col = c(1:length(pIRIR.curve.names)),
           bty = "n",
           pch = c(1:length(pIRIR.curve.names))
           )

    ##plot Tn/Tx curves
    ##select signal
    temp.curve.TnTx <-
      get_RLum.Results(temp.results.final, "LnLxTnTx.table")[, c("TnTx", "Signal")]

    temp.curve.TnTx.matrix <- matrix(NA,
                                    nrow = nrow(temp.curve.TnTx)/
                                      length(pIRIR.curve.names),
                                    ncol =  length(pIRIR.curve.names))

    ##calculate normalised values
    for(j in 1:length(pIRIR.curve.names)){

      temp.curve.TnTx.sel <- temp.curve.TnTx[
        temp.curve.TnTx[,"Signal"] == pIRIR.curve.names[j]
        , "TnTx"]

      temp.curve.TnTx.matrix[,j] <- temp.curve.TnTx.sel/temp.curve.TnTx.sel[1]

    }

    plot(NA, NA,
       xlim = c(0,nrow(get_RLum.Results(temp.results.final, "LnLxTnTx.table"))/
                     n.loops),
       ylim = range(temp.curve.TnTx.matrix),
       xlab = "# Cycle",
       ylab = expression(T[x]/T[n]),
       main = "Sensitivity change")

    ##zero line
    abline(h = 1:nrow(temp.curve.TnTx.matrix), col = "gray")

    for(j in 1:length(pIRIR.curve.names)){

     lines(1:nrow(temp.curve.TnTx.matrix),
           temp.curve.TnTx.matrix[,j],
           type = "b",
           col = j,
           pch = j)
    }

   ##plot legend
   legend("bottomleft", legend = pIRIR.curve.names,
         lty = 1, col = c(1:length(pIRIR.curve.names)),
         bty = "n",
         pch = c(1:length(pIRIR.curve.names))
         )


   ##Rejection criteria
   temp.rejection.criteria <- get_RLum.Results(temp.results.final,
                                               data.object = "rejection.criteria")

   temp.rc.reycling.ratio <- temp.rejection.criteria[
     grep("Recycling ratio",temp.rejection.criteria[,"Criteria"]),]

   temp.rc.recuperation.rate <- temp.rejection.criteria[
     grep("Recuperation rate",temp.rejection.criteria[,"Criteria"]),]

   temp.rc.palaedose.error <- temp.rejection.criteria[
     grep("Palaeodose error",temp.rejection.criteria[,"Criteria"]),]

   plot(NA,NA,
        xlim = c(-0.5,0.5),
        ylim = c(0,30),
        yaxt = "n", ylab = "",
        xaxt = "n", xlab = "",
        bty = "n",
        main = "Rejection criteria")

   axis(side = 1, at = c(-0.2,-0.1,0,0.1,0.2), labels = c("- 0.2", "- 0.1","0/1","+ 0.1", "+ 0.2"))
   ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
   ##polygon for recycling ratio
   text(x = -.4, y = 30, "Recycling ratio", pos = 1, srt = 0)
   polygon(x = c(-as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1],
                -as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1],
                as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1],
                as.numeric(as.character(temp.rc.reycling.ratio$Threshold))[1]),
          y = c(21,29,29,21), col = "gray", border = NA)
    polygon(x = c(-0.3,-0.3,0.3,0.3) , y = c(21,29,29,21))


   ##consider possibility of multiple pIRIR signals and multiple recycling ratios
   col.id  <- 1

   ##the conditional case might valid if no rejection criteria could be calculated
   if(nrow(temp.rc.recuperation.rate)>0){

   for(i in seq(1,nrow(temp.rc.recuperation.rate),
                  length(unique(temp.rc.recuperation.rate[,"Criteria"])))){


        for(j in 0:length(unique(temp.rc.recuperation.rate[,"Criteria"]))){
         points(temp.rc.reycling.ratio[i+j, "Value"]-1,
               y = 25,
               pch = col.id,
               col = col.id)

        }
        col.id <- col.id + 1
   }
   }#endif

    rm(col.id)



   ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
   ##polygon for recuperation rate

   text(x = -.4, y = 20, "Recuperation rate", pos = 1, srt = 0)

   if(length(as.character(temp.rc.recuperation.rate$Threshold))>0){
   polygon(x = c(0,
                0,
                as.numeric(as.character(temp.rc.recuperation.rate$Threshold))[1],
                as.numeric(as.character(temp.rc.recuperation.rate$Threshold))[1]),
          y = c(11,19,19,11), col = "gray", border = NA)

   polygon(x = c(-0.3,-0.3,0.3,0.3) , y = c(11,19,19,11))
   polygon(x = c(-0.3,-0.3,0,0) , y = c(11,19,19,11), border = NA, density = 10, angle = 45)


  for(i in 1:nrow(temp.rc.recuperation.rate)){

    points(temp.rc.palaedose.error[i, "Value"],
           y = 15,
           pch = i,
           col = i)

  }
  }#endif

   ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
   ##polygon for palaeodose error
   text(x = -.4, y = 10, "Palaeodose error", pos = 1, srt = 0)
   polygon(x = c(0,
                0,
                as.numeric(as.character(temp.rc.palaedose.error$Threshold))[1],
                as.numeric(as.character(temp.rc.palaedose.error$Threshold))[1]),
          y = c(1,9,9,1), col = "gray", border = NA)
   polygon(x = c(-0.3,-0.3,0.3,0.3) , y = c(1,9,9,1))
   polygon(x = c(-0.3,-0.3,0,0) , y = c(1,9,9,1), border = NA, density = 10, angle = 45)


   for(i in 1:nrow(temp.rc.palaedose.error)){

     points(temp.rc.palaedose.error[i, "Value"],
            y = 5,
            pch = i,
            col = i)

   }

   ##add 0 value
   lines(x = c(0,0), y = c(0,19), lwd = 1.5*cex)
   lines(x = c(0,0), y = c(20,29), lwd = 1.5*cex)

  ##plot legend
  legend("bottomright", legend = pIRIR.curve.names,
         col = c(1:length(pIRIR.curve.names)),
         bty = "n",
         pch = c(1:length(pIRIR.curve.names)))


   ##reset graphic settings
   if(plot.single == FALSE){par(def.par)}

}##end plot == TRUE


##============================================================================##
# Return Values -----------------------------------------------------------
##============================================================================##

  temp.results.final


}
