#' @title Analyse post-IR IRSL measurement sequences
#'
#' @description The function performs an analysis of post-IR IRSL sequences
#' including curve
#' fitting on [RLum.Analysis-class] objects.
#'
#' @details To allow post-IR IRSL protocol (Thomsen et al., 2008) measurement
#' analyses, this function has been written as extended wrapper for function
#' [analyse_SAR.CWOSL], thus facilitating an entire sequence analysis in
#' one run. With this, its functionality is strictly limited by the
#' functionality provided by  [analyse_SAR.CWOSL].
#'
#' **Defining the sequence structure**
#'
#' The argument `sequence.structure` expects a shortened pattern of your sequence structure and was
#' mainly introduced to ease the use of the function. For example: If your measurement data contains
#' the following curves: `TL`, `IRSL`, `IRSL`, `TL`, `IRSL`, `IRSL`, the sequence pattern in `sequence.structure`
#' becomes `c('TL', 'IRSL', 'IRSL')`. The second part of your sequence for one cycle should be
#' similar and can be discarded. If this is not the case (e.g., additional hotbleach) such curves
#' have to be removed before using the function.
#'
#' **If the input is a `list`**
#'
#' If the input is a list of [RLum.Analysis-class] objects, every argument
#' can be provided as list to allow
#' for different sets of parameters for every single input element.
#' For further information see [analyse_SAR.CWOSL].
#'
#' @param object [RLum.Analysis-class] or [list] of [RLum.Analysis-class] objects (**required**):
#' input object containing data for analysis.
#' If a [list] is provided the functions tries to iterate over each element
#' in the list.
#'
#' @param signal.integral.min [integer] (**required**):
#' lower bound of the signal integral. Provide this value as vector for different
#' integration limits for the different IRSL curves.
#'
#' @param signal.integral.max [integer] (**required**):
#' upper bound of the signal integral. Provide this value as vector for different
#' integration limits for the different IRSL curves.
#'
#' @param background.integral.min [integer] (**required**):
#' lower bound of the background integral. Provide this value as vector for
#' different integration limits for the different IRSL curves.
#'
#' @param background.integral.max [integer] (**required**):
#' upper bound of the background integral. Provide this value as vector for
#' different integration limits for the different IRSL curves.
#'
#' @param dose.points [numeric] (*optional*):
#' a numeric vector containing the dose points values. Using this argument overwrites dose point
#' values in the signal curves.
#'
#' @param sequence.structure [vector] [character] (*with default*):
#' specifies the general sequence structure. Allowed values are `"TL"` and
#' any `"IR"` combination (e.g., `"IR50"`,`"pIRIR225"`).
#' Additionally a parameter `"EXCLUDE"` is allowed to exclude curves from
#' the analysis (Note: If a preheat without PMT measurement is used, i.e.
#' preheat as none TL, remove the TL step.)
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param plot_singlePanels [logical] (*with default*):
#' enable/disable plotting of the results in a single windows for each plot.
#' Ignored if `plot = FALSE`.
#'
#' @param ... further arguments that will be passed to
#' [analyse_SAR.CWOSL] and [plot_GrowthCurve]. Furthermore, the
#' arguments `main` (headers), `log` (IRSL curves), `cex` (control
#' the size) and `mtext.outer` (additional text on the plot area) can be passed to influence the plotting. If the input
#' is a list, `main` can be passed as [vector] or [list].
#'
#' @return
#' Plots (*optional*) and an [RLum.Results-class] object is
#' returned containing the following elements:
#'
#' \tabular{lll}{
#' **DATA.OBJECT** \tab **TYPE** \tab **DESCRIPTION** \cr
#' `..$data` : \tab  `data.frame` \tab Table with De values \cr
#' `..$LnLxTnTx.table` : \tab `data.frame` \tab with the `LnLxTnTx` values \cr
#' `..$rejection.criteria` : \tab [data.frame] \tab rejection criteria \cr
#' `..$Formula` : \tab [list] \tab Function used for fitting of the dose response curve \cr
#' `..$call` : \tab [call] \tab the original function call
#' }
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @note
#' Best graphical output can be achieved by using the function `pdf`
#' with the following options:
#'
#' `pdf(file = "<YOUR FILENAME>", height = 18, width = 18)`
#'
#' @section Function version: 0.2.5
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [analyse_SAR.CWOSL], [calc_OSLLxTxRatio], [plot_GrowthCurve],
#' [RLum.Analysis-class], [RLum.Results-class] [get_RLum]
#'
#' @references
#' Murray, A.S., Wintle, A.G., 2000. Luminescence dating of quartz
#' using an improved single-aliquot regenerative-dose protocol. Radiation
#' Measurements 32, 57-73. \doi{10.1016/S1350-4487(99)00253-X}
#'
#' Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. Laboratory
#' fading rates of various luminescence signals from feldspar-rich sediment
#' extracts. Radiation Measurements 43, 1474-1486.
#' \doi{10.1016/j.radmeas.2008.06.002}
#'
#' @keywords datagen plot
#'
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
#' object <- get_RLum(object, record.id = c(-29,-30))
#'
#' ##(d) Define new sequence structure and set new RLum.Analysis object
#' sequence.structure  <- c(1,2,2,3,4,4)
#' sequence.structure <- as.vector(sapply(seq(0,length(object)-1,by = 4),
#'                                        function(x){sequence.structure + x}))
#'
#' object <-  sapply(1:length(sequence.structure), function(x){
#'   object[[sequence.structure[x]]]
#' })
#'
#' object <- set_RLum(class = "RLum.Analysis", records = object, protocol = "pIRIR")
#'
#' ##(2) Perform pIRIR analysis (for this example with quartz OSL data!)
#' ## Note: output as single plots to avoid problems with this example
#' results <- analyse_pIRIRSequence(object,
#'      signal.integral.min = 1,
#'      signal.integral.max = 2,
#'      background.integral.min = 900,
#'      background.integral.max = 1000,
#'      fit.method = "EXP",
#'      sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
#'      main = "Pseudo pIRIR data set based on quartz OSL",
#'      plot_singlePanels = TRUE)
#'
#'
#' ##(3) Perform pIRIR analysis (for this example with quartz OSL data!)
#' ## Alternative for PDF output, uncomment and complete for usage
#' \dontrun{
#' tempfile <- tempfile(fileext = ".pdf")
#' pdf(file = tempfile, height = 18, width = 18)
#'   results <- analyse_pIRIRSequence(object,
#'          signal.integral.min = 1,
#'          signal.integral.max = 2,
#'          background.integral.min = 900,
#'          background.integral.max = 1000,
#'          fit.method = "EXP",
#'          main = "Pseudo pIRIR data set based on quartz OSL")
#'
#'   dev.off()
#' }
#'
#' @md
#' @export
analyse_pIRIRSequence <- function(
  object,
  signal.integral.min,
  signal.integral.max,
  background.integral.min,
  background.integral.max,
  dose.points = NULL,
  sequence.structure = c("TL", "IR50", "pIRIR225"),
  plot = TRUE,
  plot_singlePanels = FALSE,
  ...
) {
  .set_function_name("analyse_pIRIRSequence")
  on.exit(.unset_function_name(), add = TRUE)

# SELF CALL -----------------------------------------------------------------------------------
 if(is.list(object)){

   lapply(object, function(x) {
     .validate_class(x, "RLum.Analysis", name = "All elements of 'object'")
   })

    ## make life easy
    if(missing("signal.integral.min")){
      signal.integral.min <- 1
      .throw_warning("'signal.integral.min' missing, set to 1")
    }

    if(missing("signal.integral.max")){
      signal.integral.max <- 2
      .throw_warning("'signal.integral.max' missing, set to 2")
    }

   ## expand input arguments
   rep.length <- length(object)

   signal.integral.min <- .listify(signal.integral.min, rep.length)
   signal.integral.max <- .listify(signal.integral.max, rep.length)
   background.integral.min <- .listify(background.integral.min, rep.length)
   background.integral.max <- .listify(background.integral.max, rep.length)
   sequence.structure <- .listify(sequence.structure, rep.length)
   dose.points <- .listify(dose.points, rep.length)

   if ("main" %in% names(list(...))) {
     main_list <- .listify(list(...)$main, rep.length)
   }

   ## run analysis
   temp <- .warningCatcher(lapply(1:length(object), function(x) {
      analyse_pIRIRSequence(object[[x]],
                        signal.integral.min = signal.integral.min[[x]],
                        signal.integral.max = signal.integral.max[[x]],
                        background.integral.min = background.integral.min[[x]],
                        background.integral.max = background.integral.max[[x]] ,
                        dose.points = dose.points[[x]],
                        sequence.structure = sequence.structure[[x]],
                        plot = plot,
                        plot_singlePanels = plot_singlePanels,
                        main = ifelse("main"%in% names(list(...)), main_list[[x]], paste0("ALQ #",x)),
                        ...)
    }))

    ##combine everything to one RLum.Results object as this as what was written ... only
    ##one object

    ##merge results and check if the output became NULL
    results <- merge_RLum(temp)

    ##DO NOT use invisible here, this will stop the function from stopping
    if(length(results) == 0)
      return(NULL)
    else
      return(results)
  }

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "RLum.Analysis", extra = "'list'")
  .validate_logical_scalar(plot)
  .validate_logical_scalar(plot_singlePanels)

  ## there must be at least an IR step
  if (!any(grepl("IR", sequence.structure))) {
    .throw_error("'sequence.structure' should contain at least one IR step")
  }

  ## check allowed values in sequence structure
  temp.collect.invalid.terms <- .collapse(
      sequence.structure[!grepl("TL",  sequence.structure) &
                         !grepl("IR",  sequence.structure) &
                         !grepl("OSL", sequence.structure) &
                         !grepl("EXCLUDE", sequence.structure)])

  if (temp.collect.invalid.terms != "") {
    .throw_error(temp.collect.invalid.terms,
                 " not allowed in 'sequence.structure'")
  }

  ## deprecated argument
  if ("plot.single" %in% names(list(...))) {
    plot_singlePanels <- list(...)$plot.single
    .throw_warning("'plot.single' is deprecated, use 'plot_singlePanels' ",
                   "instead")
  }

  ## Enforce a minimum plot device size: this is necessary as otherwise users
  ## may experience "figure margins too large" errors when trying to draw all
  ## plots on a single page. We need to round the device size values because
  ## often they are values such as 15.99999999999 which would incorrectly
  ## trigger our check
  min.size <- 16
  dev.size <- round(grDevices::dev.size("in"), 5)
  if (plot && !plot_singlePanels && any(dev.size < min.size)) {
    plot <- FALSE
    msg <- paste0("Argument 'plot' reset to 'FALSE'. The smallest plot ",
                  "size required is IN x IN in.\nConsider plotting via ",
                  "`pdf(..., width = IN, height = IN)` ",
                  "or setting `plot_singlePanels = TRUE`")
    .throw_warning(gsub(x = msg, "IN", min.size))
  }

# Deal with extra arguments -------------------------------------------------------------------
  ## default values
  mtext.outer <- "MEASUREMENT INFO"
  main <- ""
  log <- ""
  cex <- 0.7

  ##deal with addition arguments
  extraArgs <- list(...)
  mtext.outer <- if ("mtext.outer" %in% names(extraArgs)) extraArgs$mtext.outer
  main <- if ("main" %in% names(extraArgs)) extraArgs$main
  log <- if ("log" %in% names(extraArgs)) extraArgs$log
  cex <- if ("cex" %in% names(extraArgs)) extraArgs$cex


# Protocol Integrity Checks --------------------------------------------------
  ##(1) Check structure and remove curves that fit not the recordType criteria

  ##get sequence structure
  temp.sequence.structure  <- structure_RLum(object)

  ##remove data types that fit not to the allowed values
  temp.sequence.rm.id <- temp.sequence.structure[
    (!grepl("TL",temp.sequence.structure[["recordType"]])) &
    (!grepl("OSL", temp.sequence.structure[["recordType"]])) &
    (!grepl("IRSL", temp.sequence.structure[["recordType"]]))
    ,"id"]

  if(length(temp.sequence.rm.id)>0){

  ##removed record from data set
  object <- get_RLum(object, record.id = -temp.sequence.rm.id,
        drop = FALSE
      )

  .throw_warning("The following unrecognised record types have been removed: ",
                 .collapse(temp.sequence.structure[temp.sequence.rm.id,
                                                   "recordType"]))
  }

  ##(2) Apply user sequence structure

  ##get sequence structure
  temp.sequence.structure  <- structure_RLum(object)

    ##try to account for a very common mistake
    if(any(grepl(sequence.structure, pattern = "TL", fixed = TRUE)) && !any(grepl(temp.sequence.structure[["recordType"]], pattern = "TL", fixed = TRUE))){
      .throw_warning("Your sequence does not contain 'TL' curves, trying ",
                     "to adapt 'sequence.structure' for you ...")
      sequence.structure <- sequence.structure[!grepl(sequence.structure, pattern = "TL", fixed = TRUE)]
    }

  ##set values to structure data.frame
  ##but check first
  if(2 * length(
    rep(sequence.structure, nrow(temp.sequence.structure)/2/length(sequence.structure))) == length(temp.sequence.structure[["protocol.step"]])){
    temp.sequence.structure[["protocol.step"]] <- rep(
      sequence.structure, nrow(temp.sequence.structure)/2/length(sequence.structure))

  }else{
    .throw_message("The number of records is not a multiple of the defined ",
                   "sequence structure, NULL returned")
    return(NULL)
  }

  ##remove values that have been excluded
  temp.sequence.rm.id <- temp.sequence.structure[
    temp.sequence.structure[,"protocol.step"] == "EXCLUDE" ,"id"]

  if(length(temp.sequence.rm.id)>0){

    ##remove from object
    object  <- get_RLum(
      object, record.id = -temp.sequence.rm.id, drop = FALSE)

    ##remove from sequence structure
    sequence.structure  <- sequence.structure[sequence.structure != "EXCLUDE"]

    ##set new structure
    temp.sequence.structure  <- structure_RLum(object)

    temp.sequence.structure[, "protocol.step"] <- rep(
      sequence.structure, nrow(temp.sequence.structure)/2/length(temp.sequence.structure))

    ##print warning message
    .throw_warning(length(temp.sequence.rm.id),
                   " records have been removed due to EXCLUDE")
  }

##============================================================================##
# Analyse data and plotting ----------------------------------------------------
##============================================================================##

  ##(1) find out how many runs are needed for the analysis by checking for "IR"
  ##    now should by every signal except the TL curves
  n.TL <- sum(grepl("TL", sequence.structure))
  n.loops <- as.numeric(length(sequence.structure) - n.TL)

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

  if (plot && !plot_singlePanels) {

    ##first (Tx,Tn, Lx,Ln)
    temp.IRSL.layout.vector.first <- c(3,5,6,7,3,5,6,8)

    ## middle (any other Lx,Ln)
    temp.IRSL.layout.vector.middle <- NULL
    if (n.loops > 2) {
    temp.IRSL.layout.vector.middle <-
      vapply(2:(n.loops - 1),
        FUN = function(x) 5 * x - 1 + c(0:3, 0:2, 4),
        FUN.VALUE = vector(mode = "numeric", length = 8)
      )
    }

    ## last (Lx, Ln and legend)
    temp.IRSL.layout.vector.last <- c(1, 2, 4, 5, 1, 2, 4, 6) +
      (if (n.loops > 2) max(temp.IRSL.layout.vector.middle)
       else max(temp.IRSL.layout.vector.first))

    temp.IRSL.layout.vector <- c(temp.IRSL.layout.vector.first,
                                 temp.IRSL.layout.vector.middle,
                                 temp.IRSL.layout.vector.last)

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
    matrix(layout.matrix,(max(layout.matrix)/2 +
                            ifelse(n.loops > 2, 0,2)), 4, byrow = TRUE),
     widths = c(rep(c(1,1,1,.75),6),c(1,1,1,1)),
     heights = c(rep(c(1),(2+2*n.loops)),c(0.20, 0.20)))

  ## show the regions that have been allocated to each plot for debug
  #layout.show(nf)
  }

  ##(1) INFO PLOT
  if (plot) {
    plot(NA,NA,
         ylim = c(0,1), xlab = "",
         xlim = c(0,1), ylab = "",
         axes = FALSE,
         main = main)

    text(0.5,0.5, paste(sequence.structure, collapse = "\n"), cex = cex *2)
  }


  ##(2) set loop
  for(i in 1:n.loops){

    ##compile record ids
    temp.id.sel <-
      sort(c(TL.curves.id, IRSL.curves.id[seq(i,length(IRSL.curves.id),by=n.loops)]))

    ##(a) select data set (TL curves has to be considered for the data set)
    temp.curves <- get_RLum(object, record.id = temp.id.sel, drop = FALSE)

    ##(b) grep integral limits as they might be different for different curves
    if(length(signal.integral.min)>1){

      temp.signal.integral.min <- signal.integral.min[i]
      temp.signal.integral.max <- signal.integral.max[i]
      temp.background.integral.min <- background.integral.min[i]
      temp.background.integral.max <- background.integral.max[i]

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
      plot_singlePanels = temp.plot.single,
      cex.global = cex,
      ...
    ) ##TODO should be replaced be useful explicit arguments

      ##check whether NULL was return
      if (is.null(temp.results)) {
        .throw_message("Analysis skipped: check your sequence, NULL returned")
        return(NULL)
      }

      ##add signal information to the protocol step
      temp.results.pIRIR.De <- as.data.frame(c(
        get_RLum(temp.results, "data"),
        data.frame(Signal = pIRIR.curve.names[i])
      ))

      temp.results.pIRIR.LnLxTnTx <- as.data.frame(c(
        get_RLum(temp.results, "LnLxTnTx.table"),
        data.frame(Signal = pIRIR.curve.names[i])
      ))

      temp.results.pIRIR.rejection.criteria <- as.data.frame(c(
        get_RLum(temp.results, "rejection.criteria"),
        data.frame(Signal = pIRIR.curve.names[i])
      ))

      temp.results.pIRIR.formula <- list(get_RLum(temp.results,
                                                  "Formula"))
      names(temp.results.pIRIR.formula)  <- pIRIR.curve.names[i]

      ##create now object
      temp.results  <- set_RLum(
        class = "RLum.Results",
        data = list(
          data = temp.results.pIRIR.De,
          LnLxTnTx.table = temp.results.pIRIR.LnLxTnTx,
          rejection.criteria = temp.results.pIRIR.rejection.criteria,
          Formula = temp.results.pIRIR.formula
        ),
        info = list(
          call = sys.call()
        )
      )

      ##merge results
      if (exists("temp.results.final")) {
        temp.results.final <- merge_RLum(list(temp.results.final, temp.results))

      } else{
        temp.results.final <- temp.results
      }
  }

##============================================================================##
# Plotting additional --------------------------------------------------------
##============================================================================##
if(plot){
  ##extract LnLnxTnTx.table
  LnLxTnTx.table <- get_RLum(temp.results.final, "LnLxTnTx.table")

  ## remove Inf
  LnLxTnTx.table$LxTx[is.infinite(LnLxTnTx.table$LxTx)] <- NA
  LnLxTnTx.table$LxTx.Error[is.infinite(LnLxTnTx.table$LxTx.Error)] <- NA

  ##plot growth curves
  min.LxTx <- min(LnLxTnTx.table$LxTx, na.rm = TRUE)
  max.LxTx <- max(LnLxTnTx.table$LxTx, na.rm = TRUE)
  max.LxTx.Error <- max(LnLxTnTx.table$LxTx.Error, na.rm = TRUE)
  plot(NA, NA,
       xlim = range(get_RLum(temp.results.final, "LnLxTnTx.table")$Dose),
       ylim = c(min(min.LxTx - max.LxTx.Error, 0), max.LxTx + max.LxTx.Error),
       xlab = "Dose [s]",
       ylab = expression(L[x]/T[x]),
       main = "Summarised Dose Response Curves")

    ##set x for expression evaluation
    x <- seq(0,max(LnLxTnTx.table$Dose)*1.05, length.out = 100)

    for(j in 1:length(pIRIR.curve.names)){
     ##dose points
     temp.curve.points <-  LnLxTnTx.table[,c("Dose", "LxTx", "LxTx.Error", "Signal")]

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
     lines(c(0, get_RLum(temp.results.final, "data")[j,1]),
           c(temp.curve.points[1,c("LxTx")], temp.curve.points[1,c("LxTx")]),
           col = j,
           lty = 2)

     lines(c(rep(get_RLum(temp.results.final, "data")[j,1], 2)),
           c(temp.curve.points[1,c("LxTx")], 0),
           col = j,
           lty = 2)

     ##curve
     temp.curve.formula  <- get_RLum(
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
    temp.curve.TnTx <- LnLxTnTx.table[, c("TnTx", "Signal")]

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
       xlim = c(0,nrow(LnLxTnTx.table)/
                     n.loops),
       ylim = if (anyNA(range(temp.curve.TnTx.matrix))) c(0,1) else range(temp.curve.TnTx.matrix),
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
   temp.rejection.criteria <- get_RLum(temp.results.final,
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
     if(length(temp.rc.palaedose.error[i, "Value"]) > 0 && !is.na(temp.rc.palaedose.error[i, "Value"]))
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
  if (!plot_singlePanels) {
    par(def.par)
  }

}##end plot == TRUE


##============================================================================##
# Return Values -----------------------------------------------------------
##============================================================================##

  return(temp.results.final)
}
