#'Create a Dose-Response Curve Summary Plot
#'
#'While analysing OSL SAR or pIRIR-data the view on the data is limited usually to one
#'dose-response curve (DRC) at the time for one aliquot. This function overcomes this limitation
#'by plotting all DRC from an [RLum.Results-class] object created by the function [analyse_SAR.CWOSL]
#'in one single plot.
#'
#'If you want plot your DRC on an energy scale (dose in Gy), you can either use the option `source_dose_rate` provided
#'below or your can SAR analysis with the dose points in Gy (better axis scaling).
#'
#'@param object [RLum.Results-class] object (**required**): input object created by the function [analyse_SAR.CWOSL]. The input object can be provided as [list].
#'
#'@param source_dose_rate [numeric] (*optional*): allows to modify the axis and show values in Gy, instead seconds. Only a single numerical value is allowed.
#'
#'@param sel_curves [numeric] (optional): id of the curves to be plotting in its occurring order. A sequence can
#'be provided for selecting, e.g., only every 2nd curve from the input object
#'
#'@param show_dose_points [logical] (with default): enable or disable plot of dose points in the graph
#'
#'@param show_natural [logical] (with default): enable or disable the plot of the natural `Lx/Tx` values
#'
#'@param n [integer] (with default): the number of x-values used to evaluate one curve object. Large numbers slow
#'down the plotting process and are usually not needed
#'
#'@param ... Further arguments and graphical parameters to be passed. In particular: `main`, `xlab`, `ylab`, `xlim`, `ylim`, `lty`, `lwd`, `pch`, `col.pch`, `col.lty`, `mtext`
#'
#'@section Function version: 0.2.3
#'
#'@return An [RLum.Results-class] object is returned:
#'
#' Slot: **@data**\cr
#'
#' \tabular{lll}{
#' **OBJECT** \tab **TYPE** \tab **COMMENT**\cr
#' `results` \tab [data.frame] \tab with dose and LxTx values \cr
#' `data` \tab [RLum.Results-class] \tab original input data \cr
#' }
#'
#' Slot: **@info**\cr
#'
#' \tabular{lll}{
#' **OBJECT** \tab **TYPE** \tab **COMMENT** \cr
#' `call` \tab `call` \tab the original function call \cr
#' `args` \tab `list` \tab arguments of the original function call \cr
#' }
#'
#'*Note: If the input object is a [list] a list of [RLum.Results-class] objects is returned.*
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#'@seealso [RLum.Results-class], [analyse_SAR.CWOSL]
#'
#'@examples
#'
#'#load data example data
#'data(ExampleData.BINfileData, envir = environment())
#'
#'#transform the values from the first position in a RLum.Analysis object
#'object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
##perform SAR analysis
#' results <- analyse_SAR.CWOSL(
#'   object = object,
#'   signal.integral.min = 1,
#'   signal.integral.max = 2,
#'    background.integral.min = 900,
#'    background.integral.max = 1000,
#'    plot = FALSE
#'  )
#'
#'##plot only DRC
#'plot_DRCSummary(results)
#'
#'@md
#'@export
plot_DRCSummary <- function(
  object,
  source_dose_rate = NULL,
  sel_curves = NULL,
  show_dose_points = FALSE,
  show_natural = FALSE,
  n = 51L,
  ...
) {
  .set_function_name("plot_DRCSummary")
  on.exit(.unset_function_name(), add = TRUE)

# Self-call -----------------------------------------------------------------------------------
if(inherits(object, "list")){

  ##catch ... arguments
  plot_settings <- list(...)

  ##expand arguments
  if("main" %in% names(list(...))){
    main <- as.list(rep(list(...)[["main"]], length(object)))

    ##filter main from the ... argument list otherwise we will have a collusion
    plot_settings["main" %in% names(plot_settings)] <- NULL

  }else{
    main <- as.list(rep("DRC", length(object)))
  }

  results <- lapply(1:length(object), function(o){
    plot_DRCSummary(
      object = object[[o]],
      source_dose_rate = source_dose_rate,
      sel_curves = sel_curves,
      show_dose_points = show_dose_points,
      show_natural = show_natural,
      n = n,
      main = main[[o]],
      ... = plot_settings
      )
  })

  ##return merged object
  return(results)

}

# Check input ---------------------------------------------------------------------------------
  if(!inherits(object, "RLum.Results"))
    .throw_error("'object' is not of class 'RLum.Results'")

# Extract data from object --------------------------------------------------------------------
  ##get data from RLum.Results object
  if(object@originator %in% c("analyse_SAR.CWOSL", "analyse_pIRIRSequence")){
    ##set limit
    if(is.null(sel_curves)){
      sel_curves <- 1:length(object@data$Formula)

    }else{
      if(min(sel_curves) < 1 ||
         max(sel_curves) > length(object@data$Formula) ||
         length(sel_curves) > length(object@data$Formula)){
        .throw_warning("'sel_curves' out of bounds, reset to full dataset")
        sel_curves <- 1:length(object@data$Formula)
      }

    }

    ## check the whether the fitting was all the same
    if(length(unique(object@data[["data"]][["Fit"]])) != 1)
      .throw_error("I can only visualise dose-response curves based ",
                   "on the same fitting equation")

    ##get DRC
    DRC <- object@data$Formula[sel_curves]

    ## check for Lambert W function (we can only do all )
    if(all(object@data$data[["Fit"]] == "LambertW"))
      W <- lamW::lambertW0

    ##get limits for each set
    dataset_limits <- matrix(
      c(which(object@data$LnLxTnTx.table[["Name"]] == "Natural"),
        which(object@data$LnLxTnTx.table[["Name"]] == "Natural")[-1] - 1, nrow(object@data$LnLxTnTx.table)),
      ncol = 2)

   ##create list
   LxTx <- lapply(1:nrow(dataset_limits), function(x){
     object@data$LnLxTnTx.table[dataset_limits[x,1]:dataset_limits[x,2],]

   })[sel_curves]

  }else{
    .throw_error("'object' was not created by a supported function, ",
                 "see the manual for allowed originators")
  }

# Plotting ------------------------------------------------------------------------------------
  ##set default
  plot_settings <-  modifyList(x = list(
    xlab = if(is.null(source_dose_rate)) {"Dose [s]"} else {"Dose [Gy]"},
    ylab = expression(L[x]/T[x]),
    xlim = c(0,max(vapply(LxTx, function(x){max(x[["Dose"]])}, numeric(1)))),
    ylim = if(show_dose_points){
      c(0,max(vapply(LxTx, function(x){max(x[["LxTx"]] + x[["LxTx.Error"]])}, numeric(1)), na.rm = TRUE))
    }else{
      c(0,max(vapply(1:length(LxTx), function(y){
        x <- max(LxTx[[y]][["Dose"]], na.rm = TRUE)
        eval(DRC[[y]])

       },numeric(1)), na.rm = TRUE))
    },
    main = "DRC Summary",
    mtext = paste0("n_curves: ",length(sel_curves)),
    lty = 1,
    lwd = 1,
    pch = 20,
    col.lty = rgb(0,0,0,0.5),
    col.pch = rgb(0,0,0,0.5)
  ), val = list(...), keep.null = TRUE)

  ## expand parameters
  plot_settings$col.lty <- rep(plot_settings$col.lty, length(sel_curves))
  plot_settings$col.pch <- rep(plot_settings$col.pch, length(sel_curves))
  plot_settings$pch <- rep(plot_settings$pch, length(sel_curves))
  plot_settings$lty <- rep(plot_settings$lty, length(sel_curves))

  ##create empty plot window
  plot(
    x = NA,
    y = NA,
    xlab = plot_settings$xlab,
    ylab = plot_settings$ylab,
    xlim = plot_settings$xlim,
    ylim = plot_settings$ylim,
    main = plot_settings$main,
    xaxt = "n"
  )

  if(!is.null(plot_settings$mtext))
    mtext(side = 3, text = plot_settings$mtext, cex = 0.8)

  #exchange x-axis if source dose rate is set
  if(!is.null(source_dose_rate)){
    axis(side = 1, at = axTicks(side = 1), labels = round(axTicks(side = 1) * source_dose_rate[1],0))

  }else{
    axis(side = 1)

  }

  for(i in 1:length(sel_curves)){
    ##plot natural
    if(show_natural){
      segments(x0 = LxTx[[i]]$Dose[1], x1 = LxTx[[i]]$Dose[1],
               y0 = LxTx[[i]]$LxTx[1] - LxTx[[i]]$LxTx.Error[1],
               y1 = LxTx[[i]]$LxTx[1] + LxTx[[i]]$LxTx.Error[1],
               col = plot_settings$col.pch[[i]])
      points(
        x = LxTx[[i]]$Dose[1],
        y = LxTx[[i]]$LxTx[1],
        col = plot_settings$col.pch[[i]],
        pch = plot_settings$pch[[i]]
      )
    }

    ##plot dose points
    if(show_dose_points){
      segments(x0 = LxTx[[i]]$Dose[-1], x1 = LxTx[[i]]$Dose[-1],
               y0 = LxTx[[i]]$LxTx[-1] - LxTx[[i]]$LxTx.Error[-1],
               y1 = LxTx[[i]]$LxTx[-1] + LxTx[[i]]$LxTx.Error[-1],
               col = plot_settings$col.pch[[i]])
      points(
        x = LxTx[[i]]$Dose[-1],
        y = LxTx[[i]]$LxTx[-1],
        col = plot_settings$col.pch[[i]],
        pch = plot_settings$pch[[i]]
      )

    }

    ##plot lines
    x <- seq(min(plot_settings$xlim),max(plot_settings$xlim), length.out = n)
    y <- eval(DRC[[i]])

    if (any(is.na(y)) || any(is.nan(y))) {
      .throw_warning("Dose response curve ", i,
                     " is NA/NaN and was removed before plotting")
      next
    }

    lines(
      x = x,
      y = eval(DRC[[i]]),
      col = plot_settings$col.lty[[i]],
      lwd = plot_settings$lwd,
      lty = plot_settings$lty[[i]]
    )
  }

  ## Results -------------------------------------------------------------------
  results <- set_RLum(
    class = "RLum.Results",
    data = list(
      results = data.frame(
        dose = x,
        sapply(DRC, function(d, n) { eval(d) }, n)
        ),
      data = object
    ),
    info = list(
      call = sys.call(),
      args = as.list(sys.call())[-1])
  )

  ## Return value --------------------------------------------------------------
  return(results)
}
