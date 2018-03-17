#'Create a Dose-Response Curve Summary Plot
#'
#'While analysing OSL SAR or pIRIR data the view on the data is limited usually to one
#'dose-response curve (DRC) at the time for one aliquot. This function overcomes this limitation
#'by plotting all DRC from an [RLum.Results-class] object created by the function [analyse_SAR.CWOSL]
#'in one single plot.
#'
#'If you want plot your DRC on an energy scale (dose in Gy), you should run the SAR analysis with the
#'dose points in Gy. If you need more options than provided with this function, please have
#'a look into the source code to create and modify an own R script.
#'
#'@param object [RLum.Results-class] object (**required**): input object created by the function [analyse_SAR.CWOSL]
#'
#'@param sel_curves [numeric] (optional): id of the curves to be plotting in its occuring order. A sequence can
#'be provided for selecting, e.g., only every 2nd curve from the input object
#'
#'@param show_dose_points [logical] (with default): enable or disable plot of dose points in the graph
#'
#'@param show_natural [logical] (with default): enable or disable the plot of the natural Lx/Tx values
#'
#'@param n [integer] (with default): the number of x-values used to evaluate one curve object. Large numbers slow
#'down the plotting process and are usually not needed
#'
#'@param ... Further arguments and graphical parameters to be passed.
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
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
  sel_curves = NULL,
  show_dose_points = FALSE,
  show_natural = FALSE,
  n = 51L,
  ...
){


# Check input ---------------------------------------------------------------------------------
  if(class(object) != "RLum.Results")
    stop("[plot_DRCSummary()] The input is not of class 'RLum.Results'!",call. = FALSE)


# Extract data from object --------------------------------------------------------------------

  ##get data from RLum.Results object
  if(object@originator == "analyse_SAR.CWOSL"){
    ##set limit
    if(is.null(sel_curves)){
      sel_curves <- 1:length(object@data$Formula)

    }else{
      if(min(sel_curves) < 1 ||
         max(sel_curves) > length(object@data$Formula) ||
         length(sel_curves) > length(object@data$Formula)){
        warning("[plot_DRCSummary()] 'sel_curves' out of bounds, reset to full dataset.", call. = FALSE, immediate. = TRUE)
        sel_curves <- 1:length(object@data$Formula)
      }

    }

    ##get DRC
    DRC <- object@data$Formula[sel_curves]

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
    stop(
      "[plot_DRCSummary()] 'object' was created by none supported function, cf. manual for allowed originators!",call. = FALSE)


  }

# Plotting ------------------------------------------------------------------------------------

  ##set default
  plot_settings <- list(
    xlab = "Dose [s]",
    ylab = expression(L[x]/T[x]),
    xlim = c(0,max(vapply(LxTx, function(x){max(x[["Dose"]])}, numeric(1)))),
    ylim = if(show_dose_points){
      c(0,max(vapply(LxTx, function(x){max(x[["LxTx"]] + x[["LxTx.Error"]])}, numeric(1))))
    }else{
      c(0,max(vapply(1:length(LxTx), function(y){
        x <- max(LxTx[[y]][["Dose"]])
        eval(DRC[[y]])
       },numeric(1))))
    },
    main = "DRC Summary",
    lty = 1,
    pch = rep(20,length(sel_curves)),
    col = rep(rgb(0,0,0,0.5), length(sel_curves))
  )

  ##modify on request
  plot_settings <- modifyList(x = plot_settings, val = list(...))

  ##create empty plot window
  plot(
    x = NA,
    y = NA,
    xlab = plot_settings$xlab,
    ylab = plot_settings$ylab,
    xlim = plot_settings$xlim,
    ylim = plot_settings$ylim,
    main = plot_settings$main
  )

  for(i in 1:length(sel_curves)){
    ##plot natural
    if(show_natural){
      segments(x0 = LxTx[[i]]$Dose[1], x1 = LxTx[[i]]$Dose[1],
               y0 = LxTx[[i]]$LxTx[1] - LxTx[[i]]$LxTx.Error[1],
               y1 = LxTx[[i]]$LxTx[1] + LxTx[[i]]$LxTx.Error[1],
               col = plot_settings$col[[i]])
      points(
        x = LxTx[[i]]$Dose[1],
        y = LxTx[[i]]$LxTx[1],
        col = plot_settings$col[[i]],
        pch = plot_settings$pch[[i]]
      )

    }

    ##plot dose points
    if(show_dose_points){
      segments(x0 = LxTx[[i]]$Dose[-1], x1 = LxTx[[i]]$Dose[-1],
               y0 = LxTx[[i]]$LxTx[-1] - LxTx[[i]]$LxTx.Error[-1],
               y1 = LxTx[[i]]$LxTx[-1] + LxTx[[i]]$LxTx.Error[-1],
               col = plot_settings$col[[i]])
      points(
        x = LxTx[[i]]$Dose[-1],
        y = LxTx[[i]]$LxTx[-1],
        col = plot_settings$col[[i]],
        pch = plot_settings$pch[[i]]
      )

    }

    ##plot lines
    x <- seq(min(plot_settings$xlim),max(plot_settings$xlim), length.out = n)
    lines(x = x, y = eval(DRC[[i]]), col = plot_settings$col[[i]])

  }

}
