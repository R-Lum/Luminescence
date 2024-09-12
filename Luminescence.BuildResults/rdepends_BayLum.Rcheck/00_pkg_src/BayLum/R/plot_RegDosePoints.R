#'@title Plot Regeneration Dose Points
#'
#'@description
#'Simple plot functionality to visualise $L_x/T_x$ values against the dose extracted
#'from data created by [create_DataFile]
#'
#'@param object [list] (**required**): input object created by [create_DataFile]
#'
#'@param nrow [integer] (*with default*): number of rows used for the plot panel
#'
#'@param ncol [integer] (*with default*): number of columns in the plot panel
#'
#'@param ... further plot arguments passed down to modify the plot output. Supported
#'arguments are `xlab`, `ylab`, `type`, `pch`, `col`, `cex`
#'
#'@return The function returns a plot
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [create_DataFile]
#'
#'@examples
#'
#'data(DATA3,envir = environment())
#'plot_RegDosePoints(DATA3)
#'
#'@md
#'@export
plot_RegDosePoints <- function(
    object,
    nrow = 3L,
    ncol = nrow,
    ...
){
  # Check input -------------------------------------------------------------
  if(is.null(attr(object, "originator")) || attr(object, "originator") == "create_DateFile")
    stop("[plot_RegDosePoints()] Unsupported input. Supported are only objects created by 'create_DataFile()'!", 
         call. = FALSE)
  
  # Plot data ---------------------------------------------------------------
  par_old <- par(no.readonly = TRUE)
  on.exit(par(par_old))
  
  plot_settings <- modifyList(
    x = list(
      xlab = "Dose [Gy]",
      ylab = expression(paste(L[x]/T[x])), 
      type = "p", 
      pch = 1, 
      col = "black",
      cex = 0.8
    ), val = list(...))
  
  
  par(mfrow = c(nrow, ncol), cex = plot_settings$cex)
  for(i in seq_along(object$Nb_sample)) {
    for(j in 1:nrow(object$LT[[i]])) {
      ## plot graphs
      plot(
        x = object$regDose[[i]][j, ], 
        y = object$LT[[i]][j, -1],
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        type = plot_settings$type, 
        pch = plot_settings$pch,
        col = plot_settings$col,
        main = paste0(object$SampleNames[i], " | ALQ: ",j)
      )
      ## add segments
      segments(
        x0 = object$regDose[[i]][j, ], 
        x1 = object$regDose[[i]][j, ],
        y0 = object$LT[[i]][j, -1] - object$sLT[[i]][j, -1],
        y1 = object$LT[[i]][j, -1] + object$sLT[[i]][j, -1])
    }
    
  }
  
  
}
