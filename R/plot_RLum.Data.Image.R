#' Plot function for an `RLum.Data.Image` S4 class object
#'
#' The function provides a standardised plot output for image data of an
#' `RLum.Data.Image`S4 class object, mainly using the plot functions
#' provided by the [raster] package.
#'
#' **Details on the plot functions**
#'
#' Image is visualised as 2D plot using generic plot types provided by other
#' packages.
#'
#' Supported plot types:
#'
#' **`plot.type = "plot.raster"`**
#'
#' Uses the standard plot function for raster data from the package
#' [raster::raster]: [raster::plot]. For each raster layer in a
#' raster brick one plot is produced.
#'
#' Arguments that are passed through the function call:
#'
#' `main`,`axes`, `xlab`, `ylab`, `xlim`, `ylim`,
#' `col`
#'
#' **`plot.type = "plotRGB"`**
#'
#' Uses the function [raster::plotRGB] from the
#' [raster::raster] package. Only one image plot is produced as all layers
#' in a brick a combined.  This plot type is useful to see whether any signal
#' is recorded by the camera.\cr
#' Arguments that are passed through the function call:
#'
#' `main`,`axes`, `xlab`, `ylab`, `ext`, `interpolate`, `maxpixels`,
#' `alpha`, `colNA`, `stretch`
#'
#' **`plot.type = "contour"`**
#'
#' Uses the function contour plot function from the [raster]
#' function ([raster::contour]). For each raster layer one contour
#' plot is produced. Arguments that are passed through the function call:\cr
#'
#' `main`,`axes`, `xlab`, `ylab`, `xlim`, `ylim`,
#' `col`
#'
#' @param object [RLum.Data.Image-class] (**required**): S4
#' object of class `RLum.Data.Image`
#'
#' @param par.local [logical] (*with default*): use local graphical
#' parameters for plotting, e.g. the plot is shown in one column and one row.
#' If `par.local = FALSE` global parameters are inherited.
#'
#' @param plot.type [character] (*with default*): plot types.
#' Supported types are `plot.raster`, `plotRGB` or `contour`
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the specific plot functions.
#'
#' @return Returns a plot.
#'
#' @note
#' This function has been created to facilitate the plotting of image data
#' imported by the function [read_SPE2R]. However, so far the
#' function is not optimized to handle image data > ca. 200 MByte and thus
#' plotting of such data is extremely slow.
#'
#' @section Function version: 0.1
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [RLum.Data.Image-class], [plot], [plot_RLum], [raster::raster]
#'
#' @keywords aplot
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.RLum.Data.Image, envir = environment())
#'
#' ##plot data
#' plot_RLum.Data.Image(ExampleData.RLum.Data.Image)
#'
#' @md
#' @export
plot_RLum.Data.Image <- function(
  object,
  par.local = TRUE,
  plot.type = "plot.raster",
  ...
){


  # Integrity check -----------------------------------------------------------

  ##check if object is of class RLum.Data.Image
  if(class(object) != "RLum.Data.Image"){

    stop("[plot_RLum.Data.Image()] Input object is not of type RLum.Data.Image")

  }

  ##deal with addition arguments
  extraArgs <- list(...)

  ##TODO
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {"RLum.Data.Image"}

  axes <- if("axes" %in% names(extraArgs)) {extraArgs$axes} else
  {TRUE}

  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else
  {"Length [px]"}

  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else
  {"Height [px]"}

  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else
  {c(0,dim(get_RLum(object))[2])}

  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else
  {c(0,dim(get_RLum(object))[1])}

  ##plotRGB::ext
  ext <- if("ext" %in% names(extraArgs)) {extraArgs$ext} else
  {NULL}

  ##plotRGB::interpolate
  interpolate <- if("interpolate" %in% names(extraArgs)) {extraArgs$interpolate} else
  {FALSE}

  ##plotRGB::stretch
  stretch <- if("stretch" %in% names(extraArgs)) {extraArgs$stretch} else
  {"hist"}

  ##plotRGB::maxpixels
  maxpixels <- if("maxpixels" %in% names(extraArgs)) {extraArgs$maxpixels} else
  {dim(get_RLum(object))[1]*dim(get_RLum(object))[2]}

  ##plotRGB::alpha
  alpha <- if("alpha" %in% names(extraArgs)) {extraArgs$alpha} else
  {255}

  ##plotRGB::colNA
  colNA <- if("colNA" %in% names(extraArgs)) {extraArgs$colNA} else
  {"white"}

  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else
  {topo.colors(255)}

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else
  {1}

  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local == TRUE){

    par(mfrow=c(1,1), cex = cex)

  }

  ##grep raster

  if(plot.type == "plotRGB"){
    ## ==========================================================================#
    ## standard raster plotRGB (package raster)
    ## ==========================================================================#

    raster::plotRGB(
      get_RLum(object),
      main = main,
      axes = TRUE,
      xlab = xlab,
      ylab = ylab,
      ext = ext,
      interpolate = interpolate,
      maxpixels = maxpixels,
      alpha = alpha,
      colNA = colNA,
      stretch = stretch)


    ## ==========================================================================#
    ## standard raster plot (package raster)
    ## ==========================================================================#
  }else if(plot.type == "plot.raster"){

    plot(get_RLum(object),
         main = main,
         xlim = xlim,
         ylim = ylim,
         xlab = xlab,
         ylab = ylab,
         col = col)

    ## ==========================================================================#
    ## standard contour (package raster)
    ## ==========================================================================#
  }else if(plot.type == "contour"){

    for(i in 1:raster::nlayers(get_RLum(object))){


      raster::contour(raster::raster(get_RLum(object), layer = i),
                      main = main,
                      xlim = xlim,
                      ylim = ylim,
                      xlab = xlab,
                      ylab = ylab,
                      col = col)

    }

  }else{

    stop("[plot_RLum.Data.Image()] Unknown plot type.")

  }

}
